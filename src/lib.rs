#[macro_use]
extern crate bitflags;
#[macro_use]
extern crate lazy_static;
extern crate kana;
extern crate libc;

mod convert;
mod fcitx;
mod keyboard;
mod tc_table;

use libc::{c_int, c_uint};
use std::collections::HashMap;
use std::iter::Iterator;

use convert::*;
use keyboard::{KeyPos, QWERTY};

use fcitx::key::{self, KeySym};
use fcitx::{CapacityFlags, FcitxInstance, IMClass, IMInstance, InputReturnValue, MessageType};

declare_imclass!(TCodeIMClass, TCodeIMClass::new);

struct TCodeIMClass(FcitxTCode);
impl TCodeIMClass {
    fn new(fcitx: FcitxInstance) -> Self {
        TCodeIMClass(FcitxTCode::new(fcitx))
    }
}

impl fcitx::IMClass for TCodeIMClass {
    fn create(&mut self, mut fcitx: FcitxInstance) {
        fcitx.im_register(&mut self.0, "tcode", "T-Code", "tcode", 1, "ja")
    }
    fn destroy(&mut self) {}
}

#[derive(PartialEq)]
enum InputMode {
    DIRECT,
    TCODE,
    CONVERT,
}
impl Default for InputMode {
    fn default() -> InputMode {
        InputMode::DIRECT
    }
}

enum ConvertMode {
    EXACT,
    #[allow(dead_code)]
    INFLECTION,
}

struct ConvertInfo {
    start: usize,
    kanjis: Vec<String>,
    #[allow(dead_code)]
    mode: ConvertMode,
}
struct FcitxTCode {
    fcitx: fcitx::FcitxInstance,
    input: fcitx::InputState,
    candidate_list: fcitx::CandidateWordList,
    select_chars: String,
    preedit: String, // TODO: use fcitx RawInputBuffer?
    preedit_backup: String,
    last_key: Option<KeyPos>,
    kana_count: usize,
    mode: InputMode,
    last_is_space: bool,
    mazegaki_dict: Dict,
    convert_info: Option<ConvertInfo>,
}

impl FcitxTCode {
    const MAX_BYTES_PER_CHAR: usize = 5;
    const MAX_BUFFER_CHARS: usize = 10;
    fn new(fcitx: fcitx::FcitxInstance) -> Self {
        let buffer_size = Self::MAX_BYTES_PER_CHAR * Self::MAX_BUFFER_CHARS;
        let input = fcitx.input_state();
        Self {
            fcitx: fcitx,
            input: input,
            candidate_list: input.candidate_list(),
            // TODO: query QWERTY table
            select_chars: "asdfghjkl;".to_string(),
            preedit: String::with_capacity(buffer_size),
            preedit_backup: String::with_capacity(buffer_size),
            last_key: None,
            kana_count: 0,
            mode: InputMode::DIRECT,
            last_is_space: false,
            mazegaki_dict: HashMap::new(),
            convert_info: None,
        }
    }
}

#[derive(Debug, Clone)]
enum ConvertError {
    InsufficientBuffer,
    NoMatching,
}

impl FcitxTCode {
    fn support_client_preedit(&self) -> bool {
        self.fcitx
            .current_input_context()
            .map_or(false, |ic| ic.caps(CapacityFlags::PREEDIT))
    }

    fn preedit_target(&self) -> fcitx::Messages {
        let input = &self.input;
        if self.support_client_preedit() {
            input.client_preedit()
        } else {
            input.preedit()
        }
    }

    fn update_preedit(&self) {
        self.fcitx.clean_input_window_up();

        let cursor_pos = match self.mode {
            InputMode::DIRECT => self.set_preedit_direct(),
            InputMode::TCODE => self.set_preedit_tcode(),
            InputMode::CONVERT => self.set_preedit_converting(),
        };

        let input = &self.input;
        input.set_cursor_pos(cursor_pos as c_int);
        input.set_client_cursor_pos(cursor_pos as c_int);
        self.fcitx.update_input_window();
    }

    fn set_preedit_direct(&self) -> usize {
        0
    }

    fn set_preedit_tcode(&self) -> usize {
        let txt = &self.preedit;
        if !txt.is_empty() {
            self.preedit_target()
                .add_message_at_last(MessageType::INPUT, txt);
        }
        txt.len()
    }

    fn set_preedit_converting(&self) -> usize {
        let (pretxt, posttxt) = self.get_coverting_txt();

        let target = self.preedit_target();

        let cursor_pos = pretxt.len();
        if cursor_pos > 0 {
            target.add_message_at_last(MessageType::INPUT, &pretxt)
        }
        target.add_message_at_last(MessageType::FIRSTCAND | MessageType::HIGHLIGHT, &posttxt);

        pretxt.len()
    }

    fn is_hotkey(&self, hotkey: (KeySym, KeySym), pos1: KeyPos) -> bool {
        self.last_key.map_or(false, |pos0| {
            let hotkey_pos = (
                *QWERTY.get(&hotkey.0).unwrap(),
                *QWERTY.get(&hotkey.1).unwrap(),
            );
            hotkey_pos == (pos0, pos1)
        })
    }

    fn push_key(&mut self, key1: KeyPos) -> Option<char> {
        if self.last_key.is_none() {
            self.last_key = Some(key1);
            return None;
        }

        let key0 = self.last_key.unwrap();
        self.last_key = None;
        let c = lookup_tcode((key0, key1))?;
        self.preedit.push(c);
        if self.preedit.chars().count() == Self::MAX_BUFFER_CHARS {
            Some(self.preedit.remove(0))
        } else {
            None
        }
    }

    fn clear_keys(&mut self) {
        self.kana_count = 0;
        self.last_key = None;
        self.preedit.clear();
        self.convert_info = None;
    }

    fn reset(&mut self) {
        self.clear_keys();
        self.candidate_list.clear();
        self.update_preedit();
    }

    fn start_mazegaki_convertion(&mut self) -> InputReturnValue {
        let conv = self.mazegaki_convert(0..);
        if conv.is_some() {
            self.convert_info = conv;
            self.mode = InputMode::CONVERT;
            InputReturnValue::DISPLAY_CANDWORDS
        } else {
            InputReturnValue::DISPLAY_MESSAGE
        }
    }

    fn get_coverting_txt(&self) -> (String, String) {
        assert!(self.convert_info.is_some());
        let info = self.convert_info.as_ref().unwrap();
        let iter = self.preedit.chars();
        let pretxt = iter.take((*info).start).collect::<String>();
        let posttxt = if info.kanjis.len() == 1 {
            info.kanjis[0].clone()
        } else {
            let iter = self.preedit.chars();
            iter.skip((*info).start).collect::<String>()
        };

        (pretxt, posttxt)
    }

    fn mazegaki_convert_at(&self, from: usize) -> Result<Vec<String>, ConvertError> {
        // TODO: use char_indices?
        let txt = self.preedit.chars().skip(from).collect::<String>();
        if txt.is_empty() {
            return Err(ConvertError::InsufficientBuffer);
        }
        self.mazegaki_dict
            .get(&txt)
            .map(|x| x.clone())
            .ok_or(ConvertError::NoMatching)
    }

    // TODO: use reference
    fn mazegaki_convert<'a, I>(&mut self, iter: I) -> Option<ConvertInfo>
    where
        I: Iterator<Item = usize>,
    {
        for i in iter {
            match self.mazegaki_convert_at(i) {
                Err(ConvertError::InsufficientBuffer) => return None,
                Err(ConvertError::NoMatching) => continue,
                Ok(vec) => {
                    return Some(ConvertInfo {
                        start: i,
                        kanjis: vec.clone(),
                        mode: ConvertMode::EXACT,
                    })
                }
            }
        }
        None
    }

    fn do_input_direct(&mut self, keysym: KeySym, _state: c_uint) -> InputReturnValue {
        // electric switching
        if self.last_is_space && keysym == key::Key_comma {
            self.mode = InputMode::TCODE;
            self.fcitx
                .forward_key(fcitx::KeyEvent::PressKey, key::Key_BackSpace, 0);
            return InputReturnValue::FLAG_BLOCK_FOLLOWING_PROCESS;
        }

        self.last_is_space = keysym == key::Key_space;
        return InputReturnValue::FLAG_FORWARD_KEY;
    }

    fn do_input_convert(&mut self, keysym: KeySym, _state: c_uint) -> InputReturnValue {
        // hot key
        match keysym {
            // TODO: page move
            key::Key_space => return InputReturnValue::FLAG_BLOCK_FOLLOWING_PROCESS,
            key::Key_BackSpace => return InputReturnValue::FLAG_BLOCK_FOLLOWING_PROCESS,
            key::Key_less | key::Key_greater => {
                let pos = self.convert_info.as_ref().map_or(0, |x| x.start);
                let conv = if keysym == key::Key_less {
                    self.mazegaki_convert((0..pos).rev())
                } else {
                    self.mazegaki_convert((pos + 1)..)
                };
                if conv.is_some() {
                    self.convert_info = conv;
                    return InputReturnValue::DISPLAY_CANDWORDS;
                }
                return InputReturnValue::FLAG_BLOCK_FOLLOWING_PROCESS;
            }
            _ => (),
        };

        let (pretxt, posttxt) = self.get_coverting_txt();
        let to_commit = match keysym {
            key::Key_Return => Some(pretxt + &posttxt),
            // TODO: rewrite
            key::Key_a..=key::Key_z | key::Key_semicolon => match QWERTY.get(&keysym) {
                None => None,
                Some(n) => {
                    // The candidate selecting line.
                    const CANDIDATE_START: u8 = 20;
                    const CANDIDATE_MAX: u8 = 30;
                    if CANDIDATE_START <= *n && *n < CANDIDATE_MAX {
                        match self.convert_info {
                            None => None,
                            Some(ref info) => info
                                .kanjis
                                .get((*n - CANDIDATE_START) as usize)
                                .map(|posttxt| pretxt + posttxt),
                        }
                    } else {
                        None
                    }
                }
            },
            _ => None,
        };

        match to_commit {
            None => {
                self.mode = InputMode::TCODE;
                let (pretxt, posttxt) = self.get_coverting_txt();
                self.preedit = pretxt + &posttxt;
                self.candidate_list.clear();
                self.update_preedit();
                self.do_input_tcode(keysym, _state)
            }
            Some(txt) => {
                //self.fcitx.commit_string(&txt);
                self.preedit = txt;

                self.mode = InputMode::TCODE;
                //self.clear_keys();
                self.candidate_list.clear();
                self.update_preedit();
                InputReturnValue::DISPLAY_MESSAGE
            }
        }
    }

    fn do_input_tcode(&mut self, keysym: KeySym, _state: c_uint) -> InputReturnValue {
        match keysym {
            // erase backward
            key::Key_BackSpace => {
                // TODO: handle katakana count
                if self.last_key.is_some() {
                    // delete invisible input char
                    self.last_key = None;
                    return InputReturnValue::DISPLAY_MESSAGE;
                }

                if !self.preedit.is_empty() {
                    // delete one character from preedit
                    self.preedit.pop();
                    self.update_preedit();
                    InputReturnValue::DISPLAY_MESSAGE
                } else {
                    InputReturnValue::FLAG_FORWARD_KEY
                }
            }
            // electric switching
            key::Key_space => {
                self.mode = InputMode::DIRECT;
                self.fcitx.commit_string(&self.preedit);
                self.reset();
                InputReturnValue::FLAG_BLOCK_FOLLOWING_PROCESS
            }
            key::Key_percent => {
                self.kana_count += 1;
                if self.kana_count == 1 {
                    self.preedit_backup = self.preedit.clone();
                }
                let len = self.preedit.chars().count();
                let start = if self.kana_count > len {
                    0
                } else {
                    len - self.kana_count
                };
                let (pre, post) = split_char_at(&self.preedit, start);
                self.preedit = pre + &kana::hira2kata(&post);
                self.update_preedit();
                InputReturnValue::DISPLAY_MESSAGE
            }
            // TODO: handle cursor keys
            _ => match QWERTY.get(&keysym) {
                None => {
                    self.fcitx.commit_string(&self.preedit);
                    self.reset();
                    InputReturnValue::FLAG_FORWARD_KEY
                }
                Some(k) => {
                    self.kana_count = 0;
                    if self.is_hotkey((key::Key_f, key::Key_j), *k) {
                        self.last_key = None;
                        self.start_mazegaki_convertion()
                    } else {
                        self.push_key(*k)
                            .map(|c| self.fcitx.commit_string(&c.to_string()));
                        self.update_preedit();
                        InputReturnValue::DISPLAY_MESSAGE
                    }
                }
            },
        }
    }
}

impl IMInstance for FcitxTCode {
    fn do_input(&mut self, keysym: KeySym, state: c_uint) -> InputReturnValue {
        // TODO: async load on init
        if self.mazegaki_dict.is_empty() {
            // TODO: error handling
            load_mazegaki_table(MAZEGAKI_DIC.to_string(), &mut self.mazegaki_dict).unwrap();
        }

        // ignore control key sequence
        let mask = key::KeyState_Ctrl | key::KeyState_Alt;
        if (state & mask) != 0 {
            self.fcitx.commit_string(&self.preedit);
            self.reset();
            return InputReturnValue::FLAG_FORWARD_KEY;
        }

        if key::Key_Shift_L <= keysym && keysym <= key::Key_Hyper_R {
            return InputReturnValue::FLAG_FORWARD_KEY;
        }

        match self.mode {
            InputMode::DIRECT => self.do_input_direct(keysym, state),
            InputMode::CONVERT => self.do_input_convert(keysym, state),
            InputMode::TCODE => self.do_input_tcode(keysym, state),
        }
    }

    fn get_cand_words(&mut self) -> InputReturnValue {
        assert!(self.convert_info.is_some());
        match self.convert_info {
            None => {
                self.update_preedit();
                return InputReturnValue::DISPLAY_CANDWORDS;
            }
            Some(ref info) => {
                self.candidate_list.set_page_size(10);
                self.candidate_list.set_choose(&self.select_chars);
                //cand_list.set_choose(&"1234567890".to_string());

                if info.kanjis.len() < 2 {
                    self.update_preedit();
                    return InputReturnValue::DISPLAY_CANDWORDS;
                }

                // cand_list.set_choose(&"dfsgaerwtq".to_string());
                for x in info.kanjis.iter().as_ref() {
                    let mut kanji_cand = fcitx::CandidateWord::new::<FcitxTCode, u8>(
                        x,
                        MessageType::OTHER,
                        None,
                        MessageType::OTHER,
                        || println!("callback"),
                        None,
                        None,
                    );
                    self.candidate_list.append(&mut kanji_cand);
                }

                self.update_preedit();
                InputReturnValue::DISPLAY_CANDWORDS
            }
        }
    }
}

const MAZEGAKI_DIC: &'static str = "/home/naota/tcode/mazegaki.dic";

fn split_char_at(text: &str, n: usize) -> (String, String) {
    let pre = text.chars().take(n).collect::<String>();
    let post = text.chars().skip(n).collect::<String>();
    (pre, post)
}
