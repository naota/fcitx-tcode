#[macro_use]
extern crate bitflags;
#[macro_use]
extern crate lazy_static;
extern crate kana;
extern crate libc;

mod fcitx;
mod keyboard;
mod tc_table;

use libc::{c_int, c_uint};
use std::collections::HashMap;
use std::error;
use std::fmt;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::iter::Iterator;

use keyboard::{KeyPos, QWERTY};

use fcitx::key::{self, KeySym};
use fcitx::CAPACITY_PREEDIT;
use fcitx::{FcitxInstance, IMClass, IMInstance, InputReturnValue};
use fcitx::{
    IRV_DISPLAY_CANDWORDS, IRV_DISPLAY_MESSAGE, IRV_FLAG_BLOCK_FOLLOWING_PROCESS,
    IRV_FLAG_FORWARD_KEY,
};
use fcitx::{MSG_FIRSTCAND, MSG_HIGHLIGHT, MSG_INPUT, MSG_OTHER};

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
    INFLECTION,
}

type Dict = HashMap<String, Vec<String>>;
struct ConvertInfo {
    start: usize,
    kanjis: Vec<String>,
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

#[derive(Debug, PartialEq)]
enum DictionaryParseError {
    NoKeyString,
    NoConvertStrings,
}
type DictionaryParseResult<T> = std::result::Result<T, Box<error::Error>>;

impl fmt::Display for DictionaryParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DictionaryParseError::NoKeyString => {
                write!(f, "no key string found in a dictionary line")
            }
            DictionaryParseError::NoConvertStrings => {
                write!(f, "no kanjis found in a dictionary line")
            }
        }
    }
}

impl error::Error for DictionaryParseError {
    fn description(&self) -> &str {
        match self {
            DictionaryParseError::NoKeyString => "no key string found in a dictionary line",
            DictionaryParseError::NoConvertStrings => "no kanjis found in a dictionary line",
        }
    }

    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

impl FcitxTCode {
    fn support_client_preedit(&self) -> bool {
        self.fcitx
            .current_input_context()
            .map_or(false, |ic| ic.caps(CAPACITY_PREEDIT))
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
            self.preedit_target().add_message_at_last(MSG_INPUT, txt);
        }
        txt.len()
    }

    fn set_preedit_converting(&self) -> usize {
        let (pretxt, posttxt) = self.get_coverting_txt();

        let target = self.preedit_target();

        let cursor_pos = pretxt.len();
        if cursor_pos > 0 {
            target.add_message_at_last(MSG_INPUT, &pretxt)
        }
        target.add_message_at_last(MSG_FIRSTCAND | MSG_HIGHLIGHT, &posttxt);

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

    fn load_mazegaki_table(&mut self, filename: String) -> DictionaryParseResult<()> {
        let dict = load_mazegaki_table(filename)?;
        self.mazegaki_dict = dict;
        Ok(())
    }

    fn start_mazegaki_convertion(&mut self) {
        let conv = self.mazegaki_convert(0..);
        if conv.is_some() {
            self.convert_info = conv;
            self.mode = InputMode::CONVERT;
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
            return IRV_FLAG_BLOCK_FOLLOWING_PROCESS;
        }

        self.last_is_space = keysym == key::Key_space;
        return IRV_FLAG_FORWARD_KEY;
    }

    fn do_input_convert(&mut self, keysym: KeySym, _state: c_uint) -> InputReturnValue {
        // hot key
        match keysym {
            // TODO: page move
            key::Key_space => return IRV_FLAG_BLOCK_FOLLOWING_PROCESS,
            key::Key_BackSpace => return IRV_FLAG_BLOCK_FOLLOWING_PROCESS,
            key::Key_less | key::Key_greater => {
                let pos = self.convert_info.as_ref().map_or(0, |x| x.start);
                let conv = if keysym == key::Key_less {
                    self.mazegaki_convert((0..pos).rev())
                } else {
                    self.mazegaki_convert((pos + 1)..)
                };
                if conv.is_some() {
                    self.convert_info = conv;
                    return IRV_DISPLAY_CANDWORDS;
                }
                return IRV_FLAG_BLOCK_FOLLOWING_PROCESS;
            }
            _ => (),
        };

        let (pretxt, posttxt) = self.get_coverting_txt();
        let to_commit = match keysym {
            key::Key_Return => Some(pretxt + &posttxt),
            // TODO: rewrite
            key::Key_a...key::Key_z | key::Key_semicolon => match QWERTY.get(&keysym) {
                None => None,
                Some(n) => {
                    if 20 <= *n && *n < 30 {
                        match self.convert_info {
                            None => None,
                            Some(ref info) => match info.kanjis.get((*n - 20) as usize) {
                                None => None,
                                Some(posttxt) => Some(pretxt + &posttxt),
                            },
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
                IRV_DISPLAY_MESSAGE
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
                    return IRV_DISPLAY_MESSAGE;
                }

                if !self.preedit.is_empty() {
                    // delete one character from preedit
                    self.preedit.pop();
                    self.update_preedit();
                    IRV_DISPLAY_MESSAGE
                } else {
                    IRV_FLAG_FORWARD_KEY
                }
            }
            // electric switching
            key::Key_space => {
                self.mode = InputMode::DIRECT;
                self.fcitx.commit_string(&self.preedit);
                self.reset();
                IRV_FLAG_BLOCK_FOLLOWING_PROCESS
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
                IRV_DISPLAY_MESSAGE
            }
            // TODO: handle cursor keys
            _ => match QWERTY.get(&keysym) {
                None => {
                    self.fcitx.commit_string(&self.preedit);
                    self.reset();
                    IRV_FLAG_FORWARD_KEY
                }
                Some(k) => {
                    self.kana_count = 0;
                    if self.is_hotkey((key::Key_f, key::Key_j), *k) {
                        self.last_key = None;
                        self.start_mazegaki_convertion();
                        IRV_DISPLAY_CANDWORDS
                    } else {
                        self.push_key(*k)
                            .map(|c| self.fcitx.commit_string(&c.to_string()));
                        self.update_preedit();
                        IRV_DISPLAY_MESSAGE
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
            self.load_mazegaki_table(MAZEGAKI_DIC.to_string()).unwrap();
        }

        // ignore control key sequence
        let mask = key::KeyState_Ctrl | key::KeyState_Alt;
        if (state & mask) != 0 {
            self.fcitx.commit_string(&self.preedit);
            self.reset();
            return IRV_FLAG_FORWARD_KEY;
        }

        if key::Key_Shift_L <= keysym && keysym <= key::Key_Hyper_R {
            return IRV_FLAG_FORWARD_KEY;
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
                return IRV_DISPLAY_CANDWORDS;
            }
            Some(ref info) => {
                self.candidate_list.set_page_size(10);
                self.candidate_list.set_choose(&self.select_chars);
                //cand_list.set_choose(&"1234567890".to_string());

                if info.kanjis.len() < 2 {
                    self.update_preedit();
                    return IRV_DISPLAY_CANDWORDS;
                }

                // cand_list.set_choose(&"dfsgaerwtq".to_string());
                for x in info.kanjis.iter().as_ref() {
                    let mut kanji_cand = fcitx::CandidateWord::new::<FcitxTCode, u8>(
                        x,
                        MSG_OTHER,
                        None,
                        MSG_OTHER,
                        || println!("callback"),
                        None,
                        None,
                    );
                    self.candidate_list.append(&mut kanji_cand);
                }

                self.update_preedit();
                IRV_DISPLAY_CANDWORDS
            }
        }
    }
}

fn parse_mazegaki_line(line: &String) -> DictionaryParseResult<(String, Vec<String>)> {
    let mut iter0 = line.split(' ');
    let key = iter0.next().ok_or(DictionaryParseError::NoKeyString)?;

    let rest: String = iter0.collect();
    let iter1 = rest.split_terminator('/');
    let v: Vec<String> = iter1.skip(1).map(|x| String::from(x)).collect();
    if v.len() == 0 {
        Err(DictionaryParseError::NoConvertStrings)?;
    }
    Ok((key.to_string(), v))
}
#[test]
fn test_parse_mazegaki_line() {
    let line = "あ /亜/吾/唖/娃/阿/".to_string();
    let key = "あ".to_string();
    let convs = vec!["亜", "吾", "唖", "娃", "阿"]
        .into_iter()
        .map(|s| s.to_string())
        .collect::<Vec<String>>();
    assert_eq!(parse_mazegaki_line(&line).unwrap(), (key, convs));
}

fn load_mazegaki_table(filename: String) -> DictionaryParseResult<Dict> {
    let mut dict = HashMap::new();
    let f = File::open(filename)?;
    let f = BufReader::new(f);
    for line in f.lines() {
        let line = line?;
        let (key, convs) = parse_mazegaki_line(&line)?;
        dict.insert(key, convs);
    }
    Ok(dict)
}

fn lookup_tcode(key: (KeyPos, KeyPos)) -> Option<char> {
    match tc_table::TABLE[key.1 as usize]
        .chars()
        .filter(|x| *x != ' ')
        .nth(key.0 as usize)
    {
        None | Some(tc_table::NONE_CHAR) => None,
        Some(c) => Some(c),
    }
}
#[test]
fn test_lookup_tcode() {
    assert_eq!(lookup_tcode((16, 20)), Some('ス'));
    assert_eq!(lookup_tcode((16, 22)), Some('4'));
    assert_eq!(lookup_tcode((39, 39)), Some('ズ'));
}

const MAZEGAKI_DIC: &'static str = "/home/naota/tcode/mazegaki.dic";

fn split_char_at(text: &String, n: usize) -> (String, String) {
    let pre = text.chars().take(n).collect::<String>();
    let post = text.chars().skip(n).collect::<String>();
    (pre, post)
}
