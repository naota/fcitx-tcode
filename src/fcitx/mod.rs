extern crate fcitx_sys;

use libc::{c_int, c_uint};
use std::ffi::{c_void, CString};
use std::option::Option;

pub mod key;
use self::key::KeySym;

pub const ABI_VERSION: u32 = fcitx_sys::FCITX_ABI_VERSION;

macro_rules! void_func_wrapper {
    ($name:ident, $handle:tt, $func:ident, $arg1:ident, $arg1_type:tt) => {
        pub fn $name(&self, $arg1:$arg1_type) {
            let handle = self.$handle;
            unsafe { fcitx_sys::$func(handle, $arg1) };
        }
    };
    ($name:ident, $handle:tt, $func:ident) => {
        pub fn $name(&self) {
            let handle = self.$handle;
            unsafe { fcitx_sys::$func(handle) };
        }
    };
}

macro_rules! factory_func_wrapper {
    ($name:ident, $handle:tt, $func:ident, $target:ident) => {
        pub fn $name(&self) -> $target {
            let handle = self.$handle;
            let ptr = unsafe { fcitx_sys::$func(handle) };
            $target::new(ptr)
        }
    };
}

// plugin constructor
#[macro_export]
macro_rules! declare_imclass {
    ($im_type:ty, $constructor:path) => {
        #[no_mangle]
        pub static ABI_VERSION: u32 = $crate::fcitx::ABI_VERSION;

        #[no_mangle]
        pub static ime: $crate::fcitx::_IMClass = $crate::fcitx::_IMClass {
            Create: Some(_ime_create),
            Destroy: Some(_ime_destroy),
        };

        unsafe extern "C" fn _ime_create(
            instance: *mut fcitx_sys::FcitxInstance,
        ) -> *mut ::std::ffi::c_void {
            let fcitx = FcitxInstance::new(instance);
            let constructor: fn(FcitxInstance) -> $im_type = $constructor;
            let mut im = Box::new(constructor(fcitx));
            im.create(fcitx);
            Box::into_raw(im) as *mut _ as *mut ::std::ffi::c_void
        }

        pub unsafe extern "C" fn _ime_destroy(ptr: *mut ::std::ffi::c_void) {
            let ptr = ptr as *mut $im_type;
            (*ptr).destroy();
            let im = Box::from_raw(ptr);
            drop(im)
        }
    };
}

// IMClass
pub type _IMClass = fcitx_sys::FcitxIMClass;
pub trait IMClass {
    fn create(&mut self, fcitx: FcitxInstance);
    fn destroy(&mut self);
}

// FcitxInstance
#[derive(Copy, Clone)]
pub struct FcitxInstance {
    instance: *mut fcitx_sys::FcitxInstance,
}

impl FcitxInstance {
    pub fn new(handle: *mut fcitx_sys::FcitxInstance) -> Self {
        FcitxInstance { instance: handle }
    }

    factory_func_wrapper!(
        input_state,
        instance,
        FcitxInstanceGetInputState,
        InputState
    );

    void_func_wrapper!(
        clean_input_window_up,
        instance,
        FcitxInstanceCleanInputWindowUp
    );
    void_func_wrapper!(update_input_window, instance, FcitxUIUpdateInputWindow);

    pub fn current_input_context(&self) -> Option<InputContext> {
        let instance = self.instance;
        let ptr = unsafe { fcitx_sys::FcitxInstanceGetCurrentIC(instance) };
        if ptr.is_null() {
            None
        } else {
            Some(InputContext::new(ptr))
        }
    }

    pub fn commit_string(&self, txt: &String) {
        if txt.len() == 0 {
            return
        }

        let handle = self.instance;
        let cstr = CString::new((*txt).clone()).unwrap();
        unsafe {
            let ic = fcitx_sys::FcitxInstanceGetCurrentIC(handle);
            fcitx_sys::FcitxInstanceCommitString(handle, ic, cstr.as_ptr());
        }
    }

    pub fn forward_key(&self, event: KeyEvent, keysym: key::KeySym, state: c_uint) {
        let handle = self.instance;
        unsafe {
            let ic = fcitx_sys::FcitxInstanceGetCurrentIC(handle);
            fcitx_sys::FcitxInstanceForwardKey(
                handle,
                ic,
                event as fcitx_sys::FcitxKeyEventType,
                keysym,
                state,
            )
        }
    }

    pub fn im_register<T>(
        &mut self,
        im: &mut T,
        unique_name: &'static str,
        name: &'static str,
        icon_name: &'static str,
        priority: c_int,
        lang_code: &'static str,
    ) where
        T: IMInstance,
    {
        let instance = self.instance;
        let ptr = im as *mut _ as *mut c_void;
        let interface = fcitx_sys::FcitxIMIFace {
            Init: Some(_im_init),
            ResetIM: Some(_reset_im),
            DoInput: Some(_do_input::<T>),
            GetCandWords: Some(_get_cand_words::<T>),
            ..Default::default()
        };
        let unique_name = CString::new(unique_name).unwrap();
        let name = CString::new(name).unwrap();
        let icon_name = CString::new(icon_name).unwrap();
        let lang_code = CString::new(lang_code).unwrap();
        unsafe {
            fcitx_sys::FcitxInstanceRegisterIMv2(
                instance,
                ptr,
                unique_name.as_ptr(),
                name.as_ptr(),
                icon_name.as_ptr(),
                interface,
                priority,
                lang_code.as_ptr(),
            )
        }
    }
}

extern "C" fn _im_init(im: *mut c_void) -> fcitx_sys::boolean {
    assert!(!im.is_null());
    true as fcitx_sys::boolean
}

extern "C" fn _reset_im(im: *mut c_void) {
    assert!(!im.is_null());
}

extern "C" fn _do_input<T>(
    ptr: *mut c_void,
    keysym: KeySym,
    state: c_uint,
) -> fcitx_sys::INPUT_RETURN_VALUE
where
    T: IMInstance,
{
    assert!(!ptr.is_null());
    let im = unsafe { &mut *(ptr as *mut T) };
    let ret = im.do_input(keysym, state);
    ret.bits()
}

extern "C" fn _get_cand_words<T>(ptr: *mut c_void) -> fcitx_sys::INPUT_RETURN_VALUE
where
    T: IMInstance,
{
    assert!(!ptr.is_null());
    let ptr = ptr as *mut T;
    let ret = unsafe { (*ptr).get_cand_words() };
    ret.bits()
}

macro_rules! pointer_wrapper {
    ($wrapper:ident, $systype:ident) => {
        pub struct $wrapper(*mut fcitx_sys::$systype);

        impl $wrapper {
            fn new(ptr: *mut fcitx_sys::$systype) -> Self {
                $wrapper(ptr)
            }
        }
    };
}

// InputState
#[derive(Clone, Copy)]
pub struct InputState(*mut fcitx_sys::FcitxInputState);
impl InputState {
    fn new(ptr: *mut fcitx_sys::FcitxInputState) -> Self {
        InputState(ptr)
    }

    factory_func_wrapper!(
        candidate_list,
        0,
        FcitxInputStateGetCandidateList,
        CandidateWordList
    );
    factory_func_wrapper!(preedit, 0, FcitxInputStateGetPreedit, Messages);
    factory_func_wrapper!(client_preedit, 0, FcitxInputStateGetClientPreedit, Messages);
    void_func_wrapper!(
        set_cursor_pos,
        0,
        FcitxInputStateSetCursorPos,
        cursor_pos,
        c_int
    );
    void_func_wrapper!(
        set_client_cursor_pos,
        0,
        FcitxInputStateSetClientCursorPos,
        cursor_pos,
        c_int
    );
}

// InputContext
pointer_wrapper!(InputContext, FcitxInputContext);
impl InputContext {
    pub fn caps(&self, test_caps: CapacityFlags) -> bool {
        let ic = self.0;
        assert!(!ic.is_null());
        let ctx_caps = unsafe { (*ic).contextCaps };
        (ctx_caps & test_caps.bits()) != 0
    }
}

// CandidateWordList
pointer_wrapper!(CandidateWordList, FcitxCandidateWordList);
impl CandidateWordList {
    void_func_wrapper!(set_page_size, 0, FcitxCandidateWordSetPageSize, size, c_int);
    void_func_wrapper!(clear, 0, FcitxCandidateWordReset);

    pub fn set_choose(&self, choose: &String) {
        let handle = self.0;
        let cstr = CString::new((*choose).clone()).unwrap();
        unsafe {
            fcitx_sys::FcitxCandidateWordSetChoose(handle, cstr.as_ptr());
        }
    }

    pub fn append(&self, word: &mut CandidateWord) {
        let handle = self.0;
        let ptr = &mut (word.0);
        unsafe { fcitx_sys::FcitxCandidateWordAppend(handle, ptr) }
    }
}

// CandidateWord
#[derive(Debug)]
pub struct CandidateWord(fcitx_sys::FcitxCandidateWord);
impl CandidateWord {
    pub fn new<T, U>(
        word: &String,
        word_type: MessageType,
        _extra: Option<&String>,
        extra_type: MessageType,
        _cb: fn(),
        _owner: Option<&mut T>,
        _private: Option<&mut U>,
    ) -> Self {
        let word_cstr = CString::new((*word).clone()).unwrap();
        // free() by fcitx
        let word_dup = unsafe { fcitx_sys::strdup(word_cstr.as_ptr()) };

        let extra_ptr = std::ptr::null_mut(); // TODO: handle None

        let tmp = fcitx_sys::FcitxCandidateWord {
            strWord: word_dup,   // free() by fcitx
            strExtra: extra_ptr, // free() by fcitx
            callback: None,
            wordType: word_type.bits(),
            extraType: extra_type.bits(),
            owner: std::ptr::null_mut(),
            //owner as *mut _ as *mut c_void,
            priv_: std::ptr::null_mut(), // free() by fcitx
                                         //private as *mut _ as *mut c_void,
        };
        CandidateWord(tmp)
    }
}

// Messages
pointer_wrapper!(Messages, FcitxMessages);
impl Messages {
    pub fn add_message_at_last(&self, msg: MessageType, txt: &String) {
        let handle = self.0;
        let format = CString::new("%s").unwrap();
        let arg = CString::new((*txt).clone()).unwrap();
        unsafe {
            fcitx_sys::FcitxMessagesAddMessageAtLast(
                handle,
                msg.bits(),
                format.as_ptr(),
                arg.as_ptr(),
            )
        };
    }
}

// IMInstance
pub trait IMInstance {
    fn do_input(&mut self, keysym: KeySym, state: c_uint) -> InputReturnValue;
    fn get_cand_words(&mut self) -> InputReturnValue;
}

// re-export constants

bitflags! {
    pub flags InputReturnValue: fcitx_sys::INPUT_RETURN_VALUE {
        const IRV_TO_PROCESS = fcitx_sys::_INPUT_RETURN_VALUE_IRV_TO_PROCESS,
        const IRV_FLAG_BLOCK_FOLLOWING_PROCESS = fcitx_sys::_INPUT_RETURN_VALUE_IRV_FLAG_BLOCK_FOLLOWING_PROCESS,
        const IRV_FLAG_FORWARD_KEY = fcitx_sys::_INPUT_RETURN_VALUE_IRV_FLAG_FORWARD_KEY,
        const IRV_FLAG_RESET_INPUT = fcitx_sys::_INPUT_RETURN_VALUE_IRV_FLAG_RESET_INPUT,
        const IRV_FLAG_PENDING_COMMIT_STRING = fcitx_sys::_INPUT_RETURN_VALUE_IRV_FLAG_PENDING_COMMIT_STRING,
        const IRV_FLAG_UPDATE_INPUT_WINDOW = fcitx_sys::_INPUT_RETURN_VALUE_IRV_FLAG_UPDATE_INPUT_WINDOW,
        const IRV_FLAG_UPDATE_CANDIDATE_WORDS = fcitx_sys::_INPUT_RETURN_VALUE_IRV_FLAG_UPDATE_CANDIDATE_WORDS,
        const IRV_FLAG_ENG = fcitx_sys::_INPUT_RETURN_VALUE_IRV_FLAG_ENG,
        const IRV_FLAG_PUNC = fcitx_sys::_INPUT_RETURN_VALUE_IRV_FLAG_PUNC,
        const IRV_FLAG_DISPLAY_LAST = fcitx_sys::_INPUT_RETURN_VALUE_IRV_FLAG_DISPLAY_LAST,
        const IRV_FLAG_DO_PHRASE_TIPS = fcitx_sys::_INPUT_RETURN_VALUE_IRV_FLAG_DO_PHRASE_TIPS,
        const IRV_DONOT_PROCESS = fcitx_sys::_INPUT_RETURN_VALUE_IRV_DONOT_PROCESS,
        const IRV_COMMIT_STRING = fcitx_sys::_INPUT_RETURN_VALUE_IRV_COMMIT_STRING,
        const IRV_DO_NOTHING = fcitx_sys::_INPUT_RETURN_VALUE_IRV_DO_NOTHING,
        const IRV_CLEAN = fcitx_sys::_INPUT_RETURN_VALUE_IRV_CLEAN,
        const IRV_COMMIT_STRING_REMIND = fcitx_sys::_INPUT_RETURN_VALUE_IRV_COMMIT_STRING_REMIND,
        const IRV_DISPLAY_CANDWORDS = fcitx_sys::_INPUT_RETURN_VALUE_IRV_DISPLAY_CANDWORDS,
        const IRV_DONOT_PROCESS_CLEAN = fcitx_sys::_INPUT_RETURN_VALUE_IRV_DONOT_PROCESS_CLEAN,
        const IRV_COMMIT_STRING_NEXT = fcitx_sys::_INPUT_RETURN_VALUE_IRV_COMMIT_STRING_NEXT,
        const IRV_DISPLAY_MESSAGE = fcitx_sys::_INPUT_RETURN_VALUE_IRV_DISPLAY_MESSAGE,
        const IRV_ENG = fcitx_sys::_INPUT_RETURN_VALUE_IRV_ENG,
        const IRV_PUNC = fcitx_sys::_INPUT_RETURN_VALUE_IRV_PUNC,
        const IRV_DISPLAY_LAST = fcitx_sys::_INPUT_RETURN_VALUE_IRV_DISPLAY_LAST,
    }
}

bitflags! {
    pub flags CapacityFlags: fcitx_sys::FcitxCapacityFlags {
        const CAPACITY_NONE                      = fcitx_sys::_FcitxCapacityFlags_CAPACITY_NONE,
        const CAPACITY_CLIENT_SIDE_UI            = fcitx_sys::_FcitxCapacityFlags_CAPACITY_CLIENT_SIDE_UI,
        const CAPACITY_PREEDIT                   = fcitx_sys::_FcitxCapacityFlags_CAPACITY_PREEDIT,
        const CAPACITY_CLIENT_SIDE_CONTROL_STATE = fcitx_sys::_FcitxCapacityFlags_CAPACITY_CLIENT_SIDE_CONTROL_STATE,
        const CAPACITY_PASSWORD                  = fcitx_sys::_FcitxCapacityFlags_CAPACITY_PASSWORD,
        const CAPACITY_FORMATTED_PREEDIT         = fcitx_sys::_FcitxCapacityFlags_CAPACITY_FORMATTED_PREEDIT,
        const CAPACITY_CLIENT_UNFOCUS_COMMIT     = fcitx_sys::_FcitxCapacityFlags_CAPACITY_CLIENT_UNFOCUS_COMMIT,
        const CAPACITY_SURROUNDING_TEXT          = fcitx_sys::_FcitxCapacityFlags_CAPACITY_SURROUNDING_TEXT,
        const CAPACITY_EMAIL                     = fcitx_sys::_FcitxCapacityFlags_CAPACITY_EMAIL,
        const CAPACITY_DIGIT                     = fcitx_sys::_FcitxCapacityFlags_CAPACITY_DIGIT,
        const CAPACITY_UPPERCASE                 = fcitx_sys::_FcitxCapacityFlags_CAPACITY_UPPERCASE,
        const CAPACITY_LOWERCASE                 = fcitx_sys::_FcitxCapacityFlags_CAPACITY_LOWERCASE,
        const CAPACITY_NOAUTOUPPERCASE           = fcitx_sys::_FcitxCapacityFlags_CAPACITY_NOAUTOUPPERCASE,
        const CAPACITY_URL                       = fcitx_sys::_FcitxCapacityFlags_CAPACITY_URL,
        const CAPACITY_DIALABLE                  = fcitx_sys::_FcitxCapacityFlags_CAPACITY_DIALABLE,
        const CAPACITY_NUMBER                    = fcitx_sys::_FcitxCapacityFlags_CAPACITY_NUMBER,
        const CAPACITY_NO_ON_SCREEN_KEYBOARD     = fcitx_sys::_FcitxCapacityFlags_CAPACITY_NO_ON_SCREEN_KEYBOARD,
        const CAPACITY_SPELLCHECK                = fcitx_sys::_FcitxCapacityFlags_CAPACITY_SPELLCHECK,
        const CAPACITY_NO_SPELLCHECK             = fcitx_sys::_FcitxCapacityFlags_CAPACITY_NO_SPELLCHECK,
        const CAPACITY_WORD_COMPLETION           = fcitx_sys::_FcitxCapacityFlags_CAPACITY_WORD_COMPLETION,
        const CAPACITY_UPPERCASE_WORDS           = fcitx_sys::_FcitxCapacityFlags_CAPACITY_UPPERCASE_WORDS,
        const CAPACITY_UPPERCASE_SENTENCES       = fcitx_sys::_FcitxCapacityFlags_CAPACITY_UPPERCASE_SENTENCES,
        const CAPACITY_ALPHA                     = fcitx_sys::_FcitxCapacityFlags_CAPACITY_ALPHA,
        const CAPACITY_NAME                      = fcitx_sys::_FcitxCapacityFlags_CAPACITY_NAME,
        const CAPACITY_GET_IM_INFO_ON_FOCUS      = fcitx_sys::_FcitxCapacityFlags_CAPACITY_GET_IM_INFO_ON_FOCUS,
        const CAPACITY_RELATIVE_CURSOR_RECT      = fcitx_sys::_FcitxCapacityFlags_CAPACITY_RELATIVE_CURSOR_RECT,
    }
}

bitflags! {
    pub flags MessageType: fcitx_sys::FcitxMessageType {
        const MSG_TYPE_FIRST                = fcitx_sys::_FcitxMessageType_MSG_TYPE_FIRST,
        const MSG_TYPE_LAST                 = fcitx_sys::_FcitxMessageType_MSG_TYPE_LAST,
        const MSG_TIPS                      = fcitx_sys::_FcitxMessageType_MSG_TIPS,
        const MSG_INPUT                     = fcitx_sys::_FcitxMessageType_MSG_INPUT,
        const MSG_INDEX                     = fcitx_sys::_FcitxMessageType_MSG_INDEX,
        const MSG_CANDIATE_CURSOR           = fcitx_sys::_FcitxMessageType_MSG_CANDIATE_CURSOR,
        const MSG_FIRSTCAND                 = fcitx_sys::_FcitxMessageType_MSG_FIRSTCAND,
        const MSG_USERPHR                   = fcitx_sys::_FcitxMessageType_MSG_USERPHR,
        const MSG_CODE                      = fcitx_sys::_FcitxMessageType_MSG_CODE,
        const MSG_OTHER                     = fcitx_sys::_FcitxMessageType_MSG_OTHER,
        const MSG_NOUNDERLINE               = fcitx_sys::_FcitxMessageType_MSG_NOUNDERLINE,
        const MSG_HIGHLIGHT                 = fcitx_sys::_FcitxMessageType_MSG_HIGHLIGHT,
        const MSG_DONOT_COMMIT_WHEN_UNFOCUS = fcitx_sys::_FcitxMessageType_MSG_DONOT_COMMIT_WHEN_UNFOCUS,
        const MSG_REGULAR_MASK              = fcitx_sys::_FcitxMessageType_MSG_REGULAR_MASK,
    }
}

#[allow(dead_code)]
#[repr(u32)]
pub enum KeyEvent {
    PressKey = fcitx_sys::_FcitxKeyEventType_FCITX_PRESS_KEY,
    ReleaseKey = fcitx_sys::_FcitxKeyEventType_FCITX_RELEASE_KEY,
}
