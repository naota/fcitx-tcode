use fcitx::key;
use std::collections::HashMap;

pub type KeyPos = u8;

lazy_static! {
    pub static ref QWERTY: HashMap<key::KeySym, KeyPos> = {
        let mut map = HashMap::with_capacity(_QWERTY.len());
        for (pos, k) in _QWERTY.iter().enumerate() {
            map.insert(*k, pos as KeyPos);
        }
        map
    };
}

pub const _QWERTY: &'static [key::KeySym] = &[
    // 1st row
    key::Key_1, // 0
    key::Key_2,
    key::Key_3,
    key::Key_4,
    key::Key_5,
    key::Key_6,
    key::Key_7,
    key::Key_8,
    key::Key_9,
    key::Key_0,
    // 2nd row
    key::Key_q, // 10
    key::Key_w,
    key::Key_e,
    key::Key_r,
    key::Key_t,
    key::Key_y,
    key::Key_u,
    key::Key_i,
    key::Key_o,
    key::Key_p,
    // 3rd row
    key::Key_a, // 20
    key::Key_s,
    key::Key_d,
    key::Key_f,
    key::Key_g,
    key::Key_h,
    key::Key_j,
    key::Key_k,
    key::Key_l,
    key::Key_semicolon,
    // 4th row
    key::Key_z, //30
    key::Key_x,
    key::Key_c,
    key::Key_v,
    key::Key_b,
    key::Key_n,
    key::Key_m,
    key::Key_comma,
    key::Key_period,
    key::Key_slash,
];
