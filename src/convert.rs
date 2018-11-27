use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::{error, fmt};

use keyboard::KeyPos;

pub type Dict = HashMap<String, Vec<String>>;

#[derive(Debug, PartialEq)]
enum DictionaryParseError {
    NoKeyString,
    NoConvertStrings,
}
pub type DictionaryParseResult<T> = std::result::Result<T, Box<error::Error>>;

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

pub fn load_mazegaki_table(filename: String, dict: &mut Dict) -> DictionaryParseResult<()> {
    let f = File::open(filename)?;
    let f = BufReader::new(f);
    for line in f.lines() {
        let line = line?;
        let (key, convs) = parse_mazegaki_line(&line)?;
        dict.insert(key, convs);
    }
    Ok(())
}

pub fn lookup_tcode(key: (KeyPos, KeyPos)) -> Option<char> {
    match ::tc_table::TABLE[key.1 as usize]
        .chars()
        .filter(|x| *x != ' ')
        .nth(key.0 as usize)
    {
        None | Some(::tc_table::NONE_CHAR) => None,
        Some(c) => Some(c),
    }
}
#[test]
fn test_lookup_tcode() {
    assert_eq!(lookup_tcode((16, 20)), Some('ス'));
    assert_eq!(lookup_tcode((16, 22)), Some('4'));
    assert_eq!(lookup_tcode((39, 39)), Some('ズ'));
}
