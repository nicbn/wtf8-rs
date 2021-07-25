#![allow(clippy::unnecessary_operation)]

use crate::*;
use alloc::borrow::Cow;
use alloc::string::String;
use alloc::vec;
use alloc::vec::Vec;

#[test]
fn wtf8_from_str() {
    assert_eq!(&Wtf8::new("").bytes, b"");
    assert_eq!(&Wtf8::new("aé 💩").bytes, b"a\xC3\xA9 \xF0\x9F\x92\xA9");
    let mut buffer = [97, 98, 99, 100];
    let str = core::str::from_utf8_mut(&mut buffer).unwrap();
    assert_eq!(&Wtf8::new_mut(str).bytes, b"abcd");
}

#[test]
fn wtf8_len() {
    assert_eq!(Wtf8::new("").len(), 0);
    assert_eq!(Wtf8::new("aé 💩").len(), 8);
}

#[test]
fn wtf8_slice() {
    assert_eq!(&Wtf8::new("aé 💩")[1..4].bytes, b"\xC3\xA9 ");
}

#[test]
#[should_panic]
fn wtf8_slice_not_code_point_boundary() {
    &Wtf8::new("aé 💩")[2..4];
}

#[test]
fn wtf8_slice_from() {
    assert_eq!(&Wtf8::new("aé 💩")[1..].bytes, b"\xC3\xA9 \xF0\x9F\x92\xA9");
}

#[test]
#[should_panic]
fn wtf8_slice_from_not_code_point_boundary() {
    &Wtf8::new("aé 💩")[2..];
}

#[test]
fn wtf8_slice_to() {
    assert_eq!(&Wtf8::new("aé 💩")[..4].bytes, b"a\xC3\xA9 ");
}

#[test]
#[should_panic]
fn wtf8_slice_to_not_code_point_boundary() {
    &Wtf8::new("aé 💩")[5..];
}

#[test]
fn wtf8_ascii_byte_at() {
    let slice = Wtf8::new("aé 💩");
    assert_eq!(slice.ascii_byte_at(0), b'a');
    assert_eq!(slice.ascii_byte_at(1), b'\xFF');
    assert_eq!(slice.ascii_byte_at(2), b'\xFF');
    assert_eq!(slice.ascii_byte_at(3), b' ');
    assert_eq!(slice.ascii_byte_at(4), b'\xFF');
}

#[test]
fn wtf8_code_points() {
    fn c(value: u32) -> CodePoint {
        CodePoint::from_u32(value).unwrap()
    }
    fn cp(string: &Wtf8Buf) -> Vec<Option<char>> {
        string
            .code_points()
            .map(|c| c.to_char())
            .collect::<Vec<_>>()
    }
    let mut string = Wtf8Buf::from_str("é ");
    assert_eq!(cp(&string), [Some('é'), Some(' ')]);
    string.push(c(0xD83D));
    assert_eq!(cp(&string), [Some('é'), Some(' '), None]);
    string.push(c(0xDCA9));
    assert_eq!(cp(&string), [Some('é'), Some(' '), Some('💩')]);
}

#[test]
fn wtf8_str_chunks() {
    fn c(value: u32) -> CodePoint {
        CodePoint::from_u32(value).unwrap()
    }
    fn sc(string: &Wtf8Buf) -> Vec<(&str, Option<u16>)> {
        string.str_chunks().collect::<Vec<_>>()
    }
    let mut string = Wtf8Buf::new();
    string.push(c(0xD83D));
    assert_eq!(sc(&string), [("", Some(0xD83D))]);
    string.clear();
    assert_eq!(sc(&string), []);
    string.push_str("Resumé ");
    assert_eq!(sc(&string), [("Resumé ", None)]);
    string.push(c(0xD83D));
    assert_eq!(sc(&string), [("Resumé ", Some(0xD83D))]);
    string.push(c(0xDCA9));
    assert_eq!(sc(&string), [("Resumé 💩", None)]);
    string.push(c(0xDCA9));
    assert_eq!(sc(&string), [("Resumé 💩", Some(0xDCA9))]);
    string.push(c(0xDCA7));
    assert_eq!(sc(&string), [("Resumé 💩", Some(0xDCA9)), ("", Some(0xDCA7))]);
    string.push_str("香蕉");
    assert_eq!(sc(&string), [("Resumé 💩", Some(0xDCA9)), ("", Some(0xDCA7)), ("香蕉", None)]);
}

#[test]
fn wtf8_str_chunks_mut() {
    fn c(value: u32) -> CodePoint {
        CodePoint::from_u32(value).unwrap()
    }
    fn sc(string: &Wtf8Buf) -> Vec<(&str, Option<u16>)> {
        string.str_chunks().collect::<Vec<_>>()
    }
    let mut string = Wtf8Buf::from_str("The File Path is 'C:/");
    string.push(c(0xD83D));
    string.push_str("abcd");
    string.push(c(0xDD89));
    string.push(c(0xDD0A));
    string.push_str("XYZ");
    string.push(c(0xDF0A));
    string.push_str("'. Nice! 😊");
    for (text, _) in string.str_chunks_mut() {
        let (left, right) = text.split_at_mut(text.len() / 2);
        left.make_ascii_uppercase();
        right.make_ascii_lowercase();
    }
    assert_eq!(sc(&string), [
        ("THE FILE Path is 'c:/", Some(0xD83D)),
        ("ABcd", Some(0xDD89)),
        ("", Some(0xDD0A)),
        ("Xyz", Some(0xDF0A)),
        ("'. NICe! 😊", None),
    ]);
}

#[test]
fn wtf8_as_str() {
    assert_eq!(Wtf8::new("").to_str(), Ok(""));
    assert_eq!(Wtf8::new("aé 💩").to_str(), Ok("aé 💩"));
    let mut string = Wtf8Buf::new();
    string.push(CodePoint::from_u32(0xD800).unwrap());
    assert!(string.to_str().is_err());
}

#[test]
fn wtf8_to_string_lossy() {
    assert_eq!(Wtf8::new("").to_string_lossy(), Cow::Borrowed(""));
    assert_eq!(Wtf8::new("aé 💩").to_string_lossy(), Cow::Borrowed("aé 💩"));
    let mut string = Wtf8Buf::from_str("aé 💩");
    string.push(CodePoint::from_u32(0xD800).unwrap());
    let expected: Cow<'_, str> = Cow::Owned(String::from("aé 💩�"));
    assert_eq!(string.to_string_lossy(), expected);
}

#[test]
fn wtf8_display() {
    fn d(b: &[u8]) -> String {
        (unsafe { &*(b as *const [u8] as *const Wtf8) })
            .to_string_lossy()
            .into_owned()
    }

    assert_eq!("", d("".as_bytes()));
    assert_eq!("aé 💩", d("aé 💩".as_bytes()));

    let mut string = Wtf8Buf::from_str("aé 💩");
    string.push(CodePoint::from_u32(0xD800).unwrap());
    assert_eq!("aé 💩�", d(string.bytes()));
}

#[test]
fn wtf8_encode_utf16() {
    let mut string = Wtf8Buf::from_str("aé ");
    string.push(CodePoint::from_u32(0xD83D).unwrap());
    string.push_char('💩');
    assert_eq!(
        string.encode_utf16().collect::<Vec<_>>(),
        vec![0x61, 0xE9, 0x20, 0xD83D, 0xD83D, 0xDCA9]
    );
}
