use crate::*;
use alloc::format;
use alloc::string::String;

#[test]
fn wtf8buf_new() {
    assert_eq!(Wtf8Buf::new().bytes, b"");
}

#[test]
fn wtf8buf_from_str() {
    assert_eq!(Wtf8Buf::from_str("").bytes, b"");
    assert_eq!(
        Wtf8Buf::from_str("aé 💩").bytes,
        b"a\xC3\xA9 \xF0\x9F\x92\xA9"
    );
}

#[test]
fn wtf8buf_from_string() {
    assert_eq!(Wtf8Buf::from_string(String::from("")).bytes, b"");
    assert_eq!(
        Wtf8Buf::from_string(String::from("aé 💩")).bytes,
        b"a\xC3\xA9 \xF0\x9F\x92\xA9"
    );
}

#[test]
fn wtf8buf_from_utf16() {
    assert_eq!(Wtf8Buf::from_utf16([].iter().copied()).bytes, b"");
    assert_eq!(
        Wtf8Buf::from_utf16([0x61, 0xE9, 0x20, 0xD83D, 0xD83D, 0xDCA9].iter().copied()).bytes,
        b"a\xC3\xA9 \xED\xA0\xBD\xF0\x9F\x92\xA9"
    );
}

#[test]
fn wtf8buf_push_str() {
    let mut string = Wtf8Buf::new();
    assert_eq!(string.bytes, b"");
    string.push_str("aé 💩");
    assert_eq!(string.bytes, b"a\xC3\xA9 \xF0\x9F\x92\xA9");
}

#[test]
fn wtf8buf_push_char() {
    let mut string = Wtf8Buf::from_str("aé ");
    assert_eq!(string.bytes, b"a\xC3\xA9 ");
    string.push_char('💩');
    assert_eq!(string.bytes, b"a\xC3\xA9 \xF0\x9F\x92\xA9");
}

#[test]
fn wtf8buf_push() {
    let mut string = Wtf8Buf::from_str("aé ");
    assert_eq!(string.bytes, b"a\xC3\xA9 ");
    string.push(CodePoint::from_char('💩'));
    assert_eq!(string.bytes, b"a\xC3\xA9 \xF0\x9F\x92\xA9");

    fn c(value: u32) -> CodePoint {
        CodePoint::from_u32(value).unwrap()
    }

    let mut string = Wtf8Buf::new();
    string.push(c(0xD83D)); // lead
    string.push(c(0xDCA9)); // trail
    assert_eq!(string.bytes, b"\xF0\x9F\x92\xA9"); // Magic!

    let mut string = Wtf8Buf::new();
    string.push(c(0xD83D)); // lead
    string.push(c(0x20)); // not surrogate
    string.push(c(0xDCA9)); // trail
    assert_eq!(string.bytes, b"\xED\xA0\xBD \xED\xB2\xA9");

    let mut string = Wtf8Buf::new();
    string.push(c(0xD800)); // lead
    string.push(c(0xDBFF)); // lead
    assert_eq!(string.bytes, b"\xED\xA0\x80\xED\xAF\xBF");

    let mut string = Wtf8Buf::new();
    string.push(c(0xD800)); // lead
    string.push(c(0xE000)); // not surrogate
    assert_eq!(string.bytes, b"\xED\xA0\x80\xEE\x80\x80");

    let mut string = Wtf8Buf::new();
    string.push(c(0xD7FF)); // not surrogate
    string.push(c(0xDC00)); // trail
    assert_eq!(string.bytes, b"\xED\x9F\xBF\xED\xB0\x80");

    let mut string = Wtf8Buf::new();
    string.push(c(0x61)); // not surrogate, < 3 bytes
    string.push(c(0xDC00)); // trail
    assert_eq!(string.bytes, b"\x61\xED\xB0\x80");

    let mut string = Wtf8Buf::new();
    string.push(c(0xDC00)); // trail
    assert_eq!(string.bytes, b"\xED\xB0\x80");
}

#[test]
fn wtf8buf_push_wtf8() {
    let mut string = Wtf8Buf::from_str("aé");
    assert_eq!(string.bytes, b"a\xC3\xA9");
    string.push_wtf8(Wtf8::new(" 💩"));
    assert_eq!(string.bytes, b"a\xC3\xA9 \xF0\x9F\x92\xA9");

    fn w(v: &[u8]) -> &Wtf8 {
        unsafe { &*(v as *const [u8] as *const Wtf8) }
    }

    let mut string = Wtf8Buf::new();
    string.push_wtf8(w(b"\xED\xA0\xBD")); // lead
    string.push_wtf8(w(b"\xED\xB2\xA9")); // trail
    assert_eq!(string.bytes, b"\xF0\x9F\x92\xA9"); // Magic!

    let mut string = Wtf8Buf::new();
    string.push_wtf8(w(b"\xED\xA0\xBD")); // lead
    string.push_wtf8(w(b" ")); // not surrogate
    string.push_wtf8(w(b"\xED\xB2\xA9")); // trail
    assert_eq!(string.bytes, b"\xED\xA0\xBD \xED\xB2\xA9");

    let mut string = Wtf8Buf::new();
    string.push_wtf8(w(b"\xED\xA0\x80")); // lead
    string.push_wtf8(w(b"\xED\xAF\xBF")); // lead
    assert_eq!(string.bytes, b"\xED\xA0\x80\xED\xAF\xBF");

    let mut string = Wtf8Buf::new();
    string.push_wtf8(w(b"\xED\xA0\x80")); // lead
    string.push_wtf8(w(b"\xEE\x80\x80")); // not surrogate
    assert_eq!(string.bytes, b"\xED\xA0\x80\xEE\x80\x80");

    let mut string = Wtf8Buf::new();
    string.push_wtf8(w(b"\xED\x9F\xBF")); // not surrogate
    string.push_wtf8(w(b"\xED\xB0\x80")); // trail
    assert_eq!(string.bytes, b"\xED\x9F\xBF\xED\xB0\x80");

    let mut string = Wtf8Buf::new();
    string.push_wtf8(w(b"a")); // not surrogate, < 3 bytes
    string.push_wtf8(w(b"\xED\xB0\x80")); // trail
    assert_eq!(string.bytes, b"\x61\xED\xB0\x80");

    let mut string = Wtf8Buf::new();
    string.push_wtf8(w(b"\xED\xB0\x80")); // trail
    assert_eq!(string.bytes, b"\xED\xB0\x80");
}

#[test]
fn wtf8buf_truncate() {
    let mut string = Wtf8Buf::from_str("aé");
    string.truncate(1);
    assert_eq!(string.bytes, b"a");
}

#[test]
#[should_panic]
fn wtf8buf_truncate_fail_code_point_boundary() {
    let mut string = Wtf8Buf::from_str("aé");
    string.truncate(2);
}

#[test]
#[should_panic]
fn wtf8buf_truncate_fail_longer() {
    let mut string = Wtf8Buf::from_str("aé");
    string.truncate(4);
}

#[test]
fn wtf8buf_into_string() {
    let mut string = Wtf8Buf::from_str("aé 💩");
    assert_eq!(string.clone().into_string(), Ok(String::from("aé 💩")));
    string.push(CodePoint::from_u32(0xD800).unwrap());
    assert!(string.into_string().is_err());
}

#[test]
fn wtf8buf_into_string_lossy() {
    let mut string = Wtf8Buf::from_str("aé 💩");
    assert_eq!(string.clone().into_string_lossy(), String::from("aé 💩"));
    string.push(CodePoint::from_u32(0xD800).unwrap());
    assert_eq!(string.into_string_lossy(), String::from("aé 💩�"));
}

#[test]
fn wtf8buf_from_iterator() {
    fn f(values: &[u32]) -> Wtf8Buf {
        values
            .iter()
            .map(|&c| CodePoint::from_u32(c).unwrap())
            .collect::<Wtf8Buf>()
    }
    assert_eq!(
        f(&[0x61, 0xE9, 0x20, 0x1F4A9]).bytes,
        b"a\xC3\xA9 \xF0\x9F\x92\xA9"
    );

    assert_eq!(f(&[0xD83D, 0xDCA9]).bytes, b"\xF0\x9F\x92\xA9"); // Magic!
    assert_eq!(
        f(&[0xD83D, 0x20, 0xDCA9]).bytes,
        b"\xED\xA0\xBD \xED\xB2\xA9"
    );
    assert_eq!(f(&[0xD800, 0xDBFF]).bytes, b"\xED\xA0\x80\xED\xAF\xBF");
    assert_eq!(f(&[0xD800, 0xE000]).bytes, b"\xED\xA0\x80\xEE\x80\x80");
    assert_eq!(f(&[0xD7FF, 0xDC00]).bytes, b"\xED\x9F\xBF\xED\xB0\x80");
    assert_eq!(f(&[0x61, 0xDC00]).bytes, b"\x61\xED\xB0\x80");
    assert_eq!(f(&[0xDC00]).bytes, b"\xED\xB0\x80");
}

#[test]
fn wtf8buf_extend() {
    fn e(initial: &[u32], extended: &[u32]) -> Wtf8Buf {
        fn c(value: &u32) -> CodePoint {
            CodePoint::from_u32(*value).unwrap()
        }
        let mut string = initial.iter().map(c).collect::<Wtf8Buf>();
        string.extend(extended.iter().map(c));
        string
    }

    assert_eq!(
        e(&[0x61, 0xE9], &[0x20, 0x1F4A9]).bytes,
        b"a\xC3\xA9 \xF0\x9F\x92\xA9"
    );

    assert_eq!(e(&[0xD83D], &[0xDCA9]).bytes, b"\xF0\x9F\x92\xA9"); // Magic!
    assert_eq!(
        e(&[0xD83D, 0x20], &[0xDCA9]).bytes,
        b"\xED\xA0\xBD \xED\xB2\xA9"
    );
    assert_eq!(e(&[0xD800], &[0xDBFF]).bytes, b"\xED\xA0\x80\xED\xAF\xBF");
    assert_eq!(e(&[0xD800], &[0xE000]).bytes, b"\xED\xA0\x80\xEE\x80\x80");
    assert_eq!(e(&[0xD7FF], &[0xDC00]).bytes, b"\xED\x9F\xBF\xED\xB0\x80");
    assert_eq!(e(&[0x61], &[0xDC00]).bytes, b"\x61\xED\xB0\x80");
    assert_eq!(e(&[], &[0xDC00]).bytes, b"\xED\xB0\x80");
}

#[test]
fn wtf8buf_show() {
    let mut string = Wtf8Buf::from_str("a\té \u{7f}💩\r");
    string.push(CodePoint::from_u32(0xD800).unwrap());
    assert_eq!(
        format!("{:?}", string),
        "\"a\\té \\u{7f}\u{1f4a9}\\r\\u{d800}\""
    );
}

#[test]
fn wtf8buf_as_slice() {
    assert_eq!(Wtf8Buf::from_str("aé").as_wtf8(), Wtf8::new("aé"));
}

#[test]
fn wtf8buf_show_str() {
    let text = "a\té 💩\r";
    let string = Wtf8Buf::from_str(text);
    assert_eq!(format!("{:?}", text), format!("{:?}", string));
}
