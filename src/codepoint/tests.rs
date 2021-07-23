use super::*;
use alloc::format;

fn c(value: u32) -> CodePoint {
    CodePoint::from_u32(value).unwrap()
}

fn s(value: u16) -> Surrogate {
    Surrogate::from_u16(value).unwrap()
}

#[test]
fn code_point_from_u32() {
    assert!(CodePoint::from_u32(0).is_some());
    assert!(CodePoint::from_u32(0xD800).is_some());
    assert!(CodePoint::from_u32(0x10FFFF).is_some());
    assert!(CodePoint::from_u32(0x110000).is_none());
}

#[test]
fn code_point_to_u32() {
    assert_eq!(c(0).to_u32(), 0);
    assert_eq!(c(0xD800).to_u32(), 0xD800);
    assert_eq!(c(0x10FFFF).to_u32(), 0x10FFFF);
}

#[test]
fn code_point_from_char() {
    assert_eq!(CodePoint::from_char('a').to_u32(), 0x61);
    assert_eq!(CodePoint::from_char('💩').to_u32(), 0x1F4A9);
}

#[test]
fn code_point_to_string() {
    assert_eq!(format!("{:?}", CodePoint::from_char('a')), "U+0061");
    assert_eq!(format!("{:?}", CodePoint::from_char('💩')), "U+1F4A9");
}

#[test]
fn code_point_to_char() {
    assert_eq!(c(0x61).to_char(), Some('a'));
    assert_eq!(c(0x1F4A9).to_char(), Some('💩'));
    assert_eq!(c(0xD800).to_char(), None);
}

#[test]
fn code_point_to_char_lossy() {
    assert_eq!(c(0x61).to_char_lossy(), 'a');
    assert_eq!(c(0x1F4A9).to_char_lossy(), '💩');
    assert_eq!(c(0xD800).to_char_lossy(), '\u{FFFD}');
}

#[test]
fn code_point_from_surrogate() {
    assert_eq!(CodePoint::from_surrogate(s(0xD800)), c(0xD800));
    assert_eq!(CodePoint::from_surrogate(s(0xDC6A)), c(0xDC6A));
    assert_eq!(CodePoint::from_surrogate(s(0xDFFF)), c(0xDFFF));
}

#[test]
fn code_point_to_surrogate() {
    assert_eq!(c(0xD800).to_surrogate(), Some(s(0xD800)));
    assert_eq!(c(0xDC6A).to_surrogate(), Some(s(0xDC6A)));
    assert_eq!(c(0xDFFF).to_surrogate(), Some(s(0xDFFF)));
    assert_eq!(c(0xD7FF).to_surrogate(), None);
    assert_eq!(c(0xE000).to_surrogate(), None);
}

#[test]
fn code_point_categorize() {
    assert_eq!(c(0x61).categorize(), CodePointCategory::Scalar('a'));
    assert_eq!(c(0xD7FF).categorize(), CodePointCategory::Scalar('\u{D7FF}'));
    assert_eq!(c(0xD800).categorize(), CodePointCategory::Surrogate(s(0xD800)));
    assert_eq!(c(0xDC6A).categorize(), CodePointCategory::Surrogate(s(0xDC6A)));
    assert_eq!(c(0xDFFF).categorize(), CodePointCategory::Surrogate(s(0xDFFF)));
    assert_eq!(c(0xE000).categorize(), CodePointCategory::Scalar('\u{E000}'));
    assert_eq!(c(0x1F4A9).categorize(), CodePointCategory::Scalar('💩'));
}

#[test]
fn surrogate_from_u16() {
    assert!(Surrogate::from_u16(0).is_none());
    assert!(Surrogate::from_u16(0xD7FF).is_none());
    assert!(Surrogate::from_u16(0xD800).is_some());
    assert!(Surrogate::from_u16(0xDFFF).is_some());
    assert!(Surrogate::from_u16(0xE000).is_none());
}

#[test]
fn surrogate_to_u16() {
    assert_eq!(s(0xD800).to_u16(), 0xD800);
    assert_eq!(s(0xDC6A).to_u16(), 0xDC6A);
    assert_eq!(s(0xDFFF).to_u16(), 0xDFFF);
}

#[test]
fn surrogate_is_high() {
    assert_eq!(s(0xD800).is_high_surrogate(), true);
    assert_eq!(s(0xDBFF).is_high_surrogate(), true);
    assert_eq!(s(0xDC00).is_high_surrogate(), false);
    assert_eq!(s(0xDFFF).is_high_surrogate(), false);
}
