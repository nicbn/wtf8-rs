use core::fmt;
use core::iter::{FusedIterator, Peekable};

/// A Unicode code point: from U+0000 to U+10FFFF.
///
/// Compares with the `char` type,
/// which represents a Unicode scalar value:
/// a code point that is not a surrogate (U+D800 to U+DFFF).
#[derive(Eq, PartialEq, Ord, PartialOrd, Clone, Copy)]
pub struct CodePoint {
    value: u32,
}

/// Format the code point as `U+` followed by four to six hexadecimal digits.
/// Example: `U+1F4A9`
impl fmt::Debug for CodePoint {
    #[inline]
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "U+{:04X}", self.value)
    }
}

impl CodePoint {
    /// Unsafely creates a new `CodePoint` without checking the value.
    ///
    /// Only use when `value` is known to be less than or equal to 0x10FFFF.
    #[inline]
    pub unsafe fn from_u32_unchecked(value: u32) -> CodePoint {
        CodePoint { value }
    }

    /// Creates a new `CodePoint` if the value is a valid code point.
    ///
    /// Returns `None` if `value` is above 0x10FFFF.
    #[inline]
    pub fn from_u32(value: u32) -> Option<CodePoint> {
        match value {
            0..=0x10FFFF => Some(CodePoint { value }),
            _ => None,
        }
    }

    /// Creates a new `CodePoint` from a `char`.
    ///
    /// Since all Unicode scalar values are code points, this always succeeds.
    #[inline]
    pub fn from_char(value: char) -> CodePoint {
        CodePoint {
            value: value as u32,
        }
    }

    /// Returns the numeric value of the code point.
    #[inline]
    pub fn to_u32(&self) -> u32 {
        self.value
    }

    /// Optionally returns a Unicode scalar value for the code point.
    ///
    /// Returns `None` if the code point is a surrogate (from U+D800 to U+DFFF).
    #[inline]
    pub fn to_char(&self) -> Option<char> {
        match self.value {
            0xD800..=0xDFFF => None,
            _ => Some(unsafe { char::from_u32_unchecked(self.value) }),
        }
    }

    /// Returns a Unicode scalar value for the code point.
    ///
    /// Returns `'\u{FFFD}'` (the replacement character “�”)
    /// if the code point is a surrogate (from U+D800 to U+DFFF).
    #[inline]
    pub fn to_char_lossy(&self) -> char {
        self.to_char().unwrap_or('\u{FFFD}')
    }

    /// Decode potentially ill-formed UTF-16.
    #[inline]
    pub fn decode_utf16<I: Iterator<Item = u16>>(input: I) -> DecodeUtf16<I> {
        DecodeUtf16 {
            input: input.peekable(),
        }
    }

    /// Encode potentially ill-formed UTF-16.
    #[inline]
    pub fn encode_utf16<I: Iterator<Item = CodePoint>>(input: I) -> EncodeUtf16<I> {
        EncodeUtf16 { input, buf: None }
    }
}

impl From<char> for CodePoint {
    #[inline]
    fn from(c: char) -> Self {
        Self::from_char(c)
    }
}

/// An iterator for decoding potentially ill-formed UTF-16.
pub struct DecodeUtf16<I: Iterator> {
    input: Peekable<I>,
}
impl<I: Iterator<Item = u16>> Iterator for DecodeUtf16<I> {
    type Item = CodePoint;

    #[inline]
    fn next(&mut self) -> Option<CodePoint> {
        let mut val = self.input.next()? as u32;

        if let 0xD800..=0xDBFF = val {
            if let Some(y @ 0xDC00..=0xDFFF) = self.input.peek().copied() {
                val = 0x1_0000 | ((val - 0xD800) << 10) | (y as u32 - 0xDC00);
                self.input.next();
            }
        }

        Some(unsafe { CodePoint::from_u32_unchecked(val) })
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let (l, h) = self.input.size_hint();
        (l / 2, h)
    }
}
impl<I: FusedIterator<Item = u16>> FusedIterator for DecodeUtf16<I> {}

/// An iterator for encoding potentially ill-formed UTF-16.
pub struct EncodeUtf16<I> {
    input: I,
    buf: Option<u16>,
}
impl<I: Iterator<Item = CodePoint>> Iterator for EncodeUtf16<I> {
    type Item = u16;

    #[inline]
    fn next(&mut self) -> Option<u16> {
        if let Some(x) = self.buf.take() {
            return Some(x);
        }

        let p = self.input.next()?.to_u32();
        if p >= 0x1_0000 {
            self.buf = Some(((p - 0x1_0000) & 0x3FF) as u16 | 0xDC00);
            Some(((p - 0x1_0000) >> 10) as u16 | 0xD800)
        } else {
            Some(p as u16)
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let (l, h) = self.input.size_hint();
        (
            l.saturating_add(self.buf.is_some() as usize),
            h.and_then(|x| x.checked_mul(2))
                .and_then(|x| x.checked_add(self.buf.is_some() as usize)),
        )
    }
}
impl<I: FusedIterator<Item = CodePoint>> FusedIterator for EncodeUtf16<I> {}
