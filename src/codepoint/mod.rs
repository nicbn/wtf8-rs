//! A Unicode code point: from U+0000 to U+10FFFF.

use core::fmt;
use core::iter::{FusedIterator, Peekable};
use core::num::NonZeroU16;

#[cfg(test)]
mod tests;

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
    /// # Safety
    ///
    /// Only safe if `value` is less than or equal to 0x10FFFF.
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

    /// Creates a new `CodePoint` from a [Surrogate].
    ///
    /// Since all surrogates are code points, this always succeeds.
    #[inline]
    pub fn from_surrogate(value: Surrogate) -> CodePoint {
        CodePoint {
            value: value.to_u16() as u32,
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
        match self.categorize() {
            CodePointCategory::Scalar(c) => Some(c),
            _ => None,
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

    /// Optionally returns a Unicode surrogate value for the code point.
    ///
    /// Returns `None` if the code point is not a surrogate (from U+D800 to U+DFFF).
    #[inline]
    pub fn to_surrogate(&self) -> Option<Surrogate> {
        match self.categorize() {
            CodePointCategory::Surrogate(s) => Some(s),
            _ => None,
        }
    }

    /// Returns a [CodePointCategory], categorizing the code point as a surrogate
    /// or a valid Unicode scalar.
    #[inline]
    pub fn categorize(&self) -> CodePointCategory {
        match self.value {
            // Safety: value is known to be in surrogate range.
            0xD800..=0xDFFF => CodePointCategory::Surrogate(unsafe {
                Surrogate::from_u16_unchecked(self.value as u16)
            }),
            // Safety: value is known to be in char range, because it is not
            // a surrogate, and is less than (#impl-Index<T>) as this is guaranteed
            // by the type.
            _ => CodePointCategory::Scalar(unsafe {
                char::from_u32_unchecked(self.value)
            }),
        }
    }

    /// Decode potentially ill-formed UTF-16.
    #[inline]
    pub fn decode_utf16<I>(input: I) -> DecodeUtf16<I>
    where
        I: Iterator<Item = u16>,
    {
        DecodeUtf16 {
            input: input.peekable(),
        }
    }

    /// Encode potentially ill-formed UTF-16.
    #[inline]
    pub fn encode_utf16<I>(input: I) -> EncodeUtf16<I>
    where
        I: Iterator<Item = CodePoint>,
    {
        EncodeUtf16 { input, buf: None }
    }
}

impl From<char> for CodePoint {
    #[inline]
    fn from(c: char) -> Self {
        Self::from_char(c)
    }
}

impl From<Surrogate> for CodePoint {
    #[inline]
    fn from(s: Surrogate) -> Self {
        Self::from_surrogate(s)
    }
}

impl From<CodePointCategory> for CodePoint {
    #[inline]
    fn from(cat: CodePointCategory) -> Self {
        match cat {
            CodePointCategory::Scalar(c) => Self::from_char(c),
            CodePointCategory::Surrogate(s) => Self::from_surrogate(s),
        }
    }
}

/// A Unicode high or low surrogate: from U+D800 to U+DFFF.
#[derive(Eq, PartialEq, Ord, PartialOrd, Clone, Copy)]
pub struct Surrogate {
    // By using a non-zero u16 (which all surrogates are guaranteed to be),
    // we allow Option<Surrogate>s to be packed into 2 bytes, among other
    // optimizations.
    value: NonZeroU16,
}

/// Format the code point as `U+` followed by four hexadecimal digits.
/// Example: `U+D8F9`
impl fmt::Debug for Surrogate {
    #[inline]
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "U+{:04X}", self.value)
    }
}

impl Surrogate {
    /// Unsafely creates a new `Surrogate` without checking the value.
    ///
    /// # Safety
    ///
    /// Only safe if `value` is between 0xD800 and 0xDFFF, inclusive.
    #[inline]
    pub unsafe fn from_u16_unchecked(value: u16) -> Surrogate {
        // Safety: if the value is >= 0xD800, it must be non-zero.
        Surrogate { value: NonZeroU16::new_unchecked(value) }
    }

    /// Creates a new `Surrogate` if the value is a valid Unicode surrogate.
    ///
    /// Returns `None` if `value` is below 0xD800 or above 0xDFFF.
    #[inline]
    pub fn from_u16(value: u16) -> Option<Surrogate> {
        match value {
            // Safety: we have just checked the function invariant.
            0xD800..=0xDFFF => Some(unsafe { Surrogate::from_u16_unchecked(value) }),
            _ => None,
        }
    }

    /// Returns the numeric value of the surrogate.
    #[inline]
    pub fn to_u16(&self) -> u16 {
        self.value.get()
    }

    /// Returns `true` if the surrogate is a high surrogate (from U+D800 to U+DBFF)
    /// and `false` if the surrogate is a low surrogate (from U+DC00 to U+DFFF).
    #[inline]
    pub fn is_high_surrogate(&self) -> bool {
        matches!(self.value.get(), 0xD800..=0xDBFF)
    }
}

/// An enum that separates a Unicode code point into two options:
/// valid Unicode scalar, or surrogate.
///
/// Returned from the [`categorize`] method on [CodePoint].
///
/// [`categorize`]: CodePoint::categorize
#[derive(Eq, PartialEq, Ord, PartialOrd, Clone, Copy, Debug)]
pub enum CodePointCategory {
    Scalar(char),
    Surrogate(Surrogate),
}

/// An iterator for decoding potentially ill-formed UTF-16.
pub struct DecodeUtf16<I>
where
    I: Iterator<Item = u16>,
{
    input: Peekable<I>,
}
impl<I> Iterator for DecodeUtf16<I>
where
    I: Iterator<Item = u16>,
{
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

        // Safety: this can not be greater than 0x10FFFF by construction.
        Some(unsafe { CodePoint::from_u32_unchecked(val) })
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let (l, h) = self.input.size_hint();
        (l / 2, h)
    }
}
impl<I> FusedIterator for DecodeUtf16<I> where I: FusedIterator<Item = u16> {}

/// An iterator for encoding potentially ill-formed UTF-16.
pub struct EncodeUtf16<I>
where
    I: Iterator<Item = CodePoint>,
{
    input: I,
    buf: Option<u16>,
}
impl<I> Iterator for EncodeUtf16<I>
where
    I: Iterator<Item = CodePoint>,
{
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
impl<I> FusedIterator for EncodeUtf16<I> where I: FusedIterator<Item = CodePoint> {}
