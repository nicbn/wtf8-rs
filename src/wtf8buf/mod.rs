//! A WTF-8 dynamically sized, growable string.

use crate::{decode_surrogate, decode_surrogate_pair, CodePoint, Wtf8};
use alloc::borrow::ToOwned;
use alloc::boxed::Box;
use alloc::string::String;
use alloc::vec::Vec;
use core::borrow::{Borrow, BorrowMut};
use core::convert::Infallible;
use core::iter::FromIterator;
use core::ops::{Deref, DerefMut};
use core::str::FromStr;
use core::{char, fmt};

#[cfg(test)]
mod tests;

/// A WTF-8 dynamically sized, growable string.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Wtf8Buf {
    bytes: Vec<u8>,
}

impl Wtf8Buf {
    #[inline]
    pub(crate) fn from_bytes(x: Vec<u8>) -> Wtf8Buf {
        Self { bytes: x }
    }

    /// Creates a new, empty WTF-8 string.
    #[inline]
    pub const fn new() -> Wtf8Buf {
        Wtf8Buf { bytes: Vec::new() }
    }

    /// Creates a new, empty WTF-8 string with pre-allocated capacity for `capacity` bytes.
    #[inline]
    pub fn with_capacity(capacity: usize) -> Wtf8Buf {
        Wtf8Buf {
            bytes: Vec::with_capacity(capacity),
        }
    }

    /// Creates a WTF-8 string from a UTF-8 `String`.
    ///
    /// This takes ownership of the `String` and does not copy.
    ///
    /// Since WTF-8 is a superset of UTF-8, this always succeeds.
    #[inline]
    pub fn from_string(string: String) -> Wtf8Buf {
        Wtf8Buf {
            bytes: string.into_bytes(),
        }
    }

    /// Reserves capacity for at least `additional` more bytes to be inserted
    /// in the given `Wtf8Buf`.
    /// The collection may reserve more space to avoid frequent reallocations.
    ///
    /// # Panics
    ///
    /// Panics if the new capacity overflows `usize`.
    #[inline]
    pub fn reserve(&mut self, additional: usize) {
        self.bytes.reserve(additional)
    }

    /// Reserves the minimum capacity for exactly `additional` more elements to
    /// be inserted in the given `Wtf8Buf`. After calling `reserve_exact`,
    /// capacity will be greater than or equal to `self.len() + additional`.
    /// Does nothing if the capacity is already sufficient.
    ///
    /// Note that the allocator may give the collection more space than it
    /// requests. Therefore, capacity can not be relied upon to be precisely
    /// minimal. Prefer `reserve` if future insertions are expected.
    ///
    /// # Panics
    ///
    /// Panics if the new capacity overflows `usize`.
    #[inline]
    pub fn reserve_exact(&mut self, additional: usize) {
        self.bytes.reserve_exact(additional)
    }

    /// Shrinks the capacity of the vector as much as possible.
    ///
    /// It will drop down as close as possible to the length but the allocator
    /// may still inform the vector that there is space for a few more elements.
    #[inline]
    pub fn shrink_to_fit(&mut self) {
        self.bytes.shrink_to_fit()
    }

    /// Returns the number of bytes that this string buffer can hold without reallocating.
    #[inline]
    pub fn capacity(&self) -> usize {
        self.bytes.capacity()
    }

    /// Creates a WTF-8 string from a UTF-8 `&str` slice.
    ///
    /// This copies the content of the slice.
    ///
    /// Since WTF-8 is a superset of UTF-8, this always succeeds.
    #[inline]
    #[allow(clippy::should_implement_trait)]
    pub fn from_str(str: &str) -> Wtf8Buf {
        Wtf8Buf {
            bytes: <[_]>::to_vec(str.as_bytes()),
        }
    }

    /// Clears the string.
    #[inline]
    pub fn clear(&mut self) {
        self.bytes.clear()
    }

    /// Creates a WTF-8 string from a potentially ill-formed UTF-16 iterator of 16-bit code units.
    ///
    /// This is lossless: calling `.encode_utf16()` on the resulting string
    /// will always return the original code units.
    pub fn from_utf16<I>(v: I) -> Wtf8Buf
    where
        I: IntoIterator<Item = u16>,
    {
        let iter = v.into_iter();
        let mut string = Wtf8Buf::with_capacity(iter.size_hint().0);
        for item in char::decode_utf16(iter) {
            match item {
                Ok(ch) => string.push_char(ch),
                Err(surrogate) => {
                    let surrogate = surrogate.unpaired_surrogate();
                    // Surrogates are known to be in the code point range.
                    let code_point = unsafe { CodePoint::from_u32_unchecked(surrogate as u32) };
                    // Skip the WTF-8 concatenation check,
                    // surrogate pairs are already decoded by decode_utf16
                    string.push_code_point_unchecked(code_point)
                }
            }
        }
        string
    }

    /// Returns the slice of this object.
    #[inline]
    pub fn as_wtf8(&self) -> &Wtf8 {
        // Safety: Wtf8 is transparent, type layouts match.
        unsafe { &*(self.bytes.as_slice() as *const [u8] as *const Wtf8) }
    }

    /// Returns the slice of this object.
    #[inline]
    pub fn as_mut_wtf8(&mut self) -> &mut Wtf8 {
        // Safety: Wtf8 is transparent, type layouts match.
        unsafe { &mut *(self.bytes.as_mut_slice() as *mut [u8] as *mut Wtf8) }
    }

    /// Append a UTF-8 slice at the end of the string.
    #[inline]
    pub fn push_str(&mut self, other: &str) {
        self.bytes.extend_from_slice(other.as_bytes())
    }

    /// Append a string with WTF-8 encoding.
    ///
    /// This replaces newly paired surrogates at the boundary
    /// with a supplementary code point,
    /// like concatenating ill-formed UTF-16 strings effectively would.
    #[inline]
    pub fn push_wtf8(&mut self, other: &Wtf8) {
        match (
            (&*self).final_lead_surrogate(),
            other.initial_trail_surrogate(),
        ) {
            // Replace newly paired surrogates by a supplementary code point.
            (Some(lead), Some(trail)) => {
                let len_without_lead_surrogate = self.len() - 3;
                self.bytes.truncate(len_without_lead_surrogate);
                let other_without_trail_surrogate = &other.bytes()[3..];
                // 4 bytes for the supplementary code point
                self.bytes.reserve(4 + other_without_trail_surrogate.len());
                self.push_char(decode_surrogate_pair(lead, trail));
                self.bytes.extend_from_slice(other_without_trail_surrogate);
            }
            _ => self.bytes.extend_from_slice(other.bytes()),
        }
    }

    /// Append a Unicode scalar value at the end of the string.
    #[inline]
    pub fn push_char(&mut self, c: char) {
        self.push_code_point_unchecked(CodePoint::from_char(c))
    }

    /// Append a code point at the end of the string.
    ///
    /// This replaces newly paired surrogates at the boundary
    /// with a supplementary code point,
    /// like concatenating ill-formed UTF-16 strings effectively would.
    #[inline]
    pub fn push(&mut self, code_point: CodePoint) {
        if let trail @ 0xDC00..=0xDFFF = code_point.to_u32() {
            if let Some(lead) = self.final_lead_surrogate() {
                let len_without_lead_surrogate = self.len() - 3;
                self.bytes.truncate(len_without_lead_surrogate);
                self.push_char(decode_surrogate_pair(lead, trail as u16));
                return;
            }
        }

        // No newly paired surrogates at the boundary.
        self.push_code_point_unchecked(code_point)
    }

    /// Shortens a string to the specified length.
    ///
    /// # Panics
    ///
    /// Panics if `new_len` > current length,
    /// or if `new_len` is not a code point boundary.
    #[inline]
    pub fn truncate(&mut self, new_len: usize) {
        assert!(self.is_code_point_boundary(new_len));
        self.bytes.truncate(new_len)
    }

    /// Consumes the WTF-8 string and tries to convert it to UTF-8.
    ///
    /// This does not copy the data.
    ///
    /// If the contents are not well-formed UTF-8
    /// (that is, if the string contains surrogates),
    /// the original WTF-8 string is returned instead.
    pub fn into_string(self) -> Result<String, IntoStringError> {
        let chunks = self.chunks();

        match chunks.next_surrogate() {
            Some(position) => Err(IntoStringError {
                wtf8: self,
                valid_up_to: position,
            }),
            // Safety: No surrogates, so UTF-8 is guaranteed.
            None => unsafe { Ok(String::from_utf8_unchecked(self.bytes)) },
        }
    }

    /// Consumes the WTF-8 string and converts it lossily to UTF-8.
    ///
    /// This does not copy the data (but may overwrite parts of it in place).
    ///
    /// Surrogates are replaced with `"\u{FFFD}"` (the replacement character “�”)
    pub fn into_string_lossy(self) -> String {
        let chunks = self.chunks();

        if chunks.next_surrogate().is_none() {
            // Safety: No surrogates, so UTF-8 is guaranteed.
            unsafe { String::from_utf8_unchecked(self.bytes) }
        } else {
            self.to_string_lossy().into_owned()
        }
    }

    /// Converts this `Wtf8Buf` into a boxed `Wtf8`.
    #[inline]
    pub fn into_box(self) -> Box<Wtf8> {
        // Safety: type layouts match.
        unsafe { Box::from_raw(Box::into_raw(self.bytes.into_boxed_slice()) as *mut Wtf8) }
    }

    /// Converts a `Box<Wtf8>` into a `Wtf8Buf`.
    pub fn from_box(boxed: Box<Wtf8>) -> Wtf8Buf {
        // Safety: type layouts are the same.
        let bytes: Box<[u8]> = unsafe { Box::from_raw(Box::into_raw(boxed) as *mut [u8]) };
        Wtf8Buf {
            bytes: bytes.into_vec(),
        }
    }

    #[inline]
    fn push_code_point_unchecked(&mut self, code_point: CodePoint) {
        const TAG_CONT: u8 = 0b1000_0000;
        const TAG_TWO_B: u8 = 0b1100_0000;
        const TAG_THREE_B: u8 = 0b1110_0000;
        const TAG_FOUR_B: u8 = 0b1111_0000;
        const MAX_ONE_B: u32 = 0x80;
        const MAX_TWO_B: u32 = 0x800;
        const MAX_THREE_B: u32 = 0x10000;

        #[inline]
        const fn len_utf8(code: u32) -> usize {
            if code < MAX_ONE_B {
                1
            } else if code < MAX_TWO_B {
                2
            } else if code < MAX_THREE_B {
                3
            } else {
                4
            }
        }

        #[inline]
        fn encode_utf8_raw(code: u32, dst: &mut [u8]) -> &mut [u8] {
            let len = len_utf8(code);
            #[allow(clippy::redundant_slicing)]
            match (len, &mut dst[..]) {
                (1, [a, ..]) => {
                    *a = code as u8;
                }
                (2, [a, b, ..]) => {
                    *a = (code >> 6 & 0x1F) as u8 | TAG_TWO_B;
                    *b = (code & 0x3F) as u8 | TAG_CONT;
                }
                (3, [a, b, c, ..]) => {
                    *a = (code >> 12 & 0x0F) as u8 | TAG_THREE_B;
                    *b = (code >> 6 & 0x3F) as u8 | TAG_CONT;
                    *c = (code & 0x3F) as u8 | TAG_CONT;
                }
                (4, [a, b, c, d, ..]) => {
                    *a = (code >> 18 & 0x07) as u8 | TAG_FOUR_B;
                    *b = (code >> 12 & 0x3F) as u8 | TAG_CONT;
                    *c = (code >> 6 & 0x3F) as u8 | TAG_CONT;
                    *d = (code & 0x3F) as u8 | TAG_CONT;
                }
                _ => panic!(
                    "encode_utf8: need {} bytes to encode U+{:X}, but the buffer has {}",
                    len,
                    code,
                    dst.len(),
                ),
            };
            &mut dst[..len]
        }

        let mut bytes = [0; 4];
        let bytes = encode_utf8_raw(code_point.to_u32(), &mut bytes);
        self.bytes.extend_from_slice(bytes)
    }

    #[inline]
    fn final_lead_surrogate(&self) -> Option<u16> {
        match self.bytes() {
            [.., 0xED, b2 @ 0xA0..=0xAF, b3] => Some(decode_surrogate(*b2, *b3)),
            _ => None,
        }
    }
}

impl Deref for Wtf8Buf {
    type Target = Wtf8;
    #[inline]
    fn deref(&self) -> &Wtf8 {
        self.as_wtf8()
    }
}

impl DerefMut for Wtf8Buf {
    #[inline]
    fn deref_mut(&mut self) -> &mut Wtf8 {
        self.as_mut_wtf8()
    }
}

impl From<String> for Wtf8Buf {
    #[inline]
    fn from(x: String) -> Wtf8Buf {
        Wtf8Buf::from_string(x)
    }
}
impl From<&str> for Wtf8Buf {
    #[inline]
    fn from(x: &str) -> Wtf8Buf {
        Wtf8Buf::from_str(x)
    }
}
impl From<&Wtf8> for Wtf8Buf {
    #[inline]
    fn from(x: &Wtf8) -> Wtf8Buf {
        x.to_owned()
    }
}

impl AsRef<Wtf8> for Wtf8Buf {
    #[inline]
    fn as_ref(&self) -> &Wtf8 {
        self
    }
}
impl Borrow<Wtf8> for Wtf8Buf {
    #[inline]
    fn borrow(&self) -> &Wtf8 {
        self
    }
}
impl AsMut<Wtf8> for Wtf8Buf {
    #[inline]
    fn as_mut(&mut self) -> &mut Wtf8 {
        self
    }
}
impl BorrowMut<Wtf8> for Wtf8Buf {
    #[inline]
    fn borrow_mut(&mut self) -> &mut Wtf8 {
        self
    }
}

impl FromStr for Wtf8Buf {
    type Err = Infallible;

    #[inline]
    fn from_str(s: &str) -> Result<Self, Infallible> {
        Ok(Wtf8Buf::from_str(s))
    }
}

impl ToOwned for Wtf8 {
    type Owned = Wtf8Buf;

    #[inline]
    fn to_owned(&self) -> Wtf8Buf {
        Wtf8Buf {
            bytes: self.bytes().to_owned(),
        }
    }
}

/// Creates a new WTF-8 string from an iterator of code points.
///
/// This replaces surrogate code point pairs with supplementary code points,
/// like concatenating ill-formed UTF-16 strings effectively would.
impl FromIterator<CodePoint> for Wtf8Buf {
    fn from_iter<T: IntoIterator<Item = CodePoint>>(iter: T) -> Wtf8Buf {
        let mut string = Wtf8Buf::new();
        string.extend(iter);
        string
    }
}

impl FromIterator<char> for Wtf8Buf {
    fn from_iter<T: IntoIterator<Item = char>>(iter: T) -> Wtf8Buf {
        let mut string = Wtf8Buf::new();
        string.extend(iter);
        string
    }
}

impl<'a> FromIterator<&'a Wtf8> for Wtf8Buf {
    fn from_iter<T: IntoIterator<Item = &'a Wtf8>>(iter: T) -> Wtf8Buf {
        let mut string = Wtf8Buf::new();
        string.extend(iter);
        string
    }
}

impl<'a> FromIterator<&'a str> for Wtf8Buf {
    fn from_iter<T: IntoIterator<Item = &'a str>>(iter: T) -> Wtf8Buf {
        let mut string = Wtf8Buf::new();
        string.extend(iter);
        string
    }
}

impl<'a> FromIterator<&'a CodePoint> for Wtf8Buf {
    fn from_iter<T: IntoIterator<Item = &'a CodePoint>>(iter: T) -> Wtf8Buf {
        let mut string = Wtf8Buf::new();
        string.extend(iter);
        string
    }
}

impl<'a> FromIterator<&'a char> for Wtf8Buf {
    fn from_iter<T: IntoIterator<Item = &'a char>>(iter: T) -> Wtf8Buf {
        let mut string = Wtf8Buf::new();
        string.extend(iter);
        string
    }
}

/// Append code points from an iterator to the string.
///
/// This replaces surrogate code point pairs with supplementary code points,
/// like concatenating ill-formed UTF-16 strings effectively would.
impl Extend<CodePoint> for Wtf8Buf {
    fn extend<T: IntoIterator<Item = CodePoint>>(&mut self, iter: T) {
        let iterator = iter.into_iter();
        let (low, _high) = iterator.size_hint();
        // Lower bound of one byte per code point (ASCII only)
        self.bytes.reserve(low);
        for code_point in iterator {
            self.push(code_point);
        }
    }
}

impl Extend<char> for Wtf8Buf {
    fn extend<T: IntoIterator<Item = char>>(&mut self, iter: T) {
        let iterator = iter.into_iter();
        let (low, _high) = iterator.size_hint();
        self.bytes.reserve(low);
        for c in iterator {
            self.push_char(c);
        }
    }
}

impl<'a> Extend<&'a str> for Wtf8Buf {
    fn extend<T: IntoIterator<Item = &'a str>>(&mut self, iter: T) {
        let iterator = iter.into_iter();
        let (low, _high) = iterator.size_hint();
        self.bytes.reserve(low);
        for c in iterator {
            self.push_str(c);
        }
    }
}

impl<'a> Extend<&'a Wtf8> for Wtf8Buf {
    fn extend<T: IntoIterator<Item = &'a Wtf8>>(&mut self, iter: T) {
        let iterator = iter.into_iter();
        let (low, _high) = iterator.size_hint();
        self.bytes.reserve(low);
        for c in iterator {
            self.push_wtf8(c);
        }
    }
}

impl<'a> Extend<&'a CodePoint> for Wtf8Buf {
    #[inline]
    fn extend<T: IntoIterator<Item = &'a CodePoint>>(&mut self, iter: T) {
        self.extend(iter.into_iter().copied())
    }
}

impl<'a> Extend<&'a char> for Wtf8Buf {
    #[inline]
    fn extend<T: IntoIterator<Item = &'a char>>(&mut self, iter: T) {
        self.extend(iter.into_iter().copied())
    }
}

impl fmt::Debug for Wtf8Buf {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self.as_wtf8(), f)
    }
}

impl fmt::Display for Wtf8Buf {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.as_wtf8(), f)
    }
}

/// Errors which can occur when converting `Wtf8Buf` to `String`.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct IntoStringError {
    pub wtf8: Wtf8Buf,
    valid_up_to: usize,
}
impl IntoStringError {
    /// Returns the index in the given string up to which valid UTF-8 was
    /// verified.
    ///
    /// It is the maximum index such that `wstr[..index].to_str()` would
    /// return `Ok(_)`.
    #[inline]
    pub fn valid_up_to(&self) -> usize {
        self.valid_up_to
    }

    /// The length provided is that of the invalid byte sequence
    /// that starts at the index given by `valid_up_to()`.
    /// Decoding should resume after that sequence
    /// (after inserting a [`U+FFFD REPLACEMENT CHARACTER`][U+FFFD]) in case
    /// of lossy decoding.
    ///
    /// [U+FFFD]: ../../std/char/constant.REPLACEMENT_CHARACTER.html
    #[inline]
    pub fn error_len(&self) -> usize {
        3
    }
}
impl fmt::Display for IntoStringError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "invalid utf-8 sequence of 3 bytes from index {}",
            self.valid_up_to
        )
    }
}
