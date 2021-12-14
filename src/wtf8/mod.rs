//! A WTF-8 slice.

use crate::wtf8buf::Wtf8Buf;
use crate::{codepoint, decode_surrogate, CodePoint};
use alloc::borrow::Cow;
use alloc::boxed::Box;
use alloc::rc::Rc;
use alloc::sync::Arc;
use alloc::vec::Vec;
use core::iter::FusedIterator;
use core::ops::{Index, Range, RangeFrom, RangeFull, RangeTo};
use core::{fmt, ptr, slice, str};

#[cfg(test)]
mod tests;

/// A WTF-8 slice.
#[derive(PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct Wtf8 {
    bytes: [u8],
}

impl Wtf8 {
    #[inline]
    pub(crate) fn bytes(&self) -> &[u8] {
        &self.bytes
    }

    /// Coerces into a `Wtf8`. This accepts an [`&str`](prim@str) argument.
    #[inline]
    pub fn new<T: ?Sized + AsRef<Wtf8>>(x: &T) -> &Self {
        x.as_ref()
    }

    /// Returns the length, in WTF-8 bytes.
    #[inline]
    pub fn len(&self) -> usize {
        self.bytes.len()
    }

    /// Returns whether this is empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.bytes.is_empty()
    }

    /// Returns the code point at `position` if it is in the ASCII range,
    /// or `b'\xFF'` otherwise.
    ///
    /// # Panics
    ///
    /// Panics if `position` is beyond the end of the string.
    #[inline]
    pub fn ascii_byte_at(&self, position: usize) -> u8 {
        match self.bytes[position] {
            ascii_byte @ 0x00..=0x7F => ascii_byte,
            _ => 0xFF,
        }
    }

    /// Returns an iterator for the string’s code points.
    #[inline]
    pub fn code_points(&self) -> CodePoints<'_> {
        CodePoints {
            bytes: self.bytes.iter(),
        }
    }

    /// Tries to convert the string to UTF-8 and return a [`&str`](prim@str) slice.
    ///
    /// Returns `Err(_)` if the string contains surrogates.
    ///
    /// This does not copy the data.
    pub fn to_str(&self) -> Result<&str, ToStrError> {
        let mut chunks = self.chunks();

        let x = match chunks.next() {
            Some(Wtf8Chunk::Utf8(str)) => str,
            Some(Wtf8Chunk::UnpairedSurrogate(_)) => return Err(ToStrError { valid_up_to: 0 }),
            None => return Ok(""),
        };

        if chunks.next().is_some() {
            return Err(ToStrError {
                valid_up_to: x.len(),
            });
        }

        Ok(x)
    }

    /// Converts this string into a iterator of [`Wtf8Chunk`].
    ///
    /// The resulting iterator will intercalate [`Utf8`](Wtf8Chunk::Utf8) chunks
    /// with one or more [`UnpairedSurrogate`](Wtf8Chunk::UnpairedSurrogate) and
    /// all contained codepoints can be recovered from it.
    #[inline]
    pub fn chunks(&self) -> Chunks {
        Chunks(&self.bytes)
    }

    /// Lossily converts the string to UTF-8.
    /// Returns a UTF-8 `&str` slice if the contents are well-formed in UTF-8.
    ///
    /// Surrogates are replaced with `"\u{FFFD}"` (the replacement character “�”).
    ///
    /// This only copies the data if necessary (if it contains any surrogate).
    pub fn to_string_lossy(&self) -> Cow<str> {
        let mut chunks = self.chunks();

        if chunks.next_surrogate().is_none() {
            return Cow::Borrowed(chunks.next().and_then(Wtf8Chunk::utf8).unwrap_or(""));
        }

        let chunks: Vec<_> = chunks.map(|a| a.utf8().unwrap_or("\u{FFFD}")).collect();
        Cow::Owned(chunks.join(""))
    }

    /// Returns a slice of the given string for the byte range.
    ///
    /// Returns `None` whenever [`index`](#impl-Index<T>) would panic.
    #[inline]
    pub fn get<I: Wtf8Index>(&self, i: I) -> Option<&Self> {
        i.get(self)
    }

    /// Converts the WTF-8 string to potentially ill-formed UTF-16
    /// and return an iterator of 16-bit code units.
    #[inline]
    pub fn encode_utf16(&self) -> EncodeUtf16<'_> {
        EncodeUtf16(CodePoint::encode_utf16(self.code_points()))
    }

    /// Returns a slice of the given string for the byte range.
    ///
    /// # Safety
    ///
    /// Produces undefined behaviour whenever [`index`](#impl-Index<T>) would panic.
    #[inline]
    pub unsafe fn get_unchecked<I: Wtf8Index>(&self, i: I) -> &Self {
        &*i.get_unchecked(self)
    }

    /// Whether a given index is at a code point boundary.
    #[inline]
    pub fn is_code_point_boundary(&self, index: usize) -> bool {
        if index == self.len() {
            return true;
        }
        !matches!(self.bytes.get(index), None | Some(128..=191))
    }

    /// Boxes this `Wtf8`.
    #[inline]
    pub fn to_box(&self) -> Box<Wtf8> {
        let boxed: Box<[u8]> = self.bytes.into();
        // Safety: This is sound as type layouts match
        unsafe { Box::from_raw(Box::into_raw(boxed) as *mut Wtf8) }
    }

    /// Creates a boxed, empty `Wtf8`.
    pub fn empty_box() -> Box<Wtf8> {
        let boxed: Box<[u8]> = Default::default();
        // Safety: This is sound as type layouts match
        unsafe { Box::from_raw(Box::into_raw(boxed) as *mut Wtf8) }
    }

    /// Boxes this `Wtf8` with [`Arc`](alloc::sync::Arc).
    #[inline]
    pub fn to_arc(&self) -> Arc<Wtf8> {
        let arc: Arc<[u8]> = Arc::from(&self.bytes);
        // Safety: This is sound as type layouts match
        unsafe { Arc::from_raw(Arc::into_raw(arc) as *const Wtf8) }
    }

    /// Boxes this `Wtf8` with [`Rc`](alloc::rc::Rc).
    #[inline]
    pub fn to_rc(&self) -> Rc<Wtf8> {
        let rc: Rc<[u8]> = Rc::from(&self.bytes);
        // Safety: This is sound as type layouts match
        unsafe { Rc::from_raw(Rc::into_raw(rc) as *const Wtf8) }
    }

    /// Converts this slice to its ASCII lower case equivalent in-place.
    ///
    /// ASCII letters 'A' to 'Z' are mapped to 'a' to 'z',
    /// but non-ASCII letters are unchanged.
    ///
    /// To return a new lowercased value without modifying the existing one, use
    /// [`to_ascii_lowercase`].
    ///
    /// [`to_ascii_lowercase`]: #method.to_ascii_lowercase
    #[inline]
    pub fn make_ascii_lowercase(&mut self) {
        self.bytes.make_ascii_lowercase()
    }

    /// Converts this slice to its ASCII upper case equivalent in-place.
    ///
    /// ASCII letters 'a' to 'z' are mapped to 'A' to 'Z',
    /// but non-ASCII letters are unchanged.
    ///
    /// To return a new uppercased value without modifying the existing one, use
    /// [`to_ascii_uppercase`].
    ///
    /// [`to_ascii_uppercase`]: #method.to_ascii_uppercase
    #[inline]
    pub fn make_ascii_uppercase(&mut self) {
        self.bytes.make_ascii_uppercase()
    }

    /// Returns a [`Wtf8Buf`] containing a copy of this slice where each byte
    /// is mapped to its ASCII lower case equivalent.
    ///
    /// ASCII letters 'A' to 'Z' are mapped to 'a' to 'z',
    /// but non-ASCII letters are unchanged.
    #[inline]
    pub fn to_ascii_lowercase(&self) -> Wtf8Buf {
        Wtf8Buf::from_bytes(self.bytes.to_ascii_lowercase())
    }

    /// Returns a [`Wtf8Buf`] containing a copy of this slice where each byte
    /// is mapped to its ASCII upper case equivalent.
    ///
    /// ASCII letters 'a' to 'z' are mapped to 'A' to 'Z',
    /// but non-ASCII letters are unchanged.
    ///
    /// To uppercase the value in-place, use [`make_ascii_uppercase`].
    ///
    /// [`make_ascii_uppercase`]: #method.make_ascii_uppercase
    #[inline]
    pub fn to_ascii_uppercase(&self) -> Wtf8Buf {
        Wtf8Buf::from_bytes(self.bytes.to_ascii_uppercase())
    }

    /// Checks if all bytes in this slice are within the ASCII range.
    #[inline]
    pub fn is_ascii(&self) -> bool {
        self.bytes.is_ascii()
    }

    /// Checks that two slices are an ASCII case-insensitive match.
    ///
    /// Same as `to_ascii_lowercase(a) == to_ascii_lowercase(b)`,
    /// but without allocating and copying temporaries.
    #[inline]
    pub fn eq_ignore_ascii_case(&self, other: &Self) -> bool {
        self.bytes.eq_ignore_ascii_case(&other.bytes)
    }

    #[inline]
    pub(crate) fn initial_trail_surrogate(&self) -> Option<u16> {
        match self.bytes {
            [0xED, b2 @ 0xB0..=0xBF, b3, ..] => Some(decode_surrogate(b2, b3)),
            _ => None,
        }
    }
}

impl From<&Wtf8> for Box<Wtf8> {
    #[inline]
    fn from(x: &Wtf8) -> Self {
        x.to_box()
    }
}

impl From<&Wtf8> for Rc<Wtf8> {
    #[inline]
    fn from(x: &Wtf8) -> Self {
        x.to_rc()
    }
}

impl From<&Wtf8> for Arc<Wtf8> {
    #[inline]
    fn from(x: &Wtf8) -> Self {
        x.to_arc()
    }
}

impl fmt::Debug for Wtf8 {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        use core::fmt::Write;

        formatter.write_str("\"")?;

        for c in self.chunks() {
            match c {
                Wtf8Chunk::Utf8(c) => {
                    for ch in c.chars().flat_map(|x| x.escape_debug()) {
                        formatter.write_char(ch)?;
                    }
                }
                Wtf8Chunk::UnpairedSurrogate(e) => write!(formatter, "\\u{{{:x}}}", e)?,
            }
        }

        formatter.write_str("\"")
    }
}

impl fmt::Display for Wtf8 {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        for chunk in self.chunks() {
            formatter.write_str(chunk.utf8().unwrap_or("\u{FFFD}"))?;
        }

        Ok(())
    }
}

/// Returns a slice of the given string for the byte range.
///
/// # Panics
///
/// Panics when the boundaries of the range do not point to code point
/// boundaries, or point beyond the end of the string.
impl<T: Wtf8Index> Index<T> for Wtf8 {
    type Output = Wtf8;

    #[inline]
    fn index(&self, index: T) -> &Wtf8 {
        match self.get(index.clone()) {
            Some(x) => x,
            None => panic!(
                "index {:?} in `{:?}` do not lie on character boundary",
                index, self
            ),
        }
    }
}

impl AsRef<Wtf8> for str {
    #[inline]
    fn as_ref(&self) -> &Wtf8 {
        // Safety: the cast is sound because repr(transparent), matching the layout of str.
        // UTF-8 is a subset of WTF-8, so type invariants are never violated.
        unsafe { &*(self as *const str as *const Wtf8) }
    }
}

/// A helper trait to do `get` operation on `Wtf8`.
///
/// This trait is sealed and not meant to be implemented by an user of this
/// library.
#[allow(clippy::missing_safety_doc)]
pub unsafe trait Wtf8Index: fmt::Debug + Clone + private::Sealed {
    fn get(self, slice: &Wtf8) -> Option<&Wtf8>;
    unsafe fn get_unchecked(self, slice: *const Wtf8) -> *const Wtf8;
}
unsafe impl Wtf8Index for Range<usize> {
    #[inline]
    fn get(self, slice: &Wtf8) -> Option<&Wtf8> {
        if self.start > self.end
            || self.end >= slice.len()
            || !slice.is_code_point_boundary(self.start)
            || !slice.is_code_point_boundary(self.end)
        {
            None
        } else {
            // Safety: guaranteed by the conditions above.
            Some(unsafe { &*self.get_unchecked(slice) })
        }
    }
    #[inline]
    unsafe fn get_unchecked(self, slice: *const Wtf8) -> *const Wtf8 {
        ptr::slice_from_raw_parts(
            (*slice).bytes.as_ptr().add(self.start),
            self.end - self.start,
        ) as _
    }
}
unsafe impl Wtf8Index for RangeFrom<usize> {
    #[inline]
    fn get(self, slice: &Wtf8) -> Option<&Wtf8> {
        if !slice.is_code_point_boundary(self.start) {
            None
        } else {
            // Safety: guaranteed by the conditions above.
            Some(unsafe { &*self.get_unchecked(slice) })
        }
    }
    #[inline]
    unsafe fn get_unchecked(self, slice: *const Wtf8) -> *const Wtf8 {
        ptr::slice_from_raw_parts(
            (*slice).bytes.as_ptr().add(self.start),
            (*slice).len() - self.start,
        ) as _
    }
}
unsafe impl Wtf8Index for RangeTo<usize> {
    #[inline]
    fn get(self, slice: &Wtf8) -> Option<&Wtf8> {
        if self.end >= slice.len() || !slice.is_code_point_boundary(self.end) {
            None
        } else {
            // Safety: guaranteed by the conditions above.
            Some(unsafe { &*self.get_unchecked(slice) })
        }
    }
    #[inline]
    unsafe fn get_unchecked(self, slice: *const Wtf8) -> *const Wtf8 {
        ptr::slice_from_raw_parts((*slice).bytes.as_ptr(), self.end) as _
    }
}
unsafe impl Wtf8Index for RangeFull {
    #[inline]
    fn get(self, slice: &Wtf8) -> Option<&Wtf8> {
        Some(slice)
    }
    #[inline]
    unsafe fn get_unchecked(self, slice: *const Wtf8) -> *const Wtf8 {
        slice
    }
}

/// Errors which can occur when converting `Wtf8` to `str`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ToStrError {
    valid_up_to: usize,
}
impl ToStrError {
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
impl fmt::Display for ToStrError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "invalid utf-8 sequence of 3 bytes from index {}",
            self.valid_up_to
        )
    }
}

/// Iterator of points over a string.
pub struct CodePoints<'a> {
    bytes: slice::Iter<'a, u8>,
}
impl Iterator for CodePoints<'_> {
    type Item = CodePoint;

    #[inline]
    fn next(&mut self) -> Option<CodePoint> {
        // Copied from core::str::next_code_point

        /// Mask of the value bits of a continuation byte.
        const CONT_MASK: u8 = 0b0011_1111;

        /// Returns the initial codepoint accumulator for the first byte.
        /// The first byte is special, only want bottom 5 bits for width 2, 4 bits
        /// for width 3, and 3 bits for width 4.
        #[inline]
        fn utf8_first_byte(byte: u8, width: u32) -> u32 {
            (byte & (0x7F >> width)) as u32
        }

        /// Returns the value of `ch` updated with continuation byte `byte`.
        #[inline]
        fn utf8_acc_cont_byte(ch: u32, byte: u8) -> u32 {
            (ch << 6) | (byte & CONT_MASK) as u32
        }

        #[inline]
        fn unwrap_or_0(opt: Option<&u8>) -> u8 {
            match opt {
                Some(&byte) => byte,
                None => 0,
            }
        }

        let x = *self.bytes.next()?;
        if x < 128 {
            // Safety: the char is ascii.
            return Some(unsafe { CodePoint::from_u32_unchecked(x as u32) });
        }

        // Multibyte case follows
        // Decode from a byte combination out of: [[[x y] z] w]
        // NOTE: Performance is sensitive to the exact formulation here
        let init = utf8_first_byte(x, 2);
        let y = unwrap_or_0(self.bytes.next());
        let mut ch = utf8_acc_cont_byte(init, y);
        if x >= 0xE0 {
            // [[x y z] w] case
            // 5th bit in 0xE0 .. 0xEF is always clear, so `init` is still valid
            let z = unwrap_or_0(self.bytes.next());
            let y_z = utf8_acc_cont_byte((y & CONT_MASK) as u32, z);
            ch = init << 12 | y_z;
            if x >= 0xF0 {
                // [x y z w] case
                // use only the lower 3 bits of `init`
                let w = unwrap_or_0(self.bytes.next());
                ch = (init & 7) << 18 | utf8_acc_cont_byte(y_z, w);
            }
        }

        // Safety: the code point can not be greater than 0x10_FFFF.
        Some(unsafe { CodePoint::from_u32_unchecked(ch) })
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let v = self.bytes.len();
        (v.saturating_add(3) / 4, Some(v))
    }
}
impl FusedIterator for CodePoints<'_> {}

/// An iterator for encoding potentially ill-formed UTF-16 from a WTF-8 input.
pub struct EncodeUtf16<'a>(codepoint::EncodeUtf16<CodePoints<'a>>);
impl Iterator for EncodeUtf16<'_> {
    type Item = u16;

    #[inline]
    fn next(&mut self) -> Option<u16> {
        self.0.next()
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}
impl FusedIterator for EncodeUtf16<'_> {}

/// Part of a WTF-8 slice.
///
/// Either an [`Utf8`](Self::Utf8) string, or a [`UnpairedSurrogate`](Self::UnpairedSurrogate).
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Wtf8Chunk<'a> {
    /// The chunk as a UTF-8 string.
    Utf8(&'a str),

    /// The chunk as an unpaired surrogate.
    UnpairedSurrogate(u16),
}

impl<'a> Wtf8Chunk<'a> {
    /// Returns `Some(_)` if the chunk is UTF-8, and `None` if not.
    #[inline]
    pub fn utf8(self) -> Option<&'a str> {
        match self {
            Wtf8Chunk::Utf8(a) => Some(a),
            _ => None,
        }
    }
}

/// An iterator created by [`chunks`](Wtf8::chunks).
pub struct Chunks<'a>(&'a [u8]);
impl Chunks<'_> {
    #[inline]
    pub(crate) fn next_surrogate(&self) -> Option<usize> {
        let mut pos = 0;
        let mut iter = self.0.iter();

        loop {
            let b = *iter.next()?;
            if b < 0x80 {
                pos += 1;
            } else if b < 0xE0 {
                iter.next();
                pos += 2;
            } else if b == 0xED {
                match (iter.next(), iter.next()) {
                    (Some(&b2), Some(_)) if b2 >= 0xA0 => {
                        return Some(pos);
                    }
                    _ => pos += 3,
                }
            } else if b < 0xF0 {
                iter.next();
                iter.next();
                pos += 3;
            } else {
                iter.next();
                iter.next();
                iter.next();
                pos += 4;
            }
        }
    }
}
impl<'a> Iterator for Chunks<'a> {
    type Item = Wtf8Chunk<'a>;

    #[inline]
    fn next(&mut self) -> Option<Wtf8Chunk<'a>> {
        match self.next_surrogate() {
            Some(0) => {
                let s = decode_surrogate(self.0[1], self.0[2]);
                self.0 = &self.0[3..];
                Some(Wtf8Chunk::UnpairedSurrogate(s))
            }

            Some(x) => {
                let r = &self.0[..x];
                self.0 = &self.0[x..];
                // Safety: there are no surrogates, therefore the string is UTF-8.
                Some(Wtf8Chunk::Utf8(unsafe { str::from_utf8_unchecked(r) }))
            }

            None if self.0.is_empty() => None,

            None => {
                let r = self.0;
                self.0 = &[];
                // Safety: there are no surrogates, therefore the string is UTF-8.
                Some(Wtf8Chunk::Utf8(unsafe { str::from_utf8_unchecked(r) }))
            }
        }
    }
}

mod private {
    use core::ops::{Range, RangeFrom, RangeFull, RangeTo};

    pub trait Sealed {}
    impl Sealed for Range<usize> {}
    impl Sealed for RangeFrom<usize> {}
    impl Sealed for RangeTo<usize> {}
    impl Sealed for RangeFull {}
}
