#![no_std]
#![warn(clippy::all)]

//! # wtf8-rs
//!
//! Implementation of [the WTF-8 encoding](https://simonsapin.github.io/wtf-8/).

// Much of the code from this library has been copied from std sys_commmon,
// which itself copied from @SimonSapin's repo.
extern crate alloc;

pub mod codepoint;
pub mod wtf8;
pub mod wtf8buf;

#[doc(inline)]
pub use codepoint::CodePoint;

#[doc(inline)]
pub use wtf8::{Wtf8, Wtf8Chunk};

#[doc(inline)]
pub use wtf8buf::Wtf8Buf;

#[inline]
fn decode_surrogate(second_byte: u8, third_byte: u8) -> u16 {
    // The first byte is assumed to be 0xED
    0xD800 | (second_byte as u16 & 0x3F) << 6 | third_byte as u16 & 0x3F
}

#[inline]
fn decode_surrogate_pair(lead: u16, trail: u16) -> char {
    let code_point = 0x1_0000 + ((((lead - 0xD800) as u32) << 10) | (trail - 0xDC00) as u32);
    // Safety: this can not be greater than 10FFFF, by construction.
    unsafe { char::from_u32_unchecked(code_point) }
}
