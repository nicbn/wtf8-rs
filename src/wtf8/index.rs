use crate::Wtf8;
use core::ops::{Bound, Range, RangeBounds, RangeFrom, RangeFull, RangeTo};
use core::{fmt, ptr};

mod private {
    pub trait Sealed {}
}

use private::Sealed;

impl Sealed for Range<usize> {}
impl Sealed for RangeFrom<usize> {}
impl Sealed for RangeTo<usize> {}
impl Sealed for RangeFull {}

/// A helper trait to do `get` operation on `Wtf8`.
///
/// This trait is sealed and not meant to be implemented by an user of this
/// library.
#[allow(clippy::missing_safety_doc)]
pub unsafe trait Wtf8Index: Clone + fmt::Debug + Sealed {
    unsafe fn get_unchecked_mut(self, slice: *mut Wtf8) -> *mut Wtf8;

    #[inline]
    unsafe fn get_unchecked(self, slice: *const Wtf8) -> *const Wtf8 {
        self.get_unchecked_mut(slice as *mut Wtf8)
    }

    fn get(self, slice: &Wtf8) -> Option<&Wtf8>;

    fn get_mut(self, slice: &mut Wtf8) -> Option<&mut Wtf8>;
}

unsafe impl Wtf8Index for Range<usize> {
    #[inline]
    unsafe fn get_unchecked_mut(self, slice: *mut Wtf8) -> *mut Wtf8 {
        ptr::slice_from_raw_parts(
            (*slice).bytes.as_ptr().add(self.start),
            self.end - self.start,
        ) as _
    }

    #[inline]
    fn get(self, slice: &Wtf8) -> Option<&Wtf8> {
        if check_bounds(&self, slice) {
            unsafe { Some(&*self.get_unchecked(slice)) }
        } else {
            None
        }
    }

    #[inline]
    fn get_mut(self, slice: &mut Wtf8) -> Option<&mut Wtf8> {
        if check_bounds(&self, slice) {
            unsafe { Some(&mut *self.get_unchecked_mut(slice)) }
        } else {
            None
        }
    }
}

unsafe impl Wtf8Index for RangeFrom<usize> {
    #[inline]
    unsafe fn get_unchecked_mut(self, slice: *mut Wtf8) -> *mut Wtf8 {
        ptr::slice_from_raw_parts(
            (*slice).bytes.as_ptr().add(self.start),
            (*slice).len() - self.start,
        ) as _
    }

    #[inline]
    fn get(self, slice: &Wtf8) -> Option<&Wtf8> {
        to_range(&self, slice).get(slice)
    }

    #[inline]
    fn get_mut(self, slice: &mut Wtf8) -> Option<&mut Wtf8> {
        to_range(&self, slice).get_mut(slice)
    }
}

unsafe impl Wtf8Index for RangeTo<usize> {
    #[inline]
    unsafe fn get_unchecked_mut(self, slice: *mut Wtf8) -> *mut Wtf8 {
        ptr::slice_from_raw_parts((*slice).bytes.as_ptr(), self.end) as _
    }

    #[inline]
    fn get(self, slice: &Wtf8) -> Option<&Wtf8> {
        to_range(&self, slice).get(slice)
    }

    #[inline]
    fn get_mut(self, slice: &mut Wtf8) -> Option<&mut Wtf8> {
        to_range(&self, slice).get_mut(slice)
    }
}

unsafe impl Wtf8Index for RangeFull {
    #[inline]
    unsafe fn get_unchecked_mut(self, slice: *mut Wtf8) -> *mut Wtf8 {
        slice
    }

    #[inline]
    fn get(self, slice: &Wtf8) -> Option<&Wtf8> {
        Some(slice)
    }

    #[inline]
    fn get_mut(self, slice: &mut Wtf8) -> Option<&mut Wtf8> {
        Some(slice)
    }
}

#[inline]
fn to_range<T: RangeBounds<usize>>(bounds: &T, slice: &Wtf8) -> Range<usize> {
    let start = match bounds.start_bound() {
        Bound::Included(x) => *x,
        Bound::Excluded(x) => *x - 1,
        Bound::Unbounded => 0,
    };
    let end = match bounds.end_bound() {
        Bound::Included(x) => *x + 1,
        Bound::Excluded(x) => *x,
        Bound::Unbounded => slice.len(),
    };

    Range { start, end }
}

#[inline]
fn check_bounds(range: &Range<usize>, slice: &Wtf8) -> bool {
    range.start <= range.end
        && range.end <= slice.len()
        && slice.is_code_point_boundary(range.start)
        && slice.is_code_point_boundary(range.end)
}
