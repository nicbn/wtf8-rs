# wtf8-rs

Implementation of [the WTF-8 encoding](https://simonsapin.github.io/wtf-8/).

[Documentation](https://nicbn.github.io/wtf8-rs/wtf8_rs)

> WTF-8 is a hack intended to be used internally in self-contained systems with components that need to support potentially ill-formed UTF-16 for legacy reasons.
>
>Any WTF-8 data must be converted to a Unicode encoding at the system’s boundary before being emitted. UTF-8 is recommended. WTF-8 must not be used to represent text in a file format or for transmission over the Internet.
>
>In particular, the Encoding Standard [ENCODING] defines UTF-8 and other encodings for the Web. There is no and will not be any encoding label [ENCODING] or IANA charset alias [CHARSETS] for WTF-8.
>
> <cite>https://simonsapin.github.io/wtf-8/#intended-audience</cite>

## Library

Depends on the standard library’s `alloc` crate but not `std`.

* `Wtf8` and `Wtf8Buf` - Similar to `str` and `String`, provides type-safe WTF-8 strings.
* `CodePoint` - Similar to `char`, provides type-safe Unicode code points.
* Lossless conversion from potentially ill-formed UTF-16 to `CodePoint` iterator and from `CodePoint` iterators to `Wtf8Buf`, and from `str` to `Wtf8`.
* Conversion from `Wtf8` to `String`, potentially lossy.

## License

Licensed under either of

 * Apache License, Version 2.0
   ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license
   ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
