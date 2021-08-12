# Argon2

This is a fork of [the reference C implementation of Argon2](https://github.com/P-H-C/phc-winner-argon2), the password-hashing function that won the [Password Hashing Competition (PHC)](https://password-hashing.net).

## About Argon2u

In addition to the official three variants (Argon2i, Argon2d, and Argon2id), this fork also implements a fourth variant, Argon2u. It operates similarly to Argon2id, in that it is a hybrid of Argon2i and Argon2d. Where Argon2id uses Argon2i's algorithm for the first two processed segments, Argon2u does this for the first three.

## More about Argon2

Please see the [original repository](https://github.com/P-H-C/phc-winner-argon2) for information about Argon2.

## Intellectual property

Except for the components listed below, the Argon2 code in this
repository is copyright (c) 2015 Daniel Dinu, Dmitry Khovratovich (main
authors), Jean-Philippe Aumasson and Samuel Neves, and dual licensed under the
[CC0 License](https://creativecommons.org/about/cc0) and the
[Apache 2.0 License](http://www.apache.org/licenses/LICENSE-2.0). For more info
see the LICENSE file.

The string encoding routines in [`src/encoding.c`](src/encoding.c) are
copyright (c) 2015 Thomas Pornin, and under
[CC0 License](https://creativecommons.org/about/cc0).

The BLAKE2 code in [`src/blake2/`](src/blake2) is copyright (c) Samuel
Neves, 2013-2015, and under
[CC0 License](https://creativecommons.org/about/cc0).

All licenses are therefore GPL-compatible.
