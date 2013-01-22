Ed25519
-------

This is a portable implementation of [Ed25519](http://ed25519.cr.yp.to/). All
code is in the public domain.

No code uses libc, except for the random seed generation which uses standard
OS cryptography APIs. If you wish to be entirely portable define
`ED25519_NO_SEED`. This does disable the `ed25519_create_seed` function
however.

Usage
-----

Simply add all .c and .h files in the src/ folder to your project and include
ed25519.h. If you prefer to use a shared library, only copy `ed25519.h` and define
`ED25519_DLL` before importing. A windows DLL is pre-built.

The API is simple:

    int ED25519_DECLSPEC ed25519_create_seed(unsigned char *seed);