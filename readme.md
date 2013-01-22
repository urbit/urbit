Ed25519
-------

This is a portable implementation of [Ed25519](http://ed25519.cr.yp.to/). All
code is in the public domain.

Usage
-----

Simply add all .c and .h files in the src/ folder to your project and include
ed25519.h. If you prefer to use a dll, only copy ed25519.h and define
ED25519_DLL before importing.

The API is simple:

    int ED25519_DECLSPEC ed25519_create_seed(unsigned char *seed);