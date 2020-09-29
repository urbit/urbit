What is urcrypt?
----------------
urcrypt is a library of cryptography routines used by urbit jets.

Why is urcrypt?
---------------
Urbit's C runtime (long the only urbit runtime) has accumulated a collection of
cryptography dependencies, some with custom additions or patches. These
libraries have different conventions and have been managed by u3 in an ad-hoc
manner. Reproducing that arrangement in other runtimes is tricky and
error-prone. The (sometimes inconsistent) logic must be reproduced and suitable
cryptography primitives must be found (or worse, written) for the new
environment.

To ease these burdens, urcrypt isolates the quirks behind a consistent calling
convention. Everything is a little-endian byte array, and each jetted operation
has a corresponding function in the library. Jets simply unpack their nouns,
call urcrypt, and pack the results.

What is a cryptography routine?
-------------------------------
This is more of a subjective question than it might appear. Any of the following
conditions are sufficient, but not necessary, for a function to be included in
urcrypt:

  * The routine is sensitive to side-channel attacks (encryption, etc)
  * Some property of the routine is cryptographically useful (SHA, RIPE, etc)
  * The routine typically lives in a crypto library, for whatever reason.

Shared or static?
-----------------
Urcrypt has a number of dependencies, and there is something of a matrix of
combinations of static and shared versions of those dependencies. To keep things
simple, two modes are currently supported:

  * A static library archive (urcrypt.a)
  * A shared object with internal, statically linked dependencies
    (urcrypt.{so, dll, dylib, etc})

pkg-config can be used in the normal way to obtain the link parameters.

A word on OpenSSL
-----------------
Urcrypt depends on OpenSSL's libcrypto, which has custom malloc functions.
Urcrypt tries to avoid dealing with global/static state, and mostly succeeds,
but users who need to control memory management will want to use
`CRYPTO_set_mem_functions` from OpenSSL. This is no problem for a statically
linked urcrypt, but the shared object doesn't expose that symbol to users.
`urcrypt_set_openssl_mem_functions` is a trivial wrapper that is exposed,
and can be used to configure shared urcrypt's internal libcrypto malloc.
The function always fails in statically linked urcrypt.
