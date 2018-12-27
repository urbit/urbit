libent is a cross-platform wrapper around `getentropy(2)`. It exports
one symbol, `ent_getentropy`, which expands to `getentropy` if the
latter is available. Otherwise, it tries to use the next best source of
entropy -- /dev/urandom on *nix, BCryptGenRandom on Windows.

References:
* [OpenBSD getentropy](https://man.openbsd.org/cgi-bin/man.cgi/OpenBSD-current/man2/getentropy.2)
* [djb on entropy gathering](https://blog.cr.yp.to/20140205-entropy.html)
