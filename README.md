libent is a cross-platform wrapper around `getentropy(2)`. It exports
one symbol, `ent_getentropy`, which expands to `getentropy` if the
latter is available. Otherwise, it tries to use the next best source of
entropy — `/dev/urandom` on *nix.

### Why?

`getentropy` is the wave of the future. It's the correct API for
generating small amounts of entropy to create cryptographic keys or seed
PRNGs. It's good and reasonable and true, it's on Linux, *BSD, and OS X,
and it only took us fifty years of UNIX to get here.

Sadly, it only just arrived, so nobody has it yet. It didn't land in
Linux until glibc 2.25, which seems to only have made it into Debian 10.

Once `getentropy` is everywhere you care about, you can just do a
s/ent_//g on all the call sites and discard this shim.

Admittedly, 90% of the motivation behind this was to fix the randomness
shim in [Urbit](https://github.com/urbit/urbit).

References:
* [OpenBSD getentropy](https://man.openbsd.org/cgi-bin/man.cgi/OpenBSD-current/man2/getentropy.2)
* [djb on entropy gathering](https://blog.cr.yp.to/20140205-entropy.html)
