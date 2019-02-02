## `libent`

`libent` is a cross-platform wrapper around `getentropy(2)`. It exports
one symbol, `ent_getentropy`. If getentropy is available, then it's just
a shim around that. Otherwise, it uses `getrandom(2)` (available since
kernel 3.17) on Linux, or `/dev/urandom` on other \*nix.


### Building and Testing

```bash
./configure
make
make test
PREFIX=/usr/local make install
```

### Why?

`getentropy` is the wave of the future. It's the correct API for
generating small amounts of entropy to create cryptographic keys or seed
PRNGs. It's good and reasonable and true, it's on Linux, \*BSD, and OS
X, and it only took us fifty years of UNIX to get here.

Sadly, it only just arrived, so nobody has it yet. It didn't land in
Linux until glibc 2.25, which seems to only have made it into Debian 10.

Once `getentropy` is everywhere you care about, you can just do a
s/ent\_//g on all the call sites and discard this shim.

This project began because [Urbit](https://github.com/urbit/urbit)'s
entropy-generation function was bothering me. Then it got out of hand.


### References

* [OpenBSD getentropy](https://man.openbsd.org/cgi-bin/man.cgi/OpenBSD-current/man2/getentropy.2)
* [djb on entropy gathering](https://blog.cr.yp.to/20140205-entropy.html)
