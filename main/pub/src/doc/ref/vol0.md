volume 0, Kelvin Versioning
=====================

```
++  hoon  %164                                          ::  version stub
```

Declares the current Hoon version number in degrees Kelvin.


When normal people release normal software, they count by fractions, and they
count up. Thus, they can keep extending and revising their systems
incrementally. This is generally considered a good thing. It generally is.

In some cases, however, specifications needs to be permanently frozen. This
requirement is generally found in the context of standards. Some standards are
extensible or versionable, but some are not. ASCII, for instance, is
perma-frozen. So is IPv4 (its relationship to IPv6 is little more than nominal
- if they were really the same protocol, they'd have the same ethertype).
Moreover, many standards render themselves incompatible in practice through
excessive enthusiasm for extensibility. They may not be perma-frozen, but they
probably should be.

The true, Martian way to perma-freeze a system is what I call Kelvin
versioning. In Kelvin versioning, releases count down by integer degrees
Kelvin. At absolute zero, the system can no longer be changed. At 1K, one more
modification is possible. And so on. For instance, Nock is at 5K. It might
change, though it probably won't. Nouns themselves are at 0K - it is impossible
to imagine changing anything about their three sentence definition.

---

```
~zod/try=> stub
164
```

---


