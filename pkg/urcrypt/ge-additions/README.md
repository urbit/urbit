# Group Element Additions

Urbit uses the ge.h code from the ed25519 library, which was ported from the
ref10 SUPERCOP public domain implementation. That implementation doesn't
contain several functions needed for ring signatures.

This file does. The providence of this code starts with Adam Langley taking
the SUPERCOP C implementation and producing an ed25519 implementation for it
in golang (https://godoc.org/golang.org/x/crypto/ed25519). (If you look at
the go code, you'll see the comments are the same as the comments in the C
implementation.)

From there, the DEDIS group from ETH Zurich took that implementation and
added the additional methods to make a generalized ECC point library. While
their project as a whole is MPL, they deliberately left their ed25519
implementation under the Go BSD-3 license:
(https://github.com/dedis/kyber/blob/master/group/edwards25519/LICENSE)

This file is a fairly straight translation from Go to C of DEDIS' additions,
so this falls under the same license.
