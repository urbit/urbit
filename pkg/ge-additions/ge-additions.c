// Group Element Additions
//
// Urbit uses the ge.h code from the ed25519 library, which was ported from the
// ref10 SUPERCOP public domain implementation. That implementation doesn't
// contain several functions needed for ring signatures.
//
// This file is does. The providence of this code starts with Adam Langley
// taking the SUPERCOP C implementation and producing an ed25519 implementation
// for in golang (https://godoc.org/golang.org/x/crypto/ed25519). (If you look
// at the go code, you'll see the comments are the same as the comments in the
// C implementation.)
//
// From there, the DEDIS group from ETH Zurich took that implementation and
// added the additional methods to make a generalized ECC point library. While
// their project as a whole is MPL, they deliberately left their ed25519
// implementation under the Go BSD-3 license:
// (https://github.com/dedis/kyber/blob/master/group/edwards25519/LICENSE)
//
// This file is a fairly straight translation from Go to C of DEDIS' additions,
// so this falls under the same license.
//
// ------
//
// Copyright (c) 2009 The Go Authors. All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//    * Redistributions of source code must retain the above copyright
// notice, this list of conditions and the following disclaimer.
//    * Redistributions in binary form must reproduce the above
// copyright notice, this list of conditions and the following disclaimer
// in the documentation and/or other materials provided with the
// distribution.
//    * Neither the name of Google Inc. nor the names of its
// contributors may be used to endorse or promote products derived from
// this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#include "ge-additions.h"

#include <fe.h>

static unsigned char equal(signed char b, signed char c) {
    unsigned char ub = b;
    unsigned char uc = c;
    unsigned char x = ub ^ uc; /* 0: yes; 1..255: no */
    uint64_t y = x; /* 0: yes; 1..255: no */
    y -= 1; /* large: yes; 0..254: no */
    y >>= 63; /* 1: yes; 0: no */
    return (unsigned char) y;
}

static unsigned char negative(signed char b) {
    uint64_t x = b; /* 18446744073709551361..18446744073709551615: yes; 0..255: no */
    x >>= 63; /* 1: yes; 0: no */
    return (unsigned char) x;
}

void ge_cached_0(ge_cached* c) {
  fe_1(c->YplusX);
  fe_1(c->YminusX);
  fe_1(c->Z);
  fe_0(c->T2d);
}

void ge_cached_cmov(ge_cached* r, const ge_cached* u, int32_t b)
{
  fe_cmov(r->YplusX, u->YplusX, b);
  fe_cmov(r->YminusX, u->YminusX, b);
  fe_cmov(r->Z, u->Z, b);
  fe_cmov(r->T2d, u->T2d, b);
}

void ge_cached_neg(ge_cached* r, const ge_cached* t)
{
  fe_copy(r->YplusX, t->YminusX);
  fe_copy(r->YminusX, t->YplusX);
  fe_copy(r->Z, t->Z);
  fe_neg(r->T2d, t->T2d);
}

void select_cached(ge_cached* c, const ge_cached Ai[8], int32_t b)
{
  int32_t is_negative = negative(b);
  int32_t b_abs = b - (((-is_negative) & b) << 1);

  ge_cached_0(c);
  for (int32_t i = 0; i < 8; ++i) {
    ge_cached_cmov(c, &Ai[i], equal(b_abs, i+1));
  }

  ge_cached minusC;
  ge_cached_neg(&minusC, c);
  ge_cached_cmov(c, &minusC, is_negative);
}

//
void ge_scalarmult(ge_p3* h, const unsigned char* a, const ge_p3* A)
{
  signed char e[64];
  int i;
  ge_p1p1 t;
  ge_p3 u;

  for (i = 0; i < 32; ++i) {
    e[2 * i + 0] = (a[i] >> 0) & 15;
    e[2 * i + 1] = (a[i] >> 4) & 15;
  }

  /* each e[i] is between 0 and 15 */
  /* e[63] is between 0 and 7 */
  signed char carry = 0;
  for (i = 0; i < 63; ++i) {
      e[i] += carry;
      carry = e[i] + 8;
      carry >>= 4;
      e[i] -= carry << 4;
  }
  e[63] += carry;
  /* each e[i] is between -8 and 8 */

  // compute cached array of multiples of A from 1A through 8A
  ge_cached Ai[8];
  ge_p3_to_cached(&Ai[0], A);
  for (i = 0; i < 7; ++i) {
    ge_add(&t, A, &Ai[i]);
    ge_p1p1_to_p3(&u, &t);
    ge_p3_to_cached(&Ai[i+1], &u);
  }

  // special case for exponent nybble i == 63
  ge_p3_0(&u);
  ge_cached c;
  select_cached(&c, Ai, e[63]);
  ge_add(&t, &u, &c);

  ge_p2 r;
  for (i = 62; i >= 0; i--) {
    // t <<= 4
    ge_p1p1_to_p2(&r, &t);
    ge_p2_dbl(&t, &r);
    ge_p1p1_to_p2(&r, &t);
    ge_p2_dbl(&t, &r);
    ge_p1p1_to_p2(&r, &t);
    ge_p2_dbl(&t, &r);
    ge_p1p1_to_p2(&r, &t);
    ge_p2_dbl(&t, &r);

    // Add next nyble
    ge_p1p1_to_p3(&u, &t);
    select_cached(&c, Ai, e[i]);
    ge_add(&t, &u, &c);
  }

  ge_p1p1_to_p3(h, &t);
}
