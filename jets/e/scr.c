/* j/5/scr.c
**
*/
#include "all.h"

#include <stdint.h>
#include <errno.h>
#include <crypto_scrypt.h>

int _crypto_scrypt(const uint8_t *, size_t, const uint8_t *, size_t,
    uint64_t, uint32_t, uint32_t, uint8_t *, size_t);

/* functions
*/

  u3_noun
  u3qes_hsl(u3_atom p, u3_atom pl,
            u3_atom s, u3_atom sl,
            u3_atom n,
            u3_atom r, 
            u3_atom z, 
            u3_atom d)
  {
    // asserting that n is power of 2 in _crypto_scrypt
    if (!(_(u3a_is_atom(p)) && _(u3a_is_atom(s)) &&
          _(u3a_is_cat(pl)) && _(u3a_is_cat(sl)) &&
          _(u3a_is_cat(n))  && _(u3a_is_cat(r))  &&
          _(u3a_is_cat(z))  && _(u3a_is_cat(d))  &&
           (r != 0)         &&  (z != 0)         &&
           (((c3_d)r * 128 * ((c3_d)n + z - 1)) <= (1 << 30))))
        return u3m_bail(c3__exit);

    c3_y* b_p = u3a_malloc(pl + 1); c3_y* b_s= u3a_malloc(sl + 1);    
    u3r_bytes(0, pl, b_p, p);       u3r_bytes(0, sl, b_s, s);
    b_p[pl] = 0; b_s[sl]=0;
    c3_y* buf = u3a_malloc(d);

    if (_crypto_scrypt(b_p, pl, b_s, sl, n, r, z, buf, d) != 0)
        return u3m_bail(c3__exit);

    u3_noun res = u3i_bytes(d, buf);
    u3a_free(b_p); u3a_free(b_s); u3a_free(buf);

    return res;
  }

  u3_noun
  u3wes_hsl(u3_noun cor)
  {
    u3_noun p, pl, s, sl, n, r, z, d;
    u3_noun q;

    u3x_quil(u3r_at(u3x_sam, cor), &p, &pl, &s, &sl, &q);
    u3x_qual(q, &n, &r, &z, &d);

    return u3qes_hsl(p, pl, s, sl, n, r, z, d);
  }

  u3_noun
  u3qes_hsh(u3_atom p,
            u3_atom s,
            u3_atom n,
            u3_atom r,
            u3_atom z,
            u3_atom d)
  {
    // asserting that n is power of 2 in _crypto_scrypt
    if (!(_(u3a_is_atom(p)) && _(u3a_is_atom(s)) &&
          _(u3a_is_cat(n))  && _(u3a_is_cat(r))  &&
          _(u3a_is_cat(z))  && _(u3a_is_cat(d))  &&
           (r != 0)         &&  (z != 0)         &&
           (((c3_d)r * 128 * ((c3_d)n + z - 1)) <= (1 << 30))))
        return u3m_bail(c3__exit);

    c3_w   pl = u3r_met(3, p);      c3_w   sl = u3r_met(3, s);
    c3_y* b_p = u3a_malloc(pl + 1); c3_y* b_s= u3a_malloc(sl + 1);    
    u3r_bytes(0, pl, b_p, p);       u3r_bytes(0, sl, b_s, s);
    b_p[pl] = 0; b_s[sl]=0;
    c3_y* buf = u3a_malloc(d);

    if (_crypto_scrypt(b_p, pl, b_s, sl, n, r, z, buf, d) != 0)
        return u3m_bail(c3__exit);

    u3_noun res = u3i_bytes(d, buf);
    u3a_free(b_p); u3a_free(b_s); u3a_free(buf);

    return res;
  }

  u3_noun
  u3wes_hsh(u3_noun cor)
  {
    u3_noun p, s, n, r, z, d;
    u3_noun q;

    u3x_quil(u3r_at(u3x_sam, cor), &p, &s, &n, &r, &q);
    u3x_cell(q, &z, &d);

    return u3qes_hsh(p, s, n, r, z, d);
  }

  u3_noun
  u3qes_pbl(u3_atom p, u3_atom pl,
            u3_atom s, u3_atom sl,
            u3_atom c,
            u3_atom d)
  {
    if (!(_(u3a_is_atom(p)) && _(u3a_is_atom(s)) &&
          _(u3a_is_cat(pl)) && _(u3a_is_cat(sl)) &&
          _(u3a_is_cat(c))  && _(u3a_is_cat(d))  &&
           (d <= (1 << 30)) &&  (c <= (1 << 28)) &&
           (c != 0)))
        return u3m_bail(c3__exit);

    c3_y* b_p = u3a_malloc(pl + 1); c3_y* b_s= u3a_malloc(pl + 1);    
    u3r_bytes(0, pl, b_p, p);       u3r_bytes(0, sl, b_s, s);
    b_p[pl] = 0; b_s[sl]=0;
    c3_y* buf = u3a_malloc(d);

    PBKDF2_SHA256(b_p, pl, b_s, sl, c, buf, d);

    u3_noun res = u3i_bytes(d, buf);
    u3a_free(b_p); u3a_free(b_s); u3a_free(buf);

    return res;
  }

  u3_noun
  u3wes_pbl(u3_noun cor)
  {
    u3_noun p, pl, s, sl, c, d;
    u3_noun q;

    u3x_quil(u3r_at(u3x_sam, cor), &p, &pl, &s, &sl, &q);
    u3x_cell(q, &c, &d);

    return u3qes_pbl(p, pl, s, sl, c, d);
  }

  u3_noun
  u3qes_pbk(u3_atom p, u3_atom s, u3_atom c, u3_atom d)
  {
    if (!(_(u3a_is_atom(p)) && _(u3a_is_atom(s)) &&
          _(u3a_is_cat(c))  && _(u3a_is_cat(d))  &&
           (d <= (1 << 30)) &&  (c <= (1 << 28)) &&
           (c != 0)))
        return u3m_bail(c3__exit);

    c3_w   pl = u3r_met(3, p);      c3_w   sl = u3r_met(3, s);
    c3_y* b_p = u3a_malloc(pl + 1); c3_y* b_s= u3a_malloc(pl + 1);    
    u3r_bytes(0, pl, b_p, p);       u3r_bytes(0, sl, b_s, s);
    b_p[pl] = 0; b_s[sl]=0;
    c3_y* buf = u3a_malloc(d);

    PBKDF2_SHA256(b_p, pl, b_s, sl, c, buf, d);

    u3_noun res = u3i_bytes(d, buf);
    u3a_free(b_p); u3a_free(b_s); u3a_free(buf);

    return res;
  }

  u3_noun
  u3wes_pbk(u3_noun cor)
  {
    u3_noun p, s, c, d;

    u3x_qual(u3r_at(u3x_sam, cor), &p, &s, &c, &d);

    return u3qes_pbk(p, s, c, d);
  }

/*-
 * Copyright 2009 Colin Percival
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * This file was originally written by Colin Percival as part of the Tarsnap
 * online backup system.
 */

/**
 * crypto_scrypt(passwd, passwdlen, salt, saltlen, N, r, p, buf, buflen):
 * Compute scrypt(passwd[0 .. passwdlen - 1], salt[0 .. saltlen - 1], N, r,
 * p, buflen) and write the result into buf.  The parameters r, p, and buflen
 * must satisfy r * p < 2^30 and buflen <= (2^32 - 1) * 32.  The parameter N
 * must be a power of 2 greater than 1.
 *
 * Return 0 on success; or -1 on error.
 */
int
_crypto_scrypt(const uint8_t * passwd, size_t passwdlen,
    const uint8_t * salt, size_t saltlen, uint64_t N, uint32_t r, uint32_t p,
    uint8_t * buf, size_t buflen)
{
	void * B0, * V0, * XY0;
	uint8_t * B;
	uint32_t * V;
	uint32_t * XY;
	uint32_t i;

	if (((N & (N-1)) != 0) || N == 0)
		goto err0;

	/* Sanity-check parameters. */
#if SIZE_MAX > UINT32_MAX
	if (buflen > (((uint64_t)(1) << 32) - 1) * 32) {
		errno = EFBIG;
		goto err0;
	}
#endif
	if ((uint64_t)(r) * (uint64_t)(p) >= (1 << 30)) {
		errno = EFBIG;
		goto err0;
	}
	if (((N & (N - 1)) != 0) || (N == 0)) {
		errno = EINVAL;
		goto err0;
	}
	int test_size_max = (r > SIZE_MAX / 128 / p) || (N > SIZE_MAX / 128 / r);
	
#if SIZE_MAX / 256 <= UINT32_MAX
	test_size_max = (r > (SIZE_MAX - 64) / 256) || test_size_max;
#endif
	if(test_size_max) {
		errno = ENOMEM;
		goto err0;
	}

	/* Allocate memory. */
	if ((B0 = u3a_malloc(128 * r * p + 63)) == NULL)
		goto err0;
	B = (uint8_t *)(((uintptr_t)(B0) + 63) & ~ (uintptr_t)(63));
	if ((XY0 = u3a_malloc(256 * r + 64 + 63)) == NULL)
		goto err1;
	XY = (uint32_t *)(((uintptr_t)(XY0) + 63) & ~ (uintptr_t)(63));
	if ((V0 = u3a_malloc(128 * r * N + 63)) == NULL)
		goto err2;
	V = (uint32_t *)(((uintptr_t)(V0) + 63) & ~ (uintptr_t)(63));

	/* 1: (B_0 ... B_{p-1}) <-- PBKDF2(P, S, 1, p * MFLen) */
	PBKDF2_SHA256(passwd, passwdlen, salt, saltlen, 1, B, p * 128 * r);

	/* 2: for i = 0 to p - 1 do */
	for (i = 0; i < p; i++) {
		/* 3: B_i <-- MF(B_i, N) */
		smix(&B[i * 128 * r], r, N, V, XY);
	}

	/* 5: DK <-- PBKDF2(P, B, 1, dkLen) */
	PBKDF2_SHA256(passwd, passwdlen, B, p * 128 * r, 1, buf, buflen);

	/* Free memory. */

	u3a_free(V0);
	u3a_free(XY0);
	u3a_free(B0);

	/* Success! */
	return (0);

err2:
	u3a_free(XY0);
err1:
	u3a_free(B0);
err0:
	/* Failure! */
	return (-1);
}
