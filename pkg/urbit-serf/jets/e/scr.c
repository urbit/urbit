/* j/5/scr.c
**
*/
#include "all.h"

#include <stdint.h>
#include <errno.h>

#include <libscrypt.h>
#include <sha256.h>

static int _crypto_scrypt(const uint8_t *, size_t, const uint8_t *, size_t,
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

    libscrypt_PBKDF2_SHA256(b_p, pl, b_s, sl, c, buf, d);

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

    libscrypt_PBKDF2_SHA256(b_p, pl, b_s, sl, c, buf, d);

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

/**
 * crypto_scrypt(passwd, passwdlen, salt, saltlen, N, r, p, buf, buflen):
 * Compute scrypt(passwd[0 .. passwdlen - 1], salt[0 .. saltlen - 1], N, r,
 * p, buflen) and write the result into buf.  The parameters r, p, and buflen
 * must satisfy r * p < 2^30 and buflen <= (2^32 - 1) * 32.  The parameter N
 * must be a power of 2 greater than 1.
 *
 * Return 0 on success; or -1 on error.
 */
static int
_crypto_scrypt(const uint8_t * passwd, size_t passwdlen,
    const uint8_t * salt, size_t saltlen, uint64_t N, uint32_t r, uint32_t p,
    uint8_t * buf, size_t buflen)
{
	return libscrypt_scrypt(passwd, passwdlen, salt, saltlen, N, r, p, buf, buflen);
}
