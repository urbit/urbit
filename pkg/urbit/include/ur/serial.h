#ifndef UR_SERIAL_H
#define UR_SERIAL_H

#include <inttypes.h>
#include <ur/defs.h>
#include <ur/bitstream.h>
#include <ur/hashcons.h>

/*
**  bit-wise serialization of a noun into a byte-buffer.
**  supports up to 64-bits of bit-addressed output (nearly 2 EiB).
**  (as this is an impractical volume data, cursor overflow is not checked.)
**
**  jam_with* api factors out stack/dict (re)allocation,
**  for better performance inside hot loops.
**
*/

typedef struct ur_jam_s ur_jam_t;

uint64_t
ur_jam_unsafe(ur_root_t      *r,
              ur_nref       ref,
              ur_dict64_t *dict,
              uint64_t     *len,
              uint8_t     **byt);

uint64_t
ur_jam(ur_root_t  *r,
       ur_nref   ref,
       uint64_t *len,
       uint8_t **byt);

ur_jam_t*
ur_jam_init_with(ur_root_t    *r,
                 uint64_t d_prev,
                 uint64_t d_size,
                 uint32_t s_prev,
                 uint32_t s_size);

ur_jam_t*
ur_jam_init(ur_root_t *r);

uint64_t
ur_jam_with(ur_jam_t   *j,
            ur_nref   ref,
            uint64_t *len,
            uint8_t **byt);
void
ur_jam_done(ur_jam_t *j);

/*
**  bitwise deserialization of a byte-buffer into a noun.
**  supports up to 62-bits of bit-addressed input (511 PiB).
**  returns [ur_cue_good] on success.
**
**  cue_with factors out stack/dict (re)allocation,
**  for better performance of hot loops.
**
**  cue_test does not allocate nouns, but merely parses the input;
**  cue_test_with* api factors out stack/dict (re)allocation,
**  for better performance of repeated tests.
**
*/

typedef struct ur_cue_test_s ur_cue_test_t;
typedef struct ur_cue_s      ur_cue_t;

ur_cue_res_e
ur_cue(ur_root_t *r, uint64_t len, const uint8_t *byt, ur_nref *out);

ur_cue_t*
ur_cue_init_with(ur_root_t    *r,
                 uint64_t d_prev,
                 uint64_t d_size,
                 uint32_t s_prev,
                 uint32_t s_size);

ur_cue_t*
ur_cue_init(ur_root_t *r);

ur_cue_res_e
ur_cue_with(ur_cue_t        *c,
            uint64_t       len,
            const uint8_t *byt,
            ur_nref       *out);

void
ur_cue_done(ur_cue_t *c);

ur_bool_t
ur_cue_test(uint64_t len, const uint8_t *byt);

ur_cue_test_t*
ur_cue_test_init_with(uint64_t d_prev,
                      uint64_t d_size,
                      uint32_t s_prev,
                      uint32_t s_size);

ur_cue_test_t*
ur_cue_test_init(void);

ur_bool_t
ur_cue_test_with(ur_cue_test_t   *t,
                 uint64_t       len,
                 const uint8_t *byt);

void
ur_cue_test_done(ur_cue_test_t *t);

#endif
