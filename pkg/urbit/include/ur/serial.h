#ifndef UR_SERIAL_H
#define UR_SERIAL_H

#include <inttypes.h>
#include <ur/defs.h>
#include <ur/bitstream.h>

/*
**  bit-wise serialization of a noun into a byte-buffer.
**  supports up to 64-bits of bit-addressed output (nearly 2 EiB).
**  (as this is an impractical volume data, cursor overflow is not checked.)
**
**  unsafe variant is unsafe wrt its [dict] parameter, which must be empty,
**  but can be passed in order to skip reallocation inside hot loops.
**
*/
uint64_t
ur_jam_unsafe(ur_root_t      *r,
              ur_nref       ref,
              ur_dict64_t *dict,
              uint64_t     *len,
              uint8_t     **byt);

uint64_t
ur_jam(ur_root_t *r, ur_nref ref, uint64_t *len, uint8_t **byt);

/*
**  bitwise deserialization of a byte-buffer into a noun.
**  supports up to 62-bits of bit-addressed input (511 PiB).
**  returns [ur_cue_good] on success.
**
**  unsafe variant is unsafe wrt its [dict] parameter, which must be empty,
**  (present in order to skip reallocation inside hot loops).
**
**  test variant does not allocate nouns, but merely parses the input.
**
*/
ur_cue_res_e
ur_cue_unsafe(ur_root_t       *r,
              ur_dict64_t  *dict,
              uint64_t       len,
              const uint8_t *byt,
              ur_nref       *out);

ur_cue_res_e
ur_cue(ur_root_t *r, uint64_t len, const uint8_t *byt, ur_nref *out);

ur_cue_res_e
ur_cue_test_unsafe(ur_dict_t    *dict,
                   uint64_t       len,
                   const uint8_t *byt);

ur_bool_t
ur_cue_test(uint64_t len, const uint8_t *byt);

#endif
