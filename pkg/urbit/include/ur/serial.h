#ifndef UR_SERIAL_H
#define UR_SERIAL_H

#include <inttypes.h>
#include <ur/defs.h>
#include <ur/bitstream.h>

uint64_t
ur_jam(ur_root_t *r, ur_nref ref, uint64_t *len, uint8_t **byt);

typedef uint32_t (*ur_coin32_f)(ur_root_t*, ur_bsr_t*, uint64_t);
typedef uint32_t (*ur_cons32_f)(ur_root_t*, uint32_t, uint32_t);

typedef uint64_t (*ur_coin64_f)(ur_root_t*, ur_bsr_t*, uint64_t);
typedef uint64_t (*ur_cons64_f)(ur_root_t*, uint64_t, uint64_t);

ur_cue_res_e
ur_cue_walk32_unsafe(ur_root_t       *r,
                     ur_dict32_t  *dict,
                     uint64_t       len,
                     const uint8_t *byt,
                     uint32_t      *out,
                     ur_coin32_f   coin,
                     ur_cons32_f   cons);

ur_cue_res_e
ur_cue_walk32(ur_root_t       *r,
              uint64_t       len,
              const uint8_t *byt,
              uint32_t      *out,
              ur_coin32_f   coin,
              ur_cons32_f   cons);

ur_cue_res_e
ur_cue_walk64_unsafe(ur_root_t       *r,
                     ur_dict64_t  *dict,
                     uint64_t       len,
                     const uint8_t *byt,
                     uint64_t      *out,
                     ur_coin64_f   coin,
                     ur_cons64_f   cons);

ur_cue_res_e
ur_cue_walk64(ur_root_t       *r,
              uint64_t       len,
              const uint8_t *byt,
              uint64_t      *out,
              ur_coin64_f   coin,
              ur_cons64_f   cons);

ur_cue_res_e
ur_cue_unsafe(ur_root_t       *r,
              ur_dict64_t  *dict,
              uint64_t       len,
              const uint8_t *byt,
              ur_nref       *out);

ur_cue_res_e
ur_cue(ur_root_t *r, uint64_t len, const uint8_t *byt, ur_nref *out);

ur_bool_t
ur_cue_test_unsafe(ur_dict32_t  *dict,
                   uint64_t       len,
                   const uint8_t *byt);

ur_bool_t
ur_cue_test(uint64_t len, const uint8_t *byt);

#endif
