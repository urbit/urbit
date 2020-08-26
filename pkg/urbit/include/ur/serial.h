#ifndef UR_SERIAL_H
#define UR_SERIAL_H

#include <inttypes.h>
#include <ur/defs.h>
#include <ur/bitstream.h>

uint64_t
ur_jam(ur_root_t *r, ur_nref ref, uint64_t *len, uint8_t **byt);

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
