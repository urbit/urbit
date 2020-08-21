#ifndef UR_SERIAL_H
#define UR_SERIAL_H

#include <inttypes.h>
#include <ur/bitstream.h>

uint64_t
ur_jam(ur_root_t *r, ur_nref ref, uint64_t *len, uint8_t **byt);

ur_cue_res_e
ur_cue(ur_root_t *r, uint64_t len, const uint8_t *byt, ur_nref *out);

#endif
