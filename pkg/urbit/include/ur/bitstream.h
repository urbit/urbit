#ifndef UR_BITSTREAM_H
#define UR_BITSTREAM_H

#include <inttypes.h>

typedef enum {
  ur_cue_good = 0,
  ur_cue_gone = 1,
  ur_cue_meme = 2
} ur_cue_res_e;

typedef enum {
  ur_jam_atom = 0,
  ur_jam_cell = 1,
  ur_jam_back = 2
} ur_cue_tag_e;

/*
**  stateful bitstream reader, backed by a byte-buffer,
**  supporting a variety of read sizes/patterns.
**
**    NB: ur_bsr*_any() functions behave as if the stream were infinite,
**    subject to overall limit of a 64-bit bit-cursor.
**
*/
typedef struct ur_bsr_s {
  uint64_t        left;
  uint64_t        bits;
  uint8_t          off;
  const uint8_t *bytes;
} ur_bsr_t;

typedef struct ur_bsw_s {
  uint64_t    prev;
  uint64_t    size;
  uint64_t    fill;
  uint64_t    bits;
  uint8_t      off;
  uint8_t   *bytes;
} ur_bsw_t;

/*
**  initialize bitstream and check for 64-bit bit-cursor overflow.
*/
ur_cue_res_e
ur_bsr_init(ur_bsr_t *bsr, uint64_t len, const uint8_t *bytes);

ur_bool_t
ur_bsr_sane(ur_bsr_t *bsr);

ur_cue_res_e
ur_bsr_bit(ur_bsr_t *bsr, uint8_t *out);

uint8_t
ur_bsr_bit_any(ur_bsr_t *bsr);

uint8_t
ur_bsr8_any(ur_bsr_t *bsr, uint8_t len);

uint32_t
ur_bsr32_any(ur_bsr_t *bsr, uint8_t len);

uint64_t
ur_bsr64_any(ur_bsr_t *bsr, uint8_t len);

void
ur_bsr_bytes_any(ur_bsr_t *bsr, uint64_t len, uint8_t *out);

void
ur_bsr_skip_any(ur_bsr_t *bsr, uint64_t len);

ur_cue_res_e
ur_bsr_tag(ur_bsr_t *bsr, ur_cue_tag_e *out);

ur_cue_res_e
ur_bsr_rub_log(ur_bsr_t *bsr, uint8_t *out);

ur_cue_res_e
ur_bsr_rub_len(ur_bsr_t *bsr, uint64_t *out);

void
ur_bsw_grow(ur_bsw_t *bsw, uint64_t step);

ur_bool_t
ur_bsw_sane(ur_bsw_t *bsw);

void
ur_bsw_bit(ur_bsw_t *bsw, uint8_t bit);

void
ur_bsw8(ur_bsw_t *bsw, uint8_t len, uint8_t byt);

void
ur_bsw32(ur_bsw_t *bsw, uint8_t len, uint32_t val);

void
ur_bsw64(ur_bsw_t *bsw, uint8_t len, uint64_t val);

void
ur_bsw_bytes(ur_bsw_t *bsw, uint64_t len, uint8_t *byt);

void
ur_bsw_bex(ur_bsw_t *bsw, uint8_t n);

void
ur_bsw_mat64(ur_bsw_t *bsw, uint8_t len, uint64_t val);

void
ur_bsw_mat_bytes(ur_bsw_t *bsw, uint64_t len, uint8_t *byt);

void
ur_bsw_back64(ur_bsw_t *bsw, uint8_t len, uint64_t val);

void
ur_bsw_atom64(ur_bsw_t *bsw, uint8_t len, uint64_t val);

void
ur_bsw_atom_bytes(ur_bsw_t *bsw, uint64_t len, uint8_t *byt);

void
ur_bsw_cell(ur_bsw_t *bsw);

#endif
