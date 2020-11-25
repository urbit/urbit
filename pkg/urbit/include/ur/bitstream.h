#ifndef UR_BITSTREAM_H
#define UR_BITSTREAM_H

#include <inttypes.h>

/*
**  stateful bitstream reader, backed by a byte-buffer,
**  maintaing a 64-bit bit-cursor, and supporting a variety
**  of read sizes and patterns.
**
**    NB: ur_bsr*_any() functions behave as if the stream were infinite,
**    subject to the overall limit of the bit-cursor.
**
*/
typedef struct ur_bsr_s {
  uint64_t        left;
  uint64_t        bits;
  uint8_t          off;
  const uint8_t *bytes;
} ur_bsr_t;

/*
**  generalized bitstream-reader/cue response enum
*/
typedef enum {
  ur_cue_good = 0,    //  successful read
  ur_cue_back = 1,    //  missing backreference
  ur_cue_gone = 2,    //  read off the end of the stream
  ur_cue_meme = 3     //  exceeded memory representation
} ur_cue_res_e;

/*
**  jam/cue type tag enumeration
*/
typedef enum {
  ur_jam_atom = 0,
  ur_jam_cell = 1,
  ur_jam_back = 2
} ur_cue_tag_e;

/*
**  stateful bitstream writer, backed by a byte-buffer automatically
**  reallocated with fibonacc growth, maintaing a 64-bit bit-cursor,
**  and supporting a variety of write sizes and patterns.
**
*/
typedef struct ur_bsw_s {
  uint64_t    prev;
  uint64_t    size;
  uint64_t    fill;
  uint64_t    bits;
  uint8_t      off;
  uint8_t   *bytes;
} ur_bsw_t;

/*
**  initialize bitstream-reader and check for 64-bit bit-cursor overflow.
*/
ur_cue_res_e
ur_bsr_init(ur_bsr_t *bsr, uint64_t len, const uint8_t *bytes);

/*
**  validate bitstream-reader invariants.
*/
ur_bool_t
ur_bsr_sane(ur_bsr_t *bsr);

/*
**  read a bit, failing at EOS
*/
ur_cue_res_e
ur_bsr_bit(ur_bsr_t *bsr, uint8_t *out);

/*
**  read a bit
*/
uint8_t
ur_bsr_bit_any(ur_bsr_t *bsr);

/*
**  read N (up to 8) bits into a uint8.
*/
uint8_t
ur_bsr8_any(ur_bsr_t *bsr, uint8_t len);

/*
**  read N (up to 32) bits into a uint32.
*/
uint32_t
ur_bsr32_any(ur_bsr_t *bsr, uint8_t len);

/*
**  read N (up to 64) bits into a uint64.
*/
uint64_t
ur_bsr64_any(ur_bsr_t *bsr, uint8_t len);

/*
**  read N bits into a zero-initialized byte array.
*/
void
ur_bsr_bytes_any(ur_bsr_t *bsr, uint64_t len, uint8_t *out);

/*
**  advance the bitstream cursor as if we had read N bits.
*/
void
ur_bsr_skip_any(ur_bsr_t *bsr, uint64_t len);

/*
**  read a jam/cue type tag.
*/
ur_cue_res_e
ur_bsr_tag(ur_bsr_t *bsr, ur_cue_tag_e *out);

/*
**  read a binary exponent, producing the binary log.
**
**    read N (up to 255) zero bits followed by a 1, produce N.
*/
ur_cue_res_e
ur_bsr_log(ur_bsr_t *bsr, uint8_t *out);

/*
**  read an atomic run-length (a la +rub).
**
**    read a binary log N, then read N (up to 64) bits,
**    produce (N-bits ^ (1 << N))
*/
ur_cue_res_e
ur_bsr_rub_len(ur_bsr_t *bsr, uint64_t *out);

/*
**  initialize bitstream-writer with prev/size for fibonacci growth.
*/
void
ur_bsw_init(ur_bsw_t *bsw, uint64_t prev, uint64_t size);

/*
**  reallocate bitstream write buffer with max(fibonacci, step) growth.
*/
void
ur_bsw_grow(ur_bsw_t *bsw, uint64_t step);

/*
**  validate bitstream-writer invariants.
*/
ur_bool_t
ur_bsw_sane(ur_bsw_t *bsw);

/*
**  return bit-length, produce byte-buffer.
*/
uint64_t
ur_bsw_done(ur_bsw_t *bsw, uint64_t *len, uint8_t **byt);

/*
**  write a bit
*/
void
ur_bsw_bit(ur_bsw_t *bsw, uint8_t bit);

/*
**  write N (up to 8) bits of a uint8.
*/
void
ur_bsw8(ur_bsw_t *bsw, uint8_t len, uint8_t byt);

/*
**  write N (up to 32) bits of a uint32.
*/
void
ur_bsw32(ur_bsw_t *bsw, uint8_t len, uint32_t val);

/*
**  write N (up to 64) bits of a uint64.
*/
void
ur_bsw64(ur_bsw_t *bsw, uint8_t len, uint64_t val);

/*
**  write N bits of a byte array.
**
**    NB: [byt] must contain at least N bits.
*/
void
ur_bsw_bytes(ur_bsw_t *bsw, uint64_t len, uint8_t *byt);

/*
**  write a binary exponent (N zero bits, followed by a 1).
*/
void
ur_bsw_bex(ur_bsw_t *bsw, uint8_t n);

/*
**  write N (up to 64) run-length prefixed bits (a la +mat).
*/
void
ur_bsw_mat64(ur_bsw_t *bsw, uint8_t len, uint64_t val);

/*
**  write N run-length prefixed bits (a la +mat).
**
**    NB: [byt] must contain at least N bits.
*/
void
ur_bsw_mat_bytes(ur_bsw_t *bsw, uint64_t len, uint8_t *byt);

/*
**  write a backref tag (1, 1) and N (up to 64) run-length prefixed bits.
*/
void
ur_bsw_back64(ur_bsw_t *bsw, uint8_t len, uint64_t val);

/*
**  write an atom tag (0) and N (up to 64) run-length prefixed bits.
*/
void
ur_bsw_atom64(ur_bsw_t *bsw, uint8_t len, uint64_t val);

/*
**  write an atom tag (0) and N run-length prefixed bits.
**
**    NB: [byt] must contain at least N bits.
*/
void
ur_bsw_atom_bytes(ur_bsw_t *bsw, uint64_t len, uint8_t *byt);

/*
**  write a cell tag (1, 0)
*/
void
ur_bsw_cell(ur_bsw_t *bsw);

#endif
