//! @file meta.h

#ifndef U3_VERE_META_H
#define U3_VERE_META_H

//! Pier metadata managed by an event log.
typedef struct {
  c3_d who_d[2]; //!< identity
  c3_o fak_o;    //!< fake bit
  c3_w lif_w;    //!< lifecycle length
  c3_w ver_w;    //!< event log version
} u3_meta;

#endif /* ifndef U3_VERE_META_H */
