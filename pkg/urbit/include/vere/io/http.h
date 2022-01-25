//! @file http.h

#ifndef U3_VERE_IO_HTTP_H
#define U3_VERE_IO_HTTP_H

#include "vere/vere.h"

//! Http header.
typedef struct _u3_hhed {
  struct _u3_hhed* nex_u;
  c3_w             nam_w;
  c3_c*            nam_c;
  c3_w             val_w;
  c3_c*            val_c;
} u3_hhed;

//! Http body block. Also used for responses.
typedef struct _u3_hbod {
  struct _u3_hbod* nex_u;
  c3_w             len_w;
  c3_y             hun_y[0];
} u3_hbod;

//! Initialize http I/O.
u3_auto*
u3_http_io_init(u3_pier* pir_u);

//! Initialize cttp I/O.
u3_auto*
u3_cttp_io_init(u3_pier* pir_u);

#endif /* ifndef U3_VERE_IO_HTTP_H */
