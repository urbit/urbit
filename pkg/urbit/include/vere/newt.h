//! @file newt.h

#ifndef U3_VERE_NEWT_H
#define U3_VERE_NEWT_H

//! Poke callback function.
typedef c3_o (*u3_moor_poke)(void*, c3_d, c3_y*);

//! Bailout callback function.
typedef void (*u3_moor_bail)(void*, ssize_t err_i, const c3_c* err_c);

//! Blob message block.
typedef struct _u3_meat {
  struct _u3_meat* nex_u;
  c3_d             len_d;
  c3_y             hun_y[0];
} u3_meat;

//! In-process message type.
typedef enum {
  u3_mess_head = 0,  //!< awaiting header
  u3_mess_tail = 1,  //!< awaiting body
} u3_mess_type;

//! Blob message in process.
typedef struct _u3_mess {
  u3_mess_type     sat_e;     //!< msg type
  union {
    struct {                  //!< awaiting header:
      c3_y         len_y[8];  //!<   header bytes
      c3_y         has_y;     //!<   length
    } hed_u;
    struct {                  //!< awaiting body
      u3_meat*     met_u;     //!<   partial message
      c3_d         has_d;     //!<   length
    } tal_u;
  };
} u3_mess;

//! Inbound message stream.
typedef struct _u3_moat {
  uv_pipe_t        pyp_u;  //!< input stream
  u3_moor_bail     bal_f;  //!< error response function
  void*            ptr_v;  //!< callback pointer
  u3_moor_poke     pok_f;  //!< action function
  u3_mess          mes_u;  //!< message in progress
  uv_timer_t       tim_u;  //!< queue timer
  u3_meat*         ent_u;  //!< entry of message queue
  u3_meat*         ext_u;  //!< exit of message queue
} u3_moat;

//! Outbound message stream.
typedef struct _u3_mojo {
  uv_pipe_t        pyp_u;  //!< output stream
  u3_moor_bail     bal_f;  //!< error response function
  void*            ptr_v;  //!< callback pointer
} u3_mojo;

//! Two-way message stream, linked list
typedef struct _u3_moor {
  uv_pipe_t        pyp_u;  //!< duplex stream
  u3_moor_bail     bal_f;  //!< error response function
  void*            ptr_v;  //!< callback pointer
  u3_moor_poke     pok_f;  //!< action function
  u3_mess          mes_u;  //!< message in progress
  uv_timer_t       tim_u;  //!< queue timer
  u3_meat*         ent_u;  //!< entry of message queue
  u3_meat*         ext_u;  //!< exit of message queue
  struct _u3_moor* nex_u;  //!< next in list
} u3_moor;

//! Decode a (partial) length-prefixed byte buffer
void
u3_newt_decode(u3_moat* mot_u, c3_y* buf_y, c3_d len_d);

//! Write buffer to stream.
void
u3_newt_send(u3_mojo* moj_u, c3_d len_d, c3_y* byt_y);

//! Activate reading on input stream.
void
u3_newt_read(u3_moat* mot_u);

//! Print status info.
void
u3_newt_moat_info(u3_moat* mot_u);

//! Newt stop/close input stream.
void
u3_newt_moat_stop(u3_moat* mot_u, u3_moor_bail bal_f);

//! Newt stop/close output stream.
void
u3_newt_mojo_stop(u3_mojo* moj_u, u3_moor_bail bal_f);

#endif /* ifndef U3_VERE_NEWT_H */
