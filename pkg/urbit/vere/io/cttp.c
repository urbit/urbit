//! @file cttp.c

#include "vere/vere.h"
#include "vere/io.h"

//==============================================================================
// Types
//==============================================================================

typedef struct {
  c3_d  len_d;  //!< length of jammed request
  c3_y  type_y; //!< type of IO request (0 = HTTP client)
  c3_y* jam_y;  //!< jammed request
} _io_req;

typedef struct {
  c3_d len_d;    //!< length of jammed response
  c3_y resp_y[]; //!< jammed response
} _io_resp;

struct client;

typedef struct {
  uv_pipe_t      pipe_u;   //!< pipe handle
  struct client *client_u; //!< back pointer to enclosing client
} _stream;

typedef struct client {
  u3_auto        driver_u;   //!< driver handle
  c3_l           inst_num_l; //!< instance number
  struct {
    uv_process_t proc_u;     //!< process handle
    _stream      stdin_u;    //!< stdin stream to IO process
    _stream      stdout_u;   //!< stdout stream to IO process
  } child_u;                 //!< IO process
} _client;

//==============================================================================
// Macros
//==============================================================================

//! Error-handling wrapper for libuv API calls that return a non-negative
//! integer on success and a negative integer on failure.
//!
//! @param[in] uv_call         libuv API call.
//! @param[in] failure_action  Statement to execute after logging failure.
#define try_uv(uv_call, failure_action, ...)                                   \
  do {                                                                         \
    c3_i ret_i = uv_call;                                                      \
    if ( 0 != ret_i ) {                                                        \
      fprintf(stderr, "uv: " __VA_ARGS__);                                     \
      fprintf(stderr, ": %s\r\n", uv_strerror(ret_i));                         \
      failure_action;                                                          \
    }                                                                          \
  } while ( 0 )

//==============================================================================
// Static functions
//==============================================================================

static void
_child_exit_cb(uv_process_t* child_u, c3_ds status_d, c3_i term_sig_i);

static void
_driver_exit(u3_auto* driver_u);

static c3_o
_driver_kick(u3_auto* driver_u, u3_noun wire, u3_noun card);

static void
_driver_talk(u3_auto* driver_u);

static void
_write_cb(uv_write_t* req_u, c3_i status_i);

static void
_child_exit_cb(uv_process_t* child_u, c3_ds status_d, c3_i term_sig_i)
{
  if ( 0 != status_d ) {
    fprintf(stderr,
            "cttp: child process exited with code %" PRIi64 "\r\n",
            status_d);
  }
}

static void
_driver_exit(u3_auto* driver_u)
{
  _client* client_u = (_client*)driver_u;
  c3_free(client_u);
}

// card is [tag data].
static c3_o
_driver_kick(u3_auto* driver_u, u3_noun wire, u3_noun card)
{
  c3_o suc_o = c3n;

  u3_noun tag, data, wire_head;
  if ( (c3n == u3r_cell(wire, &wire_head, NULL))
       || (c3n == u3r_sing_c("http-client", wire_head)) )
  {
    goto end;
  }

  _client* client_u = (_client*)driver_u;
  _io_req* io_req_u = c3_malloc(sizeof(*io_req_u));
  u3s_jam_xeno(card, &io_req_u->len_d, &io_req_u->jam_y);
  io_req_u->type_y = IO_HTTP_CLIENT;
  //io_req_u->len_d += sizeof(io_req_u->type_y);

  uv_buf_t req_bufs_u[] = {
    {
      // Jammed request length.
      .base = (c3_c*)&io_req_u->len_d,
      .len  = sizeof(io_req_u->len_d),
    },
    {
      // Request type.
      .base = (c3_c*)&io_req_u->type_y,
      .len  = sizeof(io_req_u->type_y),
    },
    {
      // Jammed request.
      .base = (c3_c*)io_req_u->jam_y,
      .len  = io_req_u->len_d,
    },
  };

  uv_write_t* write_req_u = c3_malloc(sizeof(*write_req_u));
  try_uv(uv_write(write_req_u,
                 (uv_stream_t*)&client_u->child_u.stdin_u,
                 req_bufs_u,
                 sizeof(req_bufs_u) / sizeof(*req_bufs_u),
                 _write_cb),
         goto end,
         "failed to write %%http-client request to pipe");


  suc_o = c3y;

end:
  u3z(wire);
  u3z(card);
  return suc_o;
}

//! Notify that the HTTP client driver is live.
static void
_driver_talk(u3_auto* driver_u)
{
  _client* client_u = (_client*)driver_u;

  u3_noun wire = u3nt(u3i_string("http-client"),
                      u3dc("scot", c3__uv, client_u->inst_num_l),
                      u3_nul);
  u3_noun card = u3nc(c3__born, u3_nul);

  u3_auto_plan(driver_u, u3_ovum_init(0, c3__i, wire, card));
}

static void
_read_alloc_cb(uv_handle_t* handle_u, size_t sug_sz_i, uv_buf_t* buf_u)
{
  // TODO: replace sug_sz_i (which is 65KB) with a more apporpriate size
  buf_u->base = c3_malloc(sug_sz_i);
  buf_u->len  = sug_sz_i;
}

static void
_read_cb(uv_stream_t* stream_u, ssize_t bytes_read_i, const uv_buf_t* buf_u)
{
  static c3_d      idx_d  = 0;
  static _io_resp *resp_u = NULL;
  if ( !resp_u ) {
    resp_u = c3_calloc(sizeof(*resp_u));
  }

  _client *client_u = ((_stream*)stream_u)->client_u;

  // Read failed.
  if ( 0 > bytes_read_i ) {
    uv_read_stop(stream_u);
    if ( UV_EOF != bytes_read_i ) {
      fprintf(stderr,
              "http-client: read failed: %s\r\n",
              uv_strerror(bytes_read_i));
    }
    goto free_buf;
  }
  // EAGAIN / EWOULDBLOCK.
  else if ( 0 == bytes_read_i ) {
    goto free_buf;
  }

  c3_c* bytes_c = buf_u->base;

  // Make sure the length is read in first.
  if ( idx_d < sizeof(resp_u->len_d) ) {
    ssize_t len_bytes_i = c3_min(bytes_read_i,
                                 sizeof(resp_u->len_d) - idx_d);
    memcpy((c3_c*)resp_u + idx_d, bytes_c, len_bytes_i);
    idx_d        += len_bytes_i;
    bytes_c      += len_bytes_i;
    bytes_read_i -= len_bytes_i;
    // Allocate space for the response now that the entire length has been read.
    if ( idx_d == sizeof(resp_u->len_d) ) {
      resp_u = c3_realloc(resp_u, sizeof(*resp_u) + resp_u->len_d);
    }
  }

  // Read bytes of the jammed response.
  if ( bytes_read_i > 0 ) {
    c3_d    bytes_needed_d = resp_u->len_d - (idx_d - sizeof(resp_u->len_d));
    ssize_t resp_bytes_i   = c3_min(bytes_read_i,
                                    bytes_needed_d);
    memcpy((c3_c*)resp_u + idx_d, bytes_c, resp_bytes_i);
    idx_d        += resp_bytes_i;
    bytes_c      += resp_bytes_i;
    bytes_read_i -= resp_bytes_i;

    // Enqueue an ovum (potential event) now that an entire response has been
    // read.
    if ( idx_d == sizeof(resp_u->len_d) + resp_u->len_d ) {
      u3_weak resp = u3s_cue_xeno(resp_u->len_d, resp_u->resp_y);
      u3_noun req_num, status, headers, body;
      if ( u3_none != resp
           && c3y == u3r_qual(resp, &req_num, &status, &headers, &body)
           && c3y == u3a_is_cat(req_num)
           && c3y == u3a_is_cat(status) )
      {
        u3_noun wire = u3nt(u3i_string("http-client"),
                            u3dc("scot", c3__uv, client_u->inst_num_l),
                            u3_nul);
        u3_noun card = u3nt(u3i_string("receive"),
                            req_num,
                            u3nq(u3i_string("start"),
                                 u3nc(status, headers),
                                 body,
                                 c3y));
        u3m_p("wire", wire);
        u3m_p("card", card);
        u3_auto_plan(&client_u->driver_u, u3_ovum_init(0, c3__i, wire, card));
      }

      // Prepare for next response.
      c3_free(resp_u);
      idx_d = 0;
      resp_u = NULL;

      // The remaining bytes to read belong to the next response.
      if ( bytes_read_i > 0 ) {
        memmove(buf_u->base, bytes_c, bytes_read_i);
        _read_cb(stream_u, bytes_read_i, buf_u);
        // Return to avoid double free of buf_u.
        return;
      }
    }
  }

free_buf:
  if ( buf_u->base ) {
    c3_free(buf_u->base);
  }
}

static void
_write_cb(uv_write_t* write_req_u, c3_i status_i)
{
  c3_free(write_req_u);
}

//==============================================================================
// Functions
//==============================================================================

u3_auto*
u3_cttp_io_init(u3_pier* pir_u)
{
  _client* client_u = c3_calloc(sizeof(*client_u));

  client_u->driver_u = (u3_auto){
    .nam_m     = c3__cttp,
    .liv_o     = c3y,
    .io.talk_f = _driver_talk,
    .io.kick_f = _driver_kick,
    .io.exit_f = _driver_exit,
  };

  {
    struct timeval time_u;
    gettimeofday(&time_u, NULL);
    u3_noun now          = u3_time_in_tv(&time_u);
    client_u->inst_num_l = u3r_mug(now);
    u3z(now);
  }

  {
    // TODO: integrate the Rust binary.
    c3_c* args_c[] = {
      "/home/tlon/code/io/target/release/io",
      NULL,
    };

    _stream* stdin_u = &client_u->child_u.stdin_u;
    try_uv(uv_pipe_init(u3L, &stdin_u->pipe_u, 0),
           goto fail,
           "failed to open pipe to stdin");
    stdin_u->client_u = client_u;

    _stream* stdout_u = &client_u->child_u.stdout_u;
    try_uv(uv_pipe_init(u3L, &stdout_u->pipe_u, 0),
           goto fail,
           "failed to open pipe to stdout");
    stdout_u->client_u = client_u;

    uv_stdio_container_t stdio_u[] = {
      [STDIN_FILENO] = {
        .flags       = UV_CREATE_PIPE | UV_READABLE_PIPE,
        .data.stream = (uv_stream_t*)&stdin_u->pipe_u,
      },
      [STDOUT_FILENO] = {
        .flags       = UV_CREATE_PIPE | UV_WRITABLE_PIPE,
        .data.stream = (uv_stream_t*)&stdout_u->pipe_u,
      },
      [STDERR_FILENO] = {
        .flags       = UV_INHERIT_FD,
        .data.fd     = STDERR_FILENO,
      },
    };
    uv_process_options_t opt_u = {
      .exit_cb     = _child_exit_cb,
      .file        = args_c[0],
      .args        = args_c,
      // If any fds are inherited, libuv ignores UV_PROCESS_WINDOWS_HIDE*.
      .flags       = UV_PROCESS_WINDOWS_HIDE,
      .stdio_count = sizeof(stdio_u) / sizeof(*stdio_u),
      .stdio       = stdio_u,
    };
    try_uv(uv_spawn(u3L, &client_u->child_u.proc_u, &opt_u),
           goto fail,
           "failed to spawn %s",
           opt_u.file);

    try_uv(uv_read_start((uv_stream_t*)&stdout_u->pipe_u,
                         _read_alloc_cb,
                         _read_cb),
           goto fail,
           "failed to start reading %%http-client responses");
  }

  goto succeed;

fail:
  c3_free(client_u);
  return NULL;

succeed:
  return (u3_auto*)client_u;
}
