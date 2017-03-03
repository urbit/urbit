#include <stdio.h>
#include <uv.h>
#include "all.h"
#include "vere/vere.h"

u3_moor moj_u;
uv_connect_t conn;

c3_c *argv1;
c3_c *argv2;

void _nop_bail(void *vod_p, const c3_c *err)
{
  fprintf(stderr, "_nop_bail: %s\r\n", err);
  exit(1);
}

void _nop_noun(void *vod_p, u3_noun mat)
{
}

void _nop_conn(uv_connect_t *conn, int status)
{
  u3_atom doom;
  u3_atom pax, sys;

  pax = u3i_string(argv1);
  sys = u3i_string(argv2);

  doom = u3ke_jam(u3nt(c3__doom,
                       u3_nul,
                       u3nc(c3__boot,
                            u3nq(0, pax, sys, 0))));
  u3_newt_write(&moj_u, doom, 0);
}

c3_i main(c3_i argc, c3_c **argv)
{
  argv1 = argv[1];
  argv2 = argv[2];
  u3_Host.ops_u.dem = c3n;
  u3_Host.lup_u = uv_default_loop();

  moj_u.pok_f = _nop_noun;
  moj_u.bal_f = _nop_bail;
  uv_pipe_init(u3_Host.lup_u, &moj_u.pyp_u, 0);
  uv_pipe_connect(&conn, &moj_u.pyp_u, "/tmp/urbit.sock", _nop_conn);

  u3m_boot_pier();
  {
    extern c3_w u3_Ivory_length_w;
    extern c3_y u3_Ivory_pill_y[];
    u3_noun     lit;

    lit = u3i_bytes(u3_Ivory_length_w, u3_Ivory_pill_y);
    u3v_boot_lite(lit);
  }

  u3_newt_read(&moj_u);

  uv_run(u3L, UV_RUN_DEFAULT);
  return 0;
}
