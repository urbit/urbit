
#define C3_GLOBAL

#include "all.h"

#include "vere/lmdb.h"

// A simple memory tester.
//
int
main(int argc, char *argv[])
{
  u3m_init();
  u3m_pave(c3y, c3n);

  {
    MDB_env* env = u3m_lmdb_init("./test-env");

    u3_noun who = u3i_string("~littel-ponnys");
    u3_noun is_fake = u3i_string("is-fake");
    u3_noun life = u3i_string("life");

    u3m_lmdb_write_identity(env, who, is_fake, life);
    u3m_lmdb_shutdown(env);
  }

  {
    MDB_env* env = u3m_lmdb_init("./test-env");

    u3_noun who;
    u3_noun is_fake;
    u3_noun life;

    u3m_lmdb_read_identity(env, &who, &is_fake, &life);
    u3m_lmdb_shutdown(env);

    u3m_p("who", who);
    u3m_p("is-fake", is_fake);
    u3m_p("life", life);
  }
}
