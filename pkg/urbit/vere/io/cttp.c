//! @file cttp.c

#include "vere/vere.h"

//==============================================================================
// Types
//==============================================================================

typedef struct {
  u3_auto driver_u;   //!< driver handle
  c3_l    inst_num_l; //!< instance number
} _client;

//==============================================================================
// Static functions
//==============================================================================

static void
_driver_exit(u3_auto* driver_u);

static c3_o
_driver_kick(u3_auto* driver_u, u3_noun wire, u3_noun card);

static void
_driver_talk(u3_auto* driver_u);

static void
_driver_exit(u3_auto* driver_u)
{

}

static c3_o
_driver_kick(u3_auto* driver_u, u3_noun wire, u3_noun card)
{
  return c3n;
}

static void
_driver_talk(u3_auto* driver_u)
{

}

//==============================================================================
// Functions
//==============================================================================

u3_auto*
u3_cttp_io_init(u3_pier* pir_u)
{
  _client* client_u = c3_calloc(sizeof(*client_u));

  client_u->driver_u = (u3_auto){
    .nam_m = c3__cttp,
    .liv_o = c3y,
    .io.talk_f = _driver_talk,
    .io.kick_f = _driver_kick,
    .io.exit_f = _driver_exit,
  };

  {
    struct timeval time_u;
    gettimeofday(&time_u, NULL);
    u3_noun now = u3_time_in_tv(&time_u);
    client_u->inst_num_l = u3r_mug(now);
    u3z(now);
  }

  return (u3_auto*)&client_u->driver_u;
}
