/* vere/root.c
**
*/
#include "all.h"
#include "vere/vere.h"

/* _hind_io_talk():
*/
static void
_hind_io_talk(u3_auto* car_u)
{
}

/* _hind_io_kick(): handle generic effects, by tag
*/
static c3_o
_hind_io_kick(u3_auto* car_u, u3_noun wir, u3_noun cad)
{
  u3_noun tag, dat;
  c3_o ret_o;

  if ( c3n == u3r_cell(cad, &tag, &dat) ) {
    ret_o = c3n;
  }
  else {
    switch ( tag ) {
      default: {
        ret_o = c3n;
      } break;

      case c3__exit: {
        ret_o = c3y;
        u3l_log("<<<goodbye>>>\n");
        u3_pier_exit(car_u->pir_u);
      } break;

      //  XX fake effect, check //arvo wire?
      //
      case c3__trim: {
        ret_o = c3y;
        u3_auto_plan(car_u, u3_ovum_init(0, u3_blip,
                                         u3nc(c3__arvo, u3_nul), u3k(cad)));
      } break;

      case c3__vega: {
        ret_o = c3y;
        u3l_log("<<<reset>>>\n");
      } break;

      //  NB: startup explicitly handled in pier.c
      //
      //    XX review arvo upgrade scenaria
      //
      case c3__wend: {
        ret_o = c3y;
      } break;
    }
  }

  u3z(wir); u3z(cad);
  return ret_o;
}

/* _hind_io_exit():
*/
static void
_hind_io_exit(u3_auto* car_u)
{
  c3_free(car_u);
}

/* u3_hind_io_init():
*/
u3_auto*
u3_hind_io_init(u3_pier* pir_u)
{
  u3_auto* car_u = c3_calloc(sizeof(*car_u));
  car_u->nam_m = c3__hind;
  car_u->liv_o = c3y;
  car_u->io.talk_f = _hind_io_talk;
  car_u->io.kick_f = _hind_io_kick;
  car_u->io.exit_f = _hind_io_exit;
  // car_u->ev.bail_f = ...;

  return car_u;
}
