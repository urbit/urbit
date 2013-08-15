/* z/op/tail.c
**
** This file is in the public domain.
*/

#ifdef  U3_ZN_FORGE
/* _zn_forge_tail(): install a tail agent.
**
**   lid: cap at termination
**   bus: subject
**   fel: formula
*/
static inline void
_zn_forge_tail(u3_z   z,
               u3_ray lid_ray,
               u3_fox bus,
               u3_fox fel)
{
  u3_ray zos_ray;

  zos_ray = _zn_push_forge(z, tail);
  *_zn_forge(z, zos_ray, tail, c.ger_op) = c3__tail;
  *_zn_forge(z, zos_ray, tail, c.poq_ray) = z->n.lab_ray;
  *_zn_forge(z, zos_ray, tail, c.lid_ray) = lid_ray;

  *_zn_forge(z, zos_ray, tail, s.bus) = bus;
  *_zn_forge(z, zos_ray, tail, s.fel) = fel;
  
  z->n.lab_ray = zos_ray;
}
#endif  // U3_ZN_FORGE

#ifdef  U3_ZN_OP
# define _zn_bip_tail(field) *_zn_anvil(z, bip_ray, tail, field)

  case c3__tail: {
    u3_fox gus = _zn_bip_tail(d.gus);
    u3_fox bus = _zn_bip_tail(f.s.bus);
    u3_fox fel = _zn_bip_tail(f.s.fel);

    /* Tricky: we push [gus] above the agent below,
    ** presumably a cons.
    */
    _zn_complete(z, _zn_bip_tail(f.c.lid_ray), gus);
    _zn_forge_nock(z, z->l.cap_ray, bus, fel);

    break;
  }

#endif  // U3_ZN_OP
