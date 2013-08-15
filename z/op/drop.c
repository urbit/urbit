/* z/op/drop.c
**
** This file is in the public domain.
*/

#ifdef  U3_ZN_FORGE
/* _zn_forge_drop(): install a drop agent.
**
**   lid: cap at termination
*/
static inline void
_zn_forge_drop(u3_z   z,
               u3_ray lid_ray)
{
  u3_ray zos_ray;

  zos_ray = _zn_push_forge(z, drop);
  *_zn_forge(z, zos_ray, drop, c.ger_op) = c3__drop;
  *_zn_forge(z, zos_ray, drop, c.poq_ray) = z->n.lab_ray;
  *_zn_forge(z, zos_ray, drop, c.lid_ray) = lid_ray;

  z->n.lab_ray = zos_ray;
}
#endif  // U3_ZN_FORGE

#ifdef  U3_ZN_OP
# define _zn_bip_drop(field) *_zn_anvil(z, bip_ray, drop, field)

  case c3__drop: {
    u3_fox gus = _zn_bip_drop(d.gus);

    _zn_complete(z, _zn_bip_drop(f.c.lid_ray), gus);
    break;
  }

#endif  // U3_ZN_OP
