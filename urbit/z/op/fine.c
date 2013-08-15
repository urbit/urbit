/* z/op/fine.c
**
** This file is in the public domain.
*/

#ifdef  U3_ZN_FORGE
/* _zn_forge_fine(): install a fine agent.
**
**   lid: cap at termination
*/
static inline void
_zn_forge_fine(u3_z   z,
               u3_ray lid_ray)
{
  u3_ray zos_ray;

  zos_ray = _zn_push_forge(z, fine);
  *_zn_forge(z, zos_ray, fine, c.ger_op) = c3__fine;
  *_zn_forge(z, zos_ray, fine, c.poq_ray) = z->n.lab_ray;
  *_zn_forge(z, zos_ray, fine, c.lid_ray) = lid_ray;

  z->n.lab_ray = zos_ray;
}
#endif  // U3_ZN_FORGE

#ifdef  U3_ZN_OP
# define _zn_bip_fine(field) *_zn_anvil(z, bip_ray, fine, field)

  case c3__fine: {
    u3_fox gus = _zn_bip_fine(d.gus);

    *a = gus;
    if ( d ) {
      d->cop_w = z->l.cop_w;
    }
    return 0;
  }

#endif  // U3_ZN_OP
