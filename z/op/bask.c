/* z/op/bask.c
**
** This file is in the public domain.
*/

#ifdef  U3_ZN_FORGE
/* _zn_forge_bask(): install a basket sequence.
*/
static inline c3_b
_zn_forge_bask(u3_z    z,
               u3_ray  lid_ray,
               c3_w    num_w,
               c3_w    dem_w)
{
  u3_ray zos_ray;

  zos_ray = _zn_push_forge(z, bask);
  *_zn_forge(z, zos_ray, bask, c.ger_op) = c3__bask;
  *_zn_forge(z, zos_ray, bask, c.poq_ray) = z->n.lab_ray;
  *_zn_forge(z, zos_ray, bask, c.lid_ray) = lid_ray;

  {
    c3_w lef_w = z->l.bat_nit - 
                 (u3_ray_b(z->l.hat_ray) + 
                  u3_ray_b(z->l.cap_ray) + 128);

    c3_w wor_w = num_w * (lef_w / dem_w);

    *_zn_forge(z, zos_ray, bask, s.wor_w) = wor_w;
    z->l.cap_ray += wor_w;
  }
  z->n.lab_ray = zos_ray;
  return 1;
}
#endif  // U3_ZN_FORGE

#ifdef  U3_ZN_OP
# define _zn_bip_bask(field) *_zn_anvil(z, bip_ray, bask, field)

  case c3__bask: {
    c3_w   wor_w   = _zn_bip_bask(f.s.wor_w);
    u3_ray anv_ray = bip_ray + c3_wiseof(struct u3_zn_forge_bask) + wor_w;
    u3_fox gus     = *u3_at_ray(&z->l, anv_ray);

    _zn_complete(z, _zn_bip_bask(f.c.lid_ray), gus);
    break;
  }

#endif  // U3_ZN_OP
