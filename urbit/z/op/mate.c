/* z/op/mate.c
**
** This file is in the public domain.
*/

#ifdef  U3_ZN_FORGE
/* _zn_forge_mate(): install a mate agent.
**
**   lid: cap at termination
**   mat: saved mat for departure
**   lip: cap at departure
**   bus: subject
**   feg: then formula
**   paf: else formula
*/
static inline void
_zn_forge_mate(u3_z z,
               u3_ray lid_ray,
               u3_ray mat_ray,
               u3_ray lip_ray,
               u3_fox pod)
{
  u3_ray zos_ray;

  zos_ray = _zn_push_forge(z, mate);
  *_zn_forge(z, zos_ray, mate, c.ger_op) = c3__mate;
  *_zn_forge(z, zos_ray, mate, c.poq_ray) = z->n.lab_ray;
  *_zn_forge(z, zos_ray, mate, c.lid_ray) = lid_ray;

  *_zn_forge(z, zos_ray, mate, r.mat_ray) = mat_ray;
  *_zn_forge(z, zos_ray, mate, r.lip_ray) = lip_ray;

  *_zn_forge(z, zos_ray, mate, s.pod) = pod;
  
  z->n.lab_ray = zos_ray;
}

/* _zn_start_mate(): install a mate sequence.
**
**   lid: cap at termination
**   pod: jet-computed product
**   bus: subject
**   sef: formula
*/
static inline void
_zn_start_mate(u3_z   z,
               u3_ray lid_ray,
               u3_fox pod,
               u3_fox bus, 
               u3_fox sef)
{
  u3_ray lip_ray, mat_ray;

  lip_ray = z->l.cap_ray;
  mat_ray = _zn_depart(z);

  _zn_forge_mate(z, lid_ray, mat_ray, lip_ray, pod);
  _zn_forge_nock(z, z->l.cap_ray, bus, sef);
}
#endif  // U3_ZN_FORGE

#ifdef  U3_ZN_OP
# define _zn_bip_mate(field) *_zn_anvil(z, bip_ray, mate, field)

  case c3__mate: {
    _zn_retreat(z, _zn_bip_mate(f.r.mat_ray));
    {
      u3_ray lid_ray = _zn_bip_mate(f.c.lid_ray);
      u3_fox pod     = _zn_bip_mate(f.s.pod);
      u3_fox gus     = _zn_bip_mate(d.gus);

      if ( u3_yes == u3_lr_sing(z, pod, gus) ) {
        _zn_complete(z, lid_ray, pod);
      }
      else {
        printf("mate: jet mismatch\n");

        u3_b_print(&z->l, "hard", pod);
        u3_b_print(&z->l, "soft", gus);

        return c3__fail;
      }
    }
    break;
  }

#endif  // U3_ZN_OP
