/* z/op/hint.c
**
** This file is in the public domain.
*/

#ifdef  U3_ZN_FORGE
/* _zn_forge_hint(): install a hint agent.
**
**   lid: cap at termination
**   mat: saved mat for departure
**   lip: cap at departure
**   dep: content formula.
*/
static inline void
_zn_forge_hint(u3_z   z,
               u3_ray lid_ray,
               u3_ray mat_ray,
               u3_ray lip_ray,
               u3_fox bus,
               u3_fox dep)
{
  u3_ray zos_ray;

  zos_ray = _zn_push_forge(z, hint);
  *_zn_forge(z, zos_ray, hint, c.ger_op) = c3__hint;
  *_zn_forge(z, zos_ray, hint, c.poq_ray) = z->n.lab_ray;
  *_zn_forge(z, zos_ray, hint, c.lid_ray) = lid_ray;

  *_zn_forge(z, zos_ray, hint, r.mat_ray) = mat_ray;
  *_zn_forge(z, zos_ray, hint, r.lip_ray) = lip_ray;

  *_zn_forge(z, zos_ray, hint, s.bus) = bus;
  *_zn_forge(z, zos_ray, hint, s.dep) = dep;

  z->n.lab_ray = zos_ray;
}

/* _zn_start_hint(): install a hint sequence.
**
**   lid: cap at termination
**   bus: operand subject
**   sef: hint formula
**   dep: content formula
*/
static inline void
_zn_start_hint(u3_z   z,
               u3_ray lid_ray,
               u3_fox bus,
               u3_fox sef,
               u3_fox dep)
{
  u3_ray lip_ray, mat_ray;

  lip_ray = z->l.cap_ray;
  mat_ray = _zn_depart(z);

  _zn_forge_hint(z, lid_ray, mat_ray, lip_ray, bus, dep);
  _zn_forge_nock(z, z->l.cap_ray, bus, sef);
}

#endif  // U3_ZN_FORGE

#ifdef  U3_ZN_OP
# define _zn_bip_hint(field) *_zn_anvil(z, bip_ray, hint, field)

  case c3__hint: {
    _zn_retreat(z, _zn_bip_hint(f.r.mat_ray));
    {
      u3_ray lid_ray = _zn_bip_hint(f.c.lid_ray);
      u3_fox bus     = _zn_bip_hint(f.s.bus);
      u3_fox dep     = _zn_bip_hint(f.s.dep);
      u3_fox gus     = _zn_bip_hint(d.gus);

      if ( u3_no == u3_lr_dust(z, gus) ) {
        _zn_forge_nock(z, lid_ray, bus, dep);
      }
      else { 
        u3_fox p_gus, q_gus;

        if ( (u3_yes == u3_lr_pq(z, gus, c3__bask, &p_gus, &q_gus)) &&
             u3_rat_is_cat(p_gus) &&
             u3_rat_is_cat(q_gus) &&
             (q_gus > 0) &&
             (p_gus > 0) &&
             (p_gus < q_gus) )
        {
          if ( _zn_forge_bask(z, lid_ray, p_gus, q_gus) ) {
            _zn_forge_nock(z, z->l.cap_ray, bus, dep);
          } else {
            _zn_forge_nock(z, lid_ray, bus, dep);
          }
        }
        else if ( u3_yes == u3_lr_p(z, gus, c3__blog, &p_gus) ) {
          u3_b_print(z, "log", u3_t(z, gus));
        }
        else {
          _zn_forge_nock(z, lid_ray, bus, dep);
        }
      }
    }
    break;
  }

#endif  // U3_ZN_OP
