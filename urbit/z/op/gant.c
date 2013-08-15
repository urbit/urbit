/* z/op/gant.c
**
** This file is in the public domain.
*/

#ifdef  U3_ZN_FORGE
/* _zn_forge_gant(): install a gant agent.
**
**   lid: cap at termination
**   mat: saved mat for departure
**   lip: cap at departure
**   dep: gant formula.
*/
static inline void
_zn_forge_gant(u3_z   z,
               u3_ray lid_ray,
               u3_ray mat_ray,
               u3_ray lip_ray,
               u3_fox bus,
               u3_fox dep)
{
  u3_ray zos_ray;

  zos_ray = _zn_push_forge(z, gant);
  *_zn_forge(z, zos_ray, gant, c.ger_op) = c3__gant;
  *_zn_forge(z, zos_ray, gant, c.poq_ray) = z->n.lab_ray;
  *_zn_forge(z, zos_ray, gant, c.lid_ray) = lid_ray;

  *_zn_forge(z, zos_ray, gant, r.mat_ray) = mat_ray;
  *_zn_forge(z, zos_ray, gant, r.lip_ray) = lip_ray;

  *_zn_forge(z, zos_ray, gant, s.bus) = bus;
  *_zn_forge(z, zos_ray, gant, s.dep) = dep;

  z->n.lab_ray = zos_ray;
}

/* _zn_start_gant(): install a gant sequence.
**
**   lid: cap at termination
**   bus: operand subject
**   sef: operand formula
**   dep: gant formula
*/
static inline void
_zn_start_gant(u3_z   z,
               u3_ray lid_ray,
               u3_fox bus,
               u3_fox sef,
               u3_fox dep)
{
  u3_ray lip_ray, mat_ray;

  lip_ray = z->l.cap_ray;
  mat_ray = _zn_depart(z);

  _zn_forge_gant(z, lid_ray, mat_ray, lip_ray, bus, dep);
  _zn_forge_nock(z, z->l.cap_ray, bus, sef);
}

#endif  // U3_ZN_FORGE

#ifdef  U3_ZN_OP
# define _zn_bip_gant(field) *_zn_anvil(z, bip_ray, gant, field)

  case c3__gant: {
    u3_ray mat_ray = _zn_bip_gant(f.r.mat_ray);
    u3_ray lid_ray = _zn_bip_gant(f.c.lid_ray);
    u3_fox dep     = _zn_bip_gant(f.s.dep);
    u3_fox bus     = _zn_bip_gant(f.s.bus);
    u3_fox gus     = _zn_bip_gant(d.gus);
    u3_fox mal;
    
    mal = u3_ln_cell(z, gus, bus);
    _zn_retreat(z, mat_ray);

    _zn_forge_nock(z, lid_ray, mal, dep);

    break;
  }

#endif  // U3_ZN_OP
