/* z/op/trol.c
**
** This file is in the public domain.
*/

#ifdef  U3_ZN_FORGE
/* _zn_forge_trol(): install a trol agent.
**
**   lid: cap at termination
**   mat: saved mat for departure
**   lip: cap at departure
**   bus: subject
**   feg: then formula
**   paf: else formula
*/
static inline void
_zn_forge_trol(u3_z z,
               u3_ray lid_ray,
               u3_ray mat_ray,
               u3_ray lip_ray,
               u3_fox bus,
               u3_fox feg,
               u3_fox paf)
{
  u3_ray zos_ray;

  zos_ray = _zn_push_forge(z, trol);
  *_zn_forge(z, zos_ray, trol, c.ger_op) = c3__trol;
  *_zn_forge(z, zos_ray, trol, c.poq_ray) = z->n.lab_ray;
  *_zn_forge(z, zos_ray, trol, c.lid_ray) = lid_ray;

  *_zn_forge(z, zos_ray, trol, r.mat_ray) = mat_ray;
  *_zn_forge(z, zos_ray, trol, r.lip_ray) = lip_ray;

  *_zn_forge(z, zos_ray, trol, s.bus) = bus;
  *_zn_forge(z, zos_ray, trol, s.feg) = feg;
  *_zn_forge(z, zos_ray, trol, s.paf) = paf;
  
  z->n.lab_ray = zos_ray;
}

/* _zn_start_trol(): install a trol sequence.
**
**   lid: cap at termination
**   bus: subject
**   cor: test formula
**   feg: then formula
**   paf: else formula
*/
static inline void
_zn_start_trol(u3_z   z,
               u3_ray lid_ray,
               u3_fox bus,
               u3_fox cor,
               u3_fox feg,
               u3_fox paf)
{
  u3_ray lip_ray, mat_ray;

  lip_ray = z->l.cap_ray;
  mat_ray = _zn_depart(z);

  _zn_forge_trol(z, lid_ray, mat_ray, lip_ray, bus, feg, paf);
  _zn_forge_nock(z, z->l.cap_ray, bus, cor);
}

#endif  // U3_ZN_FORGE

#ifdef  U3_ZN_OP
# define _zn_bip_trol(field) *_zn_anvil(z, bip_ray, trol, field)

  case c3__trol: {
    _zn_retreat(z, _zn_bip_trol(f.r.mat_ray));
    {
      u3_ray lid_ray = _zn_bip_trol(f.c.lid_ray);
      u3_ray lip_ray = _zn_bip_trol(f.r.lip_ray);
      u3_fox bus     = _zn_bip_trol(f.s.bus);
      u3_fox feg     = _zn_bip_trol(f.s.feg);
      u3_fox paf     = _zn_bip_trol(f.s.paf);
      u3_fox gus     = _zn_bip_trol(d.gus);

      if ( 0 == gus ) {
        c3_assert(lip_ray == z->l.cap_ray);

        _zn_forge_nock(z, lid_ray, bus, feg); 
      }
      else if ( 1 == gus ) {
        c3_assert(lip_ray == z->l.cap_ray);

        _zn_forge_nock(z, lid_ray, bus, paf); 
      }
      else {
        return c3__exit;
      }
    }
    break;
  }

#endif  // U3_ZN_OP
