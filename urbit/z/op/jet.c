/* z/op/jet.c
**
** This file is in the public domain.
*/

#ifdef  U3_ZN_FORGE
/* _zn_forge_jet(): install a jet agent.
**
**   lid: cap at termination
**   mat: saved mat for departure
**   lip: cap at departure
**   sax: jet code
*/
static inline void
_zn_forge_jet(u3_z            z,
              u3_ray          lid_ray,
              u3_ray          mat_ray,
              u3_ray          lip_ray,
              enum u3_zj_code sax_code)
{
  u3_ray zos_ray;

  zos_ray = _zn_push_forge(z, jet);
  *_zn_forge(z, zos_ray, jet, c.ger_op) = c3__jet;
  *_zn_forge(z, zos_ray, jet, c.poq_ray) = z->n.lab_ray;
  *_zn_forge(z, zos_ray, jet, c.lid_ray) = lid_ray;

  *_zn_forge(z, zos_ray, jet, r.mat_ray) = mat_ray;
  *_zn_forge(z, zos_ray, jet, r.lip_ray) = lip_ray;

  *_zn_forge(z, zos_ray, jet, s.sax_w) = sax_code;

  z->n.lab_ray = zos_ray;
}

/* _zn_start_jet(): install a jet sequence.
**
**   lid: cap at termination
**   bus: operand subject
**   sef: operand formula
**   sax: jet code
*/
static inline void
_zn_start_jet(u3_z            z,
              u3_ray          lid_ray,
              u3_fox          bus,
              u3_fox          sef,
              enum u3_zj_code sax_code)
{
  u3_ray lip_ray, mat_ray;

  lip_ray = z->l.cap_ray;
  mat_ray = _zn_depart(z);

  _zn_forge_jet(z, lid_ray, mat_ray, lip_ray, sax_code);
  _zn_forge_nock(z, z->l.cap_ray, bus, sef);
}

#endif  // U3_ZN_FORGE

#ifdef  U3_ZN_OP
# define _zn_bip_jet(field) *_zn_anvil(z, bip_ray, jet, field)

  case c3__jet: {
    _zn_retreat(z, _zn_bip_jet(f.r.mat_ray));
    {
      /* sax: jet code
      ** gus: computed gate
      ** zec: result code
      ** pod: result
      */
      u3_ray  lid_ray = _zn_bip_jet(f.c.lid_ray);
      u3_fox  sax     = _zn_bip_jet(f.s.sax_w);
      u3_fox  gus     = _zn_bip_jet(d.gus);
      u3_mote zec;
      u3_fox  pod;

      if ( u3_yes == u3_lr_dust(z, gus) ) {
        zec = u3_zj_fire(z, &pod, sax, gus);
      } 
      else zec = c3__punt;

      switch ( zec ) {
        case 0: 
          _zn_complete(z, lid_ray, pod);
          break;

        case c3__fail:
        case c3__exit:
          return zec;

        case c3__punt: {
          u3_fox bat = u3_zj_bat(z, sax);

          _zn_forge_nock(z, lid_ray, gus, bat);
          break;
        }
        case c3__test: {
          u3_fox bat = u3_zj_bat(z, sax);

          _zn_start_mate(z, lid_ray, pod, gus, bat);
          break;
        }
        default: c3_assert(!"unknown error");
      }
    }
    break;
  }

#endif  // U3_ZN_OP
