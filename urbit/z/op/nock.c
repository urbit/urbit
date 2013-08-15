/* z/op/nock.c
**
** This file is in the public domain.
*/

#ifdef  U3_ZN_FORGE
/* _zn_forge_nock(): install a nock agent.
**
**   lid: cap at termination
**   bus: subject
**   sef: formula
*/
static inline void
_zn_forge_nock(u3_z z,
               u3_ray lid_ray,
               u3_fox bus,
               u3_fox sef)
{
  u3_ray zos_ray;

  zos_ray = _zn_push_forge(z, nock);
  *_zn_forge(z, zos_ray, nock, c.ger_op) = c3__nock;
  *_zn_forge(z, zos_ray, nock, c.poq_ray) = z->n.lab_ray;
  *_zn_forge(z, zos_ray, nock, c.lid_ray) = lid_ray;

  *_zn_forge(z, zos_ray, nock, s.bus) = bus;
  *_zn_forge(z, zos_ray, nock, s.sef) = sef;
  
  z->n.lab_ray = zos_ray;
}
#endif  // U3_ZN_FORGE

#ifdef  U3_ZN_OP
# define _zn_bip_nock(field) *_zn_anvil(z, bip_ray, nock, field)

  case c3__nock: {
    u3_ray lid_ray = _zn_bip_nock(f.c.lid_ray);
    u3_fox bus     = _zn_bip_nock(f.s.bus);
    u3_fox sef     = _zn_bip_nock(f.s.sef);

    if ( u3_no == u3_lr_dust(z, sef) ) {
      return c3__exit;
    }
    else {
      u3_fox hib = u3_h(z, sef);
      u3_fox fel = u3_t(z, sef);

      if ( u3_yes == u3_lr_dust(z, hib) ) {
        _zn_forge_cons(z, lid_ray);
        _zn_forge_tail(z, z->l.cap_ray, bus, fel);
        _zn_forge_nock(z, z->l.cap_ray, bus, hib);
      }
      else {
        switch ( hib ) {
          default: return c3__exit;

          case u3_nock_frag: {
            u3_fox pob = u3_lr_twig(z, fel, bus);

            if ( u3_none == pob ) {
              return c3__exit;
            }
            else {
              u3_fox vik = u3_ln_ice(z, pob);

              if ( u3_none == vik ) {
                return c3__fail;
              }
              else {
                _zn_complete(z, lid_ray, vik);
              }
            }
            break;
          }

          case u3_nock_bone: {
            u3_fox vik = u3_ln_ice(z, fel);

            if ( u3_none == vik ) {
              return c3__fail;
            }
            else {
              _zn_complete(z, lid_ray, vik);
            }
            break;
          }

          case u3_nock_sail: {
            _zn_start_sail(z, lid_ray, bus, fel); 
            break;
          }
          case u3_nock_dust: {
            _zn_start_root(z, c3__dust, lid_ray, bus, fel); 
            break;
          }
          case u3_nock_vint: {
            _zn_start_root(z, c3__vint, lid_ray, bus, fel); 
            break;
          }
          case u3_nock_sing: {
            _zn_start_root(z, c3__sing, lid_ray, bus, fel); 
            break;
          }
          case u3_nock_trol: {
            if ( u3_no == u3_lr_dust(z, fel) ) {
              return c3__exit;
            } 
            else {
              u3_fox p_fel = u3_h(z, fel);
              u3_fox qr_fel = u3_t(z, fel);

              if ( u3_no == u3_lr_dust(z, qr_fel) ) {
                return c3__exit;
              }
              else {
                u3_fox q_fel = u3_h(z, qr_fel);
                u3_fox r_fel = u3_t(z, qr_fel);

                _zn_start_trol(z, lid_ray, bus, p_fel, q_fel, r_fel);
              }
            }
            break;
          }

          case u3_nock_flac: {
            if ( u3_no == u3_lr_dust(z, fel) ) {
              return c3__exit;
            }
            else {
              _zn_start_flac
                (z, lid_ray, bus, u3_h(z, fel), u3_t(z, fel));
              break;
            }
          }
          case u3_nock_gant: {
            if ( u3_no == u3_lr_dust(z, fel) ) {
              return c3__exit;
            }
            else {
              _zn_start_gant
                (z, lid_ray, bus, u3_h(z, fel), u3_t(z, fel));
              break;
            }
          }
          case u3_nock_hint: {
            if ( u3_no == u3_lr_dust(z, fel) ) {
              return c3__exit;
            }
            else {
              _zn_start_hint
                (z, lid_ray, bus, u3_h(z, fel), u3_t(z, fel));
              break;
            }
          }
          case u3_nock_coat: {
            if ( u3_no == u3_lr_dust(z, fel) ) {
              return c3__exit;
            }
            else {
              u3_fox vik = u3_ln_ice(z, u3_t(z, fel));

              if ( u3_none == vik ) {
                return c3__fail;
              }
              else {
                u3_fox cor = u3_ln_cell(z, bus, vik);
                u3_fox pup = u3_h(z, fel);

                if ( 0 != pup ) {
                  u3_zj_load(z, pup, cor);
                }
                _zn_complete(z, lid_ray, u3_ln_cell(z, bus, vik));
              }
              break;
            }
          }
        }
      }
    }
    break;
  }
#endif  // U3_ZN_OP
