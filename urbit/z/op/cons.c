/* z/op/cons.c
**
** This file is in the public domain.
*/

#ifdef  U3_ZN_FORGE
/* _zn_forge_cons(): install a cons agent.
**
**   lid: cap at termination
*/
static inline void
_zn_forge_cons(u3_z   z,
               u3_ray lid_ray)
{
  u3_ray zos_ray;

  zos_ray = _zn_push_forge(z, cons);
  *_zn_forge(z, zos_ray, cons, c.ger_op) = c3__cons;
  *_zn_forge(z, zos_ray, cons, c.poq_ray) = z->n.lab_ray;
  *_zn_forge(z, zos_ray, cons, c.lid_ray) = lid_ray;

  z->n.lab_ray = zos_ray;
}
#endif  // U3_ZN_FORGE

#ifdef  U3_ZN_OP
# define _zn_bip_cons(field) *_zn_anvil(z, bip_ray, cons, field)

  case c3__cons: {
    u3_fox pux = _zn_bip_cons(d.pux);
    u3_fox nol = _zn_bip_cons(d.nol);
    u3_fox mal;
   
    /* We know this will succeed because we prereserve in the 
    ** main loop.
    */
    mal = u3_ln_cell(z, pux, nol);

    _zn_complete(z, _zn_bip_cons(f.c.lid_ray), mal);
    break;
  }

#endif  // U3_ZN_OP
