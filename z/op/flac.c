/* z/op/flac.c
**
** This file is in the public domain.
*/

/*** %flac 

  initial: 

    0                A                                     1GB
    |                |                                       | 
    |-----------------##########-----------------------------|
                                |      |
                                B      C
      zone:
        cap = B
        hat = A
        mat = C

  after start (D == lid):
       
    0                A E F                                  1GB
    |                | | |                                    | 
    |-----------------xxyy######------------------------------|
                                |  |   |
                                B  D   C
      zone:
        cap = F
        hat = B
        mat = A

      agent x (%flac):
        c.lid = D
        r.lip = B
        r.mat = C

      agent y (%nock):
        c.lid = E

  after nock:

    0                A EG                                  1GB
    |                | ||                                     | 
    |-----------------xxx####---------------------------------|
                             |  |  |   |
                             H  B  D   C
                              Q  R 
      zone:
        cap = G
        hat = H
        mat = A

      agent x (%flac):
        c.lid = D
        r.lip = B
        r.mat = C

  after retreat, in %flac operator:
    
    0                A                                      1GB
    |                |                                        | 
    |-----------------#######---------------------------------|
                             |  |  |   |
                             H  B  D   C
                              Q  R 
      zone:
        cap = H
        hat = A
        mat = C
      
  after posting second %nock (tamp failed):

    0                A                                      1GB
    |                |                                        | 
    |-----------------###wwzz---------------------------------|
                         | | |  |  |   |
                         I J H  B  D   C

      zone:
        cap = I
        hat = A
        mat = C

      agent w (%nock):
        c.lid = J

      agent z (%drop):
        c.lid = D

  after posting second %nock (H-B tamps over B-D to be K-D):

    0                A                                      1GB
    |                |                                        | 
    |-----------------#######vv-------------------------------|
                             |  |  |   |
                             L  K  D   C

      zone:
        cap = L
        hat = A
        mat = C

      agent v (%nock):
        c.lid = D
***/

#ifdef  U3_ZN_FORGE
/* _zn_forge_flac(): install a flac agent.
**
**   lid: cap at termination
**   mat: saved mat for departure
**   lip: cap at departure
**   dep: flac formula.
*/
static inline void
_zn_forge_flac(u3_z   z,
               u3_ray lid_ray,
               u3_ray mat_ray,
               u3_ray lip_ray,
               u3_fox dep)
{
  u3_ray zos_ray;

  zos_ray = _zn_push_forge(z, flac);
  *_zn_forge(z, zos_ray, flac, c.ger_op) = c3__flac;
  *_zn_forge(z, zos_ray, flac, c.poq_ray) = z->n.lab_ray;
  *_zn_forge(z, zos_ray, flac, c.lid_ray) = lid_ray;

  *_zn_forge(z, zos_ray, flac, r.mat_ray) = mat_ray;
  *_zn_forge(z, zos_ray, flac, r.lip_ray) = lip_ray;

  *_zn_forge(z, zos_ray, flac, s.dep) = dep;

  z->n.lab_ray = zos_ray;
}

/* _zn_start_flac(): install a flac sequence.
**
**   lid: cap at termination
**   bus: operand subject
**   sef: operand formula
**   dep: flac formula
*/
static inline void
_zn_start_flac(u3_z   z,
               u3_ray lid_ray,
               u3_fox bus,
               u3_fox sef,
               u3_fox dep)
{
  u3_ray lip_ray, mat_ray;

  lip_ray = z->l.cap_ray;
  mat_ray = _zn_depart(z);

  _zn_forge_flac(z, lid_ray, mat_ray, lip_ray, dep);
  _zn_forge_nock(z, z->l.cap_ray, bus, sef);
}
#endif  // U3_ZN_FORGE

#ifdef  U3_ZN_OP
# define _zn_bip_flac(field) *_zn_anvil(z, bip_ray, flac, field)

  case c3__flac: {
    _zn_retreat(z, _zn_bip_flac(f.r.mat_ray));
    {
      u3_ray lid_ray = _zn_bip_flac(f.c.lid_ray);
      u3_ray lip_ray = _zn_bip_flac(f.r.lip_ray);
      u3_fox dep     = _zn_bip_flac(f.s.dep);
      u3_fox gus     = _zn_bip_flac(d.gus);
#if 1
      if ( (lid_ray != lip_ray) && (lip_ray != z->l.cap_ray) ) {
        /* Tail optimization.
        **
        ** We seek to elide the segment whose bottom is [lid_ray],
        ** and whose top is [lip_ray].
        **
        ** [lid_ray] is the cap pointer at the end of the calculation.
        ** [lip_ray] is the base of the subject area.  Call the region
        ** from [lid_ray] to [lip_ray] region R, and the region from
        ** [lip_ray] to [l->cap_ray] region Q.
        **
        ** Because it is above [lid_ray], we know that region R will be
        ** discarded after (nock gus dep) completes.  The question is
        ** whether it can be discarded *before* this flac completion.
        **
        ** We know [dep] is a static formula.  So only [gus], the
        ** subject we have just produced, can point into Q or R.  
        **
        ** It must point into Q, or Q would be empty.  The relevant
        ** question therefore is: does Q point into R?
        **
        ** If not, we may tamp - relocating [gus] as we elide region R,
        ** and slide region Q down on top of it.  If not, we can't.
        **
        ** Without tail optimization, we cannot loop without leaking.
        **
        ** Note that both detecting and applying this optimization has
        ** a cost of the same order as the size of Q.  Moreover, if we
        ** fail to apply the optimization, we consume this same order of
        ** memory.  Consistency minimizes performance surprises.
        */
        if ( u3_yes == u3_lm_clear(z, gus, lid_ray, lip_ray) ) {
          /* Clear to tamp.
          */
          gus = u3_lm_tamp(z, gus, lid_ray, lip_ray);
        } 
        else {
          /* Not clear to tamp.
          ** 
          ** So that further attempts to tamp in this chain can work,
          ** the nock agent that completes the flac must not tamp to
          ** [lid], but rather to the current cap.  Thus, to avoid
          ** leaking, we must install a drop agent to free.
          **
          ** Otherwise, the interpreter will detect a tail violation
          ** on every call, as it tries to tamp this entire region.
          ** It will thus fail to tamp, and leak, when it should be
          ** tamping on every call but the first.
          */
          _zn_forge_drop(z, lid_ray);
          lid_ray = z->l.cap_ray;
        }
      }
#endif
      _zn_forge_nock(z, lid_ray, gus, dep);
    }
    break;
  }

#endif  // U3_ZN_OP
