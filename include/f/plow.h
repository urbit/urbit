/* f/plow.h
**
** This file is in the public domain.
*/
  /** Data structures.
  **/
    /* u2_loom_plow: temporary plow structure.
    */
      typedef struct {
        /* Set [*type *gene] in repo.
        */
        u2_pool fan;

        /* Set [*type] in verify.
        */
        u2_pool ver;

        /* Debug depth.
        */
        u2_atom bug;

        /* Trap - *(list &[p=*text q=*spot])
        */
        u2_noun meb;

        /* Book to memoize nest.
        */
        u2_book vus;

        /* Book to memoize null.
        */
        u2_book tyc;

        /* Book to memoize orth.
        */
        u2_book gam;

        /* Book to memoize show.
        */
        u2_book hos;

        /* Book to memoize play.
        */
        u2_book zor;

        /* Book to memoize make.
        */
        u2_book niq;

        /* Book to memoize safe.
        */
        u2_book fac;

        /* Book to memoize fine.
        */
        u2_book vom;

        /* Book to memoize open.
        */
        u2_book pon;

        /* Book to memoize find.
        */
        u2_book fin;

        /* Book to memoize half.
        */
        u2_book huf;
      } u2_loom_plow;

#     define   u2_plow_(wir_r, pat)  \
          *u2_at(u2_wire_plo_r(wir_r), u2_loom_plow, pat)

  /** Functions.
  **/
    /* u2_pl_boot():
    **
    **   Initialize plow support context.
    */
      void
      u2_pl_boot(u2_ray wir_r);
