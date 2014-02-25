/* include/coal.h
**
** This file is in the public domain.
*/
  /** coal.h: common noun substrate.
  ***
  *** Generally modeled on stage 223, but ideally isolated from it.
  *** Currently depends for implementation on direct-linked jets.
  ***
  *** Functions of the same name and tier may vary subtly from 
  *** kernel semantics.  Be wary and read the header comment.
  ***
  *** Most coal functions depend on u2_Wire, a thread-local global,
  *** which addresses all memory and other thread state.  This in
  *** turn depends on u2_System.
  **/
 
  /** Globals.
  **/
    c3_global u2_ray u2_Wire;                 //  __thread or equivalent


  /** Aliases - selective and syntactically unique.
  **/
#   define u2h(som)          u2_cx_h(som)
#   define u2t(som)          u2_cx_t(som)
#   define u2at(axe, som)    u2_cx_at(axe, som)

#   define u2nc(a, b)        u2_cn_cell(a, b)
#   define u2nt(a, b, c)     u2_cn_trel(a, b, c)
#   define u2nq(a, b, c, d)  u2_cn_qual(a, b, c, d)
#   define u2ni(a)           u2ic(a, 0)

#   define u2fh(som)         (*u2_at_pom_hed(som))      //  no validity check
#   define u2ft(som)         (*u2_at_pom_tel(som))      //  no validity check

#   define u2du(som)         (u2_cr_du(som))
#   define u2ud(som)         (u2_cr_ud(som))

#   define u2k(som)          u2_ct(som)
#   define u2z(som)          u2_cz(som)


  /** u2_cx*: crash-only core traversal.  
  ***
  ***    unless otherwise noted, u2_cx callers *retain* ownership of 
  ***    all argument nouns and pointers, and *preserve* ownership of
  ***    all results.
  ***
  ***    u2_cx functions always bail with c3__exit.
  **/
#if 1
#     define u2_cx_h(som)               u2_bi_h(u2_Wire, som)
#     define u2_cx_t(som)               u2_bi_t(u2_Wire, som)
#     define u2_cx_at(axe, som)         u2_bi_frag(u2_Wire, axe, som)
#     define u2_cx_cell(a, b, c)        u2_bi_cell(u2_Wire, a, b, c)
#     define u2_cx_trel(a, b, c, d)     u2_bi_trel(u2_Wire, a, b, c, d)
#     define u2_cx_qual(a, b, c, d, e)  u2_bi_qual(u2_Wire, a, b, c, d, e)
#else
    /* u2_cx_h (u2h): head.
    */
      u2_noun 
      u2_cx_h(u2_noun som);

    /* u2_cx_t (u2t): tail.
    */
      u2_noun
      u2_cx_t(u2_noun som);

    /* u2_cx_at (u2at): fragment.
    */
      u2_noun
      u2_cx_at(u2_noun axe, u2_noun som);

    /* u2_cx_cell():
    **
    **   Divide `a` as a cell `[b c]`.
    */
      void
      u2_cx_cell(u2_noun  a,
                 u2_noun* b,
                 u2_noun* c);

    /* u2_cx_trel():
    **
    **   Divide `a` as a trel `[b c d]`, or bail.
    */
      void
      u2_cx_trel(u2_noun  a,
                 u2_noun* b,
                 u2_noun* c,
                 u2_noun* d);

    /* u2_cx_qual():
    **
    **   Divide `a` as a quadruple `[b c d e]`.
    */
      void
      u2_cx_qual(u2_noun  a,
                 u2_noun* b,
                 u2_noun* c,
                 u2_noun* d,
                 u2_noun* e);
    
#endif

  /** u2_cr*: crash-free core traversal.
  ***
  ***    unless otherwise noted, u2_cr callers *retain* ownership of 
  ***    all argument nouns and pointers, and *preserve* ownership of
  ***    all results.
  ***
  ***    u2_cr functions never bail (throw an exception).
  **/
    //  XX: bound by cpp to the older, underlying loom layer.
    //
#if 1
#       define u2_cr_du(a)                     u2_dust(a)
#       define u2_cr_ud(a)                     u2_stud(a)
#       define u2_cr_at(a, b)                  u2_frag(a, b)
#       define u2_cr_mean                      u2_mean
#       define u2_cr_mug(a)                    u2_mug(a)
#       define u2_cr_mug_string(a)             u2_mug_string(a)
#       define u2_cr_mug_words(a, b)           u2_mug_words(a, b)
#       define u2_cr_mug_cell(a, b)            u2_mug_cell(a, b)
#       define u2_cr_mug_trel(a, b, c)         u2_mug_cell(a, b, c)
#       define u2_cr_mug_qual(a, b, c, d)      u2_mug_qual(a, b, c, d)
#       define u2_cr_mug_both(a, b)            u2_mug_both(a, b)
#       define u2_cr_fing(a, b)                u2_fing(a, b)
#       define u2_cr_fing_c(a_c, b)            u2_fing_c(a_c, b)
#       define u2_cr_fing_cell(p, q, b)        u2_fing_cell(p, q, b)
#       define u2_cr_fing_mixt(p_c, q, b)      u2_fing_mixt(p_c, q, b)
#       define u2_cr_fing_trel(p, q, r, b)     u2_fing_trel(p, q, r, b)
#       define u2_cr_fing_qual(p, q, r, s, b)  u2_fing_qual(p, q, r, s, b)
#       define u2_cr_sing(a, b)                u2_sing(a, b)
#       define u2_cr_sing_c(a_c, b)            u2_sing_c(a_c, b)
#       define u2_cr_sing_cell(p, q, b)        u2_sing_cell(p, q, b)
#       define u2_cr_sing_mixt(p_c, q, b)      u2_sing_mixt(p_c, q, b)
#       define u2_cr_sing_trel(p, q, r, b)     u2_sing_trel(p, q, r, b)
#       define u2_cr_sing_qual(p, q, r, s, b)  u2_sing_qual(p, q, r, s, b)
#       define u2_cr_nord(a, b)                u2_as_nord(a, b)
#       define u2_cr_bush(a, b, c)             u2_as_bush(a, b, c)
#       define u2_cr_cell(a, b, c)             u2_as_cell(a, b, c)
#       define u2_cr_trel(a, b, c, d)          u2_as_trel(a, b, c, d)
#       define u2_cr_qual(a, b, c, d, e)       u2_as_qual(a, b, c, d, e)
#       define u2_cr_p(a, b, c)                u2_as_p(a, b, c)
#       define u2_cr_pq(a, b, c, d)            u2_as_pq(a, b, c, d)
#       define u2_cr_pqr(a, b, c, d, e)        u2_as_pqr(a, b, c, d, e)
#       define u2_cr_pqrs(a, b, c, d, e, f)    u2_as_pqrs(a, b, c, d, e, f)
#       define u2_cr_pqrs(a, b, c, d, e, f)    u2_as_pqrs(a, b, c, d, e, f)
#       define u2_cr_met(a_y, b)               u2_met(a_y, b)
#       define u2_cr_bit(a_w, b)               u2_bit(a_w, b) 
#       define u2_cr_byte(a_w, b)              u2_byte(a_w, b)
#       define u2_cr_bytes(a_w, b_w, c_y, d)   u2_bytes(a_w, b_w, c_y, d)
#       define u2_cr_chop(m, f, w, t, d, s)    u2_chop(m, f, w, t, d, s)
#       define u2_cr_mp(a_mp, b)               u2_mp(a_mp, b)
#       define u2_cr_word(a_w, b)              u2_word(a_w, b)
#       define u2_cr_chub(a_w, b)              u2_chub(a_w, b)
#       define u2_cr_words(a_w, b_w, c_w, d)   u2_words(a_w, b_w, c_w, d)
#else
      /* u2_cr_du(): u2_yes iff `a` is cell.
      */
        u2_bean
        u2_cr_du(u2_noun a);

      /* u2_cr_ud(): u2_no iff `a` is cell.
      */
        u2_bean
        u2_cr_ud(u2_noun a);

      /* u2_cr_at(): fragment `a` of `b`, or none.
      */
        u2_weak
        u2_cr_at(u2_atom a, 
                 u2_weak b);

      /* u2_cr_mean():
      **
      **   Attempt to deconstruct `a` by axis, noun pairs; 0 terminates.
      **   Axes must be sorted in tree order.
      */
        u2_bean
        u2_cr_mean(u2_noun a,
                   ...);

      /* u2_cr_mug():
      **
      **   Compute and/or recall the mug (31-bit hash) of (a).
      */
        c3_w
        u2_cr_mug(u2_noun a);
    
      /* u2_cr_mug_string():
      **
      **   Compute the mug of `a`, LSB first.
      */
        c3_w
        u2_cr_mug_string(const c3_c *a_c);

      /* u2_cr_mug_words():
      **
      **   Compute the mug of `buf`, `len`, LSW first.
      */
        c3_w
        u2_cr_mug_words(const c3_w *buf_w,
                        c3_w        len_w);

      /* u2_cr_mug_cell():
      **
      **   Compute the mug of `[a b]`.
      */
        c3_w
        u2_cr_mug_cell(u2_noun a,
                       u2_noun b);

      /* u2_cr_mug_trel():
      **
      **   Compute the mug of `[a b c]`.
      */
        c3_w
        u2_cr_mug_trel(u2_noun a,
                       u2_noun b,
                       u2_noun c);

      /* u2_cr_mug_qual():
      **
      **   Compute the mug of `[a b c d]`.
      */
        c3_w
        u2_cr_mug_qual(u2_noun a,
                       u2_noun b,
                       u2_noun c,
                       u2_noun d);

      /* u2_cr_mug_both():
      **
      **   Join two mugs.
      */
        c3_w
        u2_cr_mug_both(c3_w a_w,
                       c3_w b_w);


      /* u2_cr_fing():
      **
      **   Yes iff (a) and (b) are the same copy of the same noun.
      **   (Ie, by pointer equality - u2_cr_sing with false negatives.)
      */
        u2_bean
        u2_cr_fing(u2_noun a,
                   u2_noun b);

      /* u2_cr_fing_c():
      **
      **   Yes iff (b) is the same copy of the same noun as the C string [a].
      */
        u2_bean
        u2_cr_fing_c(const c3_c* a_c,
                     u2_noun     b);

      /* u2_cr_fing_cell():
      **
      **   Yes iff `[p q]` and `b` are the same copy of the same noun.
      */
        u2_bean
        u2_cr_fing_cell(u2_noun p,
                        u2_noun q,
                        u2_noun b);

      /* u2_cr_fing_mixt():
      **
      **   Yes iff `[p q]` and `b` are the same copy of the same noun.
      */
        u2_bean
        u2_cr_fing_mixt(const c3_c* p_c,
                        u2_noun     q,
                        u2_noun     b);

      /* u2_cr_fing_trel():
      **
      **   Yes iff `[p q r]` and `b` are the same copy of the same noun.
      */
        u2_bean
        u2_cr_fing_trel(u2_noun p,
                        u2_noun q,
                        u2_noun r,
                        u2_noun b);

      /* u2_cr_fing_qual():
      **
      **   Yes iff `[p q r s]` and `b` are the same copy of the same noun.
      */
        u2_bean
        u2_cr_fing_qual(u2_noun p,
                        u2_noun q,
                        u2_noun r,
                        u2_noun s,
                        u2_noun b);

      /* u2_cr_sing():
      **
      **   Yes iff (a) and (b) are the same noun.
      */
        u2_bean
        u2_cr_sing(u2_noun a,
                   u2_noun b);

      /* u2_cr_sing_c():
      **
      **   Yes iff (b) is the same noun as the C string [a].
      */
        u2_bean
        u2_cr_sing_c(const c3_c* a_c,
                     u2_noun     b);

      /* u2_cr_sing_cell():
      **
      **   Yes iff `[p q]` and `b` are the same noun.
      */
        u2_bean
        u2_cr_sing_cell(u2_noun p,
                        u2_noun q,
                        u2_noun b);

      /* u2_cr_sing_mixt():
      **
      **   Yes iff `[p q]` and `b` are the same noun.
      */
        u2_bean
        u2_cr_sing_mixt(const c3_c* p_c,
                        u2_noun     q,
                        u2_noun     b);

      /* u2_cr_sing_trel():
      **
      **   Yes iff `[p q r]` and `b` are the same noun.
      */
        u2_bean
        u2_cr_sing_trel(u2_noun p,
                        u2_noun q,
                        u2_noun r,
                        u2_noun b);

      /* u2_cr_sing_qual():
      **
      **   Yes iff `[p q r s]` and `b` are the same noun.
      */
        u2_bean
        u2_cr_sing_qual(u2_noun p,
                        u2_noun q,
                        u2_noun r,
                        u2_noun s,
                        u2_noun b);

      /* u2_cr_nord():
      **
      **   Return 0, 1 or 2 if `a` is below, equal to, or above `b`.
      */
        u2_atom
        u2_cr_nord(u2_noun a,
                   u2_noun b);

      /* u2_cr_mold():
      **
      **   Divide `a` as a mold `[b.[p q] c]`.
      */
        u2_bean
        u2_cr_mold(u2_noun  a,
                   u2_noun* b,
                   u2_noun* c);

      /* u2_cr_cell():
      **
      **   Divide `a` as a cell `[b c]`.
      */
        u2_bean
        u2_cr_cell(u2_noun  a,
                   u2_noun* b,
                   u2_noun* c);

      /* u2_cr_trel():
      **
      **   Divide `a` as a trel `[b c]`.
      */
        u2_bean
        u2_cr_trel(u2_noun  a,
                   u2_noun* b,
                   u2_noun* c,
                   u2_noun* d);

      /* u2_cr_qual():
      **
      **   Divide (a) as a qual (b c d e).
      */
        u2_bean
        u2_cr_qual(u2_noun  a,
                   u2_noun* b,
                   u2_noun* c,
                   u2_noun* d,
                   u2_noun* e);

      /* u2_cr_p():
      **
      **   & [0] if [a] is of the form [b *c].
      */
        u2_bean
        u2_cr_p(u2_noun  a,
                u2_noun  b,
                u2_noun* c);

      /* u2_cr_pq():
      **
      **   & [0] if [a] is of the form [b *c d].
      */
        u2_bean
        u2_cr_pq(u2_noun  a,
                 u2_noun  b,
                 u2_noun* c,
                 u2_noun* d);

      /* u2_cr_pqr():
      **
      **   & [0] if [a] is of the form [b *c *d *e].
      */
        u2_bean
        u2_cr_pqr(u2_noun  a,
                  u2_noun  b,
                  u2_noun* c,
                  u2_noun* d,
                  u2_noun* e);

      /* u2_cr_pqrs():
      **
      **   & [0] if [a] is of the form [b *c *d *e *f].
      */
        u2_bean
        u2_cr_pqrs(u2_noun  a,
                   u2_noun  b,
                   u2_noun* c,
                   u2_noun* d,
                   u2_noun* e,
                   u2_noun* f);

      /* u2_cr_met(): 
      **
      **   Return the size of (b) in bits, rounded up to
      **   (1 << a_y). 
      **
      **   For example, (a_y == 3) returns the size in bytes.
      */
        c3_w
        u2_cr_met(c3_y    a_y,
                  u2_atom b);

      /* u2_cr_bit():
      **
      **   Return bit (a_w) of (b).
      */
        c3_b
        u2_cr_bit(c3_w    a_w,
                  u2_atom b);
     
      /* u2_cr_byte():
      **
      **   Return byte (a_w) of (b).
      */
        c3_y
        u2_cr_byte(c3_w    a_w,
                   u2_atom b);
                
      /* u2_cr_bytes():
      **
      **   Copy bytes (a_w) through (a_w + b_w - 1) from (d) to (c).
      */
        void
        u2_cr_bytes(c3_w    a_w,
                    c3_w    b_w,
                    c3_y*   c_y,
                    u2_atom d);

      /* u2_cr_chop():
      **
      **   Into the bloq space of `met`, from position `fum` for a
      **   span of `wid`, to position `tou`, XOR from atom `src`
      **   into ray `dst`.
      */
        void
        u2_cr_chop(c3_g    met_g,
                   c3_w    fum_w,
                   c3_w    wid_w,
                   c3_w    tou_w,
                   u2_ray  dst_r,
                   u2_atom src);

      /* u2_cr_mp():
      **
      **   Copy (b) into (a_mp).
      */
        void
        u2_cr_mp(mpz_t   a_mp,
                 u2_atom b);

      /* u2_cr_word():
      **
      **   Return word (a_w) of (b).
      */
        c3_w
        u2_cr_word(c3_w    a_w,
                   u2_atom b);

      /* u2_cr_words():
      **
      **  Copy words (a_w) through (a_w + b_w - 1) from (d) to (c).
      */
        void
        u2_cr_words(c3_w    a_w,
                    c3_w    b_w,
                    c3_w*   c_w,
                    u2_atom d);
#endif

      /* u2_cr_string(): `a`, a text atom, as malloced C string.
      */
        c3_c* 
        u2_cr_string(u2_atom a);

      /* u2_cr_tape(): `a`, a list of bytes, as malloced C string.
      */
        c3_y* 
        u2_cr_tape(u2_noun a);



  /** u2_c?: managing noun reference counts.
  **/
#if 1
#     define u2_ct(som) u2_rx(u2_Wire, som)
#     define u2_cz(som) u2_rz(u2_Wire, som)
#     define u2_co(som) u2_rl_senior(u2_Wire)
#     define u2_cy(som) u2_rl_junior(u2_Wire)
#else
    /* u2_cz(): release noun.  Return `som` for convenience.
    */
      u2_noun
      u2_cz(u2_noun som);

    /* u2_ct(): acquire noun.  Return `som` for convenience.
    */
      u2_noun
      u2_ct(u2_noun som);

#endif
    /* u2_cu(): yes iff `som` is mutable where transferred (refcount 1).
    */
      u2_bean
      u2_cu(u2_noun som);


  /** u2_ci*, u2_cf*: importing and constructing nouns.
  ***
  ***    Unless otherwise noted, u2_ci/f callers *transfer* ownership of
  ***    all argument nouns, and *retain* ownership of argument pointers.
  ***    Callers *produce* a new result which the caller must release.
  **/
    /* u2_ci_mp(): construct atom from GMP.  Caller transfers a_mp.
    */
      u2_atom
      u2_ci_mp(mpz_t a_mp);

    /* u2_ci_bytes():
    **
    **   Construct `a` bytes from `b`, LSB first, as an atom.
    */
      u2_atom
      u2_ci_bytes(c3_w        a_w,
                  const c3_y* b_y);

    /* u2_ci_words():
    **
    **   Construct `a` words from `b`, LSW first, as an atom.
    */
      u2_atom
      u2_ci_words(c3_w        a_w,
                  const c3_w* b_w);

    /* u2_ci_chubs():
    **
    **   Construct `a` double-words from `b`, LSD first, as an atom.
    */
      u2_atom
      u2_ci_chubs(c3_w        a_w,
                  const c3_d* b_d);

    /* u2_ci_string():
    **
    **   u2_ci_bytes(strlen(a_c), (c3_y *)a_c);
    */
      u2_atom
      u2_ci_string(const c3_c* a_c);

    /* u2_ci_tape(): from a C string, to a list of bytes.
    */
      u2_atom
      u2_ci_tape(const c3_c* txt_c);

    /* u2_ci_duck():
    **
    **   Create an atomic string from a list of bytes.
    */
      u2_noun 
      u2_ci_duck(u2_noun a);

    /* u2_ci_decimal():
    **
    **   Parse `a` as a list of decimal digits.
    */
      u2_atom
      u2_ci_decimal(u2_noun a);

    /* u2_ci_heximal():
    **
    **   Parse `a` as a list of hex digits.
    */
      u2_noun
      u2_ci_heximal(u2_noun a);

    /* u2_ci_list():
    **
    **   Generate a null-terminated list, with `u2_none` as terminator.
    */
      u2_noun
      u2_ci_list(u2_weak one, ...);


  /** u2_cf*: import and export to general unix filesystem.
  ***
  ***   All arguments transferred.
  ***
  ***   All paths are flat LSB-first strings including extension.
  ***   File modes:
  ***
  ***         c3__atom  LSB-first atom
  ***         c3__file  [length atom] pair
  ***         c3__line  list of atomic line
  ***
  ***   Directory modes:
  ***
  ***         c3__seq   sequence, by number - [len list] pair
  ***         c3__set   set, by hash
  ***         c3__map   key-value pairs, by hash of key
  ***         c3__nam   term-value pairs
  ***
  ***   Modes nest right, as in `[%map %file]`.
  **/
    /* u2_cf_flat_save(): save `som` as `mod` at `pas`.
    */
      u2_bean
      u2_cf_flat_save(u2_noun mod, 
                      u2_noun pas,
                      u2_noun som);

    /* u2_cf_flat_load(): load `mod` at `pas`.  Bail on error.
    */
      u2_weak
      u2_cf_flat_load(u2_noun mod,
                      u2_noun pas);

    /* u2_cf_flat_date(): date for `pas`.  Unix time * 10^6, or 0.
    */
      c3_d
      u2_cf_flat_date(u2_noun pas);

    /* u2_cf_list(): list all the files in directory `pas`.  List of cask.
    */
      u2_noun 
      u2_cf_list(u2_noun pas);

    /* u2_cf_path(): assemble local path with noun thap and ext.
    */
      u2_noun
      u2_cf_path(c3_c* top_c, 
                 c3_c* ext_c, 
                 u2_noun tah);
    

  /** u2_cn_*: natural constructors.
  ***
  ***   All argument nouns are transferred.
  **/
    /* u2_cn_inc(): increment an atom.
    */
      u2_atom
      u2_cn_inc(u2_atom a);

    /* u2_cn_dec(): decrement an atom.
    */
      u2_atom
      u2_cn_dec(u2_atom a);

    /* u2_cn_cell(): produce the cell `[a b]`.
    */
      u2_noun
      u2_cn_cell(u2_noun a,
                 u2_noun b);

    /* u2_cn_trel():
    */
      u2_noun
      u2_cn_trel(u2_noun a,
                 u2_noun b,
                 u2_noun c);

    /* u2_cn_qual():
    */
      u2_noun
      u2_cn_qual(u2_noun a,
                 u2_noun b,
                 u2_noun c,
                 u2_noun d);

    /* u2_cn_nock():
    **
    **   Nock or bail.
    */
      u2_noun
      u2_cn_nock(u2_noun bus,
                 u2_noun fol);

    /* u2_cn_mung():
    **
    **   Call `(function sample)`.
    */
      u2_noun
      u2_cn_mung(u2_noun fun,
                 u2_noun sam);

    /* u2_cn_mink(): logical virtual nock.
    */
      u2_noun
      u2_cn_mink(u2_noun bus,
                 u2_noun fol,
                 u2_noun fly);

    /* u2_cn_moch(): blind mink with empty fly.
    */
      u2_noun
      u2_cn_moch(u2_noun bus,
                 u2_noun fol);
   
  /** u2_cs_*: general-purpose internal hash tables 
  **/


  /** u2_cg_*: garbage collection (exception cleanup only)
  **/
    

  /** u2_cc_*: memoization / caching
  **/
    /* u2_cc_find():
    **
    **   Cache search for function (0 means nock) and sample.
    */
      u2_weak                                      
      u2_cc_find(u2_mote fun_m,
                 u2_noun sam);                     

    /* u2_cc_save():
    **
    **   Cache store for function (0 means nock), sample and product.
    */
      u2_weak                                      
      u2_cc_save(u2_mote fun_m,                    
                 u2_noun sam,                      
                 u2_noun pro);                     

    /* u2_cc_uniq():
    **
    **   Use cache to render `som` unique.
    */
      u2_noun
      u2_cc_uniq(u2_noun som);

    /* u2_cc_find_cell(): as u2_cc_find(), for `sam=[a b]`.
    ** u2_cc_find_trel(): as u2_cc_find(), for `sam=[a b c]`.
    ** u2_cc_find_qual(): as u2_cc_find(), for `sam=[a b d]`.
    */
      u2_weak
      u2_cc_find_cell(u2_mote, u2_noun, u2_noun);

      u2_weak                                            
      u2_cc_find_trel(u2_mote, u2_noun, u2_noun, u2_noun);   

      u2_weak                                      
      u2_cc_find_qual(u2_mote, u2_noun, u2_noun, u2_noun, u2_noun);   

    /* u2_cc_save_cell(): as u2_cc_save(), for `sam=[a b]`.
    ** u2_cc_save_trel(): as u2_cc_save(), for `sam=[a b c]`.
    ** u2_cc_save_qual(): as u2_cc_save(), for `sam=[a b c d]`.
    */
      u2_weak                                      
      u2_cc_save_cell(u2_mote, u2_noun, u2_noun, u2_noun);   

      u2_weak                                      
      u2_cc_save_trel(u2_mote, u2_noun, u2_noun, u2_noun, u2_noun);   

      u2_weak                                      
      u2_cc_save_qual(u2_ray, u2_mote, u2_noun,     
                                       u2_noun,    
                                       u2_noun,    
                                       u2_noun,    
                                       u2_noun);   

  /** u2_cw: exporting, printing, saving or publishing nouns.
  **/

  /** u2_ch: external core activation.
  **/
    /* u2_ch_molt():
    **
    **   Mutate `som` with a 0-terminated list of axis, noun pairs.
    **   Axes must be cats (31 bit).
    */
      u2_noun
      u2_ch_molt(u2_noun som,
                 ...);
  
    /* u2_ch_molf():
    **
    **   As u2_ch_molt(), with argument pointer.
    */
      u2_noun
      u2_ch_molf(u2_noun som,
                 va_list vap);

    /* u2_ch_mong(): 
    **
    **   Call a function by gate and sample.
    */
      u2_noun
      u2_ch_mong(u2_noun gat,
                 u2_noun sam);

    /* u2_ch_hook():
    **
    **   Execute hook from core.
    */ 
      u2_noun
      u2_ch_hook(u2_noun     cor,
                 const c3_c* tam_c);

    /* u2_ch_cook():
    **
    **   Reverse hook as molt.
    */
      u2_noun
      u2_ch_cook(u2_wire     wir_r,
                 u2_noun     cor,
                 const c3_c* tam_c,
                 u2_noun     som);


  /** u2_cm: meta, miscellaneous, management and magic.
  **/
    /* u2_cm_boot(): try to boot the system and set one wire (single thread).
    **
    **   Return u2_no on failure.
    */
      u2_bean
      u2_cm_boot(void);

    /* u2_cm_rind(): open and produce a new jump buffer.
    */
      void*
      u2_cm_rind();

    /* u2_cm_trap(): trap for exceptions.
    */
#     if 0
        u2_noun
        u2_cm_trap();
#     else
        //  XX breaks for 64-bit nouns - use a tmp var
        //
#       define u2_cm_trap() (u2_noun)(setjmp(u2_cm_rind()))
#     endif

    /* u2_cm_done(): terminate trap.
    */
      void
      u2_cm_done();

    /* u2_cm_wail(): retrieve and reset local trace.
    */
      u2_noun
      u2_cm_wail();

    /* u2_cm_poll(): poll for interrupts, etc.
    */
      void
      u2_cm_poll();
   
    /* u2_cm_trip(): descend into a memory region.
    **
    **   Memory allocated in the heap above is senior & frozen.
    */
      void
      u2_cm_trip();

    /* u2_cm_chin(): ascend out of a memory region.
    **
    **   Memory allocated in the heap below is junior & volatile.
    */
      void
      u2_cm_chin();

    /* u2_cm_keep(): copy volatile noun `som` to fresh heap.
    */
      u2_noun
      u2_cm_keep(u2_noun som);

    /* u2_cm_bury(): store fresh or volatile noun `som` to freezer.
    */
      u2_noun
      u2_cm_bury(u2_noun som);

    /* u2_cm_ruby(): store fresh or volatile noun `som` to freezer - debug.
    */
      u2_noun
      u2_cm_ruby(u2_noun som);

    /* u2_cm_bail(): bail out to the local trap.  Does not return.
    ** 
    **  Bail structure:
    **
    **    %exit
    **    %intr
    **    %fail
    **    [%fail p=*term]
    **    [%need p=*(list path)]
    **
    **    c3__exit for normal exit
    **    c3__fail for abnormal failure 
    **
    **  When in doubt, fail.
    */
      u2_noun
      u2_cm_bail(c3_m how_m);

    /* u2_cm_bowl(): bail out with preset report.
    */
      u2_noun
      u2_cm_bowl(u2_noun how);                                    //  transfer

    /* u2_cm_pack(): elide all volatile memory.
    */
      void
      u2_cm_pack();

    /* u2_cm_prof(): begin profiling.
    */
      void
      u2_cm_prof();

    /* u2_cm_push(): push `mon` on trace stack.
    */
      void
      u2_cm_push(u2_noun mon);

    /* u2_cm_bean(): push `[%bean roc]` on trace stack.
    */
      void
      u2_cm_bean(u2_noun roc);

    /* u2_cm_foul():
    */
      u2_noun
      u2_cm_foul(const c3_c* err_c);

    /* u2_cm_sweep(): return bytes leaked; match bytes saved.
    */
      c3_w
      u2_cm_sweep(c3_w sav_w);

    /* u2_cm_purge(): purge memo cache.
    */
      void
      u2_cm_purge();

    /* u2_cm_mark_noun(): mark individual noun.
    */
      c3_w
      u2_cm_mark_noun(u2_noun som);

    /* u2_cm_mark_ptr(): mark a root for gc (do not use again before gc)
    */
      c3_w
      u2_cm_mark_ptr(void *ptr);

    /* u2_cm_mark_internal(): mark all coal internals
    */
      c3_w
      u2_cm_mark_internal();

    /* u2_cm_drop(): drop from meaning stack.
    */
      void
      u2_cm_drop();

    /* u2_cm_frop(): extract, clear and disable profiler.
    */
      u2_noun
      u2_cm_frop();

    /* u2_cm_slab():
    **
    **   Create a blank atomic slab of `len` words.
    */
      u2_ray
      u2_cm_slab(c3_w len_w);

    /* u2_cm_slaq():
    **
    **   Create a blank atomic slab of `len` bloqs of size `met`.
    */
      u2_ray
      u2_cm_slaq(c3_g met_g,
                 c3_w len_w);

    /* u2_cm_malt():
    **
    **   Initialize slab `sal` as an atom, self-measured.
    */
      u2_atom
      u2_cm_malt(u2_ray sal_r);

    /* u2_cm_mint():
    **
    **   Initialize slab `sal` as an atom, auto-measured.
    */
      u2_atom
      u2_cm_mint(u2_ray  sal_r,
                 c3_w    len_w);

    /* u2_cm_moot():
    **
    **   Initialize slab `sal` as an atom, originally measured.
    */
      u2_atom
      u2_cm_moot(u2_ray  sal_r);

    /* u2_cm_open():
    **
    **   Yes iff [a] more words remain in the pad.
    */
      u2_bean
      u2_cm_open(c3_w a_w);

    /* u2_cm_malloc():
    **
    **   Allocate `sib_w` *bytes* of raw C storage.
    */
      void*
      u2_cm_malloc(c3_w sib_w);

    /* u2_cm_ralloc():
    **
    **   Allocate `siz_w` words of raw ray storage.
    */
      u2_ray
      u2_cm_ralloc(c3_w siz_w);

    /* u2_cm_rfree():
    **
    **   Free raw ray storage allocated by `u2_cm_ralloc()`.
    */
      void
      u2_cm_rfree(u2_ray nov_r);

  /* u2_cp: profiling and debugging.
  **
  **   Profiling information is automatically cleared on each 
  */
    /* u2_cm_slab(): produce profiling record.
    */
      u2_noun
      u2_cm_slab();

  /* u2_ck: kernel and related functions
  **
  **   All nouns transferred unless otherwise stated.
  */
    /* u2_cka: tier 1 functions
    */
        u2_noun
        u2_cka_add(u2_noun a, u2_noun b);

        u2_noun
        u2_cka_sub(u2_noun a, u2_noun b);

        u2_noun
        u2_cka_mul(u2_noun a, u2_noun b);

        u2_noun
        u2_cka_gth(u2_noun a, u2_noun b);

        u2_bean
        u2_cka_lte(u2_noun a, u2_noun b);

    /* u2_ckb: tier 2 functions
    */
      /* u2_ckb_lent(): length of a list.
      */
        u2_noun
        u2_ckb_lent(u2_noun a);

      /* u2_ckb_weld(): concatenate list `a` before `b`.
      */
        u2_noun
        u2_ckb_weld(u2_noun a, u2_noun b);

      /* u2_ckb_flop(): reverse list `a`.
      */
        u2_noun
        u2_ckb_flop(u2_noun a);

    /* u2_ckc: tier 3 functions
    */
      /* u2_ckc_lsh(): left shift.
      */
        u2_noun
        u2_ckc_lsh(u2_noun a, u2_noun b, u2_noun c);

      /* u2_ckc_rsh(): right shift.
      */
        u2_noun
        u2_ckc_rsh(u2_noun a, u2_noun b, u2_noun c);

    /* u2_ckd: tier 4 functions
    */
      /* u2_ckd_by_get(): map get for key `b` in map `a` with u2_none.
      */
        u2_weak
        u2_ckd_by_get(u2_noun a, u2_noun b);
 
      /* u2_ckd_by_got(): map get for key `b` in map `a` with fail.
      */
        u2_noun
        u2_ckd_by_got(u2_noun a, u2_noun b);
 
      /* u2_ckd_by_put(): map put for key `b`, value `c` in map `a`.
      */
        u2_weak
        u2_ckd_by_put(u2_noun a, u2_noun b, u2_noun c);

      /* u2_ckd_by_has(): test for get.
      */
        u2_bean
        u2_ckd_by_has(u2_noun a, u2_noun b);

      /* u2_ckd_by_gas(): list to map.
      */
        u2_noun
        u2_ckd_by_gas(u2_noun a, u2_noun b);

      /* u2_ckd_in_gas(): list to map.
      */
        u2_noun
        u2_ckd_in_gas(u2_noun a, u2_noun b);

      /* u2_ckd_in_has(): test for presence.
      */
        u2_bean
        u2_ckd_in_has(u2_noun a, u2_noun b);

      /* u2_ckd_in_tap(): map/set convert to list.  (solves by_tap also.)
      */
        u2_noun
        u2_ckd_in_tap(u2_noun a, u2_noun b);

#define u2_ckd_by_tap(a, b) u2_ckd_in_tap(a, b)

    /* u2_cke: tier 5 functions
    */
      /* u2_cke_cue(): expand saved pill.
      */
        u2_noun
        u2_cke_cue(u2_atom a);

      /* u2_cke_jam(): pack noun as atom.
      */
        u2_atom
        u2_cke_jam(u2_noun a);

      /* u2_cke_trip: atom to tape.
      */
        u2_noun
        u2_cke_trip(u2_noun a);

    /* u2_ckf: tier 6 functions
    */
