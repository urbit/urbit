/* include/funj.h
**
** This file is in the public domain.
*/
  /** Data structures.
  **/
    /** Various nouns.
    **/
      typedef u2_noun u2_bank;
      typedef u2_noun u2_cord;
      typedef u2_noun u2_door;
      typedef u2_noun u2_gene;
      typedef u2_noun u2_home;
      typedef u2_noun u2_init;
      typedef u2_noun u2_menu;
      typedef u2_noun u2_plan;
      typedef u2_noun u2_plot;
      typedef u2_noun u2_prop;
      typedef u2_noun u2_rack;
      typedef u2_noun u2_rung;
      typedef u2_noun u2_rope;
      typedef u2_noun u2_spec;
      typedef u2_noun u2_tack;
      typedef u2_noun u2_type;
      typedef u2_noun u2_prep;
      typedef u2_noun u2_dump;

  /** Functions.
  **/
    /** Miscellaneous operators - all old.
    **/
      /* u2_fj_op_add():
      **
      **   Produce the sum of (a) and (b).
      */
        u2_atom
        u2_fj_op_add(u2_wire wir_r,
                     u2_atom a,
                     u2_atom b);

      /* u2_fj_op_con():
      **
      **   Produce (a | b).
      */
        u2_atom
        u2_fj_op_con(u2_wire wir_r,
                     u2_atom a,
                     u2_atom b);

      /* u2_fj_op_dec():
      **
      **   Produce (atom - 1), or bull if (atom) is 0.
      */
        u2_weak
        u2_fj_op_dec(u2_wire wir_r,
                     u2_atom atom);

      /* u2_fj_op_div():
      **
      **   Produce (b / a), or bull if (a) is 0.
      */
        u2_weak
        u2_fj_op_div(u2_wire wir_r,
                     u2_atom a,
                     u2_atom b);

      /* u2_fj_op_glu():
      **
      **   Concatenate atomic strings `a` and `b`.
      */
        u2_atom
        u2_fj_op_glu(u2_wire wir_r,
                     u2_atom a,
                     u2_atom b);

      /* u2_fj_op_inc():
      **
      **   Produce (atom + 1).
      */
        u2_atom
        u2_fj_op_inc(u2_wire wir_r,
                     u2_atom atom);

      /* u2_fj_op_log():
      **
      **   Produce the lowest m_log such that (1 << m_log) > m.
      */
        u2_atom
        u2_fj_op_log(u2_wire wir_r,
                     u2_atom atom);

      /* u2_fj_op_lsh():
      **
      **   Produce (b << a).
      */
        u2_atom
        u2_fj_op_lsh(u2_wire wir_r,
                     u2_atom a,
                     u2_atom b);

      /* u2_fj_op_peg():
      **
      **   Concatenate (twig_a) above (twig_b).
      */
        u2_atom
        u2_fj_op_peg(u2_wire wir_r,
                     u2_atom twig_a,
                     u2_atom twig_b);

      /* u2_fj_op_rsh():
      **
      **   Produce (b >> a).
      */
        u2_atom
        u2_fj_op_rsh(u2_wire wir_r,
                     u2_atom a,
                     u2_atom b);

      /* u2_fj_op_sub():
      **
      **   Produce (b - a), or bull if (a > b).
      */
        u2_weak
        u2_fj_op_sub(u2_wire wir_r,
                     u2_atom a,
                     u2_atom b);

      /* u2_fj_op_tip():
      **
      **   Produce the root of (twig) - 2 or 3; o4 bull if (twig) is 1.
      */
        u2_weak
        u2_fj_op_tip(u2_atom twig);

      /* u2_fj_op_tap():
      **
      **   Produce (twig) with the root bit removed, or bull if (twig) is 1.
      */
        u2_weak
        u2_fj_op_tap(u2_wire wir_r,
                     u2_atom twig);

    /** Lists.
    **/
      /* u2_fj_list_cat(): concatenate list.
      */
        u2_list
        u2_fj_list_cat(u2_wire wir_r,
                       u2_list lit,
                       u2_list lus);

      /* u2_fj_list_flip(): invert list.
      */
        u2_list
        u2_fj_list_flip(u2_wire wir_r,
                        u2_list lit);

      /* u2_fj_list_len(): length of list.
      */
        c3_w
        u2_fj_list_len(u2_wire wir_r,
                       u2_list lit);

    /** Pools (sets).
    **/
      /* u2_fj_pool_ok(): sanity test for pool.
      */
        c3_t
        u2_fj_pool_ok(u2_pool pool);

      /* u2_fj_pool_in():
      **
      **   Return 1 iff (pig) is in (pool).
      */
        c3_t
        u2_fj_pool_in(u2_noun pig,
                      u2_pool pool);

      /* u2_fj_pool_add():
      **
      **   Produce a version of (pool_sub) which includes (pig).
      */
        u2_noun
        u2_fj_pool_add(u2_wire wir_r,
                       u2_noun pig,
                       u2_pool pool_sub);

      /* u2_fj_pool_list():
      **
      **   Convert (pool) to a pseudo-randomly sorted list,
      **   prepending to (list).
      */
        u2_list
        u2_fj_pool_list(u2_wire wir_r,
                        u2_list list,
                        u2_pool pool);

      /* u2_fj_pool_cat():
      **
      **   Produce a version of (pool_b) which includes all entries
      **   in (pool_a).
      */
        u2_noun
        u2_fj_pool_cat(u2_wire wir_r,
                       u2_pool pool_a,
                       u2_pool pool_b);

      /* u2_fj_pool_at():
      **
      **   Return path to node of (pig) in (pool), under (axe); or 0.
      */
        u2_atom
        u2_fj_pool_at(u2_wire wir_r,
                      u2_noun  pig_in,
                      u2_atom axe,
                      u2_pool  pool);

    /** Books (associative arrays).
    **/
      /* u2_fj_book_is():
      **
      **   Return 1 iff (noun) is a book.
      */
        c3_t
        u2_fj_book_is(u2_noun book);

      /* u2_fj_book_in():
      **
      **   Return 1 iff (tag_in) is in (book).
      */
        c3_t
        u2_fj_book_in(u2_noun tag_in,
                      u2_book  book);

      /* u2_fj_book_get():
      **
      **   Produce the dog in (book) matching (tag_get), or u2_none.
      */
        u2_weak
        u2_fj_book_get(u2_noun tag_get,
                       u2_book  book);

      /* u2_fj_book_add():
      **
      **   Produce a new book which adds (tag_add dog_add) to (book).
      **   Replace old dog, if any.
      */
        u2_book
        u2_fj_book_add(u2_wire wir_r,
                       u2_noun tag_add,
                       u2_noun dog_add,
                       u2_book  book);

      /* u2_fj_book_add_list():
      **
      **   Produce a new book which adds all (tag dog) cells in
      **   (list) to (book).  Replace old dog, if any.
      */
        u2_book
        u2_fj_book_add_list(u2_wire wir_r,
                            u2_list  list,
                            u2_book  book);

      /* u2_fj_book_list():
      **
      **   Convert (book) to a pseudo-randomly sorted list of (tag dog)
      **   cells, prepending to (list).
      */
        u2_list
        u2_fj_book_list(u2_wire wir_r,
                        u2_list list,
                        u2_book book);

    /** Parsing.
    **/
      /* u2_fj_watt():
      **
      **   Convert `zar`, a text atom,  to a gene.
      */
        u2_gene
        u2_fj_watt(u2_wire wir_r,
                   u2_noun zar);

    /** Plow (Watt compilation).
    **/
      /* u2_fj_plow_make():
      **
      **   Not verifying type correctness, compile to formula.
      */
        u2_noun
        u2_fj_plow_make(u2_wire wir_r,
                        u2_type sut,
                        u2_gene gen);

      /* u2_fj_plow_play():
      **
      **   Not verifying type correctness, infer product type.
      */
        u2_noun
        u2_fj_plow_play(u2_wire wir_r,
                        u2_type sut,
                        u2_gene gen);

      /* u2_fj_plow_show():
      **
      **   Verify type correctness.
      */
        u2_noun
        u2_fj_plow_show(u2_wire wir_r,
                        u2_type sut,
                        u2_gene gen);

      /* u2_fj_plow_pass():
      **
      **   Verifying type correctness, compile to formula.
      */
        u2_noun
        u2_fj_plow_pass(u2_wire wir_r,
                        u2_type sut,
                        u2_gene gen);

      /* u2_fj_plow_shop():
      **
      **   Verifying type correctness, infer product type.
      */
        u2_noun
        u2_fj_plow_shop(u2_wire wir_r,
                        u2_type sut,
                        u2_gene gen);

      /* u2_fj_plow_wish():
      **
      **   Not verifying type correctness, compile and infer.
      */
        u2_noun
        u2_fj_plow_wish(u2_wire wir_r,
                        u2_type sut,
                        u2_gene gen);

      /* u2_fj_plow_mill():
      **
      **   Verifying type correctness, compile and infer.
      */
        u2_noun
        u2_fj_plow_mill(u2_wire wir_r,
                        u2_type sut,
                        u2_gene gen);

    /** Prep and pump (prettyprinting).
    **/
      /* u2_fj_prep_textual():
      **
      **   Prep with a text bias; fall back to decimal.
      */
        u2_prep
        u2_fj_prep_textual(u2_wire wir_r,
                           u2_atom atom);

      /* u2_fj_prep_decimal():
      **
      **   Prep a decimal value.
      */
        u2_prep
        u2_fj_prep_decimal(u2_wire wir_r,
                           u2_atom atom);

      /* u2_fj_prep_heximal():
      **
      **   Prep a hexadecimal value, with 0x.
      */
        u2_prep
        u2_fj_prep_heximal(u2_wire wir_r,
                           u2_atom atom);

      /* u2_fj_prep_hexinal():
      **
      **   Prep a heximal value, without 0x.
      */
        u2_prep
        u2_fj_prep_hexinal(u2_wire wir_r,
                           u2_atom atom);

      /* u2_fj_prep_noun():
      **
      **   Convert (noun) to a prep, which is
      **
      **      (text)
      **    | (.glue *prep)
      **    | (.nail *prep)
      */
        u2_prep
        u2_fj_prep_noun(u2_wire wir_r,
                        u2_noun noun);

      /* u2_fj_prep_close():
      **
      **   Prep a list of preps, in (xb_a, xb_b).
      */
        u2_prep
        u2_fj_prep_close(u2_wire wir_r,
                         c3_y    xb_a,
                         c3_y    xb_b,
                         u2_list gah);

      /* u2_fj_pump_dump():
      **
      **   Convert (prep) to a dump, printing (cols) wide.
      */
        u2_dump
        u2_fj_pump_dump(u2_wire wir_r,
                        c3_w    xw_cols,
                        u2_prep prep);

      /* u2_err():
      **
      **   Print (nopt) with (caption), using (wir).
      */
        void
        u2_err(u2_wire     wir_r,
               const c3_c* cl_caption,
               u2_weak     noun);

      /* u2_burp():
      **
      **   Print (prep) with (caption), using (wir).
      */
        void
        u2_burp(u2_wire     wir_r,
                const c3_c* cl_caption,
                u2_prep     prep);
