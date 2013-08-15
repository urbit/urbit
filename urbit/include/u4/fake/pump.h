/* fake/pump.h
**
** This file is in the public domain.
*/
  /** Functions.
  **/
    /* u4_pump_prep():
    **
    **   Convert (noun) to a prep, which is
    **
    **      (text)
    **    | (.glue *prep)
    **    | (.nail *prep)
    */
      u4_prep
      u4_pump_prep(u4_lane lane,
                   u4_noun noun);

    /* u4_pump_dump():
    **
    **   Convert (prep) to a dump, printing (cols) wide.
    */
      u4_dump
      u4_pump_dump(u4_lane lane,
                   u4_atom cols,
                   u4_prep prep);

    /* u4_prep_decimal():
    **
    **   Prep a decimal value.
    */
      u4_prep
      u4_prep_decimal(u4_lane lane,
                      u4_atom atom);

    /* u4_prep_heximal():
    **
    **   Prep a heximal value, with 0x.
    */
      u4_prep
      u4_prep_heximal(u4_lane lane,
                      u4_atom atom);

    /* u4_prep_hexinal():
    **
    **   Prep a heximal value, without 0x.
    */
      u4_prep
      u4_prep_hexinal(u4_lane lane,
                      u4_atom atom);

    /* u4_prep_textual():
    **
    **   Prep with a text bias; fall back to hex.
    */
      u4_prep
      u4_prep_textual(u4_lane lane,
                      u4_atom atom);

    /* u4_prep_close():
    **
    **   Prep a list of preps, in (xb_a, xb_b).
    */
      u4_prep
      u4_prep_close(u4_lane lane,
                    u4_xb   xb_a,
                    u4_xb   xb_b,
                    u4_log  gah);
 
    /* u4_bug():
    **
    **   Print (nopt) with (caption).
    */
      void
      u4_bug(const u4_cl *cl_caption,
             u4_nopt     noun);

    /* u4_err():
    **
    **   Print (nopt) with (caption), using (lane).
    */
      void
      u4_err(u4_lane     lane,
             const u4_cl *cl_caption,
             u4_nopt     noun);

    /* u4_burp():
    **
    **   Print (prep) with (caption), using (lane).
    */
      void
      u4_burp(u4_lane     lane,
              const u4_cl *cl_caption,
              u4_prep     prep);
