/* include/fake/goof.h
**
** This file is in the public domain.
*/
  /** Functions.
  **/
    /* u4_goof_cook():
    **
    **   On (lane), prepare (atom) as a log of (line byte), stripping
    **   spaces and comments.
    */
      u4_log
      u4_goof_cook(u4_lane lane,
                   u4_atom atom);

    /* u4_goof_scan():
    **
    **   On (lane), scan (atom) as an ASCII LSB string in .goof format.
    */
      u4_noun
      u4_goof_scan(u4_lane lane,
                   u4_atom atom);

    /* u4_goof_scan_dogo():
    **
    **   On (lane), scan (atom) as an ASCII LSB string in .dogo format.
    */
      u4_noun
      u4_goof_scan_dogo(u4_lane lane,
                        u4_atom atom);
