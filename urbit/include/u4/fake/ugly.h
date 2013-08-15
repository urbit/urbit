/* include/fake/ugly.h
**
** This file is in the public domain.
*/
  /** Functions.
  **/
    /* u4_ugly_dump():
    **
    **   Convert (noun) to an ugly dump.
    */
      u4_noun
      u4_ugly_dump(u4_lane lane,
                   u4_noun noun);

    /* u4_ylgu_scan_():
    **
    **   Scan (noun) with the ylgu parser.
    */
      u4_noun
      u4_ylgu_scan_(u4_lane lane,
                    u4_noun noun);

    /* u4_ylgu_scan_cl_():
    **
    **  Scan (cl_text) with the ylgu parser.
    */
      u4_noun
      u4_ylgu_scan_cl_(u4_lane     lane,
                       const u4_cl *cl_text);
