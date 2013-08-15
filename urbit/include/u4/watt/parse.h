/* include/watt/watt.h
**
** This file is in the public domain.
*/
  /** Parsing.
  **/
    /* u4_watt_parse(): 
    **
    **   At (pif), convert (zar) to a gene.
    */
      u4_noun
      u4_watt_parse(u4_lane lane,
                    u4_noun pif,
                    u4_atom zar);

    /* u4_vere_parse(): 
    **
    **   Convert (mez) to a vere command.
    */
      u4_noun
      u4_vere_parse(u4_lane lane,
                    u4_atom mez);

    /* u4_hume_parse(): 
    **
    **   Convert (mez) to a data noun.
    */
      u4_noun
      u4_hume_parse(u4_lane lane,
                    u4_atom mez);
