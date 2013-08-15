/* include/fake/rat.h
**
** This file is in the public domain.
*/
  /** Data types.
  **/
    /* A text to be parsed.
    */
      typedef u4_noun u4_text;

    /* A tab (mote *rule).
    */
      typedef u4_tab u4_gram;

    /* A rule:
    **
    **   mote                   - goal name
    **   (.all  *rule)          - direct tuple
    **   (.fab  hack rule)      - use hack to modify rule
    **   (.from byte byte)      - byte-range, inclusive
    **   (.knit rule *rule)     - internal delimiter
    **   (.look rule rule)      - rule-a iff followed by rule-b
    **   (.one *rule)           - pick one
    **   (.plus rule)           - ie, +
    **   (.pfix rule rule)      - prefix, ignore first coin
    **   (.ques rule)           - ie, ?
    **   (.seq  rule rule)      - ?(b *(a b))
    **   (.sfix rule rule)      - suffix, reverse order, ignore second coin
    **   (.star rule)           - ie, *
    **   (.text text)           - match a string literal
    **   (.top  rule)           - rule, then null
    */
      typedef u4_noun u4_rule;

    /* A goal name.
    */
      typedef u4_mote u4_goal;

    /* A noun product.
    */
      typedef u4_noun u4_coin;

    /* A byte position in the text.
    */
      typedef u4_atom u4_mark;

    /* A pair (mark coin), or u4_noun_0.  
    **
    ** The mark is the first following byte.
    */
      typedef u4_noun u4_bill;


  /** Functions.
  **/
    /* u4_rat_parse():
    **
    **   Using (tab_gear), parse (text) as (language) on (lane).
    **
    **   On error, produce u4_bull and print a message.
    */
      u4_noun
      u4_rat_parse(u4_lane lane,
                   u4_tab  tab_gear,
                   u4_atom language,
                   u4_atom text);

    /* u4_rat_fab():
    **
    **   Use (hack) to transform (coin) on (lane).
    **
    **   On error, produce u4_bull.
    */
      u4_noun
      u4_rat_fab(u4_lane lane,
                 u4_noun hack,
                 u4_noun coin);

