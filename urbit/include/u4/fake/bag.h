/* include/fake/bag.h
**
** This file is in the public domain.
*/
  /** Functions.
  **/
    /* u4_bag_ok(): sanity test for bag.
    */
      u4_t
      u4_bag_ok(u4_bag bag);

    /* u4_bag_in():
    **
    **   Return 1 iff (pig) is in (bag).
    */
      u4_t
      u4_bag_in(u4_pig pig,
                u4_bag bag);

    /* u4_bag_at():
    **
    **   Return path to node of (pig) in (bag), under (axe); or 0.
    */
      u4_atom
      u4_bag_at(u4_lane lane, 
                u4_pig  pig_in,
                u4_atom axe,
                u4_bag  bag);

    /* u4_bag_add():
    **
    **   Produce a version of (bag_sub) which includes (pig).
    */
      _(u4_bag, bag_pro)
      u4_bag_add(u4_lane lane,
                 u4_pig  pig,
                 u4_bag  bag_sub);
 
    /* u4_bag_add_log(): 
    **
    **   Produce a version of (bag) which includes all nouns in (log).
    */
      _(u4_bag, bag_pro)
      u4_bag_add_log(u4_lane lane,
                     u4_log  log,
                     u4_bag  bag_sub);

    /* u4_bag_log():
    **
    **   Convert (bag) to a pseudo-randomly sorted log, 
    **   prepending to (log_sub).
    */
      _(u4_log, log_pro)
      u4_bag_log(u4_lane lane,
                 u4_log  log_sub,
                 u4_bag  bag_sub);

    /* u4_bag_cat():
    **
    **   Produce a version of (bag_b) which includes all entries
    **   in (bag_a).
    */
      _(u4_bag, bag_pro)
      u4_bag_cat(u4_lane lane,
                 u4_bag  bag_a,
                 u4_bag  bag_b);
