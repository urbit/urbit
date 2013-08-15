/* include/fake/tab.h
**
** This file is in the public domain.
*/
  /** Functions.
  **/
    /* u4_tab_is():
    **
    **   Return 1 iff (noun) is a tab.
    */
      u4_t
      u4_tab_is(u4_noun tab);

    /* u4_tab_in():
    **
    **   Return 1 iff (tag) is in (tab).
    */
      u4_t
      u4_tab_in(u4_noun tag,
                u4_tab  tab);

    /* u4_tab_get(): 
    **
    **   Produce the dog in (tab) matching (tag), or bull.
    */
      u4_nopt
      u4_tab_get(u4_noun tag,
                 u4_tab  tab);

    /* u4_tab_get_twig():
    **
    **   Produce a twig such that get(twig tag) is the dog
    **   of (tag), or bull of there is no such tag.
    */
      _(u4_nopt, twig)
      u4_tab_get_twig(u4_lane lane,
                      u4_noun tag_get,
                      u4_noun tab);

    /* u4_tab_twig():
    **
    **   Produce a twig such that get(twig tag) is the dog
    **   of (tag), or 0 if there is no such tag in (tab).
    */
      u4_twig
      u4_tab_twig(u4_lane lane,
                  u4_noun tag,
                  u4_noun tab);

    /* u4_tab_add():
    **
    **   Produce a new tab which adds (tag dog) to (tab).
    **   Replace old dog, if any.
    */
      _(u4_tab, tab_pro)
      u4_tab_add(u4_lane lane,
                 u4_noun tag,
                 u4_noun dog,
                 u4_tab  tab_sub);

    /* u4_tab_del():
    **
    **   Delete (tag) from (tab), if it is present.
    */
      _(u4_tab, tab_pro)
      u4_tab_del(u4_lane lane,
                 u4_noun tag,
                 u4_tab  tab_sub);

    /* u4_tab_add_log():
    **
    **   Produce a new tab which adds all (tag dog) cells in
    **   (log) to (tab).  Replace old dog, if any.
    */
      _(u4_tab, tab_pro)
      u4_tab_add_log(u4_lane lane,
                     u4_log  log,
                     u4_tab  tab_sub);

    /* u4_tab_log():
    **
    **   Convert (tab) to a pseudo-randomly sorted log of (tag dog) 
    **   cells, prepending to (log_sub).
    */
      _(u4_log, log_pro)
      u4_tab_log(u4_lane lane,
                 u4_log  log_sub,
                 u4_tab  tab);

    /* u4_tab_cat():
    **
    **   Produce a version of (tab_b) which includes all entries
    **   in (tab_a).  If there is a conflict, stop.
    */
      _(u4_tab, tab_pro)
      u4_tab_cat(u4_lane lane,
                 u4_tab  tab_a,
                 u4_tab  tab_b);
