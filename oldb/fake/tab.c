/* fake/tab.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _ord_tag_simple():
**
**    Return 1 iff (tag_l tag_r) are in correct simple order.
**
**    Trip if (tag_l tag_r) are equal.
*/
static u4_t
_ord_tag_simple(u4_tag tag_l, u4_tag tag_r)
{
  u4_bit bit;

  if ( u4_bull == (bit = u4_op_ord(tag_l, tag_r)) ) {
    return u4_trip;
  }
  return u4_n_true(bit);
}

/* _ord_hog_prior():
**
**    Return 1 iff (hog_l hog_r) are in correct priority order (meta-nub).
**
**    Trip iff (hog_l hog_r) are equal.
*/
static u4_t
_ord_hog_prior(u4_hog hog_l, u4_hog hog_r)
{
  u4_tag tag_l = u4_ch(hog_l);
  u4_tag tag_r = u4_ch(hog_r);
  u4_nub nib_l = u4_n_nib(tag_l);
  u4_nub nib_r = u4_n_nib(tag_r);

  if ( nib_l == nib_r ) {
    return _ord_tag_simple(tag_l, tag_r);
  }
  else return (nib_l < nib_r);
}

/* _ord_hog_tree():
**
**    Return 1 iff (hog_l hog_r) are in correct tree order (nub).
**
**    Stop iff (hog_l hog_r) are equal.
*/
static u4_t
_ord_hog_tree(u4_hog hog_l, u4_hog hog_r)
{
  u4_tag tag_l = u4_ch(hog_l);
  u4_tag tag_r = u4_ch(hog_r);
  u4_nub nub_l = u4_n_nub(tag_l);
  u4_nub nub_r = u4_n_nub(tag_r);

  if ( nub_l == nub_r ) {
    return _ord_tag_simple(tag_l, tag_r);
  }
  else return (nub_l < nub_r);
}

/* u4_tab_is():
**
**   Return 1 iff (noun) is a tab.
*/
u4_t
u4_tab_is(u4_noun tab)
{
  if ( u4_n_zero(tab) ) {
    return 1;
  }
  else {
    u4_hog hog;
    u4_tab tab_l, tab_r;

    u4_c_trel(tab, &hog, &tab_l, &tab_r);
    {
      if ( !u4_n_zero(tab_l) ) {
        u4_hog hog_l = u4_ch(tab_l);

        if ( !_ord_hog_tree(hog_l, hog) ) {
          return 0;
        }
        if ( !_ord_hog_prior(hog, hog_l) ) {
          return 0;
        }
      }

      if ( !u4_n_zero(tab_r) ) {
        u4_hog hog_r = u4_ch(tab_r);

        if ( !_ord_hog_tree(hog, hog_r) ) {
          return 0;
        }
        if ( !_ord_hog_prior(hog, hog_r) ) {
          return 0;
        }
      }
    }
    return u4_tab_is(tab_l) && u4_tab_is(tab_r);
  }
}

/* u4_tab_in():
**
**   Return 1 iff (tag_in) is in (tab).
*/
u4_t
u4_tab_in(u4_noun tag_in,
          u4_tab  tab)
{
  if ( u4_n_zero(tab) ) {
    return 0;
  }
  else {
    u4_nub nub_in = u4_n_nub(tag_in);

    while ( 1 ) {
      if ( u4_n_zero(tab) ) {
        return 0;
      }
      else {
        u4_hog hog;
        u4_tag tag;
        u4_dog dog;
        u4_tab tab_l, tab_r;

        u4_c_trel(tab, &hog, &tab_l, &tab_r);
        u4_c_cell(hog, &tag, &dog);
        {
          u4_nub nub = u4_n_nub(tag);

          if ( nub_in == nub ) {
            if ( u4_n_eq(tag_in, tag) ) {
              return 1;
            }
            else tab = _ord_tag_simple(tag_in, tag) ? tab_l : tab_r;
          }
          else {
            tab = (nub_in < nub) ? tab_l : tab_r;
          }
        }
      }
    }
  }
}

/* u4_tab_get(): 
**
**   Produce the dog in (tab) matching (tag_get), or bull.
*/
u4_nopt
u4_tab_get(u4_noun tag_get,
           u4_tab  tab)
{
  if ( u4_n_zero(tab) ) {
    return u4_bull;
  }
  else {
    u4_nub nub_get = u4_n_nub(tag_get);

    while ( 1 ) {
      if ( u4_n_zero(tab) ) {
        return u4_bull;
      }
      else {
        u4_hog hog;
        u4_tag tag;
        u4_dog dog;
        u4_tab tab_l, tab_r;

        u4_c_trel(tab, &hog, &tab_l, &tab_r);
        u4_c_cell(hog, &tag, &dog);
        {
          u4_nub nub = u4_n_nub(tag);

          if ( nub_get == nub ) {
            if ( u4_n_eq(tag_get, tag) ) {
              return dog;
            }
            else tab = _ord_tag_simple(tag_get, tag) ? tab_l : tab_r;
          }
          else {
            tab = (nub_get < nub) ? tab_l : tab_r;
          }
        }
      }
    }
  }
}

/* _twig_l(): (twig + twig).
*/
static u4_twig 
_twig_l(u4_lane lane, 
        u4_twig twig)
{
  return u4_op_add(lane, twig, twig);
}

/* _twig_r(): (twig + twig + 1).
*/
static u4_twig 
_twig_r(u4_lane lane, 
        u4_twig twig)
{
  return u4_op_inc(lane, _twig_l(lane, twig));
}

/* _get_twig_leak(): as u4_tag_get_twig(), leaking on (lane), at (twig).
*/
static _(u4_nopt, twig)
_get_twig_leak(u4_lane lane,
               u4_noun tag_get,
               u4_twig twig,
               u4_noun tab)
{
  if ( u4_n_zero(tab) ) {
    return u4_bull;
  }
  else {
    u4_nub nub_get = u4_n_nub(tag_get);

    while ( 1 ) {
      if ( u4_n_zero(tab) ) {
        return u4_bull;
      }
      else {
        u4_hog hog;
        u4_tag tag;
        u4_dog dog;
        u4_tab tab_l, tab_r;

        u4_c_trel(tab, &hog, &tab_l, &tab_r);
        u4_c_cell(hog, &tag, &dog);
        {
          u4_nub nub = u4_n_nub(tag);
          u4_t   t_l;

          if ( nub_get == nub ) {
            if ( u4_n_eq(tag_get, tag) ) {
              u4_twig twig_hog = _twig_l(lane, twig);
              u4_twig twig_dog = _twig_r(lane, twig_hog);

              return twig_dog;
            }
            else t_l = _ord_tag_simple(tag_get, tag);
          }
          else {
            t_l = (nub_get < nub);
          }

          if ( t_l ) {
            return _get_twig_leak
              (lane, tag_get, _twig_l(lane, _twig_r(lane, twig)), tab_l);
          } else {
            return _get_twig_leak
              (lane, tag_get, _twig_r(lane, _twig_r(lane, twig)), tab_r);
          }
        }
      }
    }
  }
}

/* _get_twig_road(): as u4_tab_get_twig(), on u4_hat(road).
*/
static _(u4_nopt, twig)
_get_twig_road(u4_road road,
               u4_noun tag_get,
               u4_noun tab)
{
  u4_twig twig = _get_twig_leak(u4_cap(road), tag_get, u4_noun_1, tab);

  if ( u4_bull == twig ) {
    return u4_bull;
  }
  else return u4_k_safe(u4_hat(road), twig);
}

/* u4_tab_get_twig():
**
**   Produce a twig such that get(twig tag) is the dog
**   of (tag), or bull of there is no such tag.
*/
_(u4_nopt, twig)
u4_tab_get_twig(u4_lane lane,
                u4_noun tag_get,
                u4_noun tab)
{
  u4_road road = u4_lane_road(lane);

  if ( u4_lane_side(lane) == u4_side_hat ) {
    return _get_twig_road(road, tag_get, tab);
  }
  else {
    u4_noun pro;
    u4_road road_nest;

    road_nest = u4_r_nest_in(road);
    pro = _get_twig_road(road_nest, tag_get, tab);
    u4_r_nest_out(road, road_nest);

    return pro;
  }
}

/* _tab_fix_l(): fix a tab whose left side has been rebuilt.
*/
static u4_tab
_tab_fix_l(u4_lane lane,
           u4_hog  hog,
           u4_tab  tab_l,
           u4_tab  tab_r)
{
  u4_hog hog_l;
  u4_tab tab_ll, tab_lr;

  u4_c_trel(tab_l, &hog_l, &tab_ll, &tab_lr);
  {
    if ( _ord_hog_prior(hog, hog_l) ) {
      return u4_k_trel(lane, hog, tab_l, tab_r);
    }
    else {
      /* Rotate to the right.
      */
      return u4_k_trel(lane, hog_l, tab_ll, 
                                    u4_k_trel(lane, hog, tab_lr, tab_r));
    }
  }
}

/* _tab_fix_r(): fix a tab whose right side has been rebuilt.
*/
static u4_tab
_tab_fix_r(u4_lane lane,
           u4_hog  hog,
           u4_tab  tab_l,
           u4_tab  tab_r)
{
  u4_hog hog_r;
  u4_tab tab_rl, tab_rr;

  u4_c_trel(tab_r, &hog_r, &tab_rl, &tab_rr);
  {
    if ( _ord_hog_prior(hog, hog_r) ) {
      return u4_k_trel(lane, hog, tab_l, tab_r);
    }
    else {
      /* Rotate to the left.
      */
      return u4_k_trel(lane, hog_r, u4_k_trel(lane, hog, tab_l, tab_rl), 
                                    tab_rr);
    }
  }
}

/* _tab_add_leak(): as u4_tab_add(), leaking on (lane).
*/
static
_(u4_tab, tab_pro)
_tab_add_leak(u4_lane lane,
              u4_noun tag_add,
              u4_noun dog_add,
              u4_nub  nub_add,
              u4_tab  tab)
{
  if ( u4_n_zero(tab) ) {
    return u4_k_trel(lane, u4_k_cell(lane, tag_add, dog_add), 
                           u4_noun_0,
                           u4_noun_0);
  }
  else {
    u4_hog hog;
    u4_tag tag;
    u4_dog dog;
    u4_tab tab_l, tab_r;

    u4_c_trel(tab, &hog, &tab_l, &tab_r);
    u4_c_cell(hog, &tag, &dog);
    {
      u4_nub nub = u4_n_nub(tag);

      if ( nub_add == nub ) {
        if ( u4_n_eq(tag_add, tag) ) {
          if ( u4_n_eq(dog_add, dog) ) {
            return tab;
          }
          else {
            return u4_k_trel(lane, u4_k_cell(lane, tag_add, dog_add),
                                   tab_l,
                                   tab_r);
          }
        }
        else {
          if ( _ord_tag_simple(tag_add, tag) ) {
            goto left;
          } else goto right;
        }
      }
      else {
        if ( nub_add < nub ) {
          left: {
            u4_tab tab_l_add = _tab_add_leak
              (lane, tag_add, dog_add, nub_add, tab_l);

            if ( tab_l_add == tab_l ) {
              return tab;
            }
            else return _tab_fix_l(lane, hog, tab_l_add, tab_r);
          }
        } else {
          right: {
            u4_tab tab_r_add = _tab_add_leak
              (lane, tag_add, dog_add, nub_add, tab_r);

            if ( tab_r_add == tab_r ) {
              return tab;
            }
            else return _tab_fix_r(lane, hog, tab_l, tab_r_add);
          }
        }
      }
    }
  }
}

/* _tab_add_hat(): as u4_tab_add(), on the hat of (road).
*/
static
_(u4_tab, tab_pro)
_tab_add_hat(u4_road road,
             u4_noun tag_add,
             u4_noun dog_add,
             u4_tab  tab)
{
  u4_bar bar_cap = u4_road_bar_cap(road);
  u4_tab tab_pro_cap;
  u4_tab tab_pro_hat;

  tab_pro_cap = _tab_add_leak
    (u4_cap(road), tag_add, dog_add, u4_n_nub(tag_add), tab);

  tab_pro_hat = u4_k_safe(u4_hat(road), tab_pro_cap);
  
  u4_road_bar_cap(road) = bar_cap;
  return tab_pro_hat;
}

/* u4_tab_add():
**
**   Produce a new tab which adds (tag_add dog_add) to (tab).
**   Replace old dog, if any.
*/
_(u4_tab, tab_pro)
u4_tab_add(u4_lane lane,
           u4_noun tag_add,
           u4_noun dog_add,
           u4_tab  tab)
{
  u4_road road = u4_lane_road(lane);

  if ( u4_lane_side(lane) == u4_side_hat ) {
    return _tab_add_hat(road, tag_add, dog_add, tab);
  }
  else {
    u4_tab  tab_pro;
    u4_road road_nest;

    road_nest = u4_r_nest_in(road);
    tab_pro = _tab_add_hat(road_nest, tag_add, dog_add, tab);
    u4_r_nest_out(road, road_nest);

    return tab_pro;
  }
}

/* u4_tab_del():
**
**   Delete (tag) from (tab), if it is present.
*/
_(u4_tab, tab_pro)
u4_tab_del(u4_lane lane,
           u4_noun tag,
           u4_tab  tab_sub)
{
  return u4_stub;
}

/* _tab_add_log_hat(): as u4_tab_add_log(), on u4_hat(road).
*/
static
_(u4_tab, tab_pro)
_tab_add_log_hat(u4_road road,
                 u4_log  log,
                 u4_tab  tab)
{
  u4_bar bar_cap     = u4_road_bar_cap(road);
  u4_tab tab_pro_cap = tab;
  u4_tab tab_pro_hat;

  while ( !u4_n_zero(log) ) {
    u4_hog hog = u4_ch(log);
    u4_tag tag = u4_ch(hog);
    u4_dog dog = u4_ct(hog);

    tab_pro_cap = _tab_add_leak(u4_cap(road), 
                                tag, dog, 
                                u4_n_nub(tag), 
                                tab_pro_cap);
    log = u4_ct(log);
  }
  tab_pro_hat = u4_k_safe(u4_hat(road), tab_pro_cap);

  u4_road_bar_cap(road) = bar_cap;
  return tab_pro_hat;
}

/* u4_tab_add_log():
**
**   Produce a new tab which adds all (tag dog) cells in
**   (log) to (tab).  Replace old dog, if any.
*/
_(u4_tab, tab_pro)
u4_tab_add_log(u4_lane lane,
               u4_log  log,
               u4_tab  tab)
{
  u4_road road = u4_lane_road(lane);

  if ( u4_lane_side(lane) == u4_side_hat ) {
    return _tab_add_log_hat(road, log, tab);
  }
  else {
    u4_tab  tab_pro;
    u4_road road_nest;

    road_nest = u4_r_nest_in(road);
    tab_pro = _tab_add_log_hat(road_nest, log, tab);
    u4_r_nest_out(road, road_nest);

    return tab_pro;
  }
}

/* u4_tab_log():
**
**   Convert (tab) to a pseudo-randomly sorted log of (tag dog) 
**   cells, prepending to (log).
*/
_(u4_log, log_pro)
u4_tab_log(u4_lane lane,
           u4_log  log,
           u4_tab  tab)
{
  if ( u4_n_zero(tab) ) {
    return log;
  }
  else {
    u4_hog hog;
    u4_tab tab_l, tab_r;

    u4_c_trel(tab, &hog, &tab_l, &tab_r);
    {
      log = u4_tab_log(lane, log, tab_l);
      log = u4_k_cell(lane, hog, log);
      log = u4_tab_log(lane, log, tab_r);
    }
    return log;
  }
}

/* _tab_cat_hat(): as u4_tab_cat(), on the hat of (road).
*/
static
_(u4_tab, tab_pro)
_tab_cat_hat(u4_road road,
             u4_tab  tab_a,
             u4_tab  tab_b)
{
  u4_bar bar_cap = u4_road_bar_cap(road);
  u4_tab tab_hat;
  u4_tab tab_cap;
  {
    u4_log log_a = u4_tab_log(u4_cap(road), u4_noun_0, tab_a);

    tab_cap = u4_tab_add_log(u4_cap(road), log_a, tab_b);
  }
  tab_hat = u4_k_safe(u4_hat(road), tab_cap);

  u4_road_bar_cap(road) = bar_cap;
  return tab_hat;
}

/* u4_tab_cat():
**
**   Produce a version of (tab_b) which includes all entries
**   in (tab_a).  If there is a conflict, stop.
*/
_(u4_tab, tab_pro)
u4_tab_cat(u4_lane lane,
           u4_tab  tab_a,
           u4_tab  tab_b)
{
  u4_road road = u4_lane_road(lane);

  if ( u4_lane_side(lane) == u4_side_hat ) {
    return _tab_cat_hat(road, tab_a, tab_b);
  }
  else {
    u4_tab  tab_pro;
    u4_road road_nest;

    road_nest = u4_r_nest_in(road);
    tab_pro = _tab_cat_hat(road_nest, tab_a, tab_b);
    u4_r_nest_out(road, road_nest);

    return tab_pro;
  }
}
