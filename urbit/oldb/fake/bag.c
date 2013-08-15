/* fake/bag.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _ord_simple():
**
**    Return 1 iff (pig_l pig_r) are in correct simple order.
**
**    Trip if (pig_l pig_r) are equal.
*/
static u4_t
_ord_simple(u4_pig pig_l, u4_pig pig_r)
{
  u4_bit bit;

  if ( u4_bull == (bit = u4_op_ord(pig_l, pig_r)) ) {
    return u4_trip;
  }
  return u4_n_true(bit);
}

/* _ord_prior():
**
**    Return 1 iff (pig_l pig_r) are in correct priority order (meta-nub).
**
**    Stop iff (pig_l pig_r) are equal.
*/
static u4_t
_ord_prior(u4_pig pig_l, u4_pig pig_r)
{
  u4_nub nub_nib_l = u4_n_nib(pig_l);
  u4_nub nub_nib_r = u4_n_nib(pig_r);

  if ( nub_nib_l == nub_nib_r ) {
    return _ord_simple(pig_l, pig_r);
  }
  else return (nub_nib_l < nub_nib_r);
}

/* _ord_tree():
**
**    Return 1 iff (pig_l pig_r) are in correct tree order (nub).
**
**    Stop iff (pig_l pig_r) are equal.
*/
static u4_t
_ord_tree(u4_pig pig_l, u4_pig pig_r)
{
  u4_nub nub_l = u4_n_nub(pig_l);
  u4_nub nub_r = u4_n_nub(pig_r);

  if ( nub_l == nub_r ) {
    return _ord_simple(pig_l, pig_r);
  }
  else return (nub_l < nub_r);
}

/* u4_bag_ok(): sanity test for bag.
*/
u4_t
u4_bag_ok(u4_bag bag)
{
  if ( u4_n_zero(bag) ) {
    return 1;
  }
  else {
    if ( !u4_n_trel(bag) ) {
      return 0;
    }
    else {
      u4_pig pig;
      u4_bag bag_l, bag_r;
      u4_c_trel(bag, &pig, &bag_l, &bag_r);
      {
        if ( !u4_n_zero( bag_l) ) {
          u4_pig pig_l = u4_ch(bag_l);

          if ( !_ord_tree(pig_l, pig) ) {
            return 0;
          }
          if ( !_ord_prior(pig, pig_l) ) {
            return 0;
          }
        }

        if ( !u4_n_zero(bag_r) ) {
          u4_pig pig_r = u4_ch(bag_r);

          if ( !_ord_tree(pig, pig_r) ) {
            return 0;
          }
          if ( !_ord_prior(pig, pig_r) ) {
            return 0;
          }
        }
      }
      return u4_bag_ok(bag_l) && u4_bag_ok(bag_r);
    }
  }
}

/* u4_bag_in():
**
**   Return 1 iff (pig) is in (bag).
*/
u4_t
u4_bag_in(u4_pig pig_in,
          u4_bag bag)
{
  u4_nub nub_in = u4_n_nub(pig_in);

  while ( 1 ) {
    if ( u4_n_zero(bag) ) {
      return 0;
    }
    else {
      u4_pig pig; 
      u4_bag bag_l, bag_r;

      u4_c_trel(bag, &pig, &bag_l, &bag_r);
      {
        u4_nub nub_sub = u4_n_nub(pig);

        if ( nub_in == nub_sub ) {
          if ( u4_n_eq(pig_in, pig) ) {
            return 1;
          }
          else bag = _ord_simple(pig_in, pig) ? bag_l : bag_r;
        }
        else {
          bag = (nub_in < nub_sub) ? bag_l : bag_r;
        }
      }
    }
  }
}

/* u4_bag_at():
**
**   Return path to node of (pig) in (bag), under (axe); or 0.
*/
u4_atom
u4_bag_at(u4_lane lane, 
          u4_pig  pig_in,
          u4_atom axe,
          u4_bag  bag)
{
  u4_nub nub_in = u4_n_nub(pig_in);

  if ( u4_n_zero(bag) ) {
    return u4_noun_0;
  }
  else {
    u4_pig pig; 
    u4_bag bag_l, bag_r;

    u4_c_trel(bag, &pig, &bag_l, &bag_r);
    {
      u4_nub nub_sub = u4_n_nub(pig);
      u4_t   t_l;

      if ( nub_in == nub_sub ) {
        if ( u4_n_eq(pig_in, pig) ) {
          return axe;
        }
        else t_l = _ord_simple(pig_in, pig);
      }
      else t_l = (nub_in < nub_sub);

      if ( t_l ) {
        return u4_bag_at
          (lane, pig_in, u4_op_peg(lane, axe, u4_noun_2), bag_l);
      }
      else {
        return u4_bag_at
          (lane, pig_in, u4_op_peg(lane, axe, u4_noun_3), bag_r);
      }
    }
  }
}

/* _bag_fix_l(): fix a bag whose left side has been rebuilt.
*/
static u4_bag
_bag_fix_l(u4_lane lane,
           u4_pig  pig_sub,
           u4_bag  bag_l,
           u4_bag  bag_r)
{
  u4_pig pig_sub_l;
  u4_bag  bag_ll, bag_lr;
  u4_c_trel(bag_l, &pig_sub_l, &bag_ll, &bag_lr);
  {
    if ( _ord_prior(pig_sub, pig_sub_l) ) {
      return u4_k_trel(lane, pig_sub, bag_l, bag_r);
    }
    else {
      /* Rotate to the right.
      */
      return u4_k_trel(lane, pig_sub_l, 
                             bag_ll, 
                             u4_k_trel(lane, pig_sub, bag_lr, bag_r));
    }
  }
}

/* _bag_fix_r(): fix a bag whose right side has been rebuilt.
*/
static u4_bag
_bag_fix_r(u4_lane lane,
           u4_pig  pig_sub,
           u4_bag  bag_l,
           u4_bag  bag_r)
{
  u4_pig pig_sub_r;
  u4_bag bag_rl, bag_rr;
  u4_c_trel(bag_r, &pig_sub_r, &bag_rl, &bag_rr);
  {
    if ( _ord_prior(pig_sub, pig_sub_r) ) {
      return u4_k_trel(lane, pig_sub, bag_l, bag_r);
    }
    else {
      /* Rotate to the left.
      */
      return u4_k_trel(lane, pig_sub_r, 
                             u4_k_trel(lane, pig_sub, bag_l, bag_rl), 
                             bag_rr);
    }
  }
}

/* _bag_add_leak():
**
**   As u4_bag_add_test(), leaking on (lane).
**
**   nub_add is u4_n_nub(pig_add).
*/
static
_(u4_bag, bag_pro)
_bag_add_leak(u4_lane lane,
              u4_pig  pig_add,
              u4_nub  nub_add,
              u4_bag  bag)
{
  if ( u4_n_zero(bag) ) {
    return u4_k_trel(lane, pig_add, u4_noun_0, u4_noun_0);
  }
  else {
    u4_pig pig;
    u4_bag bag_l, bag_r;
    u4_c_trel(bag, &pig, &bag_l, &bag_r);
    {
      u4_nub nub = u4_n_nub(pig);

      if ( nub_add == nub ) {
        if ( u4_n_eq(pig_add, pig) ) {

          return bag;
        }
        else {
          if ( _ord_simple(pig_add, pig) ) {
            goto left;
          } else goto right;
        }
      }
      else {
        if ( nub_add < nub ) {
          left: {
            u4_bag bag_l_add = _bag_add_leak(lane, pig_add, nub_add, bag_l);

            if ( bag_l_add == bag_l ) {
              return bag;
            }
            else {
              return _bag_fix_l(lane, pig, bag_l_add, bag_r);
            }
          }
        } else {
          right: {
            u4_bag bag_r_add = _bag_add_leak(lane, pig_add, nub_add, bag_r);

            if ( bag_r_add == bag_r ) {
              return bag;
            } 
            else {
              return _bag_fix_r(lane, pig, bag_l, bag_r_add);
            }
          }
        }
      }
    }
  }
  return u4_stub;
}

/* _bag_add_hat():
**
**   As u4_bag_add(), on the hat of (road).
*/
static
_(u4_bag, bag_pro)
_bag_add_hat(u4_road road,
             u4_pig  pig_add,
             u4_bag  bag)
{
  u4_bar bar_cap = u4_road_bar_cap(road);
  u4_bag bag_pro_cap;
  u4_bag bag_pro_hat;

  bag_pro_cap = _bag_add_leak
    (u4_cap(road), pig_add, u4_n_nub(pig_add), bag);

  bag_pro_hat = u4_k_safe(u4_hat(road), bag_pro_cap);
  
  u4_road_bar_cap(road) = bar_cap;
  return bag_pro_hat;
}

/* u4_bag_add():
**
**   Produce a version of (bag) which includes (pig).
*/
_(u4_bag, bag_pro)
u4_bag_add(u4_lane lane,
           u4_pig  pig,
           u4_bag  bag)
{
  u4_road road = u4_lane_road(lane);

  if ( u4_lane_side(lane) == u4_side_hat ) {
    u4_bag bag_pro;

    bag_pro = _bag_add_hat(road, pig, bag);
    // u4_assert(u4_bag_ok(bag_pro));

    return bag_pro;
  }
  else {
    u4_bag  bag_pro;
    u4_road road_nest;

    road_nest = u4_r_nest_in(road);
    bag_pro = _bag_add_hat(road_nest, pig, bag);
    u4_r_nest_out(road, road_nest);

    // u4_assert(u4_bag_ok(bag_pro));
    return bag_pro;
  }
}

/* _bag_add_log_hat(): as u4_bag_add_log(), on u4_hat(road).
*/
static
_(u4_bag, bag_pro)
_bag_add_log_hat(u4_road road,
                 u4_log  log,
                 u4_bag  bag)
{
  u4_bar bar_cap     = u4_road_bar_cap(road);
  u4_bag bag_pro_cap = bag;
  u4_bag bag_pro_hat;

  while ( !u4_n_zero(log) ) {
    u4_pig pig = u4_ch(log);

    bag_pro_cap = _bag_add_leak(u4_cap(road), pig, u4_n_nub(pig), bag_pro_cap);
    log = u4_ct(log);
  }
  bag_pro_hat = u4_k_safe(u4_hat(road), bag_pro_cap);
 
  u4_road_bar_cap(road) = bar_cap;
  return bag_pro_hat;
}

/* u4_bag_add_log(): 
**
**   Produce a version of (bag) which includes all nouns in (log).
*/
_(u4_bag, bag_pro)
u4_bag_add_log(u4_lane lane,
               u4_log  log,
               u4_bag  bag)
{
  u4_road road = u4_lane_road(lane);

  if ( u4_lane_side(lane) == u4_side_hat ) {
    return _bag_add_log_hat(road, log, bag);
  }
  else {
    u4_bag  bag_pro;
    u4_road road_nest;

    road_nest = u4_r_nest_in(road);
    bag_pro = _bag_add_log_hat(road_nest, log, bag);
    u4_r_nest_out(road, road_nest);

    return bag_pro;
  }
  return u4_stub;
}

/* u4_bag_log():
**
**   Convert (bag) to a pseudo-randomly sorted log, 
**   prepending to (log).
*/
_(u4_log, log_pro)
u4_bag_log(u4_lane lane,
           u4_log  log,
           u4_bag  bag)
{
  if ( u4_n_zero(bag) ) {
    return log;
  }
  else {
    u4_pig pig;
    u4_bag bag_l, bag_r;

    u4_c_trel(bag, &pig, &bag_l, &bag_r);
    {
      log = u4_bag_log(lane, log, bag_l);
      log = u4_k_cell(lane, pig, log);
      log = u4_bag_log(lane, log, bag_r);
    }
    return log;
  }
}

/* u4_bag_cat():
**
**   Produce a version of (bag_b) which includes all entries
**   in (bag_a).
*/
_(u4_bag, bag_pro)
u4_bag_cat(u4_lane lane,
           u4_bag  bag_a,
           u4_bag  bag_b)
{
  u4_log log = u4_bag_log(lane, u4_noun_0, bag_a);

  while ( !u4_n_zero(log) ) {
    bag_b = u4_bag_add(lane, u4_ch(log), bag_b);
    log = u4_ct(log);
  }
  return bag_b;
}

#if 0
  // Something broken in this code - leak instead!
  //
/* _bag_cat_hat(): as u4_bag_cat(), on the hat of (road).
*/
static
_(u4_bag, bag_pro)
_bag_cat_hat(u4_road road,
             u4_bag  bag_a,
             u4_bag  bag_b)
{
  u4_bar bar_cap = u4_road_bar_cap(road);
  u4_bag bag_hat;
  u4_bag bag_cap;
  {
    u4_log log_a = u4_bag_log(u4_cap(road), u4_noun_0, bag_a);
    u4_bag bag_cap;

    bag_cap = u4_bag_add_log(u4_cap(road), log_a, bag_b);
  }
  bag_hat = u4_k_safe(road, bag_cap);

  u4_road_bar_cap(road) = bar_cap;
  printf("bch: %x\n", bag_hat);
  abort(); 
  return bag_hat;
}
            
/* u4_bag_cat():
**
**   Produce a version of (bag_b) which includes all entries
**   in (bag_a).
*/
_(u4_bag, bag_pro)
u4_bag_cat(u4_lane lane,
           u4_bag  bag_a,
           u4_bag  bag_b)
{
  u4_road road = u4_lane_road(lane);

  if ( u4_lane_side(lane) == u4_side_hat ) {
    return _bag_cat_hat(road, bag_a, bag_b);
  }
  else {
    u4_bag  bag_pro;
    u4_road road_nest;

    road_nest = u4_r_nest_in(road);
    bag_pro = _bag_cat_hat(road_nest, bag_a, bag_b);
    u4_r_nest_out(road, road_nest);

    printf("bp: %x\n", bag_pro);
    return bag_pro;
  }
}
#endif
