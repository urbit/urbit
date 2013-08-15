/* j/pool.c
**
** This file is in the public domain.
*/
#include "all.h"

/** Warning: this file contains old code which does not comply
*** with current coding conventions.
**/

/* _ord_simple():
**
**    Return 1 iff (pig_l pig_r) are in correct simple order.
**
**    Trip if (pig_l pig_r) are equal.
*/
static c3_t
_ord_simple(u2_noun pig_l, u2_noun pig_r)
{
  return (u2_nord(pig_l, pig_r) == _0);
}

/* _ord_prior():
**
**    Return 1 iff (pig_l pig_r) are in correct priority order (meta-mug).
**
**    Stop iff (pig_l pig_r) are equal.
*/
static c3_t
_ord_prior(u2_noun pig_l, u2_noun pig_r)
{
  c3_w mug_nib_l = u2_mug(u2_mug(pig_l));
  c3_w mug_nib_r = u2_mug(u2_mug(pig_r));

  if ( mug_nib_l == mug_nib_r ) {
    return _ord_simple(pig_l, pig_r);
  }
  else return (mug_nib_l < mug_nib_r);
}

/* _ord_tree():
**
**    Return 1 iff (pig_l pig_r) are in correct tree order (mug).
**
**    Stop iff (pig_l pig_r) are equal.
*/
static c3_t
_ord_tree(u2_noun pig_l, u2_noun pig_r)
{
  c3_w mug_l = u2_mug(pig_l);
  c3_w mug_r = u2_mug(pig_r);

  if ( mug_l == mug_r ) {
    return _ord_simple(pig_l, pig_r);
  }
  else return (mug_l < mug_r);
}

/* u2_fj_pool_ok(): sanity test for pool.
*/
c3_t
u2_fj_pool_ok(u2_pool pool)
{
  if ( (u2_nul == pool) ) {
    return 1;
  }
  else {
    u2_noun pig;
    u2_pool pool_l, pool_r;

    if ( u2_no == u2_as_trel(pool, &pig, &pool_l, &pool_r) ) {
      return 0;
    }
    else {
      if ( !(u2_nul == pool_l) ) {
        u2_noun pig_l = u2_h(pool_l);

        if ( !_ord_tree(pig_l, pig) ) {
          return 0;
        }
        if ( !_ord_prior(pig, pig_l) ) {
          return 0;
        }
      }

      if ( !(u2_nul == pool_r) ) {
        u2_noun pig_r = u2_h(pool_r);

        if ( !_ord_tree(pig, pig_r) ) {
          return 0;
        }
        if ( !_ord_prior(pig, pig_r) ) {
          return 0;
        }
      }
    }
    return u2_fj_pool_ok(pool_l) && u2_fj_pool_ok(pool_r);
  }
}

/* u2_fj_pool_in():
**
**   Return 1 iff (pig) is in (pool).
*/
c3_t
u2_fj_pool_in(u2_noun pig_in,
          u2_pool pool)
{
  c3_w mug_in = u2_mug(pig_in);

  while ( 1 ) {
    if ( (u2_nul == pool) ) {
      return 0;
    }
    else {
      u2_noun pig; 
      u2_pool pool_l, pool_r;

      u2_as_trel(pool, &pig, &pool_l, &pool_r);
      {
        c3_w mug_sub = u2_mug(pig);

        if ( mug_in == mug_sub ) {
          if ( u2_yes == u2_sing(pig_in, pig) ) {
            return 1;
          }
          else pool = _ord_simple(pig_in, pig) ? pool_l : pool_r;
        }
        else {
          pool = (mug_in < mug_sub) ? pool_l : pool_r;
        }
      }
    }
  }
}

/* _pool_fix_l(): fix a pool whose left side has been rebuilt.
*/
static u2_pool
_pool_fix_l(u2_ray  wir_r,
           u2_noun  pig_sub,
           u2_pool  pool_l,
           u2_pool  pool_r)
{
  u2_noun pig_sub_l;
  u2_pool  pool_ll, pool_lr;
  u2_as_trel(pool_l, &pig_sub_l, &pool_ll, &pool_lr);
  {
    if ( _ord_prior(pig_sub, pig_sub_l) ) {
      return u2_bt(wir_r, pig_sub, pool_l, pool_r);
    }
    else {
      /* Rotate to the right.
      */
      return u2_bt(wir_r, pig_sub_l, 
                             pool_ll, 
                             u2_bt(wir_r, pig_sub, pool_lr, pool_r));
    }
  }
}

/* _pool_fix_r(): fix a pool whose right side has been rebuilt.
*/
static u2_pool
_pool_fix_r(u2_ray  wir_r,
           u2_noun  pig_sub,
           u2_pool  pool_l,
           u2_pool  pool_r)
{
  u2_noun pig_sub_r;
  u2_pool pool_rl, pool_rr;
  u2_as_trel(pool_r, &pig_sub_r, &pool_rl, &pool_rr);
  {
    if ( _ord_prior(pig_sub, pig_sub_r) ) {
      return u2_bt(wir_r, pig_sub, pool_l, pool_r);
    }
    else {
      /* Rotate to the left.
      */
      return u2_bt(wir_r, pig_sub_r, 
                             u2_bt(wir_r, pig_sub, pool_l, pool_rl), 
                             pool_rr);
    }
  }
}

/* _pool_add_mug():
**
**   As u2_fj_pool_add().  mug_add is u2_mug(pig_add).
*/
static u2_pool
_pool_add_mug(u2_ray   wir_r,
              u2_noun  pig_add,
              c3_w     mug_add,
              u2_pool  pool)
{
  if ( (u2_nul == pool) ) {
    return u2_bt(wir_r, pig_add, u2_nul, u2_nul);
  }
  else {
    u2_noun pig;
    u2_pool pool_l, pool_r;
    u2_as_trel(pool, &pig, &pool_l, &pool_r);
    {
      c3_w mug = u2_mug(pig);

      if ( mug_add == mug ) {
        if ( u2_yes == u2_sing(pig_add, pig) ) {
          return pool;
        }
        else {
          if ( _ord_simple(pig_add, pig) ) {
            goto left;
          } else goto right;
        }
      }
      else {
        if ( mug_add < mug ) {
          left: {
            u2_pool pool_l_add = _pool_add_mug(wir_r, pig_add, mug_add, pool_l);

            if ( pool_l_add == pool_l ) {
              return pool;
            }
            else {
              return _pool_fix_l(wir_r, pig, pool_l_add, pool_r);
            }
          }
        } else {
          right: {
            u2_pool pool_r_add = _pool_add_mug(wir_r, pig_add, mug_add, pool_r);

            if ( pool_r_add == pool_r ) {
              return pool;
            } 
            else {
              return _pool_fix_r(wir_r, pig, pool_l, pool_r_add);
            }
          }
        }
      }
    }
  }
}

/* u2_fj_pool_add():
**
**   Produce a version of (pool) which includes (pig).
*/
u2_noun
u2_fj_pool_add(u2_ray  wir_r,
               u2_noun pig,
               u2_pool pool)
{
  return _pool_add_mug(wir_r, pig, u2_mug(pig), pool);
}

/* u2_fj_pool_list():
**
**   Convert (pool) to a pseudo-randomly sorted list, 
**   prepending to (list).
*/
u2_list
u2_fj_pool_list(u2_ray  wir_r,
                u2_list list,
                u2_pool pool)
{
  if ( (u2_nul == pool) ) {
    return list;
  }
  else {
    u2_noun pig;
    u2_pool pool_l, pool_r;

    u2_as_trel(pool, &pig, &pool_l, &pool_r);
    {
      list = u2_fj_pool_list(wir_r, list, pool_l);
      list = u2_bc(wir_r, pig, list);
      list = u2_fj_pool_list(wir_r, list, pool_r);
    }
    return list;
  }
}

/* u2_fj_pool_cat():
**
**   Produce a version of (pool_b) which includes all entries
**   in (pool_a).
*/
u2_pool
u2_fj_pool_cat(u2_ray  wir_r,
               u2_pool  pool_a,
               u2_pool  pool_b)
{
  u2_list list = u2_fj_pool_list(wir_r, u2_nul, pool_a);

  while ( !(u2_nul == list) ) {
    pool_b = u2_fj_pool_add(wir_r, u2_h(list), pool_b);
    list = u2_t(list);
  }
  return pool_b;
}


/* u2_fj_pool_at():
**
**   Return path to node of (pig) in (pool), under (axe); or 0.
*/
u2_atom
u2_fj_pool_at(u2_ray  wir_r, 
              u2_noun  pig_in,
              u2_atom axe,
              u2_pool  pool)
{
  c3_w nub_in = u2_mug(pig_in);

  if ( _0 == pool ) {
    return _0;
  }
  else {
    u2_noun pig; 
    u2_pool pool_l, pool_r;

    u2_as_trel(pool, &pig, &pool_l, &pool_r);
    {
      c3_w nub_sub = u2_mug(pig);
      c3_t   t_l;

      if ( nub_in == nub_sub ) {
        if ( u2_yes == u2_sing(pig_in, pig) ) {
          return axe;
        }
        else t_l = _ord_simple(pig_in, pig);
      }
      else t_l = (nub_in < nub_sub);

      if ( t_l ) {
        return u2_fj_pool_at
          (wir_r, pig_in, u2_fj_op_peg(wir_r, axe, _2), pool_l);
      }
      else {
        return u2_fj_pool_at
          (wir_r, pig_in, u2_fj_op_peg(wir_r, axe, _3), pool_r);
      }
    }
  }
}
