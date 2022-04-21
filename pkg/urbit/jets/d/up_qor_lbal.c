#include "all.h"

u3_noun
_lrdub(u3_noun n_a, u3_noun l_a, u3_noun m_a, u3_noun r_a)
{
  if ( c3n == u3du(l_a) ) {
    return u3m_bail(c3__exit);
  }

  u3_noun p_l_a = u3t(u3t(l_a));
  u3_noun hol = u3h(l_a);

  if ( c3n == u3ud(hol) ) {
    return u3m_bail(c3__exit);
  }
  else switch ( hol ) {
    default:
      return u3m_bail(c3__exit);

    case c3__llos: {
      u3_noun n_p_l_a, l_p_l_a, m_p_l_a, r_p_l_a;
      u3x_qual(p_l_a, &n_p_l_a, &l_p_l_a, &m_p_l_a, &r_p_l_a);

      u3_noun pre = u3qdu_qor_llsin(n_p_l_a, l_p_l_a, m_p_l_a, r_p_l_a);
      u3_noun pro = u3qdu_qor_lrsin(n_a, pre, m_a, r_a);

      u3z(pre);

      return pro;
    }

    case c3__rlos: {
      u3_noun n_p_l_a, l_p_l_a, m_p_l_a, r_p_l_a;
      u3x_qual(p_l_a, &n_p_l_a, &l_p_l_a, &m_p_l_a, &r_p_l_a);

      u3_noun pre = u3qdu_qor_rlsin(n_p_l_a, l_p_l_a, m_p_l_a, r_p_l_a);
      u3_noun pro = u3qdu_qor_lrsin(n_a, pre, m_a, r_a);

      u3z(pre);

      return pro;
    }
  }
}

u3_noun
_lldub(u3_noun n_a, u3_noun l_a, u3_noun m_a, u3_noun r_a)
{
  if ( c3n == u3du(r_a) ) {
    return u3m_bail(c3__exit);
  }

  u3_noun p_r_a = u3t(u3t(r_a));
  u3_noun hor = u3h(r_a);

  if ( c3n == u3ud(hor) ) {
    return u3m_bail(c3__exit);
  }
  else switch ( hor ) {
    default:
      return u3m_bail(c3__exit);

    case c3__llos: {
      u3_noun n_p_r_a, l_p_r_a, m_p_r_a, r_p_r_a;
      u3x_qual(p_r_a, &n_p_r_a, &l_p_r_a, &m_p_r_a, &r_p_r_a);

      u3_noun pre = u3qdu_qor_lrsin(n_p_r_a, l_p_r_a, m_p_r_a, r_p_r_a);
      u3_noun pro = u3qdu_qor_llsin(n_a, l_a, m_a, pre);

      u3z(pre);

      return pro;
    }

    case c3__rlos: {
      u3_noun n_p_r_a, l_p_r_a, m_p_r_a, r_p_r_a;
      u3x_qual(p_r_a, &n_p_r_a, &l_p_r_a, &m_p_r_a, &r_p_r_a);

      u3_noun pre = u3qdu_qor_rrsin(n_p_r_a, l_p_r_a, m_p_r_a, r_p_r_a);
      u3_noun pro = u3qdu_qor_llsin(n_a, l_a, m_a, pre);

      u3z(pre);

      return pro;
    }
  }
}

u3_noun
_lrbal(u3_noun n, u3_noun l, u3_noun m, u3_noun r)
{
  if ( c3n == u3du(l) ) {
    return u3m_bail(c3__exit);
  }

  u3_noun p_l = u3t(u3t(l));

  u3_noun n_p_l, l_p_l, m_p_l, r_p_l;
  u3x_qual(p_l, &n_p_l, &l_p_l, &m_p_l, &r_p_l);

  u3_atom sl = u3qdu_qor_size(l_p_l);
  u3_atom sr = u3qdu_qor_size(r_p_l);

  c3_o comp = u3qa_gth(sl, sr);

  u3z(sl);
  u3z(sr);

  return ( c3y == comp ) ? u3qdu_qor_lrsin(n, l, m, r) : _lrdub(n, l, m, r);
}

u3_noun
_llbal(u3_noun n, u3_noun l, u3_noun m, u3_noun r)
{
  if ( c3n == u3du(r) ) {
    return u3m_bail(c3__exit);
  }

  u3_noun p_r = u3t(u3t(r));

  u3_noun n_p_r, l_p_r, m_p_r, r_p_r;
  u3x_qual(p_r, &n_p_r, &l_p_r, &m_p_r, &r_p_r);

  u3_atom sl = u3qdu_qor_size(l_p_r);
  u3_atom sr = u3qdu_qor_size(r_p_r);

  c3_o comp = u3qa_lth(sl, sr);

  u3z(sl);
  u3z(sr);

  return ( c3y == comp ) ? u3qdu_qor_llsin(n, l, m, r) : _lldub(n, l, m, r);
}

u3_noun
u3qdu_qor_lbal(u3_noun n_a, u3_noun l_a, u3_noun m_a, u3_noun r_a)
{
  u3_atom sl = u3qdu_qor_size(l_a);
  u3_atom sr = u3qdu_qor_size(r_a);
  u3_atom s  = u3qa_add(sl, sr);

  if ( c3y == u3qa_lth(s, 2) ) {

    u3z(sl);
    u3z(sr);
    u3z(s);

    return u3qdu_qor_llos(n_a, l_a, m_a, r_a);
  }
  else {
    u3z(s);

    u3_atom sm_l = u3qa_mul(4, sl);

    if ( c3y == u3qa_gth(sr, sm_l) ) {
      u3z(sl);
      u3z(sr);
      u3z(sm_l);

      return _llbal(n_a, l_a, m_a, r_a);
    }
    else {
      u3z(sr);
      u3z(sm_l);

      u3_atom sm_r = u3qa_mul(4, sr);

      if ( c3y == u3qa_gth(sl, sm_r) ) {
        u3z(sl);
        u3z(sm_r);

        return _lrbal(n_a, l_a, m_a, r_a);
      }
      else {
        u3z(sl);
        u3z(sm_r);

        return u3qdu_qor_llos(n_a, l_a, m_a, r_a);
      }
    }
  }
}

u3_noun
u3wdu_qor_lbal(u3_noun cor)
{
  u3_noun a;

  if ( (c3n == u3r_mean(cor, u3x_sam, &a, 0 )) ||
       (c3n == u3du(a)) )
  {
    return u3m_bail(c3__exit);
  } else {
    u3_noun n, l, m, r;
    u3x_qual(a, &n, &l, &m, &r);

    if ( (c3n == u3du(n)) || (c3n == u3ud(m)) ) {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qdu_qor_lbal(n, l, m, r);
    }
  }
}

