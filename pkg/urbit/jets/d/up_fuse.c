#include "all.h"
#include <stdio.h> // FIXME

u3_noun
u3qdu_fuse(u3_atom m, u3_noun l, u3_noun r)
{
  if ( u3_nul == l ) {
    printf("we're alive 0\n");
    return u3k(r);
  }
  else {
    u3_noun hol = u3h(l);

    // FIXME print value of hol here ?

    if ( c3n == u3du(hol) )  {
      printf("we're about to die\n");
      return u3m_bail(c3__exit);
    }
    else switch ( hol ) {
      default:
        return u3m_bail(c3__exit);

      case c3__tip: {
        if ( u3_nul == r ) {
          printf("we're alive 1\n");
          return u3k(l);
        }
        else {
          u3_noun hor = u3h(r);

          if ( c3n == u3du(hor) )  {
            return u3m_bail(c3__exit);
          }
          else switch ( hor ) {
            default:
              return u3m_bail(c3__exit);

            case c3__tip: {

              printf("we're alive 2\n");

              u3_noun p_l, k_l, v_l;
              u3_noun p_r, k_r, v_r;

              u3x_trel(u3t(l), &k_l, &p_l, &v_l);
              u3x_trel(u3t(r), &k_r, &p_r, &v_r);

              if ( c3y == u3qdu_lex(p_l, k_l, p_r, k_r) ) {
                return u3nq(c3__bin, u3k(k_l), u3k(p_l),
                         u3nq(u3k(v_l), u3k(m), u3_nul, u3k(r)));
              }
              else {
                return u3nq(c3__bin, u3k(k_r), u3k(p_r),
                         u3nq(u3k(v_r), u3k(m), u3k(l), u3_nul));
              }
            }

            case c3__bin: {
              printf("we're alive 3\n");

              u3_noun p_l, k_l, v_l;
              u3_noun p_r, k_r, v_r, m_r, l_r, r_r;
              u3_noun q_r;

              u3x_trel(u3t(l), &k_l, &p_l, &v_l);
              u3x_qual(u3t(r), &k_r, &p_r, &v_r, &q_r);
              u3x_trel(q_r, &m_r, &l_r, &r_r);

              if ( c3y == u3qdu_lex(p_l, k_l, p_r, k_r) ) {
                return u3nq(c3__bin, u3k(k_l), u3k(p_l),
                         u3nq(u3k(v_l), u3k(m), u3_nul, u3k(r)));
              }
              else {
                return u3nq(c3__bin, u3k(k_r), u3k(p_r),
                         u3nq(u3k(v_r), u3k(m), u3k(l),
                           u3qdu_fuse(m_r, l_r, r_r)));
              }
            }
          }
        }
      }

      case c3__bin: {
        if ( u3_nul == r ) {
          printf("we're alive 4\n");
          return u3k(l);
        }
        else {
          u3_noun hor = u3h(r);

          if ( c3n == u3du(hor) )  {
            return u3m_bail(c3__exit);
          }
          else switch ( hor ) {
            default:
              return u3m_bail(c3__exit);

            case c3__tip: {
              printf("we're alive 5\n");

              u3_noun p_l, k_l, v_l, m_l, l_l, r_l;
              u3_noun p_r, k_r, v_r;
              u3_noun q_l;

              u3x_trel(u3t(r), &k_r, &p_r, &v_r);
              u3x_qual(u3t(l), &k_l, &p_l, &v_l, &q_l);
              u3x_trel(q_l, &m_l, &l_l, &r_l);

              if ( c3y == u3qdu_lex(p_l, k_l, p_r, k_r) ) {
                return u3nq(c3__bin, u3k(k_l), u3k(p_l),
                         u3nq(u3k(v_l), u3k(m),
                            u3qdu_fuse(m_l, l_l, r_l), u3k(r)));
              }
              else {
                return u3nq(c3__bin, u3k(k_r), u3k(p_r),
                         u3nq(u3k(v_r), u3k(m), u3k(l), u3_nul));
              }
            }

            case c3__bin: {
              printf("we're alive 6\n");

              u3_noun p_l, k_l, v_l, m_l, l_l, r_l;
              u3_noun p_r, k_r, v_r, m_r, l_r, r_r;
              u3_noun q_l;
              u3_noun q_r;

              u3x_qual(u3t(l), &k_l, &p_l, &v_l, &q_l);
              u3x_qual(u3t(r), &k_r, &p_r, &v_r, &q_r);
              u3x_trel(q_l, &m_l, &l_l, &r_l);
              u3x_trel(q_r, &m_r, &l_r, &r_r);

              if ( c3y == u3qdu_lex(p_l, k_l, p_r, k_r) ) {
                return u3nq(c3__bin, u3k(k_l), u3k(p_l),
                         u3nq(u3k(v_l), u3k(m),
                           u3qdu_fuse(m_l, l_l, r_l), u3k(r)));
              }
              else {
                return u3nq(c3__bin, u3k(k_r), u3k(p_r),
                         u3nq(u3k(v_r), u3k(m), u3k(l),
                           u3qdu_fuse(m_r, l_r, r_r)));
              }
            }
          }
        }
      }
    }
  }
}

u3_noun
u3wdu_fuse(u3_noun cor)
{
  u3_noun m, l, r;

  if (
    (c3n == u3r_mean(cor, u3x_sam_2, &m, u3x_sam_6, &l, u3x_sam_7, &r, 0 )) ||
    (c3n == u3ud(m)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_fuse(m, l, r);
  }
}

