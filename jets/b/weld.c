/* j/2/weld.c
**
*/
#include "all.h"


/* functions
*/
  static u3_noun
  _u3_iterative_weld(u3_noun a, 
                     u3_noun b)
  {
    c3_w   i_w, len_w;
    c3_w** ray_w;

    //  Count `a`.
    {
      u3_noun c = a;

      len_w = 0;
      while ( c3y == u3du(c) ) {
        if ( 0xffffffff == len_w ) {
          return u3m_bail(c3__fail);
        }
        len_w++;
        c = u3t(c);
      }
    }

    //  Allocate an array of cells.
    {
      ray_w = u3a_walloc(len_w * c3_wiseof(c3_w *));
      for ( i_w = 0; i_w < len_w; i_w++ ) {
        ray_w[i_w] = u3a_celloc();
      }
    }

    //  Fill in the array.
    {
      for ( i_w = 0; i_w < len_w; i_w++ ) {
        u3a_cell* nov_u = (void *)ray_w[i_w];
        
        nov_u->hed = u3k(u3h(a));
        if ( i_w == (len_w - 1) ) {
          nov_u->tel = u3k(b);
        } 
        else {
          nov_u->tel = u3a_to_pom(u3a_outa(ray_w[i_w + 1]));
        }

        a = u3t(a);
      }
    }

    // Return the result.
    {
      u3_noun pro = u3a_to_pom(u3a_outa(ray_w[0]));

      u3a_wfree(ray_w);
      return pro;
    }
  }

  u3_noun
  u3qb_weld(u3_noun a,
            u3_noun b)
  {
    if ( 0 == a ) {
      return u3k(b);
    }
#if 0
    else if ( c3n == u3du(a) ) {
      return u3m_bail(c3__exit);
    }
    else {
      return u3nc(u3k(u3h(a)), u3qb_weld(u3t(a), b));
    }
#else
    return _u3_iterative_weld(a, b);
#endif
  }
  u3_noun
  u3wb_weld(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3qb_weld(a, b);
    }
  }
  u3_noun
  u3kb_weld(u3_noun a, 
            u3_noun b)
  {
    u3_noun c = u3qb_weld(a, b);

    u3z(a); u3z(b);
    return c;
  }

