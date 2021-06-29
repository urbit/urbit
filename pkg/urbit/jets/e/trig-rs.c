/* jets/e/trig-rs.c
**
*/
#include "all.h"
#include <softfloat.h>  // necessary for working with software-defined floats
#include <stdio.h>      // helpful for debugging, removable after development
#include <math.h>       // provides library fabs() and ceil()

  union sing {
    float32_t s;    //struct containing v, uint_32
    c3_w c;         //uint_32
    float b;        //float_32, compiler-native, useful for debugging printfs
  };

/* ancillary functions
*/
  bool isclose(float a,
               float b)
  {
    float atol = 1e-6;
    return ((float)fabs(a - b) <= atol);
  }

/* factorial of @rs single-precision floating-point value
*/
  u3_noun
  u3qe_trig_factorial(u3_atom u)  /* @rs */
  {
    fprintf(stderr, "u3qe_trig_factorial\n\r");
    union sing a, b, c, e;
    u3_atom bb;
    a.c = u3r_word(0, u);  // extricate value from atom as 32-bit word

    if (ceil(a.b) != a.b) {
      // raise an error if the float has a nonzero fractional part
      return u3m_bail(c3__exit);
    }

    if (isclose(a.b, 0.0)) {
      a.b = (float)1.0;
      return u3i_words(1, &a.c);
    }
    else if (isclose(a.b, 1.0)) {
      a.b = (float)1.0;
      return u3i_words(1, &a.c);
    }
    else {
      // naive recursive algorithm
      b.b = a.b - 1.0;
      bb = u3i_words(1, &b.c);
      c.c = u3r_word(0, u3qe_trig_factorial(bb));
      e.s = f32_mul(a.s, c.s);
      u3m_p("result", u3i_words(1, &e.c));  // DELETE THIS LINE LATER
      return u3i_words(1, &e.c);
    }
  }

  u3_noun
  u3we_trig_factorial(u3_noun cor)
  {
    fprintf(stderr, "u3we_trig_factorial\n\r");
    u3_noun a;

    if ( c3n == u3r_mean(cor, u3x_sam, &a, 0) ||
         c3n == u3ud(a) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      fprintf(stderr, "calling u3qe\n\r");
      return u3qe_trig_factorial(a);
    }
  }

