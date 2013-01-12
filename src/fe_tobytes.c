#include "fe.h"

/*
Preconditions:
  |h| bounded by 1.1*2^26,1.1*2^25,1.1*2^26,1.1*2^25,etc.

Write p=2^255-19; q=floor(h/p).
Basic claim: q = floor(2^(-255)(h + 19 2^(-25)h9 + 2^(-1))).

Proof:
  Have |h|<=p so |q|<=1 so |19^2 2^(-255) q|<1/4.
  Also have |h-2^230 h9|<2^231 so |19 2^(-255)(h-2^230 h9)|<1/4.

  Write y=2^(-1)-19^2 2^(-255)q-19 2^(-255)(h-2^230 h9).
  Then 0<y<1.

  Write r=h-pq.
  Have 0<=r<=p-1=2^255-20.
  Thus 0<=r+19(2^-255)r<r+19(2^-255)2^255<=2^255-1.

  Write x=r+19(2^-255)r+y.
  Then 0<x<2^255 so floor(2^(-255)x) = 0 so floor(q+2^(-255)x) = q.

  Have q+2^(-255)x = 2^(-255)(h + 19 2^(-25) h9 + 2^(-1))
  so floor(2^(-255)(h + 19 2^(-25) h9 + 2^(-1))) = q.
*/

void fe_tobytes(unsigned char *s, const fe h) {
    int32_t h0 = h[0];
    int32_t h1 = h[1];
    int32_t h2 = h[2];
    int32_t h3 = h[3];
    int32_t h4 = h[4];
    int32_t h5 = h[5];
    int32_t h6 = h[6];
    int32_t h7 = h[7];
    int32_t h8 = h[8];
    int32_t h9 = h[9];
    int32_t q;
    int32_t carry0;
    int32_t carry1;
    int32_t carry2;
    int32_t carry3;
    int32_t carry4;
    int32_t carry5;
    int32_t carry6;
    int32_t carry7;
    int32_t carry8;
    int32_t carry9;
    q = (19 * h9 + (((int32_t) 1) << 24)) >> 25;
    q = (h0 + q) >> 26;
    q = (h1 + q) >> 25;
    q = (h2 + q) >> 26;
    q = (h3 + q) >> 25;
    q = (h4 + q) >> 26;
    q = (h5 + q) >> 25;
    q = (h6 + q) >> 26;
    q = (h7 + q) >> 25;
    q = (h8 + q) >> 26;
    q = (h9 + q) >> 25;
    /* Goal: Output h-(2^255-19)q, which is between 0 and 2^255-20. */
    h0 += 19 * q;
    /* Goal: Output h-2^255 q, which is between 0 and 2^255-20. */
    carry0 = h0 >> 26;
    h1 += carry0;
    h0 -= carry0 << 26;
    carry1 = h1 >> 25;
    h2 += carry1;
    h1 -= carry1 << 25;
    carry2 = h2 >> 26;
    h3 += carry2;
    h2 -= carry2 << 26;
    carry3 = h3 >> 25;
    h4 += carry3;
    h3 -= carry3 << 25;
    carry4 = h4 >> 26;
    h5 += carry4;
    h4 -= carry4 << 26;
    carry5 = h5 >> 25;
    h6 += carry5;
    h5 -= carry5 << 25;
    carry6 = h6 >> 26;
    h7 += carry6;
    h6 -= carry6 << 26;
    carry7 = h7 >> 25;
    h8 += carry7;
    h7 -= carry7 << 25;
    carry8 = h8 >> 26;
    h9 += carry8;
    h8 -= carry8 << 26;
    carry9 = h9 >> 25;
    h9 -= carry9 << 25;
    /* h10 = carry9 */
    /*
    Goal: Output h0+...+2^255 h10-2^255 q, which is between 0 and 2^255-20.
    Have h0+...+2^230 h9 between 0 and 2^255-1;
    evidently 2^255 h10-2^255 q = 0.
    Goal: Output h0+...+2^230 h9.
    */
    s[0] = (unsigned char) (h0 >> 0);
    s[1] = (unsigned char) (h0 >> 8);
    s[2] = (unsigned char) (h0 >> 16);
    s[3] = (unsigned char) ((h0 >> 24) | (h1 << 2));
    s[4] = (unsigned char) (h1 >> 6);
    s[5] = (unsigned char) (h1 >> 14);
    s[6] = (unsigned char) ((h1 >> 22) | (h2 << 3));
    s[7] = (unsigned char) (h2 >> 5);
    s[8] = (unsigned char) (h2 >> 13);
    s[9] = (unsigned char) ((h2 >> 21) | (h3 << 5));
    s[10] = (unsigned char) (h3 >> 3);
    s[11] = (unsigned char) (h3 >> 11);
    s[12] = (unsigned char) ((h3 >> 19) | (h4 << 6));
    s[13] = (unsigned char) (h4 >> 2);
    s[14] = (unsigned char) (h4 >> 10);
    s[15] = (unsigned char) (h4 >> 18);
    s[16] = (unsigned char) (h5 >> 0);
    s[17] = (unsigned char) (h5 >> 8);
    s[18] = (unsigned char) (h5 >> 16);
    s[19] = (unsigned char) ((h5 >> 24) | (h6 << 1));
    s[20] = (unsigned char) (h6 >> 7);
    s[21] = (unsigned char) (h6 >> 15);
    s[22] = (unsigned char) ((h6 >> 23) | (h7 << 3));
    s[23] = (unsigned char) (h7 >> 5);
    s[24] = (unsigned char) (h7 >> 13);
    s[25] = (unsigned char) ((h7 >> 21) | (h8 << 4));
    s[26] = (unsigned char) (h8 >> 4);
    s[27] = (unsigned char) (h8 >> 12);
    s[28] = (unsigned char) ((h8 >> 20) | (h9 << 6));
    s[29] = (unsigned char) (h9 >> 2);
    s[30] = (unsigned char) (h9 >> 10);
    s[31] = (unsigned char) (h9 >> 18);
}
