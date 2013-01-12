#ifndef FE_H
#define FE_H

#include "pstdint.h"


/*
    fe means field element.
    Here the field is \Z/(2^255-19).
    An element t, entries t[0]...t[9], represents the integer
    t[0]+2^26 t[1]+2^51 t[2]+2^77 t[3]+2^102 t[4]+...+2^230 t[9].
    Bounds on each t[i] vary depending on context.
*/


typedef int32_t fe[10];


void fe_0(fe h);
void fe_1(fe h);

void fe_frombytes(fe h, const unsigned char *s);

void fe_copy(fe h, const fe f);
int fe_isnegative(const fe f);
int fe_isnonzero(const fe f);
void fe_cmov(fe f, const fe g, unsigned int b);

void fe_add(fe h, const fe f, const fe g);
void fe_invert(fe out, const fe z);
void fe_mul(fe h, const fe f, const fe g);










void fe_tobytes(unsigned char *,const fe);

extern int fe_isnonzero(const fe);
extern void fe_cswap(fe,fe,unsigned int);

extern void fe_sub(fe,const fe,const fe);
extern void fe_neg(fe,const fe);
extern void fe_sq(fe,const fe);
extern void fe_sq2(fe,const fe);
extern void fe_mul121666(fe,const fe);
extern void fe_pow22523(fe,const fe);

#endif
