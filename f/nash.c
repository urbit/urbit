/* f/nash.c
**
** This file is in the public domain.
*/
#include "all.h"

struct u2_nair {
  u2_noun key;
  u2_noun val;
};

struct u2_buck {
  c3_w            con_w;
  struct u2_nair* sto_u;
};

struct u2_nash {
  c3_w            cap_w;
  struct u2_buck* sto_u;
};

/* u2_na_make(): create a new nounhash-table.
**
** nashtables live in C memory and do not take refs.
*/
struct u2_nash*
u2_na_make()
{
  struct u2_nash* nas_u = c3_malloc(sizeof(struct u2_nash));
  nas_u->cap_w = 521;
  nas_u->sto_u = calloc(nas_u->cap_w, sizeof(struct u2_buck));
  c3_assert(nas_u->sto_u);
  //  fprintf(stderr, "[%%nash-make %p]\r\n", nas_u);
  return nas_u;
}

/* u2_na_put(): put into nash, replacing.
**/
void
u2_na_put(struct u2_nash* nash, u2_noun key, u2_noun val)
{
  struct u2_buck* buc_u = &(nash->sto_u[u2_mug(key) % nash->cap_w]);

  struct u2_nair* nuu_u;
  c3_w     sot_w;
  c3_w     i;

  if ( 0 == buc_u->con_w ) {
    c3_assert(buc_u->sto_u == 0);
  }
  else {
    for(i = 0; i < buc_u->con_w; i++) {
      if (u2_sing(buc_u->sto_u[i].key, key) == u2_yes) {
        buc_u->sto_u[i].val = val;
#if 0
        fprintf(stderr, "[%%nash-rep %p %p %d]\r\n",
                (void*)key, (void*)val, i);
#endif
        return;
      }
    }
  }

  sot_w = buc_u->con_w;
  buc_u->con_w++;

  nuu_u = realloc(buc_u->sto_u, buc_u->con_w * sizeof(struct u2_nair));
  c3_assert(nuu_u);

  nuu_u[sot_w].key = key;
  nuu_u[sot_w].val = val;
#if 0
  fprintf(stderr, "[%%nash-put %p %p %d]\r\n",
          (void*)key, (void*)val, sot_w);
#endif
  buc_u->sto_u = nuu_u;
}

/* u2_na_get(): get from a nounhash table
**/
u2_weak
u2_na_get(struct u2_nash* nash, u2_noun key)
{
  struct u2_buck* buc_u = &(nash->sto_u[u2_mug(key) % nash->cap_w]);
  c3_w i;
  for(i = 0; i < buc_u->con_w; i++) {
    if (u2_sing(buc_u->sto_u[i].key, key) == u2_yes) {
#if 0
      fprintf(stderr, "[%%nash-get %p %p %d]\r\n",
              (void*)key, (void*)buc_u->sto_u[i].val, i);
#endif
      return buc_u->sto_u[i].val;
    }
  }
  return 0;
}

/* u2_na_take(): destroy a nounhash table
**/
void
u2_na_take(struct u2_nash* nash)
{
  c3_w i;
  for(i=0; i < nash->cap_w; i++) {
#if 0
    fprintf(stderr, "%s%d%s", nash->sto_u[i].con_w,
            0==i? "[%%nash-pop " :"",
            i+1==nash->cap_w? "]\r\n" :" ");
#endif
    free(nash->sto_u[i].sto_u);
  }
  free(nash->sto_u);
  free(nash);
  //  fprintf(stderr, "[%%nash-take %p]\r\n", nash);
}
