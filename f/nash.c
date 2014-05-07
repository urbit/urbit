/* f/nash.c
**
** This file is in the public domain.
*/
#include "all.h"
#include <bitmapped_patricia_tree.h>

struct u2_nair {
  u2_noun key;
  u2_noun val;
};

struct u2_buck {
  c3_w            con_w;
  struct u2_nair* sto_u;
};

struct u2_nash {
  bpt_t sto;
};

void u2_na_dump(struct u2_nash* nash);

/* u2_na_make(): create a new nounhash-table.
**
** nashtables live in C memory and do not take refs.
*/
struct u2_nash*
u2_na_make()
{
  struct u2_nash* nas_u = calloc(1, sizeof(*nas_u));
  //  fprintf(stderr, "[%%nash-make %p]\r\n", nas_u);
  return nas_u;
}

/* u2_na_put(): put into nash, replacing.
**/
void
u2_na_put(struct u2_nash* nash, u2_noun key, u2_noun val)
{
  struct u2_buck* buc_u = 0;
  struct u2_nair* nuu_u = 0;
  c3_w     sot_w = 0;
  c3_w     i = 0;

  u2_noun tom = u2_mug(key);
  
  if ( !bpt_has_key(nash->sto, tom)) {
    bpt_t ots;
    buc_u = calloc(1, sizeof(*buc_u));
    ots = bpt_assoc(nash->sto, tom, buc_u);
    bpt_release(nash->sto);
    nash->sto = ots;
#if 0
    fprintf(stderr, "[%%nash-sto %p %p]\r\n", nash->sto, tom);
    if (!bpt_has_key(nash->sto, tom)) {
      u2_na_dump(nash);
      assert(0);
    }
#endif
  }

  buc_u = bpt_get(nash->sto, tom);

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
  buc_u->sto_u = nuu_u;
#if 0
  fprintf(stderr, "[%%nash-put %p %p %d]\r\n",
          (void*)key, (void*)val, sot_w);
#endif
}

/* u2_na_get(): get from a nounhash table
**/
u2_weak
u2_na_get(struct u2_nash* nash, u2_noun key)
{
  struct u2_buck* buc_u = 0;
  c3_w i;
  u2_noun tom = u2_mug(key);
  if ( !bpt_has_key(nash->sto, tom) ) {
    //  fprintf(stderr, "[%%nash-get-none %p %p]\r\n", nash->sto, tom);
    return u2_none;
  }
    
  buc_u = bpt_get(nash->sto, tom);
  for(i = 0; i < buc_u->con_w; i++) {
    if (u2_sing(buc_u->sto_u[i].key, key) == u2_yes) {
#if 0
      fprintf(stderr, "[%%nash-get %p %p %d]\r\n",
              (void*)key, (void*)buc_u->sto_u[i].val, i);
#endif
      return buc_u->sto_u[i].val;
    }
  }
  return u2_none;
}

static
void
_na_drop(bpt_key_t x, void* a, void* b)
{
  struct u2_buck* buc = a;
  free(buc->sto_u);
  free(buc);
}

void
_na_dump(bpt_key_t x, void* a, void* b)
{
  struct u2_buck* buc = a;
  int i;
  fprintf(stderr, "[%%nash-dump %x ", x);
  for(i=0;i<buc->con_w;i++) {
    fprintf(stderr, "%x->%x%s", buc->sto_u[i].key, buc->sto_u[i].val,
            i+1==buc->con_w?"":" ");
  }
  fprintf(stderr, "]\r\n");
}

void u2_na_dump(struct u2_nash* nash)
{
  if(nash->sto) bpt_for_mappings(nash->sto, _na_dump, 0);
}

/* u2_na_take(): destroy a nounhash table
**/
void
u2_na_take(struct u2_nash* nash)
{
  bpt_for_mappings(nash->sto, _na_drop, 0);
  bpt_release(nash->sto);
  free(nash);
  //  fprintf(stderr, "[%%nash-take %p]\r\n", nash);
}
