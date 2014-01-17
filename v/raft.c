/* v/raft.c
**
** This file is in the public domain.
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <uv.h>

#include "all.h"
#include "v/vere.h"

/* _raft_readname(): parse a raft host:port peer name.
*/
u2_bean
_raft_readname(u2_ropt* rop_u, const c3_c* str_c, c3_w siz_w)
{
  u2_rnam* nam_u = malloc(sizeof(*nam_u));
  c3_c*    col_c;
  c3_w     por_w;
  c3_w     nam_w;

  nam_u->str_c = malloc(siz_w + 1);
  strncpy(nam_u->str_c, str_c, siz_w);
  nam_u->str_c[siz_w] = '\0';
  //fprintf(stderr, "raft: peer %s\n", nam_u->str_c);

  if ( 0 == (col_c = strchr(nam_u->str_c, ':')) ) {
    fprintf(stderr, "raft: invalid name %s\n", str_c);
    return u2_no;
  }
  else {
    nam_w = col_c - nam_u->str_c;
    nam_u->nam_c = malloc(nam_w + 1);
    strncpy(nam_u->nam_c, nam_u->str_c, nam_w);
    nam_u->nam_c[nam_w] = '\0';

    por_w = atol(col_c + 1);
    if ( !(por_w > 0 && por_w < 65536) ) {
      fprintf(stderr, "raft: invalid port '%s'\n", col_c + 1);
      return u2_no;
    }
    else nam_u->por_s = por_w;
    //fprintf(stderr, "raft: peer %s:%d\n", nam_u->nam_c, nam_u->por_s);

    nam_u->nex_u = rop_u->nam_u;
    rop_u->nam_u = nam_u;
    return u2_yes;
  }
}

u2_bean
u2_raft_readopt(u2_ropt* rop_u, const c3_c* arg_c)
{
  c3_c* com_c;

  while ( 0 != (com_c = strchr(arg_c, ',')) ) {
    if ( u2_no == _raft_readname(rop_u, arg_c, com_c - arg_c) ) {
      return u2_no;
    } else arg_c = com_c + 1;
  }
  return _raft_readname(rop_u, arg_c, strlen(arg_c));
}
