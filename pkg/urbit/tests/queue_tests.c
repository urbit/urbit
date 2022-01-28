#include "c/queue.h"

#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define len(arr) (sizeof(arr) / sizeof(arr[0]))

static void
_test(void)
{
  c3_queue* que_u = c3_queue_init();
  c3_node* nod_u;
  static int32_t val_ws[] = {1, 2, 4, 8, 16};
  int32_t i_ws;
  int32_t v_ws;

  
  // Push elements onto the back of the queue.
  for ( i_ws = 0; i_ws < len(val_ws); i_ws++ ) {
    v_ws = val_ws[i_ws];
    nod_u = c3_queue_push_back(que_u, &v_ws, sizeof(v_ws));
    assert(v_ws == *(int32_t*)nod_u->dat_v);
    assert(sizeof(v_ws) == nod_u->siz_i);
  }

  // Peek front element.
  nod_u = c3_queue_peek_front(que_u);
  assert(val_ws[0] == *(int32_t*)nod_u->dat_v);

  // Peek back element.
  nod_u = c3_queue_peek_back(que_u);
  assert(val_ws[len(val_ws)-1] == *(int32_t*)nod_u->dat_v);

  // Push elements onto the front of the queue.
  for ( i_ws = 0; i_ws < len(val_ws); i_ws++ ) {
    v_ws = val_ws[i_ws];
    nod_u = c3_queue_push_front(que_u, &v_ws, sizeof(v_ws));
    assert(v_ws == *(int32_t*)nod_u->dat_v);
    assert(sizeof(v_ws) == nod_u->siz_i);
  }

  // Peek front element.
  nod_u = c3_queue_peek_front(que_u);
  assert(val_ws[len(val_ws)-1] == *(int32_t*)nod_u->dat_v);

  // Pop elements from the back of the queue.
  for ( i_ws = len(val_ws)-1; i_ws >= 0; i_ws-- ) {
    nod_u = c3_queue_pop_back(que_u);
    v_ws = val_ws[i_ws];
    assert(v_ws == *(int32_t*)nod_u->dat_v);
    assert(sizeof(v_ws) == nod_u->siz_i);
    free(nod_u->dat_v);
    free(nod_u);
  }

  // Pop elements from the front of the queue.
  for ( i_ws = len(val_ws)-1; i_ws >= 0; i_ws-- ) {
    v_ws = val_ws[i_ws];
    nod_u = c3_queue_pop_front(que_u);
    assert(v_ws == *(int32_t*)nod_u->dat_v);
    assert(sizeof(v_ws) == nod_u->siz_i);
    free(nod_u->dat_v);
    free(nod_u);
  }

  // Verify queue is empty.
  assert(NULL == c3_queue_peek_back(que_u));
  assert(NULL == c3_queue_peek_front(que_u));

  c3_queue_free(que_u);
}

int
main(int argc, char* argv[])
{
  _test();

  fprintf(stderr, "test_queue: ok\n");

  return 0;
}

#undef len
