// -*- mode: c; coding: utf-8 -*- */
//
// Copyright 2010, 2011, Matthias Andreas Benkard.
//
//-----------------------------------------------------------------------------
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//-----------------------------------------------------------------------------
//

// An implementation of a bitmapped Patricia tree.

//// Purpose ////
//
// The idea is to use a locally mutable, bitmapped Patricia tree as a
// variable binding store (i.e. environment) in compiled code.  In this
// way, there is no need for excessive copying when an independent
// environment must be set up (such as when initiating the processing of
// a new node in the search space).  Instead, significant amounts of
// structure can be shared between child and parent environments.

//// Motivation ////
//
// 1. Patricia trees are very amenable to structure sharing.
//
// 2. Furthermore, big-endian Patricia trees are especially efficient
//    when indices are allocated sequentially, as is the case for
//    variables in code emitted by our compiler.
//
// 3. Finally, bitmapping improves the performance of copying because
//    copying an array is much cheaper than copying an equivalent branch
//    in a tree.  As we need to shallow-copy the tree at potentially
//    each choice point, copying needs to be fast.


#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "bitmapped_patricia_tree.h"

#ifndef BPT_EXPLICIT_CONFIGURATION
#define CHUNK_LENGTH 5
#define KEY_LENGTH 32
#define OFFSET_MASK 0x1ffff  //((1 << chunk_length) - 1)
#define MAX_CHUNKS 7         //key_length / chunk_length + ((key_length % chunk_length == 0) ? 0 : 1)
#define LAST_CHUNK_LENGTH 2  //key_length - ((max_chunks - 1) * chunk_length)
#endif  //!BPT_EXPLICIT_CONFIGURATION

typedef struct bpt_nonempty *bpt_nonempty_t;
typedef struct bpt_node *bpt_node_t;
typedef struct bpt_leaf *bpt_leaf_t;

struct bpt {
  enum bpt_tag tag;
  int refcount;
  bool mutable;
  bpt_key_t prefix;
};

struct bpt_leaf {
  struct bpt bpt;  // poor man's inheritance
  void *value;
#ifdef BPT_ENABLE_DEALLOC_HOOKS
  void (*dealloc_hook)(bpt_key_t, void *);   // not actually used anywhere in client code
#endif
};

struct bpt_node {
  struct bpt bpt;  // poor man's inheritance
  unsigned int branching_chunk;
  bpt_key_bitmask_t bitmask;
  bpt_t *children;
};


// Forward declarations.
void init_bpt_leaf(bpt_t leaf, bpt_key_t key, void *value);
bpt_t bpt_make_leaf(bpt_key_t key, void *value);


// Boilerplate definitions.
void bpt_retain0(bpt_t bpt, void *user_data) {
  bpt_retain(bpt);
}

void bpt_seal0(bpt_t bpt, void *user_data) {
  bpt_seal(bpt);
}

void bpt_release0(bpt_t bpt, void *user_data) {
  bpt_release(bpt);
}


// Implementation.
void init_bpt_leaf(bpt_t a_leaf, bpt_key_t key, void *value) {
  bpt_leaf_t leaf = (bpt_leaf_t)a_leaf;
  leaf->bpt.tag = BPT_LEAF;
  leaf->bpt.mutable = true;
  leaf->bpt.prefix = key;
  leaf->value = value;
#ifdef BPT_ENABLE_DEALLOC_HOOKS
  leaf->dealloc_hook = NULL;
#endif
  leaf->bpt.refcount = 1;
}

void init_bpt_node(bpt_node_t node, bpt_key_t prefix, unsigned int branching_chunk) {
  node->bpt.tag = BPT_INNER_NODE;
  node->bpt.mutable = true;
  node->bpt.prefix = prefix;
  node->branching_chunk = branching_chunk;
  node->bitmask = 0;
  node->children = NULL;
  node->bpt.refcount = 1;
}


bpt_t bpt_make_leaf(bpt_key_t key, void *value) {
  bpt_leaf_t leaf = malloc(sizeof *leaf);
  init_bpt_leaf((bpt_t)leaf, key, value);
  return (bpt_t)leaf;
}

bpt_node_t bpt_make_node(bpt_key_t prefix, unsigned int branching_chunk) {
  bpt_node_t node = malloc(sizeof *node);
  init_bpt_node(node, prefix, branching_chunk);
  return node;
}


static inline unsigned int bpt_number_of_leading_zeros(bpt_key_t x);
static inline unsigned int bpt_number_of_trailing_zeros(bpt_key_t x);
static inline unsigned int bpt_popcount(bpt_key_bitmask_t key);
static unsigned int bpt_compute_child_index(bpt_key_bitmask_t bitmask, unsigned int child_number);
static inline uint_fast8_t bpt_offset_of_key(bpt_key_t key, unsigned int branching_chunk);
static bpt_key_t bpt_prefix_of_key(bpt_key_t key, unsigned int branching_chunk);
static inline unsigned int bpt_branching_chunk(bpt_t bpt);
static unsigned int bpt_find_diverging_chunk(bpt_key_t key1, bpt_key_t key2);
static void bpt_for_children(bpt_t bpt, void (*thunk)(bpt_t, void*), void *user_data);


static void bpt_for_children(bpt_t bpt, void (*thunk)(bpt_t, void*), void *user_data) {
  if (bpt && bpt->tag == BPT_INNER_NODE) {
    bpt_node_t b = (bpt_node_t)bpt;
    bpt_t *iter = b->children;
    bpt_t *children_end = b->children + bpt_popcount(b->bitmask);
    while (iter < children_end) {
      thunk(*iter, user_data);
      iter++;
    }
  }
}

void *bpt_get(bpt_t bpt, bpt_key_t key) {
  void **pointer = bpt_get_pointer(bpt, key);
  if (pointer) {
    return *pointer;
  } else {
    return NULL;
  }
}

bpt_leaf_t bpt_get_leaf(bpt_t bpt, bpt_key_t key)
{
  if (!bpt) {
    return NULL;
  } else if (bpt->tag == BPT_LEAF) {
    bpt_leaf_t b = (bpt_leaf_t)bpt;
    if (bpt->prefix == key) {
      return b;
    } else {
      return NULL;
    }
  } else {
    bpt_node_t b = (bpt_node_t)bpt;
    int child_number = bpt_offset_of_key(key, b->branching_chunk);
    if ((1 << child_number) & b->bitmask) {
      int child_index = bpt_compute_child_index(b->bitmask, child_number);
      return bpt_get_leaf(b->children[child_index], key);
    } else {
      return NULL;
    }
  }
}

void **bpt_get_pointer(bpt_t bpt, bpt_key_t key)
{
  bpt_leaf_t leaf = bpt_get_leaf(bpt, key);
  if (!leaf) {
    return NULL;
  } else {
    return &leaf->value;
  }
}

bool bpt_has_key(bpt_t bpt, bpt_key_t key) {
  return (bpt_get_leaf(bpt, key) != NULL);
}

bpt_t bpt_assoc(bpt_t bpt, bpt_key_t key, void *value) {
  if (!bpt) {
    return (bpt_t)bpt_make_leaf(key, value);
  } else {
    bpt_key_t prefix = bpt->prefix;
    if (bpt_prefix_of_key(key, bpt_branching_chunk(bpt)) != prefix) {
      unsigned int diverging_chunk = bpt_find_diverging_chunk(key, prefix);
      bpt_key_t my_number_in_parent = bpt_offset_of_key(prefix, diverging_chunk);
      bpt_key_t their_number_in_parent = bpt_offset_of_key(key, diverging_chunk);
      bpt_node_t new_node = bpt_make_node(bpt_prefix_of_key(prefix, diverging_chunk), diverging_chunk);
      new_node->bitmask = (1 << my_number_in_parent) | (1 << their_number_in_parent);
      new_node->children = malloc(sizeof (*new_node->children) * 2);
      if (my_number_in_parent < their_number_in_parent) {
        new_node->children[0] = bpt;
        new_node->children[1] = bpt_make_leaf(key, value);
      } else {
        new_node->children[0] = bpt_make_leaf(key, value);
        new_node->children[1] = bpt;
      }
      bpt_retain(bpt);
      return (bpt_t)new_node;
    } else {
      if (bpt->tag == BPT_LEAF) {
        bpt_leaf_t b = (bpt_leaf_t)bpt;
        if (bpt->mutable) {
          b->value = value;
          bpt_retain(bpt);
          return bpt;
        } else {
          return (bpt_t)bpt_make_leaf(key, value);
        }
      } else {
        bpt_node_t b = (bpt_node_t)bpt;
        uint_fast8_t child_number = bpt_offset_of_key(key, b->branching_chunk);
        unsigned int child_index = bpt_compute_child_index(b->bitmask, child_number);
        if ((1 << child_number) & b->bitmask) {
          // We already have a child to pass the value to.  Do that.
          bpt_t child = b->children[child_index];
          bpt_t new_child = bpt_assoc(child, key, value);
          if (new_child == child) {
            bpt_release(child);
            bpt_retain(bpt);
            return bpt;
          } else {
            if (bpt->mutable) {
              bpt_release(child);
              b->children[child_index] = new_child;
              bpt_retain(bpt);
              return bpt;
            } else {
              bpt_node_t new_node = malloc(sizeof *new_node);
              *new_node = *b;
              new_node->bpt.refcount = 1;
              new_node->bpt.mutable = true;
              unsigned int number_of_children = bpt_popcount(b->bitmask);
              size_t size_of_child_array = sizeof (*new_node->children) * number_of_children;
              new_node->children = malloc(size_of_child_array);
              memcpy(new_node->children, b->children, size_of_child_array);
              new_node->children[child_index] = new_child;
              // Retain the children copied into the new node.
              bpt_for_children((bpt_t)new_node, bpt_retain0, NULL);
              bpt_release(new_child);
              return (bpt_t)new_node;
            }
          }
        } else {
          // Create a new child.
          unsigned int number_of_children = bpt_popcount(b->bitmask);
          size_t new_size_of_child_array = sizeof (*b->children) * (number_of_children + 1);
          if (bpt->mutable) {
            b->children = realloc(b->children, new_size_of_child_array);
            memmove(b->children + child_index + 1, b->children + child_index, sizeof (*b->children) * (number_of_children - child_index));
            b->children[child_index] = bpt_make_leaf(key, value);
            b->bitmask |= 1 << child_number;
            bpt_retain(bpt);
            return bpt;
          } else {
            bpt_t *new_children = malloc(new_size_of_child_array);
            memcpy(new_children, b->children, sizeof (*b->children) * child_index);
            memcpy(new_children + child_index + 1,
                   b->children + child_index,
                   sizeof (*b->children) * (number_of_children - child_index));
            new_children[child_index] = bpt_make_leaf(key, value);
            bpt_node_t new_node = bpt_make_node(b->bpt.prefix, b->branching_chunk);
            new_node->children = new_children;
            new_node->bitmask = b->bitmask | (1 << child_number);
            // Retain the children copied into the new node.
            bpt_for_children(bpt, bpt_retain0, NULL);
            return (bpt_t)new_node;
          }
        }
      }
    }
  }
}


bpt_t bpt_dissoc(bpt_t bpt, bpt_key_t key) {
  if (!bpt || (bpt_prefix_of_key(key, bpt_branching_chunk(bpt)) != bpt->prefix)) {
    bpt_retain(bpt);
    return bpt;
  } else if (bpt->tag == BPT_LEAF) {
    // Key matches.
    return NULL;
  } else {
    // Prefix matches.
    bpt_node_t b = (bpt_node_t)bpt;
    uint_fast8_t child_number = bpt_offset_of_key(key, b->branching_chunk);
    if ((1 << child_number) & b->bitmask) {
      unsigned int child_index = bpt_compute_child_index(b->bitmask, child_number);
      bpt_t child = b->children[child_index];
      bpt_t new_child = bpt_dissoc(child, key);
      if (new_child == child) {
        bpt_release(child);
        bpt_retain(bpt);
        return bpt;
      } else {
        unsigned int number_of_children = bpt_popcount(b->bitmask);
        if (!new_child && number_of_children == 2) {
          // When there is only a single child left, we replace ourselves
          // with that child.
          bpt_t remaining_child = b->children[1-child_index];
          bpt_retain(remaining_child);
          return remaining_child;
        } else if (bpt->mutable) {
          bpt_release(child);
          if (!new_child) {
            // We don't reallocate the array because it wouldn't really
            // gain us anything (except maybe non-confusion of a
            // conservative GC).
            memmove(b->children + child_index, b->children + child_index + 1, sizeof(*b->children) * (number_of_children - child_index - 1));
            b->bitmask &= ~(1 << child_number);
            bpt_retain(bpt);
            return bpt;
          } else {
            b->children[child_index] = new_child;
            bpt_retain(bpt);
            return bpt;
          }
        } else {
          // If all else fails, allocate a new node.
          bpt_t *new_children;
          bpt_key_bitmask_t bitmask;
          if (!new_child) {
            new_children = malloc((sizeof *new_children) * (number_of_children - 1));
            memcpy(new_children, b->children, sizeof (*b->children) * child_index);
            memcpy(new_children + child_index,
                   b->children + child_index + 1,
                   sizeof (*b->children) * (number_of_children - child_index - 1));
            bitmask = b->bitmask & ~(1 << child_number);
          } else {
            new_children = malloc((sizeof *new_children) * number_of_children);
            memcpy(new_children, b->children, sizeof (*b->children) * number_of_children);
            new_children[child_index] = new_child;
            bitmask = b->bitmask;
          }
          bpt_node_t new_node = bpt_make_node(b->bpt.prefix, b->branching_chunk);
          new_node->children = new_children;
          new_node->bitmask = bitmask;
          // Retain the children copied into the new node.
          bpt_for_children((bpt_t)new_node, bpt_retain0, NULL);
          bpt_release(new_child);
          return (bpt_t)new_node;
        }
      }
    } else {
      bpt_retain(bpt);
      return bpt;
    }
  }
}


void bpt_seal(bpt_t bpt) {
  if (bpt) {
    if (bpt->mutable) {
      bpt->mutable = false;
      if (bpt->tag == BPT_INNER_NODE) {
        bpt_for_children(bpt, bpt_seal0, NULL);
      }
    }
  }
}


/////////////// Helper functions ///////////////
static unsigned int bpt_compute_child_index(bpt_key_bitmask_t bitmask, unsigned int child_number) {
  // Compute the sparse array index given a flat array index.
  return bpt_popcount(bitmask & ((1 << child_number) - 1));
}

static inline uint_fast8_t bpt_offset_of_key(bpt_key_t key, unsigned int chunk_number) {
  // Little-enidan:
  //return (key >> (chunk_number * CHUNK_LENGTH)) & OFFSET_MASK;
  // Big-endian:
  int shift = 0;
  if (chunk_number <= MAX_CHUNKS - 2) {
    shift += LAST_CHUNK_LENGTH;
  }
  if (chunk_number <= MAX_CHUNKS - 3) {
    shift += ((MAX_CHUNKS - 2 - chunk_number) * CHUNK_LENGTH);
  }
  return (key >> shift) & (chunk_number == MAX_CHUNKS - 1 ? ((1 << LAST_CHUNK_LENGTH) - 1) : OFFSET_MASK);
}

static bpt_key_t bpt_prefix_of_key(bpt_key_t key, unsigned int chunk_number) {
  if (chunk_number == MAX_CHUNKS) {
    return key;
  } else {
    // Little-endian:
    //return key & ((1 << (chunk_number * CHUNK_LENGTH)) - 1)
    // Big-endian:
    return key & (((1 << (chunk_number * CHUNK_LENGTH)) - 1) << (KEY_LENGTH - (chunk_number * CHUNK_LENGTH)));
  }
}

static inline unsigned int bpt_branching_chunk(bpt_t bpt) {
  assert(bpt);
  if (bpt->tag == BPT_LEAF) {
    return MAX_CHUNKS;
  } else {
    return ((bpt_node_t)bpt)->branching_chunk;
  }
}

static inline unsigned int bpt_popcount(bpt_key_bitmask_t x) {
  return __builtin_popcount(x);
}

static inline unsigned int bpt_number_of_leading_zeros(bpt_key_t x) {
  return __builtin_clz(x);
}

static inline unsigned int bpt_number_of_trailing_zeros(bpt_key_t x) {
  return __builtin_ctz(x);
}

static unsigned int bpt_find_diverging_chunk(bpt_key_t a, bpt_key_t b) {
  // Little-endian:
  //return bpt_number_of_trailing_zeros(a ^ b) / CHUNK_LENGTH;
  // Big-endian:
  return bpt_number_of_leading_zeros(a ^ b) / CHUNK_LENGTH;
}

void bpt_retain(bpt_t bpt) {
  if (bpt) {
    __sync_fetch_and_add(&bpt->refcount, 1);
  }
}

void bpt_release(bpt_t bpt) {
  if (bpt) {
    if (__sync_sub_and_fetch(&bpt->refcount, 1) == 0) {
      bpt_dealloc(bpt);
    }
  }
}

void bpt_dealloc(bpt_t bpt) {
  if (bpt) {
    if (bpt->tag == BPT_LEAF) {
      bpt_leaf_t b = (bpt_leaf_t)bpt;
#ifdef BPT_ENABLE_DEALLOC_HOOKS
      if (b->dealloc_hook) {
        b->dealloc_hook(b->bpt.prefix, b->value);
      }
#endif
      free(b);
    } else {
      bpt_node_t b = (bpt_node_t)bpt;
      bpt_for_children(bpt, bpt_release0, NULL);
      free(b->children);
      free(b);
    }
  }
}

#ifdef BPT_ENABLE_DEALLOC_HOOKS
void bpt_leaf_set_dealloc_hook(bpt_leaf_t bpt, void (*hook)(bpt_key_t, void*)) {
  if (bpt) {
    bpt->dealloc_hook = hook;
  }
}

void bpt_set_dealloc_hook(bpt_t bpt, bpt_key_t key, void (*hook)(bpt_key_t, void*)) {
  bpt_leaf_set_dealloc_hook(bpt_get_leaf(bpt, key), hook);
}
#endif



/* Utilities */
struct bpt_for_mappings_closure_data {
  void (*thunk)(bpt_key_t, void*, void*);
  void *user_data;
};
static void bpt_for_mappings_iter(bpt_t bpt, void *closure_data_) {
  struct bpt_for_mappings_closure_data *closure_data = closure_data_;
  if (bpt->tag == BPT_LEAF) {
    bpt_leaf_t leaf = (bpt_leaf_t)bpt;
    closure_data->thunk(bpt->prefix, leaf->value, closure_data->user_data);
  } else {
    bpt_for_children(bpt, bpt_for_mappings_iter, closure_data);
  }
}
void bpt_for_mappings(bpt_t bpt, void (*thunk)(bpt_key_t, void*, void*), void *user_data) {
  struct bpt_for_mappings_closure_data closure_data =
    { .user_data = user_data, .thunk = thunk };

  bpt_for_mappings_iter(bpt, &closure_data);
}
