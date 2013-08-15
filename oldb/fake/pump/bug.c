/* fake/pump/bug.c
**
** This file is in the public domain.
*/
#include "u4/all.h"
#include <stdio.h>

/* Return true iff $noun is an ASCII string of 3 or more bytes,
** using no characters besides a-z and -.
*/
static u4_t
_test_symbol(u4_noun noun)
{
  if ( u4_n_atom(noun) ) {
    uint32_t nbytes = u4_a_bin(noun, 3);

    if ( nbytes >= 2 ) {
      uint8_t *buffer = alloca(nbytes + 1);
      uint32_t i;

      u4_a_bytes(noun, buffer, 0, nbytes);
      for ( i=0; i < nbytes; i++ ) {
        if ( ((buffer[i] < 'a') || (buffer[i] > 'z')) &&
             (buffer[i] != '-') )
        {
          return 0;
        }
      }
      return 1;
    }
  }
  return 0;
}

/* Return true iff $noun is an ASCII string of 3 or more bytes.
*/
static u4_t
_test_ascii(u4_noun noun)
{
  if ( u4_n_atom(noun) ) {
    uint32_t nbytes = u4_a_bin(noun, 3);

    if ( nbytes >= 2 ) {
      uint8_t *buffer = alloca(nbytes + 1);
      uint32_t i;

      u4_a_bytes(noun, buffer, 0, nbytes);
      for ( i=0; i < nbytes; i++ ) {
        if ( (buffer[i] < 32) || (buffer[i] > 126) ) {
          return 0;
        }
      }
      return 1;
    }
  }
  return 0;
}

/* Measure $noun in trivial format.  Include cell parentheses unless
** $open is 1.
*/
static uint32_t
_measure_trivial(u4_noun noun,
                 u4_t open)
{
  if ( u4_n_atom(noun) ) {
    if ( _test_symbol(noun) ) {
      return u4_a_bin(noun, 3);
    }
    if ( _test_ascii(noun) ) {
      return u4_a_bin(noun, 3) + 2;
    }
    else {
      uint32_t size = u4_a_bin(noun, 2);
      uint32_t nbytes = size ? size : 1;

      return (nbytes + 2);
    }
  }
  else {
    uint32_t measure_head = _measure_trivial(u4_ch(noun), 0);
    uint32_t measure_tail = _measure_trivial(u4_ct(noun), 1);

    if ( open ) {
      return measure_head + 1 + measure_tail;
    } else {
      return 1 + measure_head + 1 + measure_tail + 1;
    }
  }
}

/* Write $noun to $text, in trivial format.  Include cell parentheses
** unless $open is 1.
**
** Return the number of bytes written in $text.
*/
static uint32_t
_write_trivial(u4_noun noun,
               uint8_t *text,
               u4_t open)
{
  if ( u4_n_atom(noun) ) {
    if ( _test_symbol(noun) ) {
      uint32_t nbytes = u4_a_bin(noun, 3);

      u4_a_bytes(noun, text, 0, nbytes);
      return nbytes;
    }
    else if ( _test_ascii(noun) ) {
      uint32_t nbytes = u4_a_bin(noun, 3);

      *text = '<';
      u4_a_bytes(noun, text + 1, 0, nbytes);
      text[nbytes + 1] = '>';

      return nbytes + 2;
    }
    else {
      uint32_t size = u4_a_bin(noun, 2);
      uint32_t nbytes = size ? size : 1;
      mpz_t mp;

      u4_a_gmp(noun, mp);
      text[0] = '0';
      text[1] = 'x';
      text += 2;
      mpz_get_str((char *)text, 16, mp);

      assert(strlen((char *)text) == nbytes);
      mpz_clear(mp);
      return nbytes + 2;
    }
  }
  else {
    uint8_t *start = text;
    uint32_t head_nbytes, tail_nbytes;

    if ( !open ) { *(text++) = '(';  }
        
    head_nbytes = _write_trivial(u4_ch(noun), text, 0);
    text += head_nbytes;

    *(text++) = ' ';

    tail_nbytes = _write_trivial(u4_ct(noun), text, 1);
    text += tail_nbytes;

    if ( !open ) { *(text++) = ')';  }
  
    *text = 0;
    return (text - start);
  }
}

/* Print $noun in trivial format.
*/
static uint8_t *
_print_trivial(u4_noun noun)
{
  uint32_t nbytes = _measure_trivial(noun, 0);
  uint8_t *text = malloc(nbytes + 1);

  text[nbytes] = 0;
  _write_trivial(noun, text, 0);
  return text;
}

/* u4_bug():
**
**   Print (noun) with (caption).
*/
void
u4_bug(const u4_cl *cl_caption,
       u4_nopt     noun)
{
  if ( u4_bull == noun ) {
    printf("%s: [bull]\n", cl_caption);
  } else {
    uint8_t *print = _print_trivial(noun);

    printf("%s: %s\n", cl_caption, print);
    free(print);
  }
  fflush(stdout);
}

