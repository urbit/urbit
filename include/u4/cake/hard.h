/* include/cake/hard.h
**
** This file is in the public domain.
*/
  enum u4_cod_hard {
#   define u4_lib_mote(x, y) \
      u4_atom_##x = ((1 << 31) | (y)),
#   include "u4/lib/motes.h"
#   undef u4_lib_mote

    u4_atom_aaaa = ((1 << 31) | u4_s4('a','a','a','a'))
  };

  enum u4_word_hard {
#   define u4_lib_mote(x, y) \
      u4__##x = (y),
#   include "u4/lib/motes.h"
#   undef u4_lib_mote

    u4__aaaa = u4_s4('a','a','a','a')
  };
