/* include/nock/jet.h
**
** This file is in the public domain.
*/

  /** Data structures.
  **/
    /* u4_jet: a function accelerator.
    */
      struct u4_jet {
        const u4_cl *name;
        u4_noun     (*jet)(u4_g, u4_noun);
      };


  /** Functions.
  **/
#     define u4_turbo(name, jet) \
        u4_noun u4_jet_##name(u4_g, u4_noun);
#       include "nock/jets.h"
#     undef u4_turbo 
 

  /** Global variables.
  **/
    /* u4_jets: the accelerator list.
    */
#ifndef U4_GLOBALS
      extern struct u4_jet u4_jets[];
#else
      struct u4_jet u4_jets[] = {
#       define u4_turbo(name, jet) {#name, jet},
#         include "nock/jets.h"
#       undef u4_turbo
        {0, 0}
      };
#endif

