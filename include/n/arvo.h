/* include/f/arvo.h
**
** This file is in the public domain.
*/
  /**   Data structures.
  **/
    /* u3_cart: ovum carton.
    */
      struct _u3_arvo;

      typedef struct _u3_cs_cart {
        u3_noun vir;                      //  effects of ovum
        u3_bean did;                      //  cart considered for commit?
        u3_bean cit;                      //  cart committed?
        c3_d    ent_d;                    //  entry in raft queue?
        struct _u3_cs_cart* nex_u;        //  next in queue
      } u3_cart;

    /* u3_cs_arvo: modern arvo structure.
    */
      typedef struct _u3_cs_arvo {
        c3_d    ent_d;                    //  event counter

        u3_noun yot;                      //  cached gates
        u3_noun now;                      //  current time, as noun
        u3_noun wen;                      //  current time, as text
        u3_noun sev_l;                    //  instance number
        u3_noun sen;                      //  instance string
        u3_noun own;                      //  owner list

        u3_noun roe;                      //  temporary unsaved events
        u3_noun key;                      //  log key, or 0

        u3_noun ken;                      //  kernel formula, not needed
        u3_noun roc;                      //  rotor core

        union {
          struct { uint64_t a; uint64_t b; };
          struct {
            struct _u3_cs_cart* egg_u;    //  exit of ovum queue
            struct _u3_cs_cart* geg_u;    //  entry of ovum queue
          } ova;
        };
      } u3_cs_arvo;

    /* u3_cs_home: complete system state.
    */
      typedef struct _u3_cs_home {
        u3_cs_road rod_u;
        u3_cs_arvo arv_u;
      } u3_cs_home;
