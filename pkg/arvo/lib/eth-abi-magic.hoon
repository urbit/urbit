/-  *eth-abi
|%
  ++  decode
  |%
  ++  decode-results
    ::  rex:  string of hex bytes with leading 0x.
    |*  [rex=@t tys=(list etyp)]
    =-  (decode-arguments - tys)
    %+  turn  (rip 9 (rsh 3 2 rex))
    (curr rash hex)
  ::
  ++  decode-arguments
    |*  [wos=(list @) tys=(list etyp)]
    =/  wos=(list @)  wos  ::  get rid of tmi
    =|  win=@ud
    =<  (decode-from 0 tys)
    |%
    ++  decode-from
      |*  [win=@ud tys=(list etyp)]
      ?~  tys  !!
      =-  ?~  t.tys  dat
          [dat $(win nin, tys t.tys)]
      (decode-one win ~[i.tys])
    ::
    ++  decode-one
      ::NOTE  we take (list etyp) even though we only operate on
      ::      a single etyp as a workaround for urbit/arvo#673
      |*  [win=@ud tys=(list etyp)]
      =-  [nin dat]=-  ::NOTE  ^= regular form broken
      ?~  tys  !!
      =*  typ  i.tys
      =+  wor=(snag win wos)
      ?+  typ
        ~|  [%unsupported-type typ]
        !!
      ::
          ?(%address %bool %uint)  ::  %int %real %ureal
        :-  +(win)
        ?-  typ
          %address  `@ux`wor
          %uint     `@ud`wor
          %bool     =(1 wor)
        ==
      ::
          %string
        =+  $(tys ~[%bytes])
        [nin (trip (swp 3 q.dat))]
      ::
          %bytes
        :-  +(win)
        ::  find the word index of the actual data.
        =/  lic=@ud  (div wor 32)
        ::  learn the bytelength of the data.
        =/  len=@ud  (snag lic wos)
        (decode-bytes-n +(lic) len)
      ::
          [%bytes-n *]
        :-  (add win +((div (dec n.typ) 32)))
        (decode-bytes-n win n.typ)
      ::
          [%array *]
        :-  +(win)
        ::  find the word index of the actual data.
        =.  win  (div wor 32)
        ::  read the elements from their location.
        %-  tail
        %^  decode-array-n  ~[t.typ]  +(win)
        (snag win wos)
      ::
          [%array-n *]
        (decode-array-n ~[t.typ] win n.typ)
          [%tuple *]
        (decode-tuple t.typ win)
      ==
    ::
    ++  decode-bytes-n
      |=  [fro=@ud bys=@ud]
      ^-  octs
      ::  parse {bys} bytes from {fro}.
      :-  bys
      ?~  bys  0
      %^  rsh  3
        =+  (mod bys 32)
        ?:(=(0 -) - (sub 32 -))
      %+  rep  8
      %-  flop
      =-  (swag [fro -] wos)
      +((div (dec bys) 32))
    ::
    ++  decode-array-n
      ::NOTE  we take (list etyp) even though we only operate on
      ::      a single etyp as a workaround for urbit/arvo#673
      ::NOTE  No longer typeless arrays as in zuse
      |*  [tys=(list etyp) fro=@ud len=@ud]
      ?~  tys  !!
      =|  res=(list _dat:(decode-one fro ~[i.tys]))
      |-  ^-  [@ud _res]
      ?:  =(len 0)  [fro (flop res)]
      =+  (decode-one fro ~[i.tys])  ::  [nin=@ud dat=*]
      $(res ^+(res [dat res]), fro nin, len (dec len))
    ++  decode-tuple
      :: ::NOTE  we take (list etyp) even though we only operate on
      :: ::      a single etyp as a workaround for urbit/arvo#673
      :: ::NOTE  careful! produces lists without type info
      ::  probably broken (shouldn't be list)
      =|  res=(list)
      |*  [tys=(list etyp) fro=@ud]
      ^-  [@ud (list)]
      ?~  tys  [fro (flop `(list)`res)]
      =+  (decode-one fro ~[i.tys])  ::  [nin=@ud dat=*]
      $(res ^+(res [dat res]), fro nin, tys t.tys)
    --
  --
  ++  event-to-tuple
    |*  [tt=(list etyp) topics=(list @) dt=(list etyp) data=@t]
    |^
    =+  tdata=(decode-arguments:decode topics tt)
    =+  data=(decode-results:decode data dt)
    (combine tt tdata data)
  ++  combine
    |*  [etyps=(list etyp) event-topics=* event-data=*]
    |-
    ?>  ?=(^ etyps)
    ?~  t.etyps  [event-topics event-data]
    ?>  ?=(^ event-topics)
    :-  -.event-topics
    $(event-topics +.event-topics, etyps t.etyps)
  --

  ++  tuple-to-eth-data  ::  n-tuple
    |=  [etyps=(list etyp) data=*]
    ^-  (list data:abi:ethereum)
    =/  =data:abi:ethereum  (one-to-eth-data [%tuple etyps] data)
    ?>  ?=([%tuple *] data)
    p.data
  ++  one-to-eth-data
    |=  [=etyp data=*]
    |-  ^-  data:abi:ethereum
    :: TODO: missing bytes-n, and probably others
    ?+  etyp  ~|("unimplemented etyp" !!)
        %uint
      ?>  ?=(@ud data)
      [etyp data]
        %address
      ?>  ?=(@ux data)
      [etyp data]
        %bool
      ?>  ?=(? data)
      [etyp data]
        %bytes
      ?>  ?=(octs data)
      [etyp data]
        %string
      :-  etyp
      |-  ^-  tape
      ?~  data  ~
      ?>  ?=([@t *] data)
      :_  ?:  ?=([@t ~] data)  ~
          $(data +.data)
      -.data
        [%array-n *]
      =/  index=@ud  0
      :-  -.etyp
      |-  ^-  (list data:abi:ethereum)
      ?>  (lth index n.etyp)
      :_  ?:  ?=([* ~] data)  ~
          $(data +.data)
      ^$(etyp t.etyp, data -.data)
        [%array *]
      :-  -.etyp
      |-  ^-  (list data:abi:ethereum)
      ?~  data  ~
      :_  ?:  ?=([* ~] data)  ~
          $(data +.data)
      ^$(etyp t.etyp, data -.data)
        [%tuple *]
      :-  -.etyp
      |-  ^-  (list data:abi:ethereum)
      ?>  ?=(^ t.etyp)
      ?~  t.t.etyp  [^$(etyp i.t.etyp) ~]
      ?>  ?=(^ data)
      [^$(etyp i.t.etyp, data -.data) $(t.etyp t.t.etyp, data +.data)]
    ==
    ++  encode-topics
      ::  multiple topics with atom types only
      |=  [types=(list etyp) topics=*]
      |^  ^-  (list ?(@ux (list @ux)))
      ?~  types  ~
      ?+  i.types
        ?~  t.types
          ~[(encode i.types topics)]
        :_  $(types t.types, topics +.topics)
        (encode i.types -.topics)
          ?(%address %bool %int %uint %real %ureal)
        ?~  t.types
          ?^  topics
            ~[(turn-encode i.types topics)]
          ~[(encode i.types topics)]
        :_  $(types t.types, topics +.topics)
        ?^  -.topics
          (turn-encode i.types -.topics)
        (encode i.types -.topics)
      ==
      ++  turn-encode
        |=  [=etyp t=*]
        |-  ^-  (list @ux)
        ?^  t
          [(encode etyp -.t) $(t +.t)]
        ~
      ++  encode
        |=  [=etyp t=*]
        ^-  @ux
        (scan (encode-data:abi:ethereum (data:abi:ethereum [etyp t])) hex)
      --
  :: ++  encode-topics
  ::   ::  multiple topics with atom types only
  ::   |*  [types=(list etyp) topics=*]
  ::   |^  ^-  (list (list @ux))
  ::   ?~  types  ~
  ::   ?+  i.types  !!
  ::     :: ?~  t.types
  ::     ::   ~[~[(decode i.types topics)]]
  ::     :: :_  $(types t.types, topics +.topics)
  ::     :: ~[(decode i.types -.topics)]
  ::   ::
  ::       ?(%address %bool %int %uint %real %ureal)
  ::     ?~  t.types
  ::       ?^  topics
  ::       ^-  (list (list @ux))
  ::         ~[~[(decode i.types topics)]]
  ::         ^-  (list (list @ux))
  ::       ~[(turn-decode i.types topics)]
  ::     ^-  (list (list @ux))
  ::     :_  ^-  (list (list @ux))
  ::     $(types t.types, topics +.topics)
  ::     ?^  -.topics
  ::     ^-  (list @ux)
  ::       (turn-decode i.types -.topics)
  ::     ^-  (list @ux)
  ::     ~[(decode i.types -.topics)]
  ::   ==
  ::   ++  turn-decode
  ::     |*  [=etyp t=(list)]
  ::     ^-  (list @ux)
  ::     =+  typ=_?>(?=(^ t) -.t)
  ::     %+  turn  `(list typ)`t
  ::     |=  =typ
  ::     (decode etyp typ)
  ::   ++  decode
  ::     |*  [=etyp t=*]
  ::     ^-  @ux
  ::     (scan (encode-data:abi:ethereum (data:abi:ethereum [etyp t])) hex)
  ::   --
--
