/-  *eth-abi
|%
  ++  petyp-to-etyp
    |=  =petyp
    ^-  etyp
    |^
    (petyp-type-to-etyp type.petyp)
    ++  petyp-type-to-etyp
      |=  t=petyp-type
      ^-  etyp
      ?+  t  !!
        [%tuple *]
      [%tuple (turn t.t petyp-to-etyp)]
        [%bytes-n *]
      t
        [%array *]
      [%array $(t t.t)]
        ?(%address %bool %int %uint %real %ureal %bytes %string)
      t
      ==
    --
  ::
    ++  martianize-name
      |=  =tape
      ^-  ^tape
      |^
      |-
      ?~  tape  tape
      ?.  ?|  &((gte i.tape 'A') (lte i.tape 'Z'))
              &((gte i.tape 'a') (lte i.tape 'z'))
          ==
        $(tape t.tape)
      :_  martianize(tape t.tape)
        ?.  (lth i.tape 91)  i.tape
        (add i.tape 32)
      ++  martianize
        %-  zing
        |-
        ?~  tape  ~
        ?:  (lth i.tape 91)
          =.  i.tape  (add i.tape 32)
          ["-" (trip i.tape) $(tape t.tape)]
        [(trip i.tape) $(tape t.tape)]
      --
::
  ++  parse-contract
    |=  [jon=json name=@tas]
    =/  entries=(list entry-raw)  (parse-abi jon)
    =/  read-functions=(list function-raw)
    %+  murn  entries
    |=  =entry-raw
    ?.  ?=([%function *] entry-raw)  ~
    ?:  mut.entry-raw  ~
    `+.entry-raw
    =/  write-functions=(list function-raw)
    %+  murn  entries
    |=  =entry-raw
    ?.  ?=([%function *] entry-raw)  ~
    ?.  mut.entry-raw  ~
    `+.entry-raw
    =/  events=(list event-raw)
    %+  murn  entries
    |=  =entry-raw
    ?.  ?=([%event *] entry-raw)  ~
    `+.entry-raw
    ^-  contract
    :^    name
        (parse-functions write-functions)
      (parse-functions read-functions)
    (parse-events events)
::
  ++  parse-abi
    |=  jon=json
    =,  dejs:format
    ^-  (list entry-raw)
    ?>  ?=([%a *] jon)
    %+  murn  p.jon
    |=  jan=json
    ^-  (unit entry-raw)
    ?~  jan  ~
    ?>  ?=([%o *] jan)
    =-  ?~  parse  ~  (some (u.parse jan))
    ^-  parse=(unit $-(json entry-raw))
    =/  typ  (so (~(got by p.jan) 'type'))
    ?+    typ  ~
        %function
      :-  ~
      |=  jun=json
      :-  %function
      ^-  function-raw
      %.  jun
      %-  ot
      :~  [%name so]
          [%inputs (ar (ou get-abi-type-raw))]
          [%outputs (ar (ou get-abi-type-raw))]
          :: should this be negated?
          [%constant |=(jen=json !(bo jen))]
          [%payable bo]
      ==
    ::
        %event
      :-  ~
      |=  jun=json
      :-  %event
      ^-  event-raw
      %.  jun
      %-  ot
      :~  [%name so]
      ::
          :-  %inputs
          %-  ar
          %-  ou
          :*  [%indexed (un bo)]
              get-abi-type-raw
          ==
      ==
    ==
  ++  get-abi-type-raw
    =,  dejs:format
    |^
    :~  [%name (uf ~ (mu so))]
        [%type (un so)]
        [%components (uf ~ decode-components)]
    ==
    ++  decode-components
        |=  =json
        ^-  (list abi-type-raw)
        %.  json
        %-  ar  %-  ou  get-abi-type-raw
  --

  ++  parse-events
    |=  events=(list event-raw)
    ^-  (map @ux event)
    %+  roll  events
    |=  [e=event-raw es=(map @ux event)]
    =/  =event
      :-  (crip (martianize-name (trip name.e)))
      %+  turn
        inputs.e
      |=  inp=event-input-raw
      ^-  event-input
      [(parse-type abi-type-raw.inp) indexed.inp]
    =/  typs=(list @t)
      %+  turn  inputs.e
      |=  inp=event-input-raw
      type.abi-type-raw.inp
    %+  ~(put by es)
      (get-hash name.e typs)
      event

  ++  parse-functions
    |=  functions=(list function-raw)
    %+  roll  functions
    |=  [f=function-raw fs=(map @tas function)]
    %+  ~(put by fs)  (crip (martianize-name (trip name.f)))
    :*
      (crip (martianize-name (trip name.f)))
      (get-selector name.f (turn inputs.f |=(=func-input-raw type.abi-type-raw.func-input-raw)))
      %+  turn  inputs.f  parse-type
      %+  turn  outputs.f  parse-type
    ==
::
  ++  parse-type
    |=  item=abi-type-raw
    ^-  petyp
    =+  taype=(trip type.item)
    =/  is-array=?
    =([']' '[' ~] (scag 2 (flop taype)))
    =.  taype  ?.  is-array  taype
    (scag (sub (lent taype) 2) taype)
    :: still need to handle dynamic type
    =.  name.item
    ?~  name.item  ~
    `(crip (martianize-name (trip u.name.item)))
    =-  ?.  is-array
          [name.item -]
        [name.item %array -]
    ^-  petyp-type
    ?+  (crip (scag 3 taype))  ~&  unimplemented-type+(crip taype)  !!
      %add  %address
      %boo  %bool
      %int  %int
      %uin  %uint
      %byt
    =/  ntape=tape  (slag 5 taype)
    ?~  ntape  %bytes
    [%bytes-n (scan ntape dem)]
      %str  %string
      %tup
    :-  %tuple
    %+  turn  comps.item  parse-type
    ==
::
  ++  get-selector
    |=  [name=@t inputs=(list @t)]
    ^-  cord
    =-  (crip "{(trip name)}({-})")
    (zing (join "," (turn inputs trip)))
::
  ++  get-hash
    |=  [name=@t inputs=(list @t)]
    ^-  @ux
    =/  sig  (get-selector name inputs)
    (keccak-256:keccak:crypto (met 3 sig) sig)
--
