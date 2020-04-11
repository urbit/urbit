=,  ethereum-types

|%
+$  etyp  etyp:abi:ethereum
+$  contract
  $:  name=@t
      write-functions=(map @tas function)
      read-functions=(map @tas function)
      events=(map @ux event)
  ==

+$  function
  $:  hash=@ux
      inputs=(list etyp)
      outputs=(list etyp)
  ==
+$  event
  $:  name=@t
      input=(list event-input)
  ==
+$  event-input  [name=@t type=etyp indexed=?]
+$  event-input-raw  [name=@t type=@t indexed=?]
+$  contract-raw
  $:  name=@t
      entries=(list entry-raw)
  ==
+$  entry-raw
  $%  [%function p=function-raw]
      [%event p=event-raw]
  ==
+$  function-raw
  $:  name=@t
      inputs=(list @t)
      outputs=(list @t)
      mut=?
      pay=?
  ==
+$  event-raw
  $:  name=@t
      inputs=(list event-input-raw)
  ==
:: +$  etyp
::   $@  $?  ::  static
::           %address  %bool
::           %int      %uint
::           %real     %ureal
::           ::  dynamic
::           %bytes    %string
::       ==
::   $%  ::  static
::       [%bytes-n n=@ud]
::       ::  dynamic
::       [%array-n t=etyp n=@ud]
::       [%array t=etyp]
::   ==

  ::
  :: :: a=97, z=122, A=65, Z=90
  ::
  :: ::
  :: ::
  :: ::
  ++  inputs-to-face-pairs
    |=  =event
      %-  zing
      %+  join  " "
      %+  turn  input.event
      |=  [=event-input]
      "{(trip name.event-input)}={(etyp-to-aura type.event-input)}"


  ++  etyp-to-aura
    |*  type=etyp
    :: ^-  tape
    ?+  type  !!
    ::
        %address
      "@ux"
        %uint
      "@ud"
        %bool
      "?"
        %int :: doesn't work with decode lib yet
      !!
        %string
      "@t"
        %bytes
      "octs"
        [%bytes-n *]
      "octs"
    ::
        [%array *]
      "@ux"
    ::
        [%array-n *]
      "@ux"
    ==

  ++  char-to-lower
    |=  =tape
    ^-  ^tape
    ?>  ?=([@tas $~] tape)
    =.  i.tape
      ?:  (lth i.tape 91)
        (add i.tape 92)
      i.tape
    tape

  ++  code-gen
    |=  =contract
    |^
      %-  crip
      %-  zing
        :~  "|_  addr=address:ethereum"  nl
            "  ++  read-calls"  nl
            "  ++  write-calls"  nl
            "  ++  call-gifts"  nl
            "  ++  events"  nl
            "    |%"  nl
            event-bucs  nl
            "--"
        ==
    :: foo
    ++  nl  (trip 10)
    :: ++  foo
    ::   %+  turn  ~(val by events.contract)
    ::   |=  =event
    ::   (trip name.event)
    ++  event-bucs
    ^-  tape
    %-  zing
    %+  turn  ~(val by events.contract)
    |=  =event
    =-  %-  zing  ~["        +$  {(trip name.event)}  [${(trip name.event)} {-}]" nl]
    (inputs-to-face-pairs event)
  --
    :: "{(trip name.event-input)}{(etyp-to-aura type.event-input)}"
    :: |^
    ::   ^-  tape
    ::   %-  zing
    ::   %+  turn  ~(val by events.contract)
    ::     |=  e=event
    ::     ^-  tape
    ::     =-  "        +$  {(trip name.e)}  [${(trip name.e)} {-}]"
    ::     %-  zing
    ::     :: %+  join  " "
    ::     %+  turn  input.e
    ::       |=  i=event-input
    ::       ^-  tape
    ::       "{(trip name.i)}={(etyp-to-aura type.i)}"
    ::   --
    ::   --
    :: =+
    ::
    ::
    :: !!
    :: ==
    ::   10
    ::   ::
    ::       "    --"  10
    ::       "--"  10
    ::   ==
  ::
  :: ++  etyp-to-aura
  ::   |=  type=etyp
  ::   ^-  tape
  ::   ?+  type  "@ux"
  ::       %address
  ::     "@ux"
  ::       %bool
  ::     "?"
  ::       %int
  ::     "@sd"
  ::       %uint
  ::     "@ud"
  ::       %string
  ::     "@tas"
  ::   ==

++  parse-contract
  |=  jon=json
  =/  =contract-raw
    =,  dejs:format
    %.  jon  %-  ot
    :~  ['contractName' so]
        [%abi parse-abi]
    ==
  ^-  contract
  :^    name.contract-raw
      (get-write-functions entries.contract-raw)
    (get-read-functions entries.contract-raw)
  (get-events entries.contract-raw)
::
++  parse-abi
  |=  jon=json
  =,  dejs:format
  ^-  (list entry-raw)
  %.  jon  %-  ar
  |=  jan=json
  ?>  ?=([$o *] jan)
  =/  typ  (so (~(got by p.jan) 'type'))
  %.  jan
  ?+    typ  ~&  "unexpected entry type"  !!
      %function
    |=  jun=json
    :-  %function
    ^-  function-raw
    %.  jun
    %-  ot
    |^
      :~  [%name so]
          [%inputs (ar extract-func-field)]
          [%outputs (ar extract-func-field)]
          [%constant |=(jen=json !(bo jen))]
          [%payable bo]
      ==
      ++  extract-func-field
        |=  jyn=json
        ?>  ?=([$o *] jyn)
        ^-  @tas
        (so (~(got by p.jyn) 'type'))
    --
  ::
      %event
    |=  jun=json
    :-  %event
    ^-  event-raw
    %.  jun
    %-  ot
    :~  [%name so]
    ::
      :-  %inputs
      %-  ar
      %-  ot
      :~  [%name so]
          [%type so]
          [%indexed bo]
      ==
    ==
  ==
::
++  get-raw-functions
  |=  abi=(list entry-raw)
  ^-  (list function-raw)
  %+  roll  abi
  |=  [e=entry-raw fs=(list function-raw)]
    ?.  ?=([$function *] e)  fs
    [p.e fs]
::
++  get-raw-events
  |=  abi=(list entry-raw)
  ^-  (list event-raw)
  %+  roll  abi
  |=  [e=entry-raw fs=(list event-raw)]
    ?.  ?=([$event *] e)  fs
    [p.e fs]
::
++  get-events
  |=  abi=(list entry-raw)
  ^-  (map @ux event)
  %+  roll  (get-raw-events abi)
  |=  [e=event-raw es=(map @ux event)]
  =/  =event
    :-  name.e
    %+  turn
      inputs.e
    |=  e=event-input-raw
    ^-  event-input
    [name.e (parse-type type.e) indexed.e]
  =/  typs=(list @t)
    %+  turn  inputs.e
    |=  i=event-input-raw
    type.i
  %+  ~(put by es)
    (get-hash name.e typs)
    event
::
++  get-write-functions
  |=  abi=(list entry-raw)
  ^-  (map @tas function)
  %+  roll  (get-raw-functions abi)
  |=  [f=function-raw fs=(map @tas function)]
  ?.  mut.f  fs
  %+  ~(put by fs)  name.f
  [(get-hash name.f inputs.f) (parse-types inputs.f) (parse-types outputs.f)]
::
++  get-read-functions
  |=  abi=(list entry-raw)
  ^-  (map @tas function)
  %+  roll  (get-raw-functions abi)
  |=  [f=function-raw fs=(map @tas function)]
  ?:  mut.f  fs
  %+  ~(put by fs)  name.f
  [(get-hash name.f inputs.f) (parse-types inputs.f) (parse-types outputs.f)]
::
++  parse-type
  |=  typ=@t
  ^-  etyp
  ?+  (crip (scag 3 (trip typ)))
  ~&  'unimplmented/unexpected solidity type'  !!
    %add  %address
    %boo  %bool
    %int  %int
    %uin  %uint
    %byt  %bytes
    %str  %string
  ==
++  parse-types
  |=  typs=(list @t)
  ^-  (list etyp)
  %+  turn  typs
  parse-type
::
++  get-selector
  |=  [name=@t inputs=(list @t)]
  ^-  cord
  =-  (crip "{(trip name)}({-})")
  (zing (join "," (turn inputs trip)))
  :: %-  zing
  :: %+  join
  ::   ","
  :: %+  turn
  ::   inputs
  :: |=  type=etyp
  :: ?>  ?=()
  :: trip
::
++  get-hash
  |=  [name=@t inputs=(list @t)]
  ^-  @ux
  =/  sig  (get-selector name inputs)
  (keccak-256:keccak:crypto (met 3 sig) sig)
--
