=,  able:jael
=,  ethereum-types
|%
+$  contract
  $:  name=@t
      write-functions=(map @tas function)
      read-functions=(map @tas function)
      events=(map @ux event)
  ==
+$  function
  $:  input-sol=(list @tas)
      input-hoon=(list @tas)
      outputs=(list @tas)
  ==
+$  event
  $:  input-sol=(list event-input)
      input-hoon=(list @tas)
  ==
+$  event-input  [type=@t indexed=?]
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
      inputs=(list @tas)
      outputs=(list @tas)
      mut=?
      pay=?
  ==
+$  event-raw
  $:  name=@t
      inputs=(list event-input)
  ==
::
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
  ?+    typ  !!
      %function
    |=  jun=json
    :-  %function
    ^-  function-raw
    %.  jun
    %-  ot
    =/  extract-func-field
      |=  jyn=json
      ?>  ?=([$o *] jyn)
      ^-  @tas
      (so (~(got by p.jyn) 'type'))
    :~  [%name so]
        [%inputs (ar extract-func-field)]
        [%outputs (ar extract-func-field)]
        [%constant |=(jen=json !(bo jen))]
        [%payable bo]
    ==
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
      :~  [%type so]
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
  =/  typs  (turn inputs.e |=(e=event-input type.e))
  %+  ~(put by es)
    (get-hash name.e typs)
    [inputs.e (parse-types typs)]
::
++  get-write-functions
  |=  abi=(list entry-raw)
  ^-  (map @tas function)
  %+  roll  (get-raw-functions abi)
  |=  [f=function-raw fs=(map @tas function)]
  ?.  mut.f  fs
  %+  ~(put by fs)  name.f
  :-  inputs.f  :-  (parse-types inputs.f)  outputs.f
::
++  get-read-functions
  |=  abi=(list entry-raw)
  ^-  (map @tas function)
  %+  roll  (get-raw-functions abi)
  |=  [f=function-raw fs=(map @tas function)]
  ?:  mut.f  fs
  %+  ~(put by fs)  name.f
  :-  inputs.f  :-  (parse-types inputs.f)  outputs.f
::
++  parse-types
  |=  typs=(list @t)
  %+  turn  typs
  |=  typ=@tas
  ?+  (crip (scag 3 (trip typ)))
  ~&  'unimplmented/unexpected solidity type'  !!
    %add  %address
    %boo  %bool
    %int  %int
    %uin  %uint
    %byt  %bytes
    %str  %string
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
