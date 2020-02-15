=,  able:jael
=,  ethereum-types
|%
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
+$  event-input  [type=@t indexed=?]
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
    :~  [%name so]
    ::
        :-  %inputs
        %-  ar
        |=  jyn=json
        ?>  ?=([$o *] jyn)
        ^-  @tas
        (so (~(got by p.jyn) 'type'))
    ::
        :-  %outputs
        %-  ar
        |=  jyn=json
        ?>  ?=([$o *] jyn)
        ^-  @tas
        (so:dejs (~(got by p.jyn) 'type'))
    ::
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

++  get-sig
  |=  [name=@t inputs=(list @t)]
  ^-  cord
  %-  crip
  :-  name  :-  '('  ?~  inputs  :-  ')'  ~
  :-  i.inputs
  |-  ^-  tape
  ?~  t.inputs  :-  ')'  ~
  :-  ','  :-  i.t.inputs  $(inputs t.inputs)
::
++  get-hash
  |=  [name=@t inputs=(list @t)]
  ^-  @ux
  =/  sig  (get-sig name inputs)
  (keccak-256:keccak:crypto (lent (trip sig)) sig)
::
++  parse-contract
  |=  jon=json
  ^-  contract-raw
  =,  dejs:format
  %.  jon  %-  ot
  :~  :-  'contractName'  so
      :-  %abi  parse-abi
  ==
--
