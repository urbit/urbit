=>
|%
  +$  atr  [name=(unit @t) type=@t comps=(list atr)]
--
|%
+$  abi-type-raw    atr
+$  etyp  etyp:abi:ethereum
:: ++  some-core
::   |%
+$  petyp-type
  $@  $?  ::  static
          %address  %bool
          %int      %uint
          %real     %ureal
          ::  dynamic
          %bytes    %string
      ==
  $%  ::  static
      [%bytes-n n=@ud]
      ::  dynamic
      [%array-n t=petyp-type n=@ud]
      [%array t=petyp-type]
      [%tuple t=(list petyp)]
  ==
+$  petyp
  $:  name=(unit term)
      type=petyp-type
  ==
+$  contract
  $:  name=@tas
      write-functions=(map @tas function)
      read-functions=(map @tas function)
      events=(map @ux event)
  ==
+$  function
  $:  name=@tas
      sel=@t
      inputs=(list func-input)
      outputs=(list func-output)
  ==
+$  event
  $:  name=@tas
      inputs=(list event-input)
  ==
+$  event-input  [=petyp indexed=?]
+$  func-input  [=petyp]
+$  func-output  [=petyp]
:: compos is ~ when ?!  ?=(?(_'tuple' _'tuple[]'')
::  'tuple[]' should be turned into array<tuple>

+$  event-input-raw  [indexed=? =abi-type-raw]
+$  func-input-raw  [=abi-type-raw]
+$  func-output-raw  [=abi-type-raw]
+$  contract-raw
  $:  entries=(list entry-raw)
  ==
+$  entry-raw
  $%  [%function function-raw]
      [%event event-raw]
  ==
+$  function-raw
  $:  name=@t
      inputs=(list func-input-raw)
      outputs=(list func-output-raw)
      mut=?
      pay=?
  ==
+$  event-raw
  $:  name=@t
      inputs=(list event-input-raw)
  ==
--
