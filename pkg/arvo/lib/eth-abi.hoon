=,  able:jael
=,  ethereum-types
::
::  eth-watcher-diff
::  [%history =loglist]
::  ::  %log: newly added log
::  ::
::  [%log =event-log:rpc:ethereum]
::  ::  %disavow: forget logs
::  ::
::  ::    this is sent when a reorg happens that invalidates
::  ::    previously-sent logs
::  ::
::  [%disavow =id:block]
::
::  parse ABI
::  generate topic id -> event data types map
::  generate function name -> call data types map
::
::  rare %disavow case w/ ++release-logs only confirming blocks after 30 confirmations
::  (reorgs greater than 2 very rare)
::
::  taking $event-log from %eth-watcher-diff %log
::  i.topics.event-log is keccak hash
::  look up appropriate (list data:abi:eth) from parsed ABI
::  (decode-topics t.topcs.event-logc data-list-scheme)
::
|%
+$  contract
  $:  events=(map @ux event)
      funcs=(map @ta function)
  ==
+$  event  _!!
+$  function  _!!
::
+$  contract-raw
  $:  name=@t
      entries=(list entry-raw)
  ==
+$  entry-raw
  $%  [%function =function]
      [%event =event]
  ==
+$  function-raw
  $:  name=@t
      inputs=(list field)
      pay=?
      mut=?
      outputs=(list field)
  ==
+$  event-raw
  $:  name=@t
      inputs=(list event-input)
  ==
+$  event-input  [=field indexed=?]
+$  field  [name=@t type=@t]
::
++  separate-indexed-inputs
  |=  pts=(list value-type)
  ^-  (pair (list value-type) (list value-type))
  (skid pts |=(value-type indexed))
::
++  parse-event-types
  |=  entries=(list entry)
  ^-  events
  %-  molt
  |-  ^-  (list (pair @ux event-type))
  ?~  entries  ~
  =/  [dxt=(list value-type) nxt=(list value-type)]
    (separate-indexed-inputs inputs.i.entries)
  :-  :-  %+  get-hash  name.i.entries
      %+  turn  inputs.i.entries  |=(=value-type type.value-type)
      [name.i.entries dxt nxt]
  $(entries t.entries)
::
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
++  cord-to-entype
  |=  jon=json
  ^-  entry-type
  =,  dejs:format
  =/  typ  (so jon)
  ?+  (crip (scag 3 (trip typ)))  ~&  [%unknown-abi-type typ]  !!
    %fun  %function
    %eve  %event
  ==
::
++  to-value-type
  =,  dejs:format
  ^-  $-(json value-type)
  %-  ou
  :~
    :-  %name  (un so)
    :-  %type  (un so)
    :-  %indexed  %+  uf  %|  bo
  ==
::
++  parse-abi
  |=  jon=json
  ^-  contract
  ::
  =,  dejs:format
  %.  jon  %-  ot
  :~  :-  %contractName  so
      :-  %abi  parse-entries
  ==
::
++  parse-entries
  |=  jon=json
  ^-  (list entry)
  =,  dejs:format
  %.  jon  %-  ar
  %-  ou
  :~
    :-  %name  (un so)
    :-  %type  %-  un  cord-to-entype
    :-  %inputs  %-  un  %-  ar  to-value-type
    :-  %outputs  %+  uf  ~  %-  ar  to-value-type
  ==
::
++  separate-abi
  |=  entries=(list entry)
  ^-  (pair (list entry) (list entry))
  %+  skid  entries
  |=  =entry
  ^-  ?
  ?.  =(type.entry %function)
  ?.  =(type.entry %event)
  ~&  [%unexpected-entry-type type.entry]  !!
  %|
  %&
--
