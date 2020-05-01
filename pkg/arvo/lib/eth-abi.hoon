=,  ethereum-types

|%
  +$  etyp  etyp:abi:ethereum
  +$  abi-events  (pair (map @ux @tas) (map @tas (pair (list etyp) type)))
  +$  contract
    $:  name=@tas
        write-functions=(map @tas function)
        read-functions=(map @tas function)
        events=(map @ux event)
    ==
  +$  function
    $:  name=@tas
        sol-name=@tas
        hash=@ux
        inputs=(list func-input)
        outputs=(list etyp)
    ==
  +$  event
    $:  name=@tas
        inputs=(list event-input)
    ==
  +$  event-input  [name=@tas type=etyp indexed=?]
  +$  func-input  [name=@tas type=etyp]
  +$  event-input-raw  [name=@t type=@t indexed=?]
  +$  func-input-raw  [name=@t type=@t]
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
        inputs=(list func-input-raw)
        outputs=(list @t)
        mut=?
        pay=?
    ==
  +$  event-raw
    $:  name=@t
        inputs=(list event-input-raw)
    ==

  ++  etyp-to-type
    |*  type=etyp
    ^-  ^type
    ?+  type  !!
    ::
        %address
      [%atom %ux ~]
        %uint
      [%atom %ud ~]
        %bool
      [%atom %f ~]    ::
        %int :: doesn't work with decode lib yet
      !!
    ::
        %string
      [%atom %t ~]    ::
        %bytes
      -:!>(octs)
    ::
        [%bytes-n *]
      -:!>(*octs)
    ::
        [%array *]
      [%atom %ux ~]
    ::
        [%array-n *]
      [%atom %ux ~]
    ==
::  maybe (list ?(?(@ [@ @] (list @)) (list ?(@ [@ @] (list @))))))
:: (list ?(?(@ [@ @]) (list ?(@ [@ @] (list @)))))
  ++  encode-topics
    |=  [types=(list etyp) topics=(list ?(@ (list @)))]
    |-  ^-  (list ?(@ux (list @ux)))
    ?~  topics  ~
    ?~  types  ~
    ?+  i.types  ~&  'type not implemented'  !!
        ?(%address %bool %int %uint %real %ureal)
      ?~  i.topics  ~
      ?^  i.topics
        :: topic is (list @)
        :_  ^-  (list ?(@ux (list @ux)))  $(types t.types, topics t.topics)
        ^-  (list @ux)
        %+  turn  %-  (list @)  i.topics
        |=  t=@
        (scan (encode-data:abi:ethereum (data:abi:ethereum [i.types t])) hex)
      :: topic is @
      :-  ^-  @ux  %+  scan
            (encode-data:abi:ethereum (data:abi:ethereum [i.types i.topics]))
          hex
      ^-  (list ?(@ux (list @ux)))
      $(types t.types, topics t.topics)
    ==
::
  ++  event-inputs-to-face-pairs
    |=  =event
    ^-  tape
    %-  zing
    %+  join  " "
    %+  turn  inputs.event
    |=  [=event-input]
    "{(trip name.event-input)}={(etyp-to-aura type.event-input)}"
::
  ++  event-indexed-inputs-to-face-pairs
    |=  =event
    ^-  tape
    %-  zing
    %+  join  " "
    %+  turn  (skim inputs.event |=(i=event-input indexed.i))
    |=  [=event-input]
    "{(trip name.event-input)}={(etyp-to-topic type.event-input)}"
::
  ++  function-inputs-to-face-pairs
    |=  =function
    ^-  tape
    %-  zing
    %+  join  " "
    %+  turn  inputs.function
    |=  [=func-input]
    "{(trip name.func-input)}={(etyp-to-aura type.func-input)}"
::
  ++  function-outputs-to-face-pairs
    |=  =function
    %-  zing
    %+  join  " "
    %+  turn  outputs.function
    |=  =etyp
    "{(etyp-to-aura etyp)}"
::
  ++  etyp-to-topic
    |*  type=etyp
    :: ^-  tape
    ?+  type  !!
    ::
        %address
      "?(@ux (list @ux))"
        %uint
      "?(@ux (list @ud))"
        %bool
      "?(? (list ?))"
        %int :: doesn't work with decode lib yet
      !!
        %string
      "?(@t (list @t))"
        %bytes
      "?(octs (list octs))"
        [%bytes-n *]
      "?(octs (list octs))"
    ::
        [%array *]
      "?(@ux (list @ux))"
    ::
        [%array-n *]
      "?(@ux (list @ux))"
    ==
::
  ++  etyp-to-aura
    |*  type=etyp
    :: ^-  tape
    ?+  type  !!
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
        [%array *]
      "@ux"
        [%array-n *]
      "@ux"
    ==
    ::
  ++  name-to-lower
    |=  =tape
    ^-  ^tape
    ?~  tape  ~
    =/  tape=^tape
    :_  t.tape
      ?:  (lth i.tape 91)
        (add i.tape 32)
      i.tape
    %-  zing
    |-
    ?~  tape  ~
    ?~  t.tape
      [(trip i.tape) ~]
    ?:  (lth i.t.tape 91)
      =.  i.t.tape  (add i.t.tape 32)
      [(trip i.tape) "-" $(tape t.tape)]
    [(trip i.tape) $(tape t.tape)]
::
  ++  code-gen-mark
    |=  [sur-name=tape]
    %-  crip
    """
    /-  ethers
    /+  *eth-contracts-{sur-name}
    |_  gif=gift
    ++  grow
      |%
      ++  json  (gift-to-json gif)
      --
    ++  grab
      |%
        ++  noun  gift
        ++  ethers-gift
        |=  gaf=gift:ethers
        (gift gaf)
      --
    --
    """


  ++  code-gen-types
    |=  =contract
    =/  read-functions=(list function)
    (turn ~(tap by read-functions.contract) |=([* =function] function))
    =/  write-functions=(list function)
    (turn ~(tap by write-functions.contract) |=([* =function] function))
    =/  events=(list event)
    (turn ~(tap by events.contract) |=([* =event] event))
    |^
      %-  crip
      """
      /-  ethers
      =,  able:jael
      =/  builders  builders:ethers
      |%
        +$  gift
          $%  [$history =loglist]
              [$log =event-log]
              [$disavow =id:block]
              [$read-call read-call]
              [$read-tx read-tx]
          ==
        +$  poke
          $%  (make-action:builders $call call)
              (make-action:builders $send-tx send-tx)
              [$event-subscribe =path config=watch-config]
          ==
        +$  event-log  (event-log-config:builders event-update)
        +$  watch-config
          %-  watch-config:builders
          event-subscribe
        +$  loglist  (list event-log)
        +$  call
      {(function-calls read-functions)}
          ==
        +$  send-tx
      {(function-calls write-functions)}
          ==
        +$  read-call
      {(function-reads read-functions)}
          ==
        +$  read-tx
      {(function-reads write-functions)}
          ==
        +$  event-update
      {event-bucs}
          ==
        +$  event-subscribe
      {event-indexed-bucs}
          ==
      --
      """
      ++  nl  (trip 10)
      ++  event-bucs
        ^-  tape
        %-  zing
        ?~  events  ~
        :-
        =-  (zing ~["    $%  [${(trip name.i.events)} {-}]" nl])
          (event-inputs-to-face-pairs i.events)
        %+  turn  t.events
        |=  =event
        =-  (zing ~["        [${(trip name.event)} {-}]" nl])
          (event-inputs-to-face-pairs event)
      ++  event-indexed-bucs
        ^-  tape
        %-  zing
        ?~  events  ~
        :-
        =-  (zing ~["    $%  [${(trip name.i.events)} " ?~(- "~]" "{-} ~]") nl])
          (event-indexed-inputs-to-face-pairs i.events)
        %+  turn  t.events
        |=  =event
        =-  (zing ~["        [${(trip name.event)} " ?~(- "~]" "{-} ~]") nl])
        (event-indexed-inputs-to-face-pairs event)
      ++  function-calls
        |=  functions=(list function)
        ^-  tape
        %-  zing
        ?~  functions  ~
        :-
        =-  (zing ~["    $%  [${(trip name.i.functions)}" ?~(- " ~]" " {-}]") nl])
          (function-inputs-to-face-pairs i.functions)
        %+  turn  t.functions
        |=  =function
        =-  (zing ~["        [${(trip name.function)}" ?~(- " ~]" " {-}]") nl])
          (function-inputs-to-face-pairs function)
      ++  function-reads
        |=  functions=(list function)
        ^-  tape
        %-  zing
        ?~  functions  ~
        :-  =-  (zing ~["    $%  [${(trip name.i.functions)} out={-}]" nl])
          (function-outputs-to-face-pairs i.functions)
        %+  turn  t.functions
        |=  =function
        =-  %-  zing  ~["        [${(trip name.function)} out={-}]" nl]
          (function-outputs-to-face-pairs function)
    --
  ++  code-gen-lib
  |=  [=contract sur-name=@tas]
  ^-  cord
  =/  read-functions=(list function)
  (turn ~(tap by read-functions.contract) |=([* =function] function))
  =/  write-functions=(list function)
  (turn ~(tap by write-functions.contract) |=([* =function] function))
  =/  events=(list event)
  (turn ~(tap by events.contract) |=([* =event] event))
  |^
    %-  crip
    """
    /-  *eth-contracts-{(trip sur-name)}
    |%
      ++  gift-to-json
      |=  =gift
      =,  enjs:format
      ?-  -.gift
          %history
        %+  frond  %history
        [%a (turn loglist.gift event-log-to-json)]
          %log
        %+  frond  %event-update
        (event-log-to-json event-log.gift)
          %disavow
        !!
          %read-call
        !!
          %read-tx
        !!
      ==
      ++  event-log-to-json
        |=  [=event-log]
        ^-  json
        =,  enjs:format
    {log-json-cases}
    --
    """
  ++  nl  (trip 10)
  ++  log-json-cases
    =-
    """
        ?-  -.event-data.event-log
      {-}
        ==
    """
    %-  zing
    %+  turn  events
    |=  =event
    ^-  tape
    =-
    """
            %{(trip name.event)}
          %-  pairs
          :~
            [%type [%s %{(trip name.event)}]]
            [%address [%s (crip ((x-co:co 1) address.event-log))]]
            :-  %payload
            %-  pairs
            :~
    {-}
            ==
          ==
    """
    (inputs-json event)
  ++  inputs-json
    |=  =event
    %-  zing
    %+  turn  inputs.event
    |=  =event-input
    =-
    (zing ~["          [%{(trip name.event-input)} {-}]" nl])
    (etyp-to-json type.event-input name.event-input)
  ++  etyp-to-json
    |=  [=etyp name=@tas]
    ^-  tape
    ?+  etyp  ~|("unimplemented etyp-to-json type" !!)
        %uint
      "[%n (crip ((d-co:co 1) {(trip name)}.event-data.event-log))]"
        %int
      !!
    :: "[%n (scot %sd {(trip name)}.event-data.event-log)]"
        %address
      "[%s (crip ((x-co:co 1) {(trip name)}.event-data.event-log))]"
        %bool
      "[%b {(trip name)}.event-data.event-log]"
    ==
  --
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
    ?+    typ  ~&  "unexpected entry type"  !!
        %function
      |=  jun=json
      :-  %function
      ^-  function-raw
      %.  jun
      %-  ot
      |^
        :~  [%name so]
            [%inputs extract-func-input]
            [%outputs (ar extract-func-output)]
            [%constant |=(jen=json !(bo jen))]
            [%payable bo]
        ==
        ++  extract-func-input
          %-  ar
          %-  ot
          :~  [%name so]
              [%type so]
          ==
        ++  extract-func-output
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
      :-  (crip (name-to-lower (trip name.e)))
      %+  turn
        inputs.e
      |=  i=event-input-raw
      ^-  event-input
      [(crip (name-to-lower (trip name.i))) (parse-type type.i) indexed.i]
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
    %+  ~(put by fs)  (crip (name-to-lower (trip name.f)))
    :*
      (crip (name-to-lower (trip name.f)))
      name.f
      (get-hash name.f (turn inputs.f |=(=func-input-raw type.func-input-raw)))
      (parse-input-types inputs.f)
      (parse-output-types outputs.f)
    ==
::
  ++  get-read-functions
    |=  abi=(list entry-raw)
    ^-  (map @tas function)
    %+  roll  (get-raw-functions abi)
    |=  [f=function-raw fs=(map @tas function)]
    ?:  mut.f  fs
    %+  ~(put by fs)  (crip (name-to-lower (trip name.f)))
    :*
      (crip (name-to-lower (trip name.f)))
      name.f
      (get-hash name.f (turn inputs.f |=(=func-input-raw type.func-input-raw)))
      (parse-input-types inputs.f)
      (parse-output-types outputs.f)
    ==
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
  ++  parse-input-types
    |=  typs=(list func-input-raw)
    ^-  (list func-input)
    %+  turn  typs
    |=([name=@t type=@t] [(crip (name-to-lower (trip name))) (parse-type type)])
  ++  parse-output-types
    |=  typs=(list @t)
    ^-  (list etyp)
    %+  turn  typs  parse-type
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
