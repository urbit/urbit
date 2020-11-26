/+  *eth-abi-parse
|%
::  TODO: for some reason u.name.petyp is null sometimes
::  see petyp-to-bape hack
  ++  damn-skim
    |=  [es=(list event-input) b=?]
    ^-  (list event-input)
    %+  skim  es
    |=  =event-input
    =(indexed.event-input b)
::
  ++  get-all-functions-with-outputs
    |=  =contract
    ^-  (list function)
    =/  writes=(list function)
    %+  skim  ~(val by write-functions.contract)
    |=  =function
    ?~  outputs.function  |  &
    (weld ~(val by read-functions.contract) writes)

  ++  petyp-type-to-bape
    |=  type=petyp-type
    ^-  tape
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
      "tape"
        %bytes
      "octs"
        [%bytes-n *]
      "octs"
        [%array *]
      "(list {$(type t.type)})"
        [%array-n *]
      "(list {$(type t.type)})"
        [%tuple *]
      =-  :(weld "[" - "]")
      %-  tape
      %-  zing
      %+  join  " "
      %+  turn  t.type
      petyp-to-bape
    ==
  ++  petyp-to-bape
    |=  =petyp
    =-
    ?:  ?=(?(~ [~ ~]) name.petyp)  "{-}"
    "{(trip u.name.petyp)}={-}"
    (petyp-type-to-bape type.petyp)

  ++  petyp-to-topic-sub
    |=  =petyp
    :: only support multiple on atoms, because otherwise hard to encode
    :: need better types for topic formation
    :: could do better than rpc with urbit-side filtering
    =/  type=tape
    =-  ?.  ?=(?(%uint %bool %int %address %bytes %real %ureal) type.petyp)  "{-}"
        "?({-} (list {-}))"
    ?:  ?=([?(%array %array-n %tuple %bytes-n %string) *] type.petyp)
      "@ux"
    (petyp-type-to-bape type.petyp)
    ?~  name.petyp  type
    "{(trip u.name.petyp)}={type}"
    ::
  ++  petyp-type-to-etape
    |=  type=petyp-type
    |-  ^-  tape
    ?+  type  ~&  unexpected-type+type  !!
      ?(%uint %bool %int %address %bytes %real %ureal %string)
    "%{(trip type)}"
      [%bytes-n *]
    "[%bytes-n {(scow %ud n.type)}]"
      [%array *]
    "[%array {$(type t.type)}]"
      [%tuple *]
    =-  "[%tuple ~[{-}]]"
    %-  zing
    %+  join  " "
    (turn t.type petyp-to-etape)
    ==
  ++  petyp-to-etape
  ::  TODO: not working on arraysn or bytesn or tuples
    |=  =petyp
    ^-  tape
    (petyp-type-to-etape type.petyp)
::
  ++  code-gen-diff-mark
    |=  [sur-name=tape]
    %-  crip
    """
    /+  *eth-contracts-{sur-name}
    |_  dif=diff
    ++  grow
      |%
      ++  json  (diff-to-json dif)
      --
    ++  grab
      |%
        ++  noun  diff
        ++  eth-watcher-diff  decode-diff
      --
    --
    """
  ++  code-gen-esub-mark
    |=  [sur-name=tape]
    %-  crip
    """
    /+  *eth-contracts-{sur-name}
    |_  zub=esub
    ++  grow
      |%
      ++  eth-watcher-poke  (encode-esub zub)
      --
    ++  grab
      |%
        ++  noun  esub
      --
    --
    """

  ++  code-gen-call-mark
    |=  [sur-name=tape]
    %-  crip
    """
    /+  *eth-contracts-{sur-name}
    |_  cal=call:methods
    ++  grow
      |%
        ++  eth-call-data  (encode-call cal)
      --
    ++  grab
      |%
        ++  noun  call:methods
      --
    --
    """
  ++  code-gen-send-mark
    |=  [sur-name=tape]
    %-  crip
    """
    /+  *eth-contracts-{sur-name}
    |_  sen=send:methods
    ++  grow
      |%
        ++  eth-call-data  (encode-send sen)
      --
    ++  grab
      |%
        ++  noun  send:methods
      --
    --
    """
  ++  code-gen-res-mark
    |=  [sur-name=tape]
    %-  crip
    """
    /+  *eth-contracts-{sur-name}
    |_  re=res
    ++  grab
      |%
        ++  noun  res
        ++  eth-call-result  decode-res
      --
    --
    """

  ++  code-gen-types
    |=  [name=@tas =contract]  :: remove name since we're doing cages now
    =/  read-functions=(list function)
    ~(val by read-functions.contract)
    =/  write-functions=(list function)
    ~(val by write-functions.contract)
    =/  functions-with-outputs=(list function)
    (get-all-functions-with-outputs contract)
    =/  events=(list event)
    ~(val by events.contract)
    |^
      %-  crip
      """
      /-  eth-abi-magic
      =,  able:jael
      =,  builders=builders:eth-abi-magic
      |%
      {event-arms}
      {res}
      {methods}
      --
      """
      ++  event-arms
      ^-  tape
      ?~  events  ""
      """
      {event-update}
      {watch}
      +$  diff  ::  gift
          $%  [$history =loglist]
              [$log =event-log]
              [$disavow =id:block]
          ==
      +$  esub  ::  poke
          $%  [$watch =path config=watch-config]
              [$clear =path]
          ==
      +$  event-log  (event-log-config:builders event-update)
      +$  watch-config
          %-  watch-config:builders
          watch
      +$  loglist  (list event-log)
      """
      ++  methods
      ^-  tape
      ?.  ?!  |(?=(~ read-functions) ?=(~ write-functions))  ~
      """
      ++  methods
        |%
      {send}
      {call}
        --
      """
      ++  nl  (trip 10)
      ++  call
        ?~  read-functions  ""
        """
          ++  call
              $%
        {(function-calls read-functions)}
              ==
        """
      ++  send
        ?~  write-functions  ""
        """
          ++  send
              $%
        {(function-calls write-functions)}
              ==
        """
      ++  res
        ?~  functions-with-outputs  ""
        """
        +$  res
            $%
        {function-results}
            ==
        """
      ++  event-update
        ::  TODO: think no work for aren't anonymous functions
        """
        +$  event-update
            $%
        {event-bucs}
            ==
        """
      ++  watch
        """
        +$  watch
            $%
        {event-indexed-bucs}
            ==
        """
      ++  event-bucs
        ^-  tape
        %-  zing
        %+  join  nl
        %+  turn  events
        |=  =event
        :: =-  (zing ~["        [${(trip name.event)} {-}]" nl])
        =-  "        [${(trip name.event)} {(zing -)}]"
        %+  join  " "
        %+  turn  inputs.event
        |=  [=petyp ?]
        (petyp-to-bape petyp)
      ++  event-indexed-bucs
        ^-  tape
        %-  zing
        %+  join  nl
        %+  turn  events
        |=  =event
        =-  (zing ~["        [${(trip name.event)} " ?~(- "~]" "{(zing -)}]")])
        %+  join  " "
        %+  turn  (skim inputs.event |=([* i=?] i))
        |=  [=petyp ?]
        (petyp-to-topic-sub petyp)
      ++  function-calls
        |=  functions=(list function)
        ^-  tape
        %-  zing
        ?~  functions  ~
        %+  join  nl
        %+  turn  functions
        |=  =function
        =-  (zing ~["        [${(trip name.function)}" ?~(- " ~]" " {(zing -)}]")])
        %+  join  " "
        %+  turn  inputs.function
        petyp-to-bape
      ++  function-results
        :: |=  functions=(list function)
        ^-  tape
        %-  zing
        ?~  read-functions  ~
        %+  join  nl
        %+  turn  functions-with-outputs
        |=  =function
        =-  "        [${(trip name.function)} {(zing -)}]"
        %+  join  " "
        %+  turn  outputs.function
        petyp-to-bape
    --
::
  ++  code-gen-lib
    |=  [=contract sur-name=@tas]
    ^-  cord
    =/  read-functions=(list function)
    (turn ~(tap by read-functions.contract) |=([* =function] function))
    =/  write-functions=(list function)
    (turn ~(tap by write-functions.contract) |=([* =function] function))
    =/  functions-with-outputs=(list function)
    (get-all-functions-with-outputs contract)
    =/  events=(list event)
    (turn ~(tap by events.contract) |=([* =event] event))
    |^
      %-  crip
      """
      /-  eth-watcher, *eth-contracts-{(trip sur-name)}
      /+  eth-abi-magic
      |%
      :: ++  res-to-json
      ::   |=  =res
      ::   =,  enjs:format
      {event-arms}
      {encode-send}
      {encode-call}
      {decode-res}
      --
      """
    ++  event-arms
      ?~  events  ""
      """
      {decode-log}
      {decode-diff}
      {diff-to-json}
      {event-log-to-json}
      """
    ++  diff-to-json
    """
    ++  diff-to-json
    |=  =diff
    =,  enjs:format
    ^-  json
    %+  frond  %{(trip sur-name)}
    ?-  -.diff
        %history
      %+  frond  %history
      [%a (turn loglist.diff event-log-to-json)]
        %log
      %+  frond  %log
      (event-log-to-json event-log.diff)
        %disavow
      !!
    ==
    """
    ++  event-log-to-json
    """
    ++  event-log-to-json
      |=  [=event-log]
      ^-  json
      =,  enjs:format
    {log-json-cases}
    ++  encode-esub
      |=  =esub
      ^-  poke:eth-watcher
      ?-  -.esub
          %watch
    {encode-esub-cases}
          %clear
        esub
      ==
    """
    ++  decode-diff
    """
    ++  decode-diff
      |=  =diff:eth-watcher
      ^-  ^diff
      ?-  -.diff
          %history
        :-  %history
        %+  turn  loglist.diff  decode-log
          %log
        :-  %log  (decode-log event-log.diff)
          %disavow
        diff
      ==
    """
    ++  decode-res
    ?~  functions-with-outputs  ""
    """
    ++  decode-res
      |=  [name=@tas result=@t]
      ^-  res
      ?+  name  ~|  "unexpected result in contract {(trip sur-name)}"  !!
    {encode-result-cases}
      ==
    """
    ++  encode-send
    ?~  write-functions  ""
    """
    ++  encode-send
      |=  =send:methods
      ^-  call-data:rpc:ethereum
      ?-  -.send
    {(encode-method-cases write-functions.contract %send)}
      ==
    """
    ++  decode-log
    """
    ++  decode-log
      |=  [=event-log:rpc:ethereum]
      ^-  ^event-log
    {decode-log-cases}
    ~|  "unexpected event in {(trip name.contract)}"  !!
    """
    ++  encode-call
    ?~  read-functions  ""
    """
    ++  encode-call
    |=  =call:methods
    ^-  call-data:rpc:ethereum
    ?-  -.call
    {(encode-method-cases read-functions.contract %call)}
    ==
    """
    ++  nl  (trip 10)
    ++  encode-result-cases
      :: |=  [functions=(map @tas function) face=@tas]
      %-  zing
      %+  join  nl
      %+  turn  functions-with-outputs
      |=  =function
      =/  outputs=tape
        %-  zing
        %+  join  " "
        (turn outputs.function |=(fo=func-output (petyp-to-etape petyp.fo)))
      =-
      """
              %{(trip name.function)}
            {-}
      """
      ?~  outputs  "[%{(trip name.function)} ~]"
      "[%{(trip name.function)} (decode-results:decode:eth-abi-magic result ~[{outputs}])]"
    ++  encode-method-cases
      |=  [functions=(map @tas function) arm=@tas]
      %-  zing
      %+  join  nl
      %+  turn  ~(tap by functions)
      |=  [name=@tas =function]
      =/  inputs=tape
        %-  zing
        %+  join  " "
        (turn inputs.function |=(fi=func-input (petyp-to-etape petyp.fi)))
      =-
      """
              %{(trip name)}
            :-  '{(trip sel.function)}'
            {-}
      """
      ?~  inputs  "~"
      "(tuple-to-eth-data:eth-abi-magic ~[{inputs}] +.{(trip arm)})"
    ++  decode-log-cases
      %-  zing
      %+  join  nl
      %+  turn  ~(tap by events.contract)
      |=  [hash=@ux =event]
      ^-  tape
      ?~  inputs.event
      """
        ?:  =(i.topics.event-log {(scow %ux hash)})
          [%{(trip name.event)} ~]
      """
      |^
      =/  topic-types=tape
      %-  topics-to-etape  (damn-skim inputs.event &)
      =/  data-types=tape
      %-  topics-to-etape  (damn-skim inputs.event |)
      =-
      """
        ?:  =(i.topics.event-log {(scow %ux hash)})
          :*  mined.event-log
              address.event-log
      {-}
          ==
      """
      ?~  topic-types
        ?~  data-types
          "        [%{(trip name.event)} ~]"
        =-  "        [%{(trip name.event)} {-}]"
        "(decode-results:decode:eth-abi-magic data.event-log ~[{data-types}])"
      ?~  data-types
        =-  "        [%{(trip name.event)} {-}]"
        "(decode-topics:decode:eth-abi-magic t.topics.event-log ~[{topic-types}])"
      """
              :-  %{(trip name.event)}
              %:        event-to-tuple:eth-abi-magic
                      ~[{topic-types}]
                    t.topics.event-log
                ~[{data-types}]
              data.event-log
              ==
      """
      ++  topics-to-etape
        |=  topics=(list event-input)
        %-  zing
        %+  join  " "
        %+  turn  topics
        |=  [=petyp *]
        ^-  tape
        (petyp-to-etape petyp)
      --
      ::
    ++  encode-esub-cases
      =-
      """
            =/  =topics:eth-watcher
              ?-  -.topics.config.esub
      {-}
              ==
            =/  =config:eth-watcher
              :*  url=url.config.esub
                  eager=eager.config.esub
                  refresh-rate=refresh-rate.config.esub
                  timeout-time=timeout-time.config.esub
                  from=from.config.esub
                  contracts=contracts.config.esub
                  topics
              ==
            :*  %watch
                path.esub
                config
            ==
      """
      %-  zing
      %+  join  (trip `@t`10)
      %+  turn  ~(tap by events.contract)
      |=  [hash=@ux =event]
      ^-  tape
      =/  indexed=(list event-input)  (skim inputs.event |=(inp=event-input indexed.inp))
      =-
      ?~  indexed
      """
                  %{(trip name.event)}
                [{(scow %ux hash)} ~]
      """
      """
                  %{(trip name.event)}
                :-  {(scow %ux hash)}
                (encode-topics:eth-abi-magic ~[{-}] +.topics.config.esub)
      """
      %-  zing
      %+  join  " "
      %+  turn   indexed
      |=  [inp=event-input]
      :: ?.  indexed.i  ""
      (petyp-to-etape petyp.inp)

    ++  log-json-cases
      =-
      """
            ?-  -.event-data.event-log
      {-}
            ==
      """
      %-  zing
      %+  join  (trip `@t`10)
      %+  turn  events
      |=  =event
      ^-  tape
      =-
      """
                %{(trip name.event)}
              %-  pairs
              :*
                [%type [%s %{(trip name.event)}]]
                [%address [%s (crip (z-co:co address.event-log))]]
                :-  %payload
                %-  pairs
                :~
      {-}
                ==
                ?~  mined.event-log  ~
                :~
                :-  'txHash'
                [%s (crip (z-co:co transaction-hash.u.mined.event-log))]
                :-  'block'
                [%s (crip ((d-co:co 1) block-number.u.mined.event-log))]
                ==
              ==
      """
      (inputs-json event)
    ++  inputs-json
      |=  =event
      %-  zing
      %+  turn  inputs.event
      |=  inp=event-input
      ?>  ?=([~ *] name.petyp.inp)
      =-
      (zing ~["          [%{(trip u.name.petyp.inp)} {-}]" nl])
      (etyp-to-json (petyp-to-etyp petyp.inp) u.name.petyp.inp)
    ++  etyp-to-json
      :: TODO: tuple, array, etc
      |=  [=etyp name=@tas]
      ^-  tape
      ?+  etyp  ~|(unimplemented-type+etyp !!)
          %uint
        "[%n (crip ((d-co:co 1) {(trip name)}.event-data.event-log))]"
          %int
        !!
      :: "[%n (scot %sd {(trip name)}.event-data.event-log)]"
          [%bytes-n *]
        "[%s (crip (zing ~[\"0\" \"x\" (trip (en:base16:mimes:html {(trip name)}.event-data.event-log))]))]"
          %string
        "[%s (crip {(trip name)}.event-data.event-log)]"
          %bytes
        "[%s (crip (zing ~[\"0\" \"x\" (trip (en:base16:mimes:html {(trip name)}.event-data.event-log))]))]"
          %address
        "[%s (crip (z-co:co {(trip name)}.event-data.event-log))]"
          %bool
        "[%b {(trip name)}.event-data.event-log]"
      ==
  --
--
