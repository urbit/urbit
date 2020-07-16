/+  default-agent, *server
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  [%0 state-0]
  ==
+$  ship-status
  $%  [%waiting-for-coup poked=@da]
      [%on-delay wake=@da]
  ==
+$  data  (list [poked=@da couped=@da])
+$  ship-info  (map @p [status=ship-status =data])
+$  state-0
  $:  =ship-info
  ==
++  jael-delay  ~m10
++  poke-delay  ~m30
--
=|  [%0 state-0]
=*  state  -
^-  agent:gall
|_  bol=bowl:gall
+*  this  .
    def  ~(. (default-agent this %|) bol)
::
++  on-init
  ^-  (quip card _this)
  =/  ships
    .^((set @p) %j /(scot %p our.bol)/ships-with-deeds/(scot %da now.bol))
  =^  pokes  ship-info
    %-  ~(rep in ships)
    |=  [who=@p [cad=(list card) dat=_ship-info]]
    :_  (~(put by dat) who [%waiting-for-coup now.bol] ~)
    :_  cad
    [%pass /poke/(scot %p who) %agent [who %publish] %poke %noun !>('')]
  :_  this
  %+  welp  pokes
  :~  [%pass /bind %arvo %e %connect [~ /'~radar'] %radar]
      [%pass /jael-scry %arvo %b %wait (add now.bol jael-delay)]
  ==
::
++  on-save   !>(state)
::
++  on-load
  |=  old=vase
  ^-  (quip card _this)
  =/  old-state  !<([%0 state-0] old)
  [~ this(state old-state)]
::
++  on-agent
  |=  [wir=wire sin=sign:agent:gall]
  ^-  (quip card _this)
  ?.  ?=(%poke-ack -.sin)
    [~ this]
  ?>  ?=([%poke @ ~] wir)
  =/  who=ship  (slav %p i.t.wir)
  =/  info  (~(got by ship-info) who)
  ?>  ?=(%waiting-for-coup -.status.info)
  =.  data.info  [[poked.status.info now.bol] data.info]
  =/  wake-time=@da  (add now.bol poke-delay)
  =.  ship-info  (~(put by ship-info) who [%on-delay wake-time] data.info)
  :_  this
  [%pass /delay/(scot %p who) %arvo %b %wait wake-time]~
::
++  on-arvo
  |=  [wir=wire sin=sign-arvo]
  ^-  (quip card _this)
  ?+  wir  !!
  ::
      [%bind ~]
    [~ this]
  ::
      [%jael-scry ~]
    =/  new-ships
      .^((set @p) %j /(scot %p our.bol)/ships-with-deeds/(scot %da now.bol))
    =/  old-ships  ~(key by ship-info)
    =/  added    (~(dif in new-ships) old-ships)
    ::  removed should always be empty? do nothing with it for now
    =/  removed  (~(dif in old-ships) new-ships)
    ~?  (gth ~(wyt in added) 0)
      [%added added]
    ~?  (gth ~(wyt in removed) 0)
      [%removed removed]
    =^  pokes  ship-info
      %-  ~(rep in added)
      |=  [who=@p [cad=(list card) dat=_ship-info]]
      :_  (~(put by dat) who [%waiting-for-coup now.bol] ~)
      :_  cad
      [%pass /poke/(scot %p who) %agent [who %hood] %poke %helm-hi !>('')]
    :_  this
    :_  pokes
    [%pass /jael-scry %arvo %b %wait (add now.bol jael-delay)]
  ::
      [%delay @ ~]
    =/  who=ship  (slav %p i.t.wir)
    =/  info  (~(got by ship-info) who)
    ?>  ?=(%on-delay -.status.info)
    =.  ship-info
      (~(put by ship-info) who [%waiting-for-coup now.bol] data.info)
    :_  this
    [%pass /poke/(scot %p who) %agent [who %publish] %poke %noun !>('')]~
  ==
::
++  on-poke
  |=  [mar=mark vas=vase]
  ^-  (quip card _this)
  ?+  mar  (on-poke:def mar vas)
      %noun
    ~&  state
    [~ this]
  ::
      %handle-http-request
    =+  !<([id=@ta req=inbound-request:eyre] vas)
    :_  this
    %+  give-simple-payload:app    id
    %+  require-authorization:app  req
    |=  req=inbound-request:eyre
    =/  url  (parse-request-line url.request.req)
    ?.  ?=([[[~ %json] [%'~radar' ~]] ~] url)
      not-found:gen
    ^-  simple-payload:http
    %-  json-response:gen
    %-  json-to-octs
    :-  %o
    %-  ~(rep by ship-info)
    |=  [[who=@p ship-status =data] out=(map @t json)]
    %+  ~(put by out)  (scot %p who)
    :-  %a
    %+  turn  data
    |=  [poked=@da couped=@da]
    %-  pairs:enjs:format
    :~  ping+(time:enjs:format poked)
        response+(time:enjs:format couped)
    ==
  ==
::
++  on-watch
  |=  pax=path
  ^-  (quip card _this)
  ?+  pax  (on-watch:def pax)
    [%http-response *]  [~ this]
  ==
::
++  on-fail   on-fail:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
--
