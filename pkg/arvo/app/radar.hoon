/+  default-agent, *server, dbug
::
|%
+$  card  card:agent:gall
::
+$  state-1
  $:  =ship-info
  ==
::
+$  ship-info    (map @p info)
+$  info         [status=ship-status data=(list datum)]
+$  datum        [sent=@da resp=[wen=@da wat=@uv]]
+$  ship-status
  $%  [%await-resp sent=@da]
      [%await-wake wake=@da]
  ==
::
++  jael-delay  ~m10
++  poke-delay  ~h1
++  save-delay  ~d1
--
::
=|  [%1 state-1]
=*  state  -
^-  agent:gall
%-  agent:dbug
::
=>  |%
    ++  deeded-ships
      |=  bowl:gall
      .^((set @p) %j /(scot %p our)/ships-with-deeds/(scot %da now))
    ::
    ++  request-kids-cz
      |=  [who=ship wen=@da]
      ^-  card:agent:gall
      :+  %pass  /clay-read/(scot %p who)
      [%arvo %c %warp who %kids ~ %sing %z da+wen /]
    ::
    ++  request-new
      |=  [woz=(set ship) =^ship-info wen=@da]
      %-  ~(rep in woz)
      |=  [who=@p [cad=(list card) dat=_ship-info]]
      :-  [(request-kids-cz who wen) cad]
      %+  ~(put by dat)  who
      [[%await-resp wen] data:(~(gut by dat) who *info)]
    ::
    ++  flush-data
      |=  si=^ship-info
      ^-  ^ship-info
      %-  ~(run by si)
      |=  i=info
      ^-  info
      [status.i ~]
    ::
    ++  data-as-json
      ^-  json
      :-  %o
      %-  ~(rep by ship-info)
      |=  [[who=@p ship-status data=(list datum)] out=(map @t json)]
      %+  ~(put by out)  (scot %p who)
      :-  %a
      %+  turn  (scag 100 data)
      |=  datum
      %-  pairs:enjs:format
      :~  ping+(time:enjs:format sent)
          response+(time:enjs:format wen.resp)
          result+s+(scot %uv wat.resp)
      ==
    ::
    --
|_  =bowl:gall
+*  this  .
    def  ~(. (default-agent this %|) bowl)
::
++  on-init
  ^-  (quip card _this)
  =/  jampath=path
    /(scot %p our.bowl)/home/(scot %da now.bowl)/radar/jam
  |^  =^  pokes  ship-info
        ?:  .^(? %cu jampath)  import
        =/  ships=(set ship)
          (deeded-ships bowl)
        (request-new ships ship-info now.bowl)
      :_  this
      %+  welp  pokes
      :~  [%pass /bind %arvo %e %connect [~ /'~radar'] dap.bowl]
          [%pass /jael-scry %arvo %b %wait (add now.bowl jael-delay)]
      ==
  ::.
  ++  import
    ^-  (quip card ^ship-info)
    =/  old
      ;;  versioned-state
      (cue .^(@ %cx jampath))
    =?  old  ?=(%0 -.old)
      :-  %1
      %-  ~(run by ship-info.old)
      |=  [status=ship-status-0 =data]
      ^-  [ship-status (list datum)]
      :-  ^-  ship-status
          ?-  -.status
            %waiting-for-coup  [%await-resp poked.status]
            %on-delay          [%await-wake wake.status]
          ==
      ^-  (list datum)
      %+  turn  data
      |=  [poked=@da couped=@da]
      [poked [couped 0v0]]
    ?>  ?=(%1 -.old)
    (request-new ~(key by ship-info.old) ship-info.old now.bowl)
  ::
  +$  versioned-state
    $%  [%0 ship-info=ship-info-0]
        [%1 state-1]
    ==
  ::
  +$  ship-info-0  (map @p [status=ship-status-0 =data])
  +$  data         (list [poked=@da couped=@da])
  +$  ship-status-0
    $%  [%waiting-for-coup poked=@da]
        [%on-delay wake=@da]
    ==
  --
::
++  on-save   !>(state)
::
++  on-load
  |=  old=vase
  ^-  (quip card _this)
  =/  old-state  !<([%1 state-1] old)
  [~ this(state old-state)]
::
++  on-arvo
  |=  [wir=wire sin=sign-arvo]
  ^-  (quip card _this)
  ~|  wir
  ?+  wir  !!
  ::
      [%bind ~]
    [~ this]
  ::
      [%save-timer ~]
    =/  =dill-blit:dill  [%sav /(scot %da now.bowl)/json q:(json-to-octs data-as-json)]
    =.  ship-info  (flush-data ship-info)
    :_  this
    :~  [%pass /save %agent [our.bowl %hood] %poke %dill-blit !>(dill-blit)]
        [%pass /save-timer %arvo %b %wait (add now.bowl save-delay)]
    ==
  ::
      [%jael-scry ~]
    ?>  ?=(%wake +<.sin)
    =/  new-ships  (deeded-ships bowl)
    =/  old-ships  ~(key by ship-info)
    =/  added      (~(dif in new-ships) old-ships)
    ::  removed should always be empty? do nothing with it for now
    =/  removed    (~(dif in old-ships) new-ships)
    ~?  (gth ~(wyt in added) 0)
      [%added added]
    ~?  (gth ~(wyt in removed) 0)
      [%removed removed]
    =^  pokes  ship-info
      (request-new added ship-info now.bowl)
    :_  this
    :_  pokes
    [%pass /jael-scry %arvo %b %wait (add now.bowl jael-delay)]
  ::
      [%delay @ ~]
    ?>  ?=(%wake +<.sin)
    =/  who=ship  (slav %p i.t.wir)
    =/  info  (~(got by ship-info) who)
    ?>  ?=(%await-wake -.status.info)
    =.  ship-info
      (~(put by ship-info) who [%await-resp now.bowl] data.info)
    :_  this
    [(request-kids-cz who now.bowl)]~
  ::
      [%clay-read @ ~]
    ~|  [%sign +<.sin]
    ?>  ?=(%writ +<.sin)
    =/  res=@uv
      ::NOTE  we don't *really* care about the result, only the ping,
      ::      but if we get it, it's a nice side-effect
      ::
      ?~  p.sin  0v0
      ~|  p.r.u.p.sin
      !<(@uvI q.r.u.p.sin)
    =/  who=ship  (slav %p i.t.wir)
    =/  info      (~(got by ship-info) who)
    ~|  [%status -.status.info]
    ?>  ?=(%await-resp -.status.info)
    =/  wake-time=@da  (add now.bowl poke-delay)
    =.  data.info  [[sent.status.info now.bowl res] data.info]
    =.  ship-info  (~(put by ship-info) who [%await-wake wake-time] data.info)
    :_  this
    [%pass /delay/(scot %p who) %arvo %b %wait wake-time]~
  ==
::
++  on-poke
  |=  [mar=mark vas=vase]
  ^-  (quip card _this)
  ?+  mar  (on-poke:def mar vas)
      %noun
    ?:  =(q.vas %start-saving)
      ~&  "saving on an interval of {<save-delay>}"
      :_  this
      [%pass /save-timer %arvo %b %wait (add now.bowl save-delay)]~
    ::
    ?:  =(q.vas %save-now)
      =/  =dill-blit:dill
        [%sav /(scot %da now.bowl)/json q:(json-to-octs data-as-json)]
      :_  this
      [%pass /save %agent [our.bowl %hood] %poke %dill-blit !>(dill-blit)]~
    ::
    ?:  =(-.q.vas %peers)
      =/  peers=(map ship ?(%alien %known))
        ;;((map ship ?(%alien %known)) +.q.vas)
      =/  new-ships  ~(key by peers)
      =/  old-ships  ~(key by ship-info)
      =/  added      (~(dif in new-ships) old-ships)
      ::  removed should always be empty? do nothing with it for now
      ~?  (gth ~(wyt in added) 0)
        [%added added]
      =^  pokes  ship-info
        (request-new added ship-info now.bowl)
      [pokes this]
    [~ this]
  ::
      %handle-http-request
    =+  !<([id=@ta req=inbound-request:eyre] vas)
    :_  this
    %+  give-simple-payload:app    id
    =/  url=request-line
      (parse-request-line url.request.req)
    ?+  url  not-found:gen
        [[[~ %json] [%'~radar' ~]] ~]
      ^-  simple-payload:http
      (json-response:gen data-as-json)
    ::
        [[[~ %json] [%'~radar' %alive @ ~]] ~]
      ^-  simple-payload:http
      %-  json-response:gen
      :-  %b
      ::  produce true if the ship was responsive in the last day
      ::
      ?~  ship=(rush i.t.t.site.url fed:ag)  |
      ?~  info=(~(get by ship-info) u.ship)  |
      ?~  data.u.info                        |
      (gth wen.resp.i.data.u.info (sub now.bowl ~d1))
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
++  on-agent  on-agent:def
++  on-fail   on-fail:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
--
