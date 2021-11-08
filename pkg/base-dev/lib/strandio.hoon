/-  spider
/+  libstrand=strand
=,  strand=strand:libstrand
=,  strand-fail=strand-fail:libstrand
|%
++  send-raw-cards
  |=  cards=(list =card:agent:gall)
  =/  m  (strand ,~)
  ^-  form:m
  |=  strand-input:strand
  [cards %done ~]
::
++  send-raw-card
  |=  =card:agent:gall
  =/  m  (strand ,~)
  ^-  form:m
  (send-raw-cards card ~)
::
++  ignore
  |=  tin=strand-input:strand
  `[%fail %ignore ~]
::
++  get-bowl
  =/  m  (strand ,bowl:strand)
  ^-  form:m
  |=  tin=strand-input:strand
  `[%done bowl.tin]
::
++  get-beak
  =/  m  (strand ,beak)
  ^-  form:m
  |=  tin=strand-input:strand
  `[%done [our q.byk da+now]:bowl.tin]
::
++  get-time
  =/  m  (strand ,@da)
  ^-  form:m
  |=  tin=strand-input:strand
  `[%done now.bowl.tin]
::
++  get-our
  =/  m  (strand ,ship)
  ^-  form:m
  |=  tin=strand-input:strand
  `[%done our.bowl.tin]
::
++  get-entropy
  =/  m  (strand ,@uvJ)
  ^-  form:m
  |=  tin=strand-input:strand
  `[%done eny.bowl.tin]
::
::  Convert skips to %ignore failures.
::
::    This tells the main loop to try the next handler.
::
++  handle
  |*  a=mold
  =/  m  (strand ,a)
  |=  =form:m
  ^-  form:m
  |=  tin=strand-input:strand
  =/  res  (form tin)
  =?  next.res  ?=(%skip -.next.res)
    [%fail %ignore ~]
  res
::
::  Wait for a poke with a particular mark
::
++  take-poke
  |=  =mark
  =/  m  (strand ,vase)
  ^-  form:m
  |=  tin=strand-input:strand
  ?+  in.tin  `[%skip ~]
      ~  `[%wait ~]
      [~ %poke @ *]
    ?.  =(mark p.cage.u.in.tin)
      `[%skip ~]
    `[%done q.cage.u.in.tin]
  ==
::
::
::
++  take-sign-arvo
  =/  m  (strand ,[wire sign-arvo])
  ^-  form:m
  |=  tin=strand-input:strand
  ?+  in.tin  `[%skip ~]
      ~  `[%wait ~]
      [~ %sign *]
    `[%done [wire sign-arvo]:u.in.tin]
  ==
::
::  Wait for a subscription update on a wire
::
++  take-fact-prefix
  |=  =wire
  =/  m  (strand ,[path cage])
  ^-  form:m
  |=  tin=strand-input:strand
  ?+  in.tin  `[%skip ~]
      ~  `[%wait ~]
      [~ %agent * %fact *]
    ?.  =(watch+wire (scag +((lent wire)) wire.u.in.tin))
      `[%skip ~]
    `[%done (slag (lent wire) wire.u.in.tin) cage.sign.u.in.tin]
  ==
::
::  Wait for a subscription update on a wire
::
++  take-fact
  |=  =wire
  =/  m  (strand ,cage)
  ^-  form:m
  |=  tin=strand-input:strand
  ?+  in.tin  `[%skip ~]
      ~  `[%wait ~]
      [~ %agent * %fact *]
    ?.  =(watch+wire wire.u.in.tin)
      `[%skip ~]
    `[%done cage.sign.u.in.tin]
  ==
::
::  Wait for a subscription close
::
++  take-kick
  |=  =wire
  =/  m  (strand ,~)
  ^-  form:m
  |=  tin=strand-input:strand
  ?+  in.tin  `[%skip ~]
      ~  `[%wait ~]
      [~ %agent * %kick *]
    ?.  =(watch+wire wire.u.in.tin)
      `[%skip ~]
    `[%done ~]
  ==
::
++  echo
  =/  m  (strand ,~)
  ^-  form:m
  %-  (main-loop ,~)
  :~  |=  ~
      ^-  form:m
      ;<  =vase  bind:m  ((handle ,vase) (take-poke %echo))
      =/  message=tape  !<(tape vase)
      %-  (slog leaf+"{message}..." ~)
      ;<  ~      bind:m  (sleep ~s2)
      %-  (slog leaf+"{message}.." ~)
      (pure:m ~)
  ::
      |=  ~
      ^-  form:m
      ;<  =vase  bind:m  ((handle ,vase) (take-poke %over))
      %-  (slog leaf+"over..." ~)
      (pure:m ~)
  ==
::
++  take-watch
  =/  m  (strand ,path)
  |=  tin=strand-input:strand
  ?+  in.tin  `[%skip ~]
      ~  `[%wait ~]
      [~ %watch *]
    `[%done path.u.in.tin]
  ==
::
++  take-wake
  |=  until=(unit @da)
  =/  m  (strand ,~)
  ^-  form:m
  |=  tin=strand-input:strand
  ?+  in.tin  `[%skip ~]
      ~  `[%wait ~]
      [~ %sign [%wait @ ~] %behn %wake *]
    ?.  |(?=(~ until) =(`u.until (slaw %da i.t.wire.u.in.tin)))
      `[%skip ~]
    ?~  error.sign-arvo.u.in.tin
      `[%done ~]
    `[%fail %timer-error u.error.sign-arvo.u.in.tin]
  ==
::
++  take-poke-ack
  |=  =wire
  =/  m  (strand ,~)
  ^-  form:m
  |=  tin=strand-input:strand
  ?+  in.tin  `[%skip ~]
      ~  `[%wait ~]
      [~ %agent * %poke-ack *]
    ?.  =(wire wire.u.in.tin)
      `[%skip ~]
    ?~  p.sign.u.in.tin
      `[%done ~]
    `[%fail %poke-fail u.p.sign.u.in.tin]
  ==
::
++  take-watch-ack
  |=  =wire
  =/  m  (strand ,~)
  ^-  form:m
  |=  tin=strand-input:strand
  ?+  in.tin  `[%skip ~]
      ~  `[%wait ~]
      [~ %agent * %watch-ack *]
    ?.  =(watch+wire wire.u.in.tin)
      `[%skip ~]
    ?~  p.sign.u.in.tin
      `[%done ~]
    `[%fail %watch-ack-fail u.p.sign.u.in.tin]
  ==
::
++  poke
  |=  [=dock =cage]
  =/  m  (strand ,~)
  ^-  form:m
  =/  =card:agent:gall  [%pass /poke %agent dock %poke cage]
  ;<  ~  bind:m  (send-raw-card card)
  (take-poke-ack /poke)
::
++  raw-poke
  |=  [=dock =cage]
  =/  m  (strand ,~)
  ^-  form:m
  =/  =card:agent:gall  [%pass /poke %agent dock %poke cage]
  ;<  ~  bind:m  (send-raw-card card)
  =/  m  (strand ,~)
  ^-  form:m
  |=  tin=strand-input:strand
  ?+  in.tin  `[%skip ~]
      ~
    `[%wait ~]
  ::
      [~ %agent * %poke-ack *]
    ?.  =(/poke wire.u.in.tin)
      `[%skip ~]
    `[%done ~]
  ==
::
++  raw-poke-our
  |=  [app=term =cage]
  =/  m  (strand ,~)
  ^-  form:m
  ;<  =bowl:spider  bind:m  get-bowl
  (raw-poke [our.bowl app] cage)
::
++  poke-our
  |=  [=term =cage]
  =/  m  (strand ,~)
  ^-  form:m
  ;<  our=@p  bind:m  get-our
  (poke [our term] cage)
::
++  watch
  |=  [=wire =dock =path]
  =/  m  (strand ,~)
  ^-  form:m
  =/  =card:agent:gall  [%pass watch+wire %agent dock %watch path]
  ;<  ~  bind:m  (send-raw-card card)
  (take-watch-ack wire)
::
++  watch-one
  |=  [=wire =dock =path]
  =/  m  (strand ,cage)
  ^-  form:m
  ;<  ~  bind:m  (watch wire dock path)
  ;<  =cage  bind:m  (take-fact wire)
  ;<  ~  bind:m  (take-kick wire)
  (pure:m cage)
::
++  watch-our
  |=  [=wire =term =path]
  =/  m  (strand ,~)
  ^-  form:m
  ;<  our=@p  bind:m  get-our
  (watch wire [our term] path)
::
++  scry
  |*  [=mold =path]
  =/  m  (strand ,mold)
  ^-  form:m
  ?>  ?=(^ path)
  ?>  ?=(^ t.path)
  ;<  =bowl:spider  bind:m  get-bowl
  %-  pure:m
  .^(mold i.path (scot %p our.bowl) i.t.path (scot %da now.bowl) t.t.path)
::
++  leave
  |=  [=wire =dock]
  =/  m  (strand ,~)
  ^-  form:m
  =/  =card:agent:gall  [%pass watch+wire %agent dock %leave ~]
  (send-raw-card card)
::
++  leave-our
  |=  [=wire =term]
  =/  m  (strand ,~)
  ^-  form:m
  ;<  our=@p  bind:m  get-our
  (leave wire [our term])
::
++  rewatch
  |=  [=wire =dock =path]
  =/  m  (strand ,~)
  ;<  ~  bind:m  ((handle ,~) (take-kick wire))
  ;<  ~  bind:m  (flog-text "rewatching {<dock>} {<path>}")
  ;<  ~  bind:m  (watch wire dock path)
  (pure:m ~)
::
++  wait
  |=  until=@da
  =/  m  (strand ,~)
  ^-  form:m
  ;<  ~  bind:m  (send-wait until)
  (take-wake `until)
::
++  sleep
  |=  for=@dr
  =/  m  (strand ,~)
  ^-  form:m
  ;<  now=@da  bind:m  get-time
  (wait (add now for))
::
++  send-wait
  |=  until=@da
  =/  m  (strand ,~)
  ^-  form:m
  =/  =card:agent:gall
    [%pass /wait/(scot %da until) %arvo %b %wait until]
  (send-raw-card card)
::
++  map-err
  |*  computation-result=mold
  =/  m  (strand ,computation-result)
  |=  [f=$-([term tang] [term tang]) computation=form:m]
  ^-  form:m
  |=  tin=strand-input:strand
  =*  loop  $
  =/  c-res  (computation tin)
  ?:  ?=(%cont -.next.c-res)
    c-res(self.next ..loop(computation self.next.c-res))
  ?.  ?=(%fail -.next.c-res)
    c-res
  c-res(err.next (f err.next.c-res))
::
++  set-timeout
  |*  computation-result=mold
  =/  m  (strand ,computation-result)
  |=  [time=@dr computation=form:m]
  ^-  form:m
  ;<  now=@da  bind:m  get-time
  =/  when  (add now time)
  =/  =card:agent:gall
    [%pass /timeout/(scot %da when) %arvo %b %wait when]
  ;<  ~        bind:m  (send-raw-card card)
  |=  tin=strand-input:strand
  =*  loop  $
  ?:  ?&  ?=([~ %sign [%timeout @ ~] %behn %wake *] in.tin)
          =((scot %da when) i.t.wire.u.in.tin)
      ==
    `[%fail %timeout ~]
  =/  c-res  (computation tin)
  ?:  ?=(%cont -.next.c-res)
    c-res(self.next ..loop(computation self.next.c-res))
  ?:  ?=(%done -.next.c-res)
    =/  =card:agent:gall
      [%pass /timeout/(scot %da when) %arvo %b %rest when]
    c-res(cards [card cards.c-res])
  c-res
::
++  send-request
  |=  =request:http
  =/  m  (strand ,~)
  ^-  form:m
  (send-raw-card %pass /request %arvo %i %request request *outbound-config:iris)
::
++  send-cancel-request
  =/  m  (strand ,~)
  ^-  form:m
  (send-raw-card %pass /request %arvo %i %cancel-request ~)
::
++  take-client-response
  =/  m  (strand ,client-response:iris)
  ^-  form:m
  |=  tin=strand-input:strand
  ?+  in.tin  `[%skip ~]
      ~  `[%wait ~]
    ::
      [~ %sign [%request ~] %iris %http-response %cancel *]
    ::NOTE  iris does not (yet?) retry after cancel, so it means failure
    :-  ~
    :+  %fail
      %http-request-cancelled
    ['http request was cancelled by the runtime']~
    ::
      [~ %sign [%request ~] %iris %http-response %finished *]
    `[%done client-response.sign-arvo.u.in.tin]
  ==
::
::  Wait until we get an HTTP response or cancelation and unset contract
::
++  take-maybe-sigh
  =/  m  (strand ,(unit httr:eyre))
  ^-  form:m
  ;<  rep=(unit client-response:iris)  bind:m
    take-maybe-response
  ?~  rep
    (pure:m ~)
  ::  XX s/b impossible
  ::
  ?.  ?=(%finished -.u.rep)
    (pure:m ~)
  (pure:m (some (to-httr:iris +.u.rep)))
::
++  take-maybe-response
  =/  m  (strand ,(unit client-response:iris))
  ^-  form:m
  |=  tin=strand-input:strand
  ?+  in.tin  `[%skip ~]
      ~  `[%wait ~]
      [~ %sign [%request ~] %iris %http-response %cancel *]
    `[%done ~]
      [~ %sign [%request ~] %iris %http-response %finished *]
    `[%done `client-response.sign-arvo.u.in.tin]
  ==
::
++  extract-body
  |=  =client-response:iris
  =/  m  (strand ,cord)
  ^-  form:m
  ?>  ?=(%finished -.client-response)
  %-  pure:m
  ?~  full-file.client-response  ''
  q.data.u.full-file.client-response

::
++  fetch-cord
  |=  url=tape
  =/  m  (strand ,cord)
  ^-  form:m
  =/  =request:http  [%'GET' (crip url) ~ ~]
  ;<  ~                      bind:m  (send-request request)
  ;<  =client-response:iris  bind:m  take-client-response
  (extract-body client-response)
::
++  fetch-json
  |=  url=tape
  =/  m  (strand ,json)
  ^-  form:m
  ;<  =cord  bind:m  (fetch-cord url)
  =/  json=(unit json)  (de-json:html cord)
  ?~  json
    (strand-fail %json-parse-error ~)
  (pure:m u.json)
::
++  hiss-request
  |=  =hiss:eyre
  =/  m  (strand ,(unit httr:eyre))
  ^-  form:m
  ;<  ~  bind:m  (send-request (hiss-to-request:html hiss))
  take-maybe-sigh
::
::  +build-file: build the source file at the specified $beam
::
++  build-file
  |=  [[=ship =desk =case] =spur]
  =*  arg  +<
  =/  m  (strand ,(unit vase))
  ^-  form:m
  ;<  =riot:clay  bind:m
    (warp ship desk ~ %sing %a case spur)
  ?~  riot
    (pure:m ~)
  ?>  =(%vase p.r.u.riot)
  (pure:m (some !<(vase q.r.u.riot)))
::  +build-mark: build a mark definition to a $dais
::
++  build-mark
  |=  [[=ship =desk =case] mak=mark]
  =*  arg  +<
  =/  m  (strand ,dais:clay)
  ^-  form:m
  ;<  =riot:clay  bind:m
    (warp ship desk ~ %sing %b case /[mak])
  ?~  riot
    (strand-fail %build-mark >arg< ~)
  ?>  =(%dais p.r.u.riot)
  (pure:m !<(dais:clay q.r.u.riot))
::  +build-tube: build a mark conversion gate ($tube)
::
++  build-tube
  |=  [[=ship =desk =case] =mars:clay]
  =*  arg  +<
  =/  m  (strand ,tube:clay)
  ^-  form:m
  ;<  =riot:clay  bind:m
    (warp ship desk ~ %sing %c case /[a.mars]/[b.mars])
  ?~  riot
    (strand-fail %build-tube >arg< ~)
  ?>  =(%tube p.r.u.riot)
  (pure:m !<(tube:clay q.r.u.riot))
::
::  +build-nave: build a mark definition to a $nave
::
++  build-nave
  |=  [[=ship =desk =case] mak=mark]
  =*  arg  +<
  =/  m  (strand ,vase)
  ^-  form:m
  ;<  =riot:clay  bind:m
    (warp ship desk ~ %sing %e case /[mak])
  ?~  riot
    (strand-fail %build-nave >arg< ~)
  ?>  =(%nave p.r.u.riot)
  (pure:m q.r.u.riot)
::  +build-cast: build a mark conversion gate (static)
::
++  build-cast
  |=  [[=ship =desk =case] =mars:clay]
  =*  arg  +<
  =/  m  (strand ,vase)
  ^-  form:m
  ;<  =riot:clay  bind:m
    (warp ship desk ~ %sing %f case /[a.mars]/[b.mars])
  ?~  riot
    (strand-fail %build-cast >arg< ~)
  ?>  =(%cast p.r.u.riot)
  (pure:m q.r.u.riot)
::
::  Read from Clay
::
++  warp
  |=  [=ship =riff:clay]
  =/  m  (strand ,riot:clay)
  ;<  ~  bind:m  (send-raw-card %pass /warp %arvo %c %warp ship riff)
  (take-writ /warp)
::
++  read-file
  |=  [[=ship =desk =case:clay] =spur]
  =*  arg  +<
  =/  m  (strand ,cage)
  ;<  =riot:clay  bind:m  (warp ship desk ~ %sing %x case spur)
  ?~  riot
    (strand-fail %read-file >arg< ~)
  (pure:m r.u.riot)
::
++  check-for-file
  |=  [[=ship =desk =case:clay] =spur]
  =/  m  (strand ,?)
  ;<  =riot:clay  bind:m  (warp ship desk ~ %sing %x case spur)
  (pure:m ?=(^ riot))
::
++  list-tree
  |=  [[=ship =desk =case:clay] =spur]
  =*  arg  +<
  =/  m  (strand ,(list path))
  ;<  =riot:clay  bind:m  (warp ship desk ~ %sing %t case spur)
  ?~  riot
    (strand-fail %list-tree >arg< ~)
  (pure:m !<((list path) q.r.u.riot))
::
::  Take Clay read result
::
++  take-writ
  |=  =wire
  =/  m  (strand ,riot:clay)
  ^-  form:m
  |=  tin=strand-input:strand
  ?+  in.tin  `[%skip ~]
      ~  `[%wait ~]
      [~ %sign * ?(%behn %clay) %writ *]
    ?.  =(wire wire.u.in.tin)
      `[%skip ~]
    `[%done +>.sign-arvo.u.in.tin]
  ==
::  +check-online: require that peer respond before timeout
::
++  check-online
  |=  [who=ship lag=@dr]
  =/  m  (strand ,~)
  ^-  form:m
  %+  (map-err ,~)  |=(* [%offline *tang])
  %+  (set-timeout ,~)  lag
  ;<  ~  bind:m
    (poke [who %hood] %helm-hi !>(~))
  (pure:m ~)
::
::  Queue on skip, try next on fail %ignore
::
++  main-loop
  |*  a=mold
  =/  m  (strand ,~)
  =/  m-a  (strand ,a)
  =|  queue=(qeu (unit input:strand))
  =|  active=(unit [in=(unit input:strand) =form:m-a forms=(list $-(a form:m-a))])
  =|  state=a
  |=  forms=(lest $-(a form:m-a))
  ^-  form:m
  |=  tin=strand-input:strand
  =*  top  `form:m`..$
  =.  queue  (~(put to queue) in.tin)
  |^  (continue bowl.tin)
  ::
  ++  continue
    |=  =bowl:strand
    ^-  output:m
    ?>  =(~ active)
    ?:  =(~ queue)
      `[%cont top]
    =^  in=(unit input:strand)  queue  ~(get to queue)
    ^-  output:m
    =.  active  `[in (i.forms state) t.forms]
    ^-  output:m
    (run bowl in)
  ::
  ++  run
    ^-  form:m
    |=  tin=strand-input:strand
    ^-  output:m
    ?>  ?=(^ active)
    =/  res  (form.u.active tin)
    =/  =output:m
      ?-  -.next.res
          %wait  `[%wait ~]
          %skip  `[%cont ..$(queue (~(put to queue) in.tin))]
          %cont  `[%cont ..$(active `[in.u.active self.next.res forms.u.active])]
          %done  (continue(active ~, state value.next.res) bowl.tin)
          %fail
        ?:  &(?=(^ forms.u.active) ?=(%ignore p.err.next.res))
          %=  $
            active  `[in.u.active (i.forms.u.active state) t.forms.u.active]
            in.tin  in.u.active
          ==
        `[%fail err.next.res]
      ==
    [(weld cards.res cards.output) next.output]
  --
::
++  retry
  |*  result=mold
  |=  [crash-after=(unit @ud) computation=_*form:(strand (unit result))]
  =/  m  (strand ,result)
  =|  try=@ud
  |-  ^-  form:m
  =*  loop  $
  ?:  =(crash-after `try)
    (strand-fail %retry-too-many ~)
  ;<  ~                  bind:m  (backoff try ~m1)
  ;<  res=(unit result)  bind:m  computation
  ?^  res
    (pure:m u.res)
  loop(try +(try))
::
++  backoff
  |=  [try=@ud limit=@dr]
  =/  m  (strand ,~)
  ^-  form:m
  ;<  eny=@uvJ  bind:m  get-entropy
  %-  sleep
  %+  min  limit
  ?:  =(0 try)  ~s0
  %+  add
    (mul ~s1 (bex (dec try)))
  (mul ~s0..0001 (~(rad og eny) 1.000))
::
::    ----
::
::  Output
::
++  flog
  |=  =flog:dill
  =/  m  (strand ,~)
  ^-  form:m
  (send-raw-card %pass / %arvo %d %flog flog)
::
++  flog-text
  |=  =tape
  =/  m  (strand ,~)
  ^-  form:m
  (flog %text tape)
::
++  flog-tang
  |=  =tang
  =/  m  (strand ,~)
  ^-  form:m
  =/  =wall
    (zing (turn (flop tang) (cury wash [0 80])))
  |-  ^-  form:m
  =*  loop  $
  ?~  wall
    (pure:m ~)
  ;<  ~  bind:m  (flog-text i.wall)
  loop(wall t.wall)
::
++  trace
  |=  =tang
  =/  m  (strand ,~)
  ^-  form:m
  (pure:m ((slog tang) ~))
::
++  app-message
  |=  [app=term =cord =tang]
  =/  m  (strand ,~)
  ^-  form:m
  =/  msg=tape  :(weld (trip app) ": " (trip cord))
  ;<  ~  bind:m  (flog-text msg)
  (flog-tang tang)
::
::    ----
::
::  Handle domains
::
++  install-domain
  |=  =turf
  =/  m  (strand ,~)
  ^-  form:m
  (send-raw-card %pass / %arvo %e %rule %turf %put turf)
::
::    ----
::
::  Threads
::
++  start-thread
  |=  file=term
  =/  m  (strand ,tid:spider)
  ;<  =bowl:spider  bind:m  get-bowl
  (start-thread-with-args byk.bowl file *vase)
::
++  start-thread-with-args
  |=  [=beak file=term args=vase]
  =/  m  (strand ,tid:spider)
  ^-  form:m
  ;<  =bowl:spider  bind:m  get-bowl
  =/  tid
    (scot %ta (cat 3 (cat 3 'strand_' file) (scot %uv (sham file eny.bowl))))
  =/  poke-vase  !>([`tid.bowl `tid beak file args])
  ;<  ~  bind:m  (poke-our %spider %spider-start poke-vase)
  ;<  ~  bind:m  (sleep ~s0)  ::  wait for thread to start
  (pure:m tid)
::
+$  thread-result
  (each vase [term tang])
::
++  await-thread
  |=  [file=term args=vase]
  =/  m  (strand ,thread-result)
  ^-  form:m
  ;<  =bowl:spider  bind:m  get-bowl
  =/  tid  (scot %ta (cat 3 'strand_' (scot %uv (sham file eny.bowl))))
  =/  poke-vase  !>([`tid.bowl `tid file args])
  ;<  ~      bind:m  (watch-our /awaiting/[tid] %spider /thread-result/[tid])
  ;<  ~      bind:m  (poke-our %spider %spider-start poke-vase)
  ;<  ~      bind:m  (sleep ~s0)  ::  wait for thread to start
  ;<  =cage  bind:m  (take-fact /awaiting/[tid])
  ;<  ~      bind:m  (take-kick /awaiting/[tid])
  ?+  p.cage  ~|([%strange-thread-result p.cage file tid] !!)
    %thread-done  (pure:m %& q.cage)
    %thread-fail  (pure:m %| !<([term tang] q.cage))
  ==
--
