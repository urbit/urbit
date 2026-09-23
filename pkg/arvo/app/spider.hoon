/-  spider
/+  libstrand=strand, default-agent, verb, server, dbug
=,  strand=strand:libstrand
~%  %spider-top  ..part  ~
|%
+$  card           card:agent:gall
+$  arvo-resource  arvo-resource:gall
+$  resource-deet  resource-deet:gall
+$  card-rand      card:libstrand
+$  thread         thread:spider
+$  tid            tid:spider
+$  input          input:spider
+$  yarn           (list tid)
+$  thread-form    _*eval-form:eval:(strand ,vase)
+$  trying         ?(%build %none)
+$  state
  $:  starting=(map yarn [=trying =vase])
      running=(axal thread-form)
      tid=(map tid yarn)
      serving=(map tid [(unit [rid=@ta take=?(%json %noun)]) =mark =desk from=desk])
      scrying=(jug tid [=wire =ship =path])
      resources=(jug tid arvo-resource)
      resource-deets=(map tid (map arvo-resource resource-deet))
  ==
::
+$  clean-slate-any
  $^  clean-slate-ket
  $%  clean-slate-sig
      clean-slate-1
      clean-slate-2
      clean-slate-3
      clean-slate-4
      clean-slate-5
      clean-slate-6
      clean-slate-7
      clean-slate-8
      clean-slate
  ==
::
+$  clean-slate
  $:  %9
      starting=(map yarn [=trying =vase])
      running=(list yarn)
      tid=(map tid yarn)
      serving=(map tid [(unit [rid=@ta take=?(%json %noun)]) =mark =desk from=desk])
      scrying=(jug tid [wire ship path])
      resources=(jug tid arvo-resource)
      resource-deets=(map tid (map arvo-resource resource-deet))
  ==
+$  clean-slate-8
  $:  %8
      starting=(map yarn [=trying =vase])
      running=(list yarn)
      tid=(map tid yarn)
      serving=(map tid [(unit [rid=@ta take=?(%json %noun)]) =mark =desk])
      scrying=(jug tid [wire ship path])
  ==
::
+$  clean-slate-7
  $:  %7
      starting=(map yarn [=trying =vase:h136])
      running=(list yarn)
      tid=(map tid yarn)
      serving=(map tid [(unit [rid=@ta take=?(%json %noun)]) =mark =desk])
      scrying=(jug tid [wire ship path])
  ==
::
+$  clean-slate-6
  $:  %6
      starting=(map yarn [=trying =vase:h136])
      running=(list yarn)
      tid=(map tid yarn)
      serving=(map tid [(unit @ta) =mark =desk])
      scrying=(jug tid [wire ship path])
  ==
::
+$  clean-slate-5
  $:  %5
      starting=(map yarn [=trying =vase:h136])
      running=(list yarn)
      tid=(map tid yarn)
      serving=(map tid [(unit @ta) =mark =desk])
      scrying=(map tid [ship path])
  ==
::
+$  clean-slate-4
  $:  %4
      starting=(map yarn [=trying =vase:h136])
      running=(list yarn)
      tid=(map tid yarn)
      serving=(map tid [(unit @ta) =mark =desk])
  ==
::
+$  clean-slate-3
  $:  %3
      starting=(map yarn [=trying =vase:h136])
      running=(list yarn)
      tid=(map tid yarn)
      serving=(map tid [@ta =mark =desk])
  ==
::
+$  clean-slate-2
  $:  %2
      starting=(map yarn [=trying =vase:h136])
      running=(list yarn)
      tid=(map tid yarn)
      serving=(map tid [@ta =mark])
  ==
::
+$  clean-slate-1
  $:  %1
      starting=(map yarn [=trying =vase:h136])
      running=(list yarn)
      tid=(map tid yarn)
  ==
::
+$  clean-slate-ket
  $:  starting=(map yarn [trying=?(%build %find %none) =vase:h136])
      running=(list yarn)
      tid=(map tid yarn)
  ==
::
+$  clean-slate-sig
  $:  starting=~
      running=(list yarn)
      tid=(map tid yarn)
  ==
--
::
%-  agent:dbug
^-  agent:gall
=|  =state
=<
  %+  verb  |
  ~%  %spider-agent  ..bind-eyre  ~
  |_  =bowl:gall
  +*  this         .
      spider-core  +>
      sc           ~(. spider-core bowl)
      def          ~(. (default-agent this %|) bowl)
      bec          byk.bowl(r da+now.bowl)
  ::
  ++  on-init
    ^-  (quip card _this)
    :_  this
    ~[bind-eyre:sc]
  ++  on-save   clean-state:sc
  ++  on-load
    |=  old-state=vase
    |^
    =+  !<(any=clean-slate-any old-state)
    =?  any  ?=(^ -.any)  (old-to-1 any)
    =?  any  ?=(~ -.any)  (old-to-1 any)
    =^  upgrade-cards  any
      (old-to-2 any)
    =.  any  (old-to-3 any)
    =.  any  (old-to-4 any)
    =.  any  (old-to-5 any)
    =.  any  (old-to-6 any)
    =.  any  (old-to-7 any)
    =.  any  (old-to-8 any)
    =.  any  (old-to-9 any)
    ?>  ?=(%9 -.any)
    ::
    =.  tid.state  tid.any
    =/  yarns=(list yarn)
      %+  welp  running.any
      ~(tap in ~(key by starting.any))
    |-  ^-  (quip card _this)
    ?~  yarns
      [~[bind-eyre:sc] this]
    =^  cards-1  state
      %.  [(yarn-to-tid i.yarns) nice=%.n]
      ::  the |sc core needs to now about the previous
      ::  scrying state in order to send $yawns to %ames
      ::
      %*(handle-stop-thread sc scrying.state scrying.any)
    =^  cards-2  this
      $(yarns t.yarns)
    [:(weld upgrade-cards cards-1 cards-2) this]
    ::
    ++  old-to-1
      |=  old=clean-slate-ket
      ^-  clean-slate-1
      1+old(starting (~(run by starting.old) |=([* v=vase:h136] none+v)))
    ::
    ++  old-to-2
      |=  old=clean-slate-any
      ^-  (quip card clean-slate-any)
      ?>  ?=(?(%1 %2 %3 %4 %5 %6 %7 %8 %9) -.old)
      ?:  ?=(?(%2 %3 %4 %5 %6 %7 %8 %9) -.old)
        `old
      :-  ~[bind-eyre:sc]
      :*  %2
        starting.old
        running.old
        tid.old
        ~
      ==
    ::
    ++  old-to-3
      |=  old=clean-slate-any
      ^-  clean-slate-any
      ?>  ?=(?(%2 %3 %4 %5 %6 %7 %8 %9) -.old)
      ?:  ?=(?(%3 %4 %5 %6 %7 %8 %9) -.old)
        old
      :*  %3
        starting.old
        running.old
        tid.old
        (~(run by serving.old) |=([id=@ta =mark] [id mark q.byk.bowl]))
      ==
    ::
    ++  old-to-4
      |=  old=clean-slate-any
      ^-  clean-slate-any
      ?>  ?=(?(%3 %4 %5 %6 %7 %8 %9) -.old)
      ?:  ?=(?(%4 %5 %6 %7 %8 %9) -.old)
        old
      :*  %4
        starting.old
        running.old
        tid.old
        (~(run by serving.old) |=([id=@ta =mark =desk] [`id mark q.byk.bowl]))
      ==
    ::
    ++  old-to-5
      |=  old=clean-slate-any
      ^-  clean-slate-any
      ?>  ?=(?(%4 %5 %6 %7 %8 %9) -.old)
      ?:  ?=(?(%5 %6 %7 %8 %9) -.old)  old
      [%5 +.old(serving [serving.old ~])]
    ::
    ++  old-to-6
      |=  old=clean-slate-any
      ^-  clean-slate-any
      ?>  ?=(?(%5 %6 %7 %8 %9) -.old)
      ?:  ?=(?(%6 %7 %8 %9) -.old)  old
      :-  %6
      %=    +.old
          scrying
        %-  ~(run by scrying.old)
        |=  [=ship =path]
        %-  ~(gas in *(set [wire ^ship ^path]))
        ::  XX +keen:strandio used /keen as the default wire
        ::  this assumes that any old thread used that as well
        ::
        [/keen ship path]~
      ==
    ::
    ++  old-to-7
      |=  old=clean-slate-any
      ^-  clean-slate-any
      ?>  ?=(?(%6 %7 %8 %9) -.old)
      ?:  ?=(?(%7 %8 %9) -.old)  old
      =-  old(- %7, serving -)
      %-  ~(run by serving.old)
      |=  [request=(unit @ta) =mark =desk]
      [(bind request (late %json)) mark desk]
    ::
    ++  old-to-8
      |=  old=clean-slate-any
      ^-  clean-slate-any
      ?>  ?=(?(%7 %8 %9) -.old)
      ?:  ?=(?(%8 %9) -.old)  old
      =-  old(- %8, starting -)
      %-  ~(run by starting.old)
      |=  [=trying =vase:h136]
      [trying (next-vase:h136 vase)]
    ::
    ++  old-to-9
      |=  old=clean-slate-any
      ^-  clean-slate-any
      ?>  ?=(?(%8 %9) -.old)
      ?:  ?=(%9 -.old)  old
      =-  old(- %9, serving -, scrying [scrying.old resources=~ res-deets=~])
      %-  ~(run by serving.old)
      |=  [request=(unit [rid=@ta take=?(%json %noun)]) =mark =desk]
      [request mark desk %base]
    --
  ::
  ++  on-poke
    ~/  %on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?>  (team:title [our src]:bowl)
    ?:  ?=(%spider-kill mark)
      (on-load on-save)
    =^  cards  state
      ?+  mark  (on-poke:def mark vase)
          %spider-input   (on-poke-input:sc !<(input vase))
          %spider-start   (handle-start-thread:sc !<(start-args:spider vase))
          %spider-inline  (handle-inline-thread:sc !<(inline-args:spider vase))
          %spider-stop    (handle-stop-thread:sc !<([tid ?] vase))
          %handle-http-request
        (handle-http-request:sc !<([@ta =inbound-request:eyre] vase))
      ==
    [cards this]
  ::
  ++  on-watch
    ~/  %on-watch
    |=  =path
    ^-  (quip card _this)
    =^  cards  state
      ?+  path  (on-watch:def path)
        [%thread @ *]         (on-watch:sc t.path)
        [%thread-result @ ~]  (on-watch-result:sc i.t.path)
        [%http-response *]     `state
      ==
    [cards this]
  ::
  ++  on-leave  on-leave:def
  ++  on-peek
    ~/  %on-peek
    |=  =path
    ^-  (unit (unit cage))
    ?+    path  (on-peek:def path)
        [%x %tree ~]
      ``noun+!>((turn ~(tap of running.state) head))
    ::
        [%x %starting @ ~]
      ``noun+!>((~(has of running.state) (~(got by tid.state) i.t.t.path)))
    ::
        [%x %saxo @ ~]
      ``noun+!>((~(got by tid.state) i.t.t.path))
    ==
  ::
  ++  on-agent
    ~/  %on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    =^  cards  state
      ?+    wire  !!
        [%thread @ *]  (on-agent:sc i.t.wire t.t.wire sign)
      ==
    [cards this]
  ::
  ++  on-arvo
    ~/  %on-arvo
    |=  [=wire gift=gift-user-v1:gall]
    ^-  (quip card _this)
    ?:  ?=(%unsupported -.gift)  (on-arvo:def wire gift) :: TODO: handle failed syscalls
    =^  cards  state
      ?+  wire  (on-arvo:def wire gift)
        [%thread @ *]  (handle-gift:sc i.t.wire t.t.wire gift)
        [%build @ ~]   (handle-build:sc i.t.wire gift)
        [%bind ~]      `state
      ==
    [cards this]
  ::  On unexpected failure, kill all outstanding strands
  ::
  ++  on-fail
    |=  [frag:agent:gall =call:agent:gall]
    ^-  (quip card _this)
    %-  (slog leaf+"spider crashed, killing all strands: {<from>} during {<-.call>}" tang)
    (on-load on-save)
  --
::
~%  %spider-helper  ..card  ~
|_  =bowl:gall
++  bec  `beak`byk.bowl(r da+now.bowl)
++  bind-eyre
  ^-  card
  [%pass /bind %arvo %eyre %connect [~ /spider] %spider]
::
++  new-thread-id
  |=  file=term
  :((cury cat 3) file '--' (scot %uv (sham eny.bowl)))
::
++  ted-mock
  |*  =mold
  |=  [=yarn run=_^?(|.(*mold))]
  ^-  (each mold tang)
  =/  desk  from:(~(got by serving.state) (yarn-to-tid yarn))
  =+  peg=(yarn-to-peg yarn bowl)
  =/  out  (mock [run %9 2 %0 1] (ted-look desk peg))
  ?-  -.out
    %0  [%& !<(mold [-:!>(*mold) p.out])]
    %1  [%| 'spider: scry blocked on permissions' ~]
    %2  [%| p.out]
  ==
::
++  ted-look
  |=  [=desk peg=(set perm:gall)]
  |=  ref=^
  ^-  (unit (unit))
  =+  ;;(=path +.ref)
  ?~  omen=(de-omen path)  [~ ~]
  ?.  ?|  =(%base desk)
          (rite:guard:gall our.bowl peg u.omen)
      ==
    %-  (slog leaf+"spider: scry on {<path>} blocked" ~)
    ~
  ``.^(noun path)
::
++  handle-http-request
  ~/  %handle-http-request
  |=  [eyre-id=@ta =inbound-request:eyre]
  ^-  (quip card _state)
  ?>  authenticated.inbound-request
  =/  url
    (parse-request-line:server url.request.inbound-request)
  ?>  ?=([%spider @t @t @t @t ~] site.url)
  =*  desk         i.t.site.url
  =*  input-mark   i.t.t.site.url
  =*  thread       i.t.t.t.site.url
  =*  output-mark  i.t.t.t.t.site.url
  ::  TODO: speed this up somehow. we spend about 15ms in this arm alone
  ::
  ?>  ?=(^ body.request.inbound-request)
  =/  test=$-(@t ?(%json %noun))
    |=  head=@t
    =;  type=(unit @t)
      ?:(=(`'application/x-urb-jam' type) %noun %json)
    %+  bind
      (get-header:http head header-list.request.inbound-request)
    :(cork trip cass crip)
  =/  give  (test 'content-type')
  =/  take  (test 'accept')
  ::
  =/  =tid  (new-thread-id thread)
  =/  from  (desk-from-sap bowl)
  ?~  from
    %:  thread-http-fail-response
        %bad-request  ['missing-provenance' ~]
        `[eyre-id take]  output-mark  desk  ~
    ==
  =.  serving.state
    (~(put by serving.state) tid [`[eyre-id take] output-mark desk u.from])
  ::
  =/  input=vase
    %+  slop  !>(~)
    ?-  give
        %json
      =/  tube  (convert-tube %json input-mark desk bowl)
      =/  body=json  (need (de:json:html q.u.body.request.inbound-request))
      (tube !>(body))
    ::
        %noun
      =/  tube  (convert-tube %noun input-mark desk bowl)
      =/  body=noun  (cue q.u.body.request.inbound-request)
      (tube !>(body))
    ==
  =/  boc  bec
  =/  =start-args:spider  [~ `tid boc(q desk, r da+now.bowl) thread input]
  (handle-start-thread start-args)
::
++  on-poke-input
  |=  input
  =/  yarn  (~(got by tid.state) tid)
  (take-input yarn ~ %poke cage)
::
++  on-watch
  |=  [=tid =path]
  (take-input (~(got by tid.state) tid) ~ %watch path)
::
++  on-watch-result
  |=  =tid
  ^-  (quip card ^state)
  `state
::
++  handle-gift
  ~/  %handle-gift
  |=  [=tid =wire gift=gift-user-v1:gall]
  =/  yarn  (~(get by tid.state) tid)
  ?~  yarn
    %-  (slog leaf+"spider got gift for non-existent {<tid>}" ~)
    `state
  =/  res=(set arvo-resource)  (~(gut by resources.state) tid ~)
  =/  deets=(map arvo-resource resource-deet)
    (~(gut by resource-deets.state) tid ~)
  =/  new-res  (apply-gift:track:gall [%thread tid wire] gift res deets)
  =.  resources.state
    (~(put by resources.state) tid res.new-res)
  =.  resource-deets.state
    (~(put by resource-deets.state) tid dets.new-res)
  (take-input u.yarn ~ %gift wire gift)
::
++  on-agent
  |=  [=tid =wire =sign:agent:gall]
  =/  yarn  (~(get by tid.state) tid)
  ?~  yarn
    %-  (slog leaf+"spider got agent for non-existent {<tid>}" ~)
    `state
  (take-input u.yarn ~ %agent wire sign)
::
++  handle-start-thread
  ~/  %handle-start-thread
  |=  [parent-tid=(unit tid) use=(unit tid) =beak file=term =vase]
  (prep-thread parent-tid use beak %| file vase)
::
++  handle-inline-thread
  ~/  %handle-inline-thread
  |=  [parent-tid=(unit tid) use=(unit tid) =beak =shed:khan]
  (prep-thread parent-tid use beak %& shed)
::
++  prep-thread
  |=  $:  parent-tid=(unit tid)  use=(unit tid)  =beak
          source=(each shed:khan [file=term =vase])
      ==
  ^-  (quip card ^state)
  =/  parent-yarn=yarn
    ?~  parent-tid
      /
    (~(got by tid.state) u.parent-tid)
  =/  new-tid
    ?^  use
      u.use
    %-  new-thread-id
    ?-  -.source
      %&  (cat 3 'inline-' q.beak)
      %|  file.p.source
    ==
  ::
  =/  =yarn  (snoc parent-yarn new-tid)
  ::
  ?:  (~(has of running.state) yarn)
    ~|  [%already-started yarn]
    !!
  ?:  (~(has by starting.state) yarn)
    ~|  [%already-starting yarn]
    !!
  ::
  =/  from  (desk-from-sap bowl)
  ?~  from  ~|([%missing-provenance yarn] !!)
  =?  serving.state  !(~(has by serving.state) new-tid)
    (~(put by serving.state) new-tid [~ %noun q.beak u.from])
  ::
  =.  tid.state       (~(put by tid.state) new-tid yarn)
  ?-    -.source
      %&  (begin-shed yarn p.source)
      %|
    =.  starting.state  (~(put by starting.state) yarn [%build vase.p.source])
    =/  pax=path
      ~|  no-file-for-thread+file.p.source
      (need (get-fit:clay beak %ted file.p.source))
    :_  state
    :_  ~
    :+  %pass  /build/[new-tid]
    [%arvo %clay %read ~ p.beak q.beak %sing %a r.beak pax]
  ==
::
++  handle-build
  ~/  %handle-build
  |=  [=tid gift=$<([%unsupported ~] gift-user-v1:gall)]
  ^-  (quip card ^state)
  =/  =yarn  (~(got by tid.state) tid)
  =.  starting.state
    (~(jab by starting.state) yarn |=([=trying =vase] [%none vase]))
  ~|  gift+[- +<]:gift
  =/  =riot:clay
    ?>(?=([%clay %read *] gift) riot.gift)
  ?~  riot
    (thread-fail-not-running tid %build-thread-error *tang)
  ?.  ?=(%vase p.r.u.riot)
    (thread-fail-not-running tid %build-thread-strange >[p q]:u.riot< ~)
  =/  maybe-thread  (mule |.(!<(thread !<(vase q.r.u.riot))))
  ?:  ?=(%| -.maybe-thread)
    (thread-fail-not-running tid %thread-not-thread ~)
  (slam-thread yarn p.maybe-thread)
::
++  slam-thread
  ~/  %slam-thread
  |=  [=yarn =thread]
  ^-  (quip card ^state)
  =/  =vase  vase:(~(got by starting.state) yarn)
  =/  res  ((ted-mock shed:khan) yarn |.((thread vase)))
  ?:  ?=(%| -.res)
    (thread-fail-not-running (yarn-to-tid yarn) %false-start p.res)
  =.  starting.state  (~(del by starting.state) yarn)
  (begin-shed yarn p.res)
::
::  thread effects inherit and restricted by caller desk permissions
++  perm-check
  |=  [=yarn cards=(list card)]
  ^-  (each ~ [term tang])
  =/  dat  (~(got by serving.state) (yarn-to-tid yarn))
  ?:  =(%base from.dat)  [%& ~]
  =+  peg=(yarn-to-peg yarn bowl)
  =/  has-per  (cres-tang:guard:gall our.bowl peg cards)
  ?:  -.has-per  [%& ~]
  [%| err=[%missing-permissions +.has-per]]
::
++  begin-shed
  |=  [=yarn =shed:khan]
  ?<  (~(has of running.state) yarn)
  =/  m  (strand ,vase)
  =/  =eval-form:eval:m  (from-form:eval:m shed)
  =.  running.state  (~(put of running.state) yarn eval-form)
  (take-input yarn ~)
::
++  handle-stop-thread
  |=  [=tid nice=?]
  ^-  (quip card ^state)
  =/  yarn=(unit yarn)  (~(get by tid.state) tid)
  ?~  yarn
    ~&  %stopping-nonexistent-thread
    [~ state]
  ?:  (~(has of running.state) u.yarn)
      ?.  nice
        (thread-fail u.yarn %cancelled ~)
      =^  done-cards  state  (thread-done u.yarn *vase silent=%.n)
      [done-cards state]
  ?:  (~(has by starting.state) u.yarn)
    (thread-fail-not-running tid %stopped-before-started ~)
  ~&  [%thread-not-started u.yarn]
  ?:  nice
    (thread-done u.yarn *vase silent=%.y)
  (thread-fail u.yarn %cancelled ~)
::
++  take-input
  ~/  %take-input
  |=  [=yarn input=(unit input:strand)]
  ^-  (quip card ^state)
  =/  m  (strand ,vase)
  ?.  (~(has of running.state) yarn)
    %-  (slog leaf+"spider got input for non-existent {<yarn>}" ~)
    `state
  =/  =eval-form:eval:m
    (need fil:(~(dip of running.state) yarn))
  =|  cards=(list card)
  |-  ^-  (quip card ^state)
  =/  ted-bowl  (convert-bowl yarn bowl)
  =^  r=[cards=(list card-rand) =eval-result:eval:m]  eval-form
    =.  input
      ?~  input  ~
      =+  in=u.input
      ?.  ?=([%agent * %fact *] in)        `in
      ?:  ?=(%thread-done p.cage.sign.in)  `in
      :-  ~
      :^  %agent  wire.in  %fact
      (validate-mark:eval:m q.q.cage.sign.in p.cage.sign.in ted-bowl(byk byk.bowl))
    =/  out
      %+  %-  ted-mock
          $:  [cards=(list card-rand) =eval-result:eval:m]
              eval-form:eval:m
          ==
      yarn  |.
      (take:eval:m eval-form ted-bowl input)
    ?.  ?=(%& -.out)  [[~ [%fail %crash p.out]] eval-form]
    =/  perm  (perm-check yarn -.-.p.out)
    ?:  -.perm  p.out
    [[~ [%fail %crash +.perm]] eval-form]
  ::
  =.  running.state  (~(put of running.state) yarn eval-form)
  =/  =tid  (yarn-to-tid yarn)
  =^  new-cards  state
    ^-  [(list card) _state]
    %+  roll  cards.r
    |=  [card=card-rand cards=(list card) s=_state]
    =.  card
      ?+  card  card
        [%pass * *]  [%pass [%thread tid p.card] q.card]
      ::
          [%give ?(%fact %kick) *]
        =-  card(paths.p -)
        %+  turn  paths.p.card
        |=(=path [%thread tid path])
      ==
    ::REVIEW  ok to check on syscalls y/n?
    ::TODO  avoid having to do ;; at least
    :_  =?  scrying.s  ?|  ?=([%pass ^ %arvo %syscall %a %keen ?(~ ^) @ *] card)
                           ?=([%pass ^ %arvo %ames %keen @ *] card)
                           ?=([%pass ^ %arvo %syscall %a %chum *] card)
                       ==
          ?:  ?|  ?=([%pass ^ %arvo %syscall %a %chum *] card)
                  ?=([%pass ^ %arvo %ames %keen @ *] card)
              ==
            ::  &2=wire &7=ship 7|=path
            (~(put ju scrying.s) tid ;;([wire ship path] [&2 &7 |7]:card))
          ?>  ?=([%pass ^ %arvo %syscall %a %keen ?(~ ^) @ *] card)
          ::  &2=wire &8=ship 8|=path
          (~(put ju scrying.s) tid ;;([wire ship path] [&2 &8 |8]:card))
      ::
        ?.  ?=([%pass *] card)  s
        ?~  res=(card-resource:track:gall card)  s
        =+  ted-res=[tid [p.card res.res]]
        =.  resources.s
          ?:  |(?=(^ add.res) add.res)
            (~(put ju resources.s) ted-res)
          (~(del ju resources.s) ted-res)
      ::
        =?  resource-deets.s  |(?=(^ add.res) !add.res)
          =/  res-deet  (~(gut by resource-deets.s) tid ~)
          %+  ~(put by resource-deets.s)  tid
          ?.  ?=(^ add.res)
            (~(del by res-deet) [p.card res.res])
          (~(put by res-deet) [p.card res.res] add.res)
        s
    ::
    :_  cards
    (card-to-dole card tid)
  ::
  =.  cards  (weld cards (flop new-cards))
  =^  final-cards=(list card)  state
    ?-  -.eval-result.r
      %next  `state
      %fail  (thread-fail yarn err.eval-result.r)
      %done  (thread-done yarn value.eval-result.r silent=%.y)
    ==
  [(weld cards final-cards) state]
::
++  thread-fail-not-running
  |=  [=tid =term =tang]
  ^-  (quip card ^state)
  =/  =yarn  (~(got by tid.state) tid)
  :_  state(starting (~(del by starting.state) yarn))
  =/  moz  (thread-say-fail tid term tang)
  ?.  ?=([~ %build *] (~(get by starting.state) yarn))
    moz
  :_(moz [%pass /build/[tid] %arvo %clay %rest ~])
::
++  thread-say-fail
  |=  [=tid =term =tang]
  ^-  (list card)
  :~  [%give %fact ~[/thread-result/[tid]] %thread-fail !>([term tang])]
      [%give %kick ~[/thread-result/[tid]] ~]
  ==
::
++  cancel-scry
  |=  [=tid silent=?]
  ^-  (quip card _state)
  ?~  scrying=(~(get ju scrying.state) tid)
    `state
  :_  state(scrying (~(del by scrying.state) tid))
  ?:  silent  ~  ::  REVIEW: dropping scrying state without %yawn
  %-  ~(rep in `(set [wire ship path])`scrying)
  |=  [[=wire =ship =path] cards=(list card)]
  %-  (slog leaf+"cancelling {<tid>}: [{<[wire ship path]>}]" ~)
  :_  cards
  [%pass wire %arvo %ames %yawn ship path]
::
++  thread-http-fail
  |=  [=tid =term =tang]
  ^-  (quip card ^state)
  ?~  dat=(~(get by serving.state) tid)
    `state
  (thread-http-fail-response term tang u.dat)
::
++  thread-http-fail-response
  |=  [=term =tang request=(unit [rid=@ta take=?(%json %noun)]) output=mark =desk *]
  ^-  (quip card ^state)
  :_  state
  ?~  request
    ~
  %+  give-simple-payload:app:server  rid.u.request
  ^-  simple-payload:http
  ?.  ?=(http-error:spider term)
    %-  (slog tang)
    ?-  take.u.request
        %json
      =/  tube  (convert-tube %tang %json desk bowl)
      :-  [500 [['content-type' 'application/json'] ~]]
      =-  `(as-octs:mimes:html (en:json:html -))
      o/(malt `(list [key=@t json])`[term+s/term tang+!<(json (tube !>(tang))) ~])
    ::
        %noun
      :-  [500 [['content-type' 'application/x-urb-jam'] ~]]
      `(as-octs:mimes:html (jam [term tang]))
    ==
  :_  ~  :_  ~
  ?-  term
    %bad-request  400
    %forbidden    403
    %nonexistent  404
    %offline      504
  ==
::
++  thread-fail
  |=  [=yarn =term =tang]
  ^-  (quip card ^state)
  ::%-  (slog leaf+"strand {<yarn>} failed" leaf+<term> tang)
  =/  =tid  (yarn-to-tid yarn)
  =/  fail-cards  (thread-say-fail tid term tang)
  =^  http-cards  state  (thread-http-fail tid term tang)
  =^  scry-card   state  (cancel-scry tid silent=%.n)
  =^  cards       state  (thread-clean yarn)
  :_  state
  :(weld fail-cards cards http-cards scry-card)
::
++  thread-http-response
  |=  [=tid =vase]
  ^-  (list card)
  =-  (fall - ~)
  %+  bind
    (~(get by serving.state) tid)
  |=  [request=(unit [rid=@ta take=?(%json %noun)]) output=mark =desk *]
  ?~  request  ~
  ?-  take.u.request
      %json
    =/  tube  (convert-tube output %json desk bowl)
    %+  give-simple-payload:app:server  rid.u.request
    (json-response:gen:server !<(json (tube vase)))
  ::
      %noun
    %+  give-simple-payload:app:server  rid.u.request
    :-  [200 ['content-type' 'application/x-urb-jam']~]
    `(as-octs:mimes:html (jam q.vase))
  ==
::
++  thread-done
  |=  [=yarn =vase silent=?]
  ^-  (quip card ^state)
  ::  %-  (slog leaf+"strand {<yarn>} finished" (sell vase) ~)
  =/  =tid  (yarn-to-tid yarn)
  =/  done-cards=(list card)
    :~  [%give %fact ~[/thread-result/[tid]] %thread-done vase]
        [%give %kick ~[/thread-result/[tid]] ~]
    ==
  =/  http-cards        (thread-http-response tid vase)
  =^  scry-card  state  (cancel-scry tid silent)
  =^  cards      state  (thread-clean yarn)
  [:(weld done-cards cards http-cards scry-card) state]
::
++  thread-clean
  |=  =yarn
  ^-  (quip card ^state)
  =/  children=(list ^yarn)
    %+  turn
      ~(tap of (~(dip of running.state) yarn))
    |=  [child=^yarn *]
    (welp yarn child)
  |-  ^-  (quip card ^state)
  ?~  children
    `state
  =+  tid=(yarn-to-tid i.children)
  =^  cards-children  state  $(children t.children)
  =^  cards-resource  state
    =/  child-res=(list arvo-resource)
      ~(tap in (~(get ju resources.state) tid))
    =|  cards=(list card)
    |-
    ?~  child-res  [cards state(resources (~(del by resources.state) tid))]
    =/  dets  (~(gut by resource-deets.state) tid ~)
    =/  c=(unit card)
      ?~  tac=(drop-resource:track:gall i.child-res dets)  ~
      `[%pass wire.i.child-res %arvo u.tac]
    %=  $
      cards      ?~(c cards [(card-to-dole u.c tid) cards])
      resource-deets.state
        %+  ~(put by resource-deets.state)  tid
        (~(del by dets) i.child-res)
      child-res  t.child-res
    ==
  =^  cards-our  state
    =/  =^yarn  i.children
    =:  running.state  (~(lop of running.state) yarn)
        tid.state      (~(del by tid.state) tid)
        serving.state  (~(del by serving.state) tid)
      ==
    :_  state
    %+  murn  ~(tap by wex.bowl)
    |=  [[=wire =ship =term] [acked=? =path]]
    ^-  (unit card)
    ?.  ?&  ?=([%thread @ *] wire)
            =(tid i.t.wire)
        ==
      ~
    `[%pass wire %agent [ship term] %leave ~]
  [:(welp cards-children cards-resource cards-our) state]
::
++  convert-bowl
  |=  [=yarn =bowl:gall]
  ^-  bowl:spider
  :*  our.bowl
      src.bowl
      (yarn-to-tid yarn)
      (yarn-to-parent yarn)
      wex.bowl  ::  REVIEW: leaking?
      sup.bowl  ::  REVIEW: leaking?
      eny.bowl  ::  REVIEW: safety
      now.bowl
      (yarn-to-byk yarn bowl)
      (yarn-to-peg yarn bowl)
  ==
::
++  yarn-to-tid
  |=  =yarn
  ^-  tid
  =/  nary  (flop yarn)
  ?>  ?=([@ *] nary)
  i.nary
::
++  yarn-to-parent
  |=  =yarn
  ^-  (unit tid)
  =/  nary  (flop yarn)
  ?>  ?=([@ *] nary)
  ?~  t.nary
    ~
  `i.t.nary
::
++  yarn-to-byk
  |=  [=yarn =bowl:gall]
  =/  [* * =desk *]
    ~|  "no desk associated with {<tid>}"
    %-  ~(got by serving.state)  (yarn-to-tid yarn)
  =/  boc  bec
  boc(q desk)
::
++  yarn-to-peg
  |=  [=yarn =bowl:gall]
  ^-  (set perm:gall)
  =/  [* * * from=desk]
    ~|  "no desk associated with {<tid>}"
    %-  ~(got by serving.state)  (yarn-to-tid yarn)
  =+  .^(=bond:ward:clay %cx /(scot %p our.bowl)//(scot %da now.bowl)/bond/[from])
  peg.bond
::
++  desk-from-sap
  |=  =bowl:gall
  ^-  (unit desk)
  ?:  ?=([?(%eyre %khan %gall) @ ~] sap.bowl)
    `i.t.sap.bowl
  ~&('unknown provenance' ~)
::
++  clean-state
  !>  ^-  clean-slate
  9+state(running (turn ~(tap of running.state) head))
::
++  convert-tube
  |=  [from=mark to=mark =desk =bowl:gall]
  .^
    tube:clay
    %cc
    /(scot %p our.bowl)/[desk]/(scot %da now.bowl)/[from]/[to]
  ==
::
++  card-to-dole
  |=  [=card =tid]
  =/  dat  (~(got by serving.state) tid)
  ?.  ?&  ?=([%pass * *] card)
          !=(q.byk.bowl from.dat)
      ==
    card
  ?>  ?=(?(%agent %arvo) -.q.card)
  [%pass p.card %dole from.dat q.card]
--
