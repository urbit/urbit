::  test-agent: testing harness for agents
::
::    this library helps you write tests for agents in the monadic ;< style.
::    the general concept is that an agent and its bowl are continuously
::    "passed forward" as you perform operations on them.
::
::    the bowl is partially managed for you by the library: when the agent
::    emits %watch or %leave cards, outgoing subscriptions in wex.bowl are
::    updated. likewise for %watch-ack and %kick calls to +on-agent. incoming
::    subscriptions in sup.bowl are updated in response to %kick cards and
::    calls to +on-watch and +on-leave.
::
::    an example test arm, which initializes an agent and pokes it, might look
::    something like this (assuming /+ *test-agent):
::
::    ++  test-example
::      %-  eval-mare
::      =/  m  (mare ,~)
::      ^-  form:m
::      ;<  ~                bind:m  (set-scry-gate |=(path `!>(%some-noun)))
::      ;<  caz=(list card)  bind:m  (do-init %my-agent-name my-agent-core)
::      ;<  ~                bind:m  (ex-cards caz ~)
::      ;<  ~                bind:m  (set-src ~dev)
::      ;<  caz=(list card)  bind:m  (do-poke %noun !>(123))
::      (ex-cards caz (ex-fact [/echo]~ %noun !>([~dev 123])) ~)
::
/+  test
!.
::
^?
|%
::  voodoo basics
::
+$  agent  $+(agent agent:gall)
+$  bowl   $+(bowl bowl:gall)
+$  card   $+(card card:agent:gall)
+$  scry   $-(path (unit vase))
::
+$  state       [=agent =bowl =scry]                    ::  passed continuously
++  form-raw    |$  [a]  $-(state (output-raw a))       ::  continuation
++  output-raw  |$  [a]  (each [out=a =state] tang)     ::  continue or fail
::
++  mare
  |*  a=mold
  |%
  ++  output  (output-raw a)
  ++  form  (form-raw a)
  ++  pure
    |=  arg=a
    ^-  form
    |=  =state
    [%& arg state]
  ::
  ++  bind
    |*  b=mold
    |=  [m-b=(form-raw b) fun=$-(b form)]
    ^-  form
    |=  =state
    =/  b-res=(output-raw b)  (m-b state)
    ?-  -.b-res
      %&  ((fun out.p.b-res) state.p.b-res)
      %|  [%| p.b-res]
    ==
  --
::
++  eval-mare
  =/  m  (mare ,~)
  |=  f=form:m
  ^-  tang
  =/  res  (f %*(. *state agent skeleton))
  ?:  ?=(%& -.res)  ~
  ::NOTE  in rare cases, execution may crash in such a way that the resulting
  ::      trace is empty. make sure we catch that here so the /ted/test will
  ::      consider it failed.
  ?^  p.res  p.res
  ~['+eval-mare:test-agent didn\'t finish cleanly']
::
++  skeleton
  ^-  agent  !:
  |_  bowl:gall
  ++  on-init        ~&(>>> %test-agent-not-initialized !!)
  ++  on-save        ~&(>>> %test-agent-not-initialized !!)
  ++  on-load   |=(* ~&(>>> %test-agent-not-initialized !!))
  ++  on-poke   |=(* ~&(>>> %test-agent-not-initialized !!))
  ++  on-watch  |=(* ~&(>>> %test-agent-not-initialized !!))
  ++  on-leave  |=(* ~&(>>> %test-agent-not-initialized !!))
  ++  on-agent  |=(* ~&(>>> %test-agent-not-initialized !!))
  ++  on-arvo   |=(* ~&(>>> %test-agent-not-initialized !!))
  ++  on-peek   |=(* ~&(>>> %test-agent-not-initialized !!))
  ++  on-fail   |=(* ~&(>>> %test-agent-not-initialized !!))
  --
::
::  internal transformations (you shouldn't be calling these directly)
::
++  play-cards
  |=  [=bowl cards=(list card)]
  ^+  bowl
  ?~  cards  bowl
  =*  card  i.cards
  =*  next  $(cards t.cards)
  ?+  card  next
      [%pass * %agent * ?(%watch %watch-as %leave) *]
    =-  =.(wex.bowl - next)
    =/  key=[=wire =gill:gall]  [p [ship name]:q]:card
    ?:  ?=(%leave -.task.q.card)
      (~(del by wex.bowl) key)
    ?.  (lien ~(tap by wex.bowl) |=([k=[wire gill:gall] *] =(k key)))
      =;  =path
        (~(put by wex.bowl) key [| path])
      ?-  -.task.q.card
        %watch     path.task.q.card
        %watch-as  path.task.q.card
      ==
    ~_  'subscribe wire not unique'  ::TODO  maybe integrate the test tang instead?
    ~|  key
    !!
  ::
      [%give %kick *]
    =-  =.(sup.bowl - next)
    %+  roll  paths.p.card
    |=  [=path =_sup.bowl]
    %-  ~(gas by *bitt:gall)
    %+  skip  ~(tap by sup)
    |=  [duct s=ship p=^path]
    &(=(p path) |(?=(~ ship.p.card) =(s u.ship.p.card)))
  ==
::
++  play-sign
  |=  [=bowl =wire =gill:gall =sign:agent:gall]
  ^+  bowl
  =.  src.bowl  p.gill
  ?:  ?=(%poke-ack -.sign)  bowl
  =;  =_wex.bowl  bowl(wex wex)
  ?.  (~(has by wex.bowl) [wire gill])
    ~_  leaf+"missing subscription, got %{(trip -.sign)} on {(spud wire)}"
    !!
  ?+  sign  wex.bowl
      ?([%watch-ack ~ *] [%kick ~])
    (~(del by wex.bowl) [wire gill])
  ::
      [%watch-ack ~]
    %+  ~(jab by wex.bowl)  [wire gill]
    |=  [ack=? =path]
    ~_  'duplicate watch-ack'
    ?<(ack [& path])
  ==
::
++  do  ::  execute agent lifecycle step with mocked scry
  |=  call=$-(state [(list card) agent:gall])
  =/  m  (mare ,(list card))
  ^-  form:m
  |=  s=state
  =;  result=(each [(list card) agent:gall] (list tank))
    ?:  ?=(%| -.result)
      |+p.result
    =^  c  agent.s  p.result
    =.  bowl.s  (play-cards bowl.s c)
    &+[c s]
  =;  res=toon
    ?-  -.res
      %0  :-  %&
          ::NOTE  we would ;;, but it's too slow.
          ::      we know for a fact p.res is of the type we expect,
          ::      so we just play pretend with vases instead.
          !<  [(list card) agent:gall]
          [-:!>(*[(list card) agent:gall]) p.res]
      %1  |+~['blocking on scry' >;;(path p.res)<]
      %2  |+p.res
    ==
  %+  mock  [. !=((call s))]
  |=  [ref=* pax=*]
  ^-  (unit (unit *))
  ?>  ?=(^ ref)
  ?>  =(hoon-version -.ref)
  =+  ;;(pax=path pax)
  =/  res=(unit vase)  (scry.s pax)
  %.  ?~(res ~ ``q.u.res)
  ::  warn about type mismatches if the tested code expects a result type
  ::  different from what the mocked scry produces.
  ::
  ::NOTE  we would ;;, but it's too slow.
  ::      we can safely assume +.ref is indeed a type,
  ::      so we just play pretend with vases instead.
  =+  !<(typ=type [-:!>(*type) +.ref])
  ?.  &(?=(^ res) !(~(nest ut typ) | p.u.res))
    same
  %-  %*(. slog pri 2)
  :~  'mocked scry result mismatches expected type'
      >pax<
      (~(dunk ut typ) %need)
      (~(dunk ut p.u.res) %have)
  ==
::
++  jab-state
  |=  f=$-(state state)
  =/  m  (mare ,~)
  ^-  form:m
  |=  s=state
  &+[~ (f s)]
::
::  managed agent lifecycle
::
++  do-init
  =+  scry-warn=&
  |=  [dap=term =agent]
  =/  m  (mare ,(list card))
  ^-  form:m
  ;<  old-scry=scry  bind:m  |=(s=state &+[scry.s s])
  ;<  ~              bind:m  %-  set-scry-gate
                             |=  p=path
                             ~?  >>  scry-warn
                               ['scrying during +on-init... careful!' p]
                             (old-scry p)
  ;<  b=bowl         bind:m  get-bowl
  ;<  ~              bind:m  (set-bowl %*(. *bowl dap dap, our our.b, src our.b))
  ;<  c=(list card)  bind:m  (do |=(s=state ~(on-init agent bowl.s)))
  ;<  ~              bind:m  (set-scry-gate old-scry)
  (pure:m c)
::
++  do-load
  =+  scry-warn=&
  |=  [=agent vase=(unit vase)]
  =/  m  (mare ,(list card))
  ^-  form:m
  ;<  old-scry=scry  bind:m  |=(s=state &+[scry.s s])
  ;<  ~              bind:m  %-  set-scry-gate
                             |=  p=path
                             ~?  >>  scry-warn
                               ['scrying during +on-load... careful!' p]
                             (old-scry p)
  ;<  c=(list card)  bind:m  %-  do  |=  s=state
                             %-  ~(on-load agent bowl.s)
                             ?^  vase  u.vase
                             ~(on-save agent.s bowl.s)
  ;<  ~              bind:m  (set-scry-gate old-scry)
  (pure:m c)
::
++  do-poke
  |=  [=mark =vase]
  %-  do
  |=  s=state
  (~(on-poke agent.s bowl.s) mark vase)
::
++  do-watch
  |=  =path
  =/  m  (mare ,(list card))
  ^-  form:m
  |=  s=state
  =.  sup.bowl.s
    =/  =duct  [%test-sub (scot %p src.bowl.s) path]~
    ~_  leaf+"sub on {(spud path)} already made by {(scow %p src.bowl.s)}"
    ?<  (~(has by sup.bowl.s) duct)
    (~(put by sup.bowl.s) duct [src.bowl.s path])
  %.  s  %-  do
  |=  s=state
  (~(on-watch agent.s bowl.s) path)
::
++  do-leave
  |=  =path
  =/  m  (mare ,(list card))
  ^-  form:m
  |=  s=state
  =.  sup.bowl.s
    =/  =duct  [%test-sub (scot %p src.bowl.s) path]~
    ~_  leaf+"sub on {(spud path)} not yet made by {(scow %p src.bowl.s)}"
    ?>  (~(has by sup.bowl.s) duct)
    (~(del by sup.bowl.s) duct)
  %.  s  %-  do
  |=  s=state
  (~(on-leave agent.s bowl.s) path)
::
++  do-agent
  |=  [=wire =gill:gall =sign:agent:gall]
  =/  m  (mare ,(list card))
  ^-  form:m
  |=  s=state
  =.  bowl.s  (play-sign bowl.s wire gill sign)
  %.  s  %-  do
  |=  s=state
  (~(on-agent agent.s bowl.s) wire sign)
::
++  do-arvo
  |=  [=wire gift=gift-user-v1:gall]
  %-  do
  |=  s=state
  (~(on-arvo agent.s bowl.s) wire gift)
::
++  do-fail
  |=  [=term =tang]
  %-  do
  |=  s=state
  (~(on-fail agent.s bowl.s) term tang)
::
::  data extraction
::
++  get-save
  =/  m  (mare ,vase)
  ^-  form:m
  |=  s=state
  &+[~(on-save agent.s bowl.s) s]
::
++  get-peek
  |=  =path
  =/  m  (mare ,(unit (unit cage)))
  ^-  form:m
  |=  s=state
  &+[(~(on-peek agent.s bowl.s) path) s]
::
++  got-peek
  |=  =path
  =/  m  (mare ,cage)
  ^-  form:m
  |=  s=state
  =/  peek=(unit (unit cage))
    (~(on-peek agent.s bowl.s) path)
  ?.  ?=(^ peek)  |+~['invalid scry path' (spat path)]
  ?.  ?=(^ u.peek)  |+~['unexpected empty result at scry path' (spat path)]
  &+[u.u.peek s]
::
++  get-agent
  =/  m  (mare agent)
  ^-  form:m
  |=  s=state
  &+[agent.s s]
::
++  get-bowl
  =/  m  (mare bowl)
  ^-  form:m
  |=  s=state
  &+[bowl.s s]
::
::  bowl modification
::
++  jab-bowl
  |=  f=$-(bowl bowl)
  =/  m  (mare ,~)
  ^-  form:m
  |=  s=state
  &+[~ s(bowl (f bowl.s))]
::
++  set-bowl
  |=(b=bowl (jab-bowl |=(* b)))
::
++  set-src
  |=  src=ship
  %-  jab-bowl
  |=(b=bowl b(src src))
::
++  wait
  |=  d=@dr
  %-  jab-bowl
  |=(b=bowl b(now (add now.b d)))
::
++  set-scry-gate
  |=  f=scry
  =/  m  (mare ,~)
  ^-  form:m
  |=  s=state
  &+[~ s(scry f)]
::
++  do-as  ::  temporary src.bowl
  |=  who=ship
  =/  m  (mare ,(list card))
  |=  do=form:m
  ^-  form:m
  ;<  pre=bowl:gall    bind:m  get-bowl
  ;<  ~                bind:m  (set-bowl pre(src who))
  ;<  cas=(list card)  bind:m  do
  ;<  ~                bind:m  (jab-bowl |=(b=bowl:gall b(src src.pre)))
  (pure:m cas)
::
::  testing utilities
::
++  ex-equal
  |=  [actual=vase expected=vase]  ::NOTE  reverse order from /lib/test
  =/  m  (mare ,~)
  ^-  form:m
  |=  s=state
  =/  =tang  (expect-eq:test expected actual)
  ?~(tang &+[~ s] |+tang)
::
++  ex-fail
  |=  form=(form-raw ,*)
  =/  m  (mare ,~)
  ^-  form:m
  |=  =state
  =/  res  (form state)
  ?-(-.res %| &+[~ state], %& |+['expected failure, but succeeded']~)
::
++  ex-cards
  =/  drop-verb=?  &
  =/  drop-logs=?  &
  |=  [caz=(list card) exes=(list $-(card tang))]
  =?  caz  drop-verb
    ::  remove cards unconditionally emitted by /lib/verb
    ::
    %+  skip  caz
    |=  =card
    ?=([%give %fact [[%verb ?(%events %events-plus) ~] ~] *] card)
  =?  caz  drop-logs
    ::  unconditionnaly remove logging cards
    ::
    %+  skip  caz
    |=  =card
    ?|  ?=([%pass [%logs ~] %agent [ship=@ %logs] %poke *] card)
        ?=([%pass [%~.~ %negotiate %logs ~] %agent [ship=@ %logs] %poke *] card)
    ==
  =/  m  (mare ,~)
  ^-  form:m
  |=  s=state
  =;  =tang
    ?~(tang &+[~ s] |+tang)
  |-  ^-  tang
  ?~  exes
    ?~  caz
      ~
    ['got more cards than expected' >caz< ~]
  ?~  caz
    ['expected more cards than got' ~]
  %+  weld
    (i.exes i.caz)
  $(exes t.exes, caz t.caz)
::
++  ex-card
  |=  caw=card
  |=  cav=card
  (expect-eq:test !>(caw) !>(cav))
::
++  ex-fact
  |=  [paths=(list path) =mark =vase]
  |=  car=card
  ^-  tang
  =*  nope
    %-  expect-eq:test
    [!>(`card`[%give %fact paths mark vase]) !>(`card`car)]
  ?.  ?=([%give %fact *] car)  nope
  ?.  =(paths paths.p.car)     nope
  ?.  =(mark p.cage.p.car)     nope
  =/  =tang  (expect-eq:test vase q.cage.p.car)
  ?~  tang  ~
  ['in %fact vase,' tang]
::
++  ex-fact-paths
  |=  paths=(list path)
  |=  car=card
  ^-  tang
  =*  nope
    %-  expect-eq:test
    [!>(`card`[%give %fact paths *mark *vase]) !>(`card`car)]
  ?.  ?=([%give %fact *] car)  nope
  =/  =tang  (expect-eq:test !>(paths) !>(paths.p.car))
  ?~  tang  ~
  ['in %fact paths,' tang]
::
++  ex-poke
  |=  [=wire =gill:gall =mark =vase]
  |=  car=card
  ^-  tang
  =*  nope
    %-  expect-eq:test
    [!>(`card`[%pass wire %agent gill %poke mark vase]) !>(`card`car)]
  ?.  ?=([%pass * %agent * %poke *] car)  nope
  ?.  =(wire p.car)              nope
  ?.  =(gill [ship name]:q.car)  nope
  ?.  =(mark p.cage.task.q.car)  nope
  =/  =tang  (expect-eq:test vase q.cage.task.q.car)
  ?~  tang  ~
  ['in %poke vase on ,' tang]
::
++  ex-poke-wire
  |=  =wire
  |=  car=card
  ^-  tang
  =*  fail
    %-  expect-eq:test
    [!>(`card`[%pass wire %agent *gill:gall %poke *mark *vase]) !>(`card`car)]
  ?.  ?=([%pass * %agent * %poke *] car)  fail
  =/  =tang  (expect-eq:test !>(wire) !>(p.car))
  ?~  tang  ~
  ['in %poke wire,' tang]
::
++  ex-task
  |=  [=wire =gill:gall =task:agent:gall]
  (ex-card %pass wire %agent gill task)
::
++  ex-arvo
  |=  [=wire note=note-arvo]
  (ex-card %pass wire %arvo %syscall note)
::
++  ex-scry-result
  |=  [=path =vase]
  =/  m  (mare ,~)
  ^-  form:m
  ;<  res=(unit (unit cage))  bind:m  (get-peek path)
  (ex-equal q:(need (need res)) vase)
--