/-  *aquarium, spider
/+  libstrand=strand, *strandio, util=ph-util, aqua-azimuth, vere
=,  strand=strand:libstrand
|_  agent=?(%lago %aqua)
+$  drivers  (map term tid:spider)
::
++  send-events
  |=  events=(list aqua-event)
  =/  m  (strand ,~)
  ^-  form:m
  (poke-our agent %aqua-events !>(events))
::
++  send-azimuth-action
  |=  =azimuth-action
  =/  m  (strand ,~)
  ^-  form:m
  (poke-our agent %azimuth-action !>(azimuth-action))
::
++  take-unix-effect
  =/  m  (strand ,[ship unix-effect])
  ^-  form:m
  ;<  [=path =cage]  bind:m  (take-fact-prefix /effect)
  ?>  ?=(%aqua-effect p.cage)
  (pure:m !<([aqua-effect] q.cage))
::
++  take-aqua-rule
  =/  m  (strand ,rule-actions)
  ^-  form:m
  ;<  =cage  bind:m  (take-fact /net-control)
  ?>  ?=(%aqua-rule p.cage)
  (pure:m !<(rule-actions q.cage))
::
++  net-hold
  |=  [from=@p to=@p]
  =/  m  (strand ,~)
  ^-  form:m
  (poke-our agent %aqua-rule !>(`rule-actions`[%hold-link from to]))
::
++  net-flush
  |=  [from=@p to=@p]
  =/  m  (strand ,~)
  ^-  form:m
  ;<  =bowl:spider  bind:m  get-bowl
  %-  send-raw-card
  [%pass /net-flush %agent [our.bowl %aqua] %poke %aqua-rule !>(`rule-actions`[%flush-link from to])]
::
++  start-simple
  (start-test %aqua-ames %aqua-behn %aqua-dill %aqua-eyre ~)
::
++  start-lago
  (start-test %aqua-behn %aqua-dill %aqua-eyre ~)
::
++  start-azimuth
  =/  m  (strand ,drivers)
  ^-  form:m
  ;<  tids=drivers  bind:m  start-simple
  ;<  ~  bind:m  init
  (pure:m tids)
::
++  start-azimuth-lago
  =/  m  (strand ,drivers)
  ^-  form:m
  ;<  tids=drivers  bind:m  start-lago
  ;<  ~  bind:m  init
  (pure:m tids)
::
++  end  end-test
::
++  start-test
  |=  vane-threads=(list term)
  =/  m  (strand ,drivers)
  ^-  form:m
  ;<  tids=drivers  bind:m  (start-threads vane-threads)
  ;<  ~  bind:m  (watch-our /effect agent /effect)
  ::  Get our very own event with no mistakes in it... yet.
  ::
  ::  We want to wait for the vane threads to actually start and get
  ::  their subscriptions started.  Other ways to do this are delaying
  ::  the ack from spider until the build is finished (does that
  ::  guarantee the subscriptions have started?) or subscribe to the
  ::  threads themselves for a notification when they're done.  This is
  ::  probably the best option because the thread can delay until it
  ::  gets a positive ack on the subscription.
  ::
  ::  Threads might not get built until a %writ is dripped back to
  ::  spider.  Drips are at +(now), so we sleep until two clicks in the
  ::  future.
  ::
  ;<  ~  bind:m  (sleep `@dr`2)
  (pure:m tids)
::
++  end-test
  |=  tids=drivers
  =/  m  (strand ,~)
  ^-  form:m
  ;<  ~  bind:m  (stop-threads tids)
  ;<  ~  bind:m  (leave-our /effect agent)
  (pure:m ~)
::
++  start-threads
  |=  threads=(list term)
  =/  m  (strand ,drivers)
  ^-  form:m
  ;<  =bowl:spider  bind:m  get-bowl
  =|  tids=drivers
  |-  ^-  form:m
  =*  loop  $
  ?~  threads
    (pure:m tids)
  =/  tid
    ^-  @ta
    (cat 3 (cat 3 'strand_' i.threads) (scot %uv (sham i.threads eny.bowl)))
  =/  poke-vase  !>([`tid.bowl `tid byk.bowl(q %base) i.threads *vase])
  ;<  ~  bind:m  (poke-our %spider %spider-start poke-vase)
  loop(threads t.threads, tids (~(put by tids) i.threads tid))
::
++  stop-threads
  |=  tids=drivers
  =/  m  (strand ,~)
  ^-  form:m
  =/  tids=(list tid:spider)  ~(val by tids)
  |-  ^-  form:m
  ?~  tids  (pure:m ~)
  ;<  ~  bind:m  (poke-our %spider spider-stop+!>([i.tids |]))
  $(tids t.tids)
::
++  init
  =/  m  (strand ,~)
  ^-  form:m
  (send-azimuth-action %init-azimuth ~)
::
++  spawn
  |=  =ship
  =/  m  (strand ,~)
  ^-  form:m
  (send-azimuth-action %spawn ship)
::
++  breach
  |=  =ship
  =/  m  (strand ,~)
  ^-  form:m
  (send-azimuth-action %breach ship)
::
::  who: breachee
::  her: wait until hears about breach
::
++  breach-and-hear
  |=  [who=ship her=ship]
  =/  m  (strand ,~)
  ;<  =bowl:spider  bind:m  get-bowl
  ;<  old-rut=(unit @ud)  bind:m
    (scry-aqua (unit @ud) her /j/(scot %p her)/rift/(scot %da now.bowl)/(scot %p who)/noun)
  =/  new-rut
    ?~  old-rut
      1
    +(+.old-rut)
  ;<  ~  bind:m  (send-azimuth-action %breach who)
  ;<  ~  bind:m  ?~  old-rut  (sleep ~s20) :: XX don't wait if possible
                 (wait-for-sunk her who)   :: wait only if previously talked
  |-  ^-  form:m
  =*  loop  $
  ;<  ~  bind:m  (sleep ~s10)
  ;<  =bowl:spider  bind:m  get-bowl
  ;<  rut=(unit @ud)  bind:m
    (scry-aqua (unit @ud) her /j/(scot %p her)/rift/(scot %da now.bowl)/(scot %p who)/noun)
  ?:  =([~ new-rut] rut)
    (pure:m ~)
  loop
::
++  init-moon  ::NOTE  real moon always have the same keys
  |=  [moon=ship fake=?]
  ?>  ?=(%earl (clan:title moon))
  ?:  fake  (init-ship moon &)
  =/  m  (strand ,~)
  ^-  form:m
  ;<  ~  bind:m
    %+  dojo  (^sein:title moon)
    =/  =pass  pub:ex:(get-keys:aqua-azimuth moon 1)
    "|moon {(scow %p moon)}, =public-key {(scow %uw pass)}"
  (init-ship moon |)
::
++  init-ship
  |=  [=ship fake=?]
  =/  m  (strand ,~)
  ^-  form:m
  ;<  ~  bind:m  (send-events (init:util ship fake ~))
  (check-ship-booted ship)
::
++  init-comet
  |=  comet=ship
  =/  m  (strand ,~)
  ^-  form:m
  ::  hardcoded for:
  ::    ~londeg-tirlys-somlyd-poltus--pintyn-tarbyl-bicnux-marbud
  ::
  =/  =feed:jael
    :*  [%2 ~]
        who=comet
        ryf=0
        :_  ~
        :-  lyf=1
        key=0wfm.lBEWM.08gfy.AxYjy.8-tBQ.uq-aa.LZt9c.CVQqd.XBJIs.
            CoG90.BNNGV.1ZmVi.ZbAhY.LuhwC.idNnU.lCVkt.Z4qug.7iY92
    ==
  ::
  ?>  ?=(^ (veri:dawn:vere comet feed *point:azimuth-types ~))
  ~&  >  "mining comet under {<(^sein:title comet)>}"
  ;<  ~  bind:m  (send-events (init:util comet fake=%.n `feed))
  (check-ship-booted comet)
::
::  Load network core protocol
::
++  load
  |=  [who=ship ore=?(%mesa %ames)]
  =/  m  (strand ,~)
  ^-  form:m
  ;<  ~  bind:m  (send-events [%event who [/a/aqua/load %load ore]]~)
  (pure:m ~)
::
++  aqua-setup
  |=  =aqua-action
  =/  m  (strand ,~)
  ^-  form:m
  (poke-our agent %noun !>(aqua-action))
::
++  switch-network-core
  |=  core=?(%mesa %ames)
  =/  m  (strand ,~)
  ^-  form:m
  (aqua-setup network-core/core)
::
++  check-ship-booted
  |=  =ship
  =/  m  (strand ,~)
  ^-  form:m
  =*  loop  $
  ;<  [her=^ship =unix-effect]  bind:m  take-unix-effect
  =/  f  |=(=tape (is-dojo-output:util ship her unix-effect tape))
  ::  This is a pretty bad heuristic, but in general galaxies will
  ::  hit the first of these cases, and other ships will hit the
  ::  second.
  ::
  ?:  ?|  (f ":dojo>")
          (f "is your neighbor")
      ==
    (pure:m ~)
  loop
::
++  dojo
  |=  [=ship =tape]
  =/  m  (strand ,~)
  ^-  form:m
  (send-events (dojo:util ship tape))
::
++  wait-for-output
  |=  [=ship =tape]
  =/  m  (strand ,~)
  ^-  form:m
  |-  ^-  form:m
  =*  loop  $
  ;<  [her=^ship =unix-effect]  bind:m  take-unix-effect
  ~?  >>  !?=([^ %blit *] unix-effect)  her^unix-effect
  ?:  (is-dojo-output:util ship her unix-effect tape)
    (pure:m ~)
  loop
::
++  wait-for-flub
  |=  [our=ship her=ship dap=term]
  =/  m  (strand ,~)
  ^-  form:m
  ;<  =bowl:spider  bind:m  get-bowl
  |-  ^-  form:m
  =*  loop  $
  ;<  [from=^ship =unix-effect]  bind:m  take-unix-effect
  ;<  now=@da                    bind:m  get-time
  ?.  =(from our)
    ::  our sends the $boon with the halted flow number, at this point
    ::  gall has updated its state adding the app to gall's .halts map
    ::  XX  search deeper in the .unix-effect?
    ::
    loop
  ;<  flubs=(unit (jug ship term))  bind:m
    (scry-aqua (unit (jug ship term)) our /gg/(scot %p our)//(scot %da now)//noun)
  ?~  flubs  loop
  ?.  (~(has ju u.flubs) her dap)
    loop
  (pure:m ~)
::
++  wait-for-spur
  |=  [our=ship her=ship dap=term]
  =/  m  (strand ,~)
  ^-  form:m
  ;<  =bowl:spider  bind:m  get-bowl
  |-  ^-  form:m
  =*  loop  $
  ;<  [from=ship =unix-effect]  bind:m  take-unix-effect
  ;<  now=@da                   bind:m  get-time
  ?.  =(from our)
    ::  our ack to the %spur $boon is sent, at this point
    ::  gall has updated its state adding the app to gall's .flubs map
    ::  XX  search deeper in the .unix-effect?
    ::
    loop
  ;<  flubs=(unit (jug ship term))  bind:m
    (scry-aqua (unit (jug ship term)) our /gg/(scot %p our)//(scot %da now)//noun)
  ?~  flubs  loop
  ?:  (~(has ju u.flubs) her dap)
    loop
  (pure:m ~)
::
++  wait-for-has-halt
  |=  [our=ship her=ship dap=term]
  =/  m  (strand ,~)
  ^-  form:m
  ;<  =bowl:spider  bind:m  get-bowl
  |-  ^-  form:m
  =*  loop  $
  ;<  [from=ship =unix-effect]  bind:m  take-unix-effect
  ;<  now=@da                   bind:m  get-time
  ::  only %send or %push effects
  ::
  ?.  ?=(?(%send %push) -.q.unix-effect)
    loop
  ?.  =(from our)
    ::  wait until our ack for the %flub $boon is sent, at this point
    ::  gall has updated its state adding the app to gall's .flubs map
    ::  XX  search deeper in the .unix-effect?
    ::
    loop
  ;<  halts=(unit (jug app=term [ship =duct]))  bind:m
    %+  scry-aqua  (unit ,(jug app=term [ship =duct]))
    [our /gh/(scot %p our)//(scot %da now)//noun]
  ?~  halts  loop
  ?.  (~(has by u.halts) dap)  ::  XX check .her as well
    loop
  (pure:m ~)
::
++  wait-for-del-halt
  |=  [our=ship her=ship dap=term]
  =/  m  (strand ,~)
  ^-  form:m
  ;<  =bowl:spider  bind:m  get-bowl
  |-  ^-  form:m
  =*  loop  $
  ;<  [from=ship =unix-effect]  bind:m  take-unix-effect
  ;<  now=@da                   bind:m  get-time
  ?.  =(from our)
    ::  when the %spur $boon has been sent, gall has updated its state deleting
    ::  the app from gall's .flubs map
    ::  XX  search deeper in the .unix-effect?
    ::
    loop
  ;<  halts=(unit (jug app=term [ship =duct]))  bind:m
    (scry-aqua (unit ,(jug app=term [ship =duct])) our /gh/(scot %p our)//(scot %da now)//noun)
  ?~  halts  loop
  ?:  (~(has by u.halts) dap) ::  XX check .her as well
    loop
  (pure:m ~)
::
++  wait-for-pac
  |=  [our=ship to=ship]
  =/  m  (strand ,~)
  ^-  form:m
  ~&  >  "waiting for ack: {<[from=our to=to]>}"
  |-  ^-  form:m
  =*  loop  $
  ;<  [from=ship =unix-effect]  bind:m  take-unix-effect
  ?.  =(our from)
    loop
  ::  only %send or %push effects
  ::
  ?.  ?=(?(%send %push) -.q.unix-effect)
    loop
  ::  XX  check that this is an %ack?
  ::  XX  check that this is for .to?
  ::
  (pure:m ~)
::  Send "|hi" from one ship to another
::
++  send-hi
  |=  [from=@p to=@p]
  =/  m  (strand ,~)
  ^-  form:m
  ;<  ~  bind:m  (dojo from "|hi {(scow %p to)}")
  (wait-for-output from "hi {(scow %p to)} successful")
::
::  Send "|hi" and wait for "not responding" message
::
++  wait-for-sunk
  |=  [from=@p to=@p]
  =/  m  (strand ,~)
  (wait-for-output from "{(scow %p to)} has sunk")
::
++  wait-for-fact
  |=  [=ship =mark =wire gate=$-([mark noun] ?)]
  =/  m  (strand ,noun)
  ^-  form:m
  ~&  >  "waiting for fact: {<mark>}"
  |-  ^-  form:m
  =*  loop  $
  ;<  [her=^ship =unix-effect]  bind:m  take-unix-effect
  ?.  =(her ship)
    loop
  ?.  ?&  =(wire p.unix-effect)
          ?=([%unto %raw-fact *] q.unix-effect)
          =(mark mark.unto.q.unix-effect)
          (gate mark noun.unto.q.unix-effect)
      ==
    loop
  (pure:m noun.unto.q.unix-effect)
::
++  peek-for-cork
  |=  [our=ship her=ship flow=(each bone:ames side:ames)]
  =/  m  (strand ,?)
  ^-  form:m
  ;<  =bowl:spider  bind:m  get-bowl
  ;<  now=@da  bind:m  get-time
  =*  loop  $
  =/  aqua-pax
    %+  weld
        /ax/(scot %p our)//(scot %da now)
    ?:  ?=(%& -.flow)
      /corked/(scot %p her)/(scot %ud +.flow)/noun
    =/  [=bone:ames =dire:ames]  +.flow
    /corked/(scot %p her)/[dire]/(scot %ud bone)/noun
  ::
  ;<  corked=(unit ?)  bind:m  (scry-aqua (unit ?) our aqua-pax)
  ?~  corked
    loop  ::  (pure:m %.n)
  ?.  u.corked  ::  XX check .her as well
    loop  ::  (pure:m %.n)
  ~&  >>  flow-is-corked/flow
  (pure:m %.y)
::
++  wait-for-cork
  |=  [our=ship her=ship flow=(each bone:ames side:ames)]
  =/  m  (strand ,~)
  ^-  form:m
  ;<  =bowl:spider  bind:m  get-bowl
  |-  ^-  form:m
  =*  loop  $
  ;<  [from=ship =unix-effect]  bind:m  take-unix-effect
  ;<  now=@da                   bind:m  get-time
  ::  only %send or %push effects
  ::
  ?.  ?=(?(%send %push) -.q.unix-effect)
    loop
  ::  XX  check that this is an %ack?
  ::
  ?.  =(from our)
    ::  wait until the ack for the %cork $plea is sent, at this point
    ::  the publisher has already corked the flow
    ::
    loop
  =/  aqua-pax
    %+  weld
        /ax/(scot %p our)//(scot %da now)
    ?:  ?=(%& -.flow)
      /corked/(scot %p her)/(scot %ud +.flow)/noun
    =/  [=bone:ames =dire:ames]  +.flow
    /corked/(scot %p her)/[dire]/(scot %ud bone)/noun
  ::
  ;<  corked=(unit ?)  bind:m  (scry-aqua (unit ?) our aqua-pax)
  ?~  corked  loop
  ?.  u.corked  ::  XX check .her as well
    loop
  ~&  >>  flow-is-corked/flow
  (pure:m ~)
::
::  Send "|hi" and wait for "not responding" message
::
++  send-hi-not-responding
  |=  [from=@p to=@p]
  =/  m  (strand ,~)
  ;<  ~  bind:m  (dojo from "|hi {(scow %p to)}")
  (wait-for-output from "{(scow %p to)} not responding still trying")
::
::  Mount a desk.
::
++  mount
  |=  [=ship =desk]
  =/  m  (strand ,~)
  ^-  form:m
  ;<  ~                         bind:m  (dojo ship "|mount /={(trip desk)}=")
  |-  ^-  form:m
  =*  loop  $
  ;<  [her=^ship =unix-effect]  bind:m  take-unix-effect
  ?:  (is-ergo:util ship her unix-effect)
    (pure:m ~)
  loop
::
::  Modify /sur/aquarium/hoon on the given ship
::
++  touch-file
  |=  [her=ship =desk extra=@t]
  =/  m  (strand ,@t)
  ^-  form:m
  (touch her desk /sur/aquarium/hoon extra)
::
::  Modify path on the given ship
::
++  touch
  |=  [her=ship =desk pax=path extra=@t]
  =/  m  (strand ,@t)
  ^-  form:m
  ;<  ~        bind:m  (mount her desk)
  ;<  our=@p   bind:m  get-our
  ;<  now=@da  bind:m  get-time
  =/  aqua-pax
    ;:  weld
        /cx/(scot %p her)/[desk]/(scot %da now)
        pax
        /noun
    ==
  ;<  file=(unit @t)  bind:m  (scry-aqua (unit @t) her aqua-pax)
  =/  warped
    %^  cat  3  (crip "=>  [. {<extra>}]  ")
    (need file)
  ;<  ~  bind:m  (send-events (insert-files:util her desk [pax warped] ~))
  (pure:m warped)
::
++  copy-file
  =/  m  (strand ,~)
  |=  [her=ship pax=path file=@t]
  ^-  form:m
  ;<  ~  bind:m
    (send-events (insert-files:util her %base [pax file] ~))
  (sleep ~s1)
::
::  Check /sur/aquarium/hoon on the given has the given contents.
::
++  check-file-touched
  |=  [=ship =desk warped=@t]
  =/  m  (strand ,~)
  (check-touched ship desk /sur/aquarium/hoon warped)
::
::  Check path on the given desk has the given contents.
::
++  check-touched
  |=  [=ship =desk pax=path warped=@t]
  =/  m  (strand ,~)
  ;<  ~                         bind:m  (mount ship desk)
  ^-  form:m
  |-  ^-  form:m
  =*  loop  $
  ;<  [her=^ship =unix-effect]  bind:m  take-unix-effect
  ;<  our=@p                    bind:m  get-our
  ;<  now=@da                   bind:m  get-time
  ::  %ergo is no longer sufficient because .^ is pinned to beginning of
  ::  the event.  So we hope somebody sets a timer for something.
  ::
  ?.  &(=(ship her) ?=(?(%init %ergo %doze) -.q.unix-effect))
    loop
  =/  aqua-pax
    ;:  weld
        /cx/(scot %p ship)/[desk]/(scot %da now)
        pax
        /noun
    ==
  ;<  file=(unit @t)  bind:m  (scry-aqua (unit @t) ship aqua-pax)
  ?:  =(warped (need file))
    (pure:m ~)
  loop
::
::  Turns poke into a dojo command
::
++  poke-app
  |=  [=ship app=term =mark data=*]
  =/  m  (strand ,~)
  ^-  form:m
  =/  command=tape  ":{(trip app)} &{(trip mark)} {<data>}"
  (send-events (dojo:util ship command))
::
++  dojo-thread
  |=  [=ship ted=term =mark data=*]
  =/  m  (strand ,~)
  ^-  form:m
  =/  command=tape  "-{(trip ted)} &{(trip mark)} {<data>}"
  (send-events (dojo:util ship command))
::
++  scry-aqua
  |*  [=mold =ship pax=path]
  =/  m  (strand ,mold)
  ^-  form:m
  ;<  =bowl:spider  bind:m  get-bowl
  =/  aqua-pax=path
    %+  weld
      /i/(scot %p ship)
    pax
  %-  pure:m
  .^  mold
      %gx
      (scot %p our.bowl)
      agent
      (scot %da now.bowl)
      aqua-pax
  ==
::
--