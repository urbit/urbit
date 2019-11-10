/-  *aquarium
/+  libthread=thread, *threadio, util=ph-util
=,  thread=thread:libthread
|%
++  send-events
  |=  events=(list aqua-event)
  =/  m  (thread ,~)
  ^-  form:m
  (poke-our %aqua %aqua-events !>(events))
::
++  take-unix-effect
  =/  m  (thread ,[ship unix-effect])
  ^-  form:m
  ;<  =cage  bind:m  (take-fact /effects)
  ?>  ?=(%aqua-effect p.cage)
  (pure:m !<([aqua-effect] q.cage))
::
++  start-simple
  (start-test %aqua-ames %aqua-behn %aqua-dill %aqua-eyre ~)
++  end-simple
  (end-test %aqua-ames %aqua-behn %aqua-dill %aqua-eyre ~)
::
++  start-azimuth
  (start-test %aqua-ames %aqua-behn %aqua-dill %aqua-eyre-azimuth ~)
++  end-azimuth
  (end-test %aqua-ames %aqua-behn %aqua-dill %aqua-eyre-azimuth ~)
::
++  start-test
  |=  vane-imps=(list term)
  =/  m  (thread ,~)
  ^-  form:m
  ~&  >  "starting"
  ;<  ~  bind:m  (start-imps vane-imps)
  ;<  ~  bind:m  (watch-our /effects %aqua /effect)
  ::  Get our very own event with no mistakes in it... yet.
  ::
  ::  We want to wait for the vane imps to actually start and get their
  ::  subscriptions started.  Other ways to do this are delaying the ack
  ::  from spider until the build is finished (does that guarantee the
  ::  subscriptions have started?) or subscribe to the imps themselves
  ::  for a notification when they're done.  This is probably the best
  ::  option because the imp can delay until it gets a positive ack on
  ::  the subscription.
  ::
  ;<  ~  bind:m  (sleep ~s0)
  (pure:m ~)
::
++  end-test
  |=  vane-imps=(list term)
  =/  m  (thread ,~)
  ^-  form:m
  ~&  >  "done"
  ;<  ~  bind:m  (stop-imps vane-imps)
  ;<  ~  bind:m  (leave-our /effects %aqua)
  (pure:m ~)
::
++  start-imps
  |=  imps=(list term)
  =/  m  (thread ,~)
  ^-  form:m
  ;<  our=@p  bind:m  get-our
  |-  ^-  form:m
  =*  loop  $
  ?~  imps
    (pure:m ~)
  ;<  now=@da  bind:m  get-time
  =/  imp-started
    .^(? %mx /(scot %p our)/spider/(scot %da now)/started/[i.imps]/noun)
  ?:  imp-started
    loop(imps t.imps)
  =/  poke-vase  !>([i.imps i.imps *vase])
  ;<  ~  bind:m  (poke-our %spider %spider-start poke-vase)
  loop(imps t.imps)
::
++  stop-imps
  |=  imps=(list term)
  =/  m  (thread ,~)
  ^-  form:m
  ;<  our=@p  bind:m  get-our
  |-  ^-  form:m
  =*  loop  $
  ?~  imps
    (pure:m ~)
  ;<  now=@da  bind:m  get-time
  =/  imp-started
    .^(? %mx /(scot %p our)/spider/(scot %da now)/started/[i.imps]/noun)
  ?.  imp-started
    loop(imps t.imps)
  =/  poke-vase  !>([i.imps &])
  ;<  ~  bind:m  (poke-our %spider %spider-stop poke-vase)
  loop(imps t.imps)
::
++  spawn
  |=  =ship
  =/  m  (thread ,~)
  =/  =vase  !>([%aqua-eyre-azimuth %azimuth-command !>([%spawn ship])])
  (poke-our %spider %spider-imput vase)
::
++  breach
  |=  who=ship
  =/  m  (thread ,~)
  ~&  >  "breaching {<who>}"
  =/  =vase
    !>([%aqua-eyre-azimuth %azimuth-command !>([%breach who])])
  (poke-our %spider %spider-imput vase)
::
++  breach-and-hear
  |=  [who=ship her=ship]
  =/  m  (thread ,~)
  ~&  >  "breaching {<who>} for {<her>}"
  =/  =vase
    !>([%aqua-eyre-azimuth %azimuth-command !>([%breach-and-hear who her])])
  (poke-our %spider %spider-imput vase)
::
++  real-ship
  |=  =ship
  =/  m  (thread ,~)
  =/  =vase  !>([%aqua-eyre-azimuth %azimuth-command !>([%create-ship ship])])
  ;<  ~  bind:m  (poke-our %spider %spider-imput vase)
  (check-ship-booted ship)
::
++  raw-ship
  |=  [=ship keys=(unit dawn-event:able:jael)]
  =/  m  (thread ,~)
  ^-  form:m
  ~&  >  "starting {<ship>}"
  ;<  ~  bind:m  (send-events (init:util ship keys))
  (check-ship-booted ship)
::
++  check-ship-booted
  |=  =ship
  =/  m  (thread ,~)
  ^-  form:m
  =*  loop  $
  ;<  [her=^ship =unix-effect]  bind:m  take-unix-effect
  =/  f  |=(=tape (is-dojo-output:util ship her unix-effect tape))
  ::  This is a pretty bad heuristic, but in general galaxies will
  ::  hit the first of these cases, and other ships will hit the
  ::  second.
  ::
  ?:  ?|  (f "clay: committed initial filesystem (all)")
          (f "is your neighbor")
      ==
    (pure:m ~)
  loop
::
++  dojo
  |=  [=ship =tape]
  =/  m  (thread ,~)
  ^-  form:m
  ~&  >  "dojo: {tape}"
  (send-events (dojo:util ship tape))
::
++  wait-for-output
  |=  [=ship =tape]
  =/  m  (thread ,~)
  ^-  form:m
  ~&  >  "waiting for output: {tape}"
  |-  ^-  form:m
  =*  loop  $
  ;<  [her=^ship =unix-effect]  bind:m  take-unix-effect
  ?:  (is-dojo-output:util ship her unix-effect tape)
    (pure:m ~)
  loop
::
::  Send "|hi" from one ship to another
::
++  send-hi
  |=  [from=@p to=@p]
  =/  m  (thread ,~)
  ^-  form:m
  ;<  ~  bind:m  (dojo from "|hi {(scow %p to)}")
  (wait-for-output from "hi {(scow %p to)} successful")
::
::  Send "|hi" and wait for "not responding" message
::
++  send-hi-not-responding
  |=  [from=@p to=@p]
  =/  m  (thread ,~)
  ;<  ~  bind:m  (dojo from "|hi {(scow %p to)}")
  (wait-for-output from "{(scow %p to)} not responding still trying")
::
::  Mount a desk.
::
++  mount
  |=  [=ship =desk]
  =/  m  (thread ,~)
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
  |=  [her=ship =desk]
  =/  m  (thread ,@t)
  ^-  form:m
  ~&  >  "touching file on {<her>}/{<desk>}"
  ;<  ~        bind:m  (mount her desk)
  ;<  our=@p   bind:m  get-our
  ;<  now=@da  bind:m  get-time
  =/  host-pax
    /(scot %p our)/home/(scot %da now)/sur/aquarium/hoon
  =/  pax  /sur/aquarium/hoon
  =/  aqua-pax
    ;:  weld
        /i/(scot %p her)/cx/(scot %p her)/[desk]/(scot %da now)
        pax
        /noun
    ==
  =/  warped
    %^  cat  3  '=>  .  '
    (need (scry-aqua:util (unit @) our now aqua-pax))
  ;<  ~  bind:m  (send-events (insert-file:util her desk host-pax warped))
  (pure:m warped)
::
::  Check /sur/aquarium/hoon on the given has the given contents.
::
++  check-file-touched
  |=  [=ship =desk warped=@t]
  =/  m  (thread ,~)
  ~&  >  "checking file touched on {<ship>}/{<desk>}"
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
  =/  pax  /sur/aquarium/hoon
  =/  aqua-pax
    ;:  weld
        /i/(scot %p ship)/cx/(scot %p ship)/[desk]/(scot %da now)
        pax
        /noun
    ==
  ?:  =(warped (need (scry-aqua:util (unit @) our now aqua-pax)))
    (pure:m ~)
  loop
--
