::  observe-hook:
::
::  helper that observes an app at a particular path and forwards all facts 
::  to a particular thread. kills the subscription if the thread crashes
::
/-  sur=observe-hook
/+  default-agent, dbug
::
|%
+$  card  card:agent:gall
+$  state-0
  $:  observers=(map serial observer:sur)
      warm-cache=_|
      static-conversions=(set [term term])
  ==
::
+$  versioned-state
  $%  [%0 observers=(map serial observer:sur)]
      [%1 observers=(map serial observer:sur)]
      [%2 observers=(map serial observer:sur)]
      [%3 observers=(map serial observer:sur)]
      [%4 observers=(map serial observer:sur)]
      [%5 observers=(map serial observer:sur) warm-cache=_|]
      [%6 state-0]
      [%7 state-0]
  ==
::
+$  serial   @uv
++  got-by-val
  |=  [a=(map serial observer:sur) b=observer:sur]
  ^-  serial
  %-  need
  %+  roll  ~(tap by a)
  |=  [[key=serial val=observer:sur] output=(unit serial)]
  ?:(=(val b) `key output)
--
::
%-  agent:dbug
=|  [%7 state-0]
=*  state  -
::
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
    bec   byk.bowl(r da+now.bowl)
    desk  q.byk.bowl
::
++  on-init
  |^  ^-  (quip card _this)
  :_  this
  :~  (act [%watch %invite-store /invitatory/graph %invite-accepted-graph])
      (act [%watch %group-store /groups %group-on-leave])
      (act [%watch %group-store /groups %group-on-remove-member])
      (act [%watch %metadata-store /updates %md-on-add-group-feed])
      (act [%warm-cache-all ~])
    ::
      (warm-static %graph-validator-chat %graph-indexed-post)
      (warm-static %graph-validator-publish %graph-indexed-post)
      (warm-static %graph-validator-link %graph-indexed-post)
      (warm-static %graph-validator-post %graph-indexed-post)
      (warm-static %graph-validator-dm %graph-indexed-post)
    ::
      (warm-static %graph-validator-chat %graph-permissions-add)
      (warm-static %graph-validator-publish %graph-permissions-add)
      (warm-static %graph-validator-link %graph-permissions-add)
      (warm-static %graph-validator-post %graph-permissions-add)
    ::
      (warm-static %graph-validator-chat %graph-permissions-remove)
      (warm-static %graph-validator-publish %graph-permissions-remove)
      (warm-static %graph-validator-link %graph-permissions-remove)
      (warm-static %graph-validator-post %graph-permissions-remove)
    ::
      (warm-static %graph-validator-chat %notification-kind)
      (warm-static %graph-validator-publish %notification-kind)
      (warm-static %graph-validator-link %notification-kind)
      (warm-static %graph-validator-post %notification-kind)
      (warm-static %graph-validator-dm %notification-kind)
    ::
      (warm-static %graph-validator-chat %transform-add-nodes)
      (warm-static %graph-validator-publish %transform-add-nodes)
      (warm-static %graph-validator-link %transform-add-nodes)
      (warm-static %graph-validator-post %transform-add-nodes)
  ==
  ::
  ++  act
    |=  =action:sur
    ^-  card
    :*  %pass
        /poke
        %agent
        [our.bowl %observe-hook]
        %poke
        %observe-action
        !>(action)
    ==
  ::
  ++  warm-static
    |=  [from=term to=term]
    ^-  card
    :*  %pass
        /poke
        %agent
        [our.bowl %observe-hook]
        %poke
        %observe-action
        !>  ^-  action:sur
        [%warm-static-conversion from to]
    ==
  --
::
++  on-save   !>(state)
++  on-load
  |=  old-vase=vase
  ^-  (quip card _this)
  |^
  =/  old-state  !<(versioned-state old-vase)
  =|  cards=(list card)
  |-
  ?-  -.old-state
      %7
    [cards this(state old-state)]
      %6
    $(-.old-state %7, cards :_(cards (act %warm-cache-all ~)))
  ::
      %5
    =.  cards
      %+  weld  cards
      :~  (warm-static %graph-validator-chat %graph-indexed-post)
          (warm-static %graph-validator-publish %graph-indexed-post)
          (warm-static %graph-validator-link %graph-indexed-post)
          (warm-static %graph-validator-post %graph-indexed-post)
          (warm-static %graph-validator-dm %graph-indexed-post)
        ::
          (warm-static %graph-validator-chat %graph-permissions-add)
          (warm-static %graph-validator-publish %graph-permissions-add)
          (warm-static %graph-validator-link %graph-permissions-add)
          (warm-static %graph-validator-post %graph-permissions-add)
        ::
          (warm-static %graph-validator-chat %graph-permissions-remove)
          (warm-static %graph-validator-publish %graph-permissions-remove)
          (warm-static %graph-validator-link %graph-permissions-remove)
          (warm-static %graph-validator-post %graph-permissions-remove)
        ::
          (warm-static %graph-validator-chat %notification-kind)
          (warm-static %graph-validator-publish %notification-kind)
          (warm-static %graph-validator-link %notification-kind)
          (warm-static %graph-validator-post %notification-kind)
          (warm-static %graph-validator-dm %notification-kind)
        ::
          (warm-static %graph-validator-chat %transform-add-nodes)
          (warm-static %graph-validator-publish %transform-add-nodes)
          (warm-static %graph-validator-link %transform-add-nodes)
          (warm-static %graph-validator-post %transform-add-nodes)
      ==
    $(old-state [%6 observers.old-state %.n ~])
  ::
      %4
    =.  cards
      :_  cards
      (act [%warm-cache-all ~])
    $(old-state [%5 observers.old-state %.n])
  ::
      %3
    =.  cards
      :_  cards
      (act [%watch %metadata-store /updates %md-on-add-group-feed])
    $(-.old-state %4)
  ::
      %2
    =.  cards
      :_  cards
      (act [%watch %group-store /groups %group-on-leave])
    $(-.old-state %3)
  ::
      %1
    $(-.old-state %2)
  ::
      %0
    =.  cards
      :_  cards
      (act [%watch %group-store /groups %group-on-remove-member])
    $(-.old-state %1)
  ==
  ::
  ++  act
    |=  =action:sur
    ^-  card
    :*  %pass
        /poke
        %agent
        [our.bowl %observe-hook]
        %poke
        %observe-action
        !>(action)
    ==
  ::
  ++  warm-static
    |=  [from=term to=term]
    ^-  card
    :*  %pass
        /poke
        %agent
        [our.bowl %observe-hook]
        %poke
        %observe-action
        !>  ^-  action:sur
        [%warm-static-conversion from to]
    ==
  --
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?>  (team:title our.bowl src.bowl)
  ?.  ?=(%observe-action mark)
    (on-poke:def mark vase)
  |^
  =/  =action:sur  !<(action:sur vase)
  =*  observer  observer.action
  =/  vals  (silt ~(val by observers))
  ?-  -.action
    %watch                   (watch observer vals)
    %ignore                  (ignore observer vals)
    %warm-cache-all          warm-cache-all
    %cool-cache-all          cool-cache-all
    %warm-static-conversion  (warm-static-conversion from.action to.action)
    %cool-static-conversion  (cool-static-conversion from.action to.action)
  ==
  ::
  ++  watch
    |=  [=observer:sur vals=(set observer:sur)]
    ?:  ?|(=(app.observer %spider) =(app.observer %observe-hook)) 
      ~|('we avoid infinite loops' !!)
    ?:  (~(has in vals) observer)
      ~|('duplicate observer' !!)
    :_  this(observers (~(put by observers) (sham eny.bowl) observer))  
    :_  ~
    :*  %pass
        /observer/(scot %uv (sham eny.bowl))
        %agent
        [our.bowl app.observer]
        %watch
        path.observer
    == 
  ::
  ++  ignore
    |=  [=observer:sur vals=(set observer:sur)]
    ?.  (~(has in vals) observer)
      ~|('cannot remove nonexistent observer' !!)
    =/  key  (got-by-val observers observer)
    :_  this(observers (~(del by observers) key))
    :_  ~
    :*  %pass
        /observer/(scot %uv key)
        %agent
        [our.bowl app.observer]
        %leave
        ~
    == 
  ::
  ++  warm-cache-all
    ?:  warm-cache
      ~|('cannot warm up cache that is already warm' !!)
    :_  this(warm-cache %.y)
    =/  =rave:clay  [%sing [%t da+now.bowl /mar]]
    [%pass /warm-cache %arvo %c %warp our.bowl desk `rave]~
  ::
  ++  cool-cache-all
    ?.  warm-cache
      ~|('cannot cool down cache that is already cool' !!)
    [~ this(warm-cache %.n)]
  ::
  ++  warm-static-conversion
    |=  [from=term to=term]
    ^-  (quip card _this)
    ?:  (~(has in static-conversions) [from to])
      ~|('cannot warm up a static conversion that is already warm' !!)
    :_  this(static-conversions (~(put in static-conversions) [from to]))
    =/  =wire  /static-convert/[from]/[to]
    =/  =rave:clay  [%sing %f [%da now.bowl] /[from]/[to]]
    [%pass wire %arvo %c %warp our.bowl desk `rave]~
  ::
  ++  cool-static-conversion
    |=  [from=term to=term]
    ^-  (quip card _this)
    ?.  (~(has in static-conversions) [from to])
      ~|('cannot cool a static conversion that is already cool' !!)
    [~ this(static-conversions (~(del in static-conversions) [from to]))]
  --
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  |^
  ?+  wire  (on-agent:def wire sign)
    [%observer @ ~]          on-observer
    [%thread-result @ @ ~]   on-thread-result
    [%thread-start @ @ ~]    on-thread-start
  ==
  ::
  ++  on-observer
    ?>  ?=([%observer @ ~] wire)
    ?+    -.sign  (on-agent:def wire sign)
        %watch-ack
      ?~  p.sign  [~ this]
      =/  =serial  (slav %uv i.t.wire)
      ~&  watch-ack-deleting-observer+(~(got by observers) serial)
      [~ this(observers (~(del by observers) serial))]
    ::
        %kick
      =/  =serial  (slav %uv i.t.wire)
      =/  =observer:sur  (~(got by observers) serial)
      :_  this
      :_  ~
      :*  %pass
          wire
          %agent
          [our.bowl app.observer]
          %watch
          path.observer
      == 
    ::
        %fact
      =/  =serial  (slav %uv i.t.wire)
      =/  =observer:sur  (~(got by observers) serial)
      =/  tid  (scot %uv (sham eny.bowl))
      :_  this
      :~  :*  %pass
              [%thread-result i.t.wire tid ~]
              %agent
              [our.bowl %spider]
              %watch
              [%thread-result tid ~]
          ==  
          :*  %pass
              [%thread-start i.t.wire tid ~]
              %agent
              [our.bowl %spider]
              %poke
              %spider-start
              !>([~ `tid bec thread.observer (slop !>(~) q.cage.sign)])
      ==  ==
    ==
  ::
  ++  on-thread-result
    ?>  ?=([%thread-result @ @ ~] wire)
    ?+    -.sign  (on-agent:def wire sign)
      %kick       [~ this]
      %watch-ack  [~ this]
    ::
        %fact
      ?.  =(p.cage.sign %thread-fail)
        :_  this
        :_  ~
        :*  %pass
            wire
            %agent
            [our.bowl %spider]
            %leave
            ~
        == 
      =/  =serial  (slav %uv i.t.wire)
      =/  =observer:sur  (~(got by observers) serial)
      ~&  observer-failed+observer
      :_  this(observers (~(del by observers) serial))
      :~  :*  %pass
              [%observer i.t.wire ~]
              %agent
              [our.bowl app.observer]
              %leave
              ~
          ==
          :*  %pass
              wire
              %agent
              [our.bowl %spider]
              %leave
              ~
          == 
      ==
    ==
  ::
  ++  on-thread-start
    ?>  ?=([%thread-start @ @ ~] wire)
    ?.  ?=(%poke-ack -.sign)  (on-agent:def wire sign)
    ?~  p.sign  [~ this]
    =/  =serial  (slav %uv i.t.wire)
    =/  =observer:sur  (~(got by observers) serial)
    ~&  added-invalid-observer+observer
    :_  this(observers (~(del by observers) serial))
    :~  :*  %pass
            [%observer i.t.wire ~]
            %agent
            [our.bowl app.observer]
            %leave
            ~
        ==
        :*  %pass
            wire
            %agent
            [our.bowl app.observer]
            %leave
            ~
    ==  == 
  --
::
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card _this)
  :_  this
  ?+    wire  (on-arvo:def wire sign-arvo)
      [%warm-cache ~]
    ?.  warm-cache
      ~
    ?>  ?=([%clay %writ *] sign-arvo)
    =*  riot  p.sign-arvo
    ?~  riot
      =/  =rave:clay  [%next [%t da+now.bowl /mar]]
      [%pass /warm-cache %arvo %c %warp our.bowl desk `rave]~
    :-  =/  =rave:clay  [%next [%t q.p.u.riot /mar]]
        [%pass /warm-cache %arvo %c %warp our.bowl desk `rave]
    %+  turn  !<((list path) q.r.u.riot)
    |=  pax=path
    ^-  card
    =.  pax  (snip (slag 1 pax))
    =/  mark=@ta
      %+  roll  pax
      |=  [=term mark=term]
      ?:  ?=(%$ mark)
        term
      :((cury cat 3) mark '-' term)
    =/  =rave:clay  [%sing %b da+now.bowl /[mark]]
    [%pass [%mar mark ~] %arvo %c %warp our.bowl desk `rave]
  ::
      [%mar ^]
    ?.  warm-cache
      ~
    ?>  ?=([%clay %writ *] sign-arvo)
    =*  riot  p.sign-arvo
    =*  mark  t.wire
    ?~  riot
      ~
    =/  =rave:clay  [%next %b q.p.u.riot mark]
    [%pass wire %arvo %c %warp our.bowl desk `rave]~
  ::
      [%static-convert @ @ ~]
    =*  from  i.t.wire
    =*  to    i.t.t.wire
    ?.  (~(has in static-conversions) [from to])
      ~
    ?>  ?=([%clay %writ *] sign-arvo)
    =*  riot  p.sign-arvo
    ?~  riot
      ~
    =/  =rave:clay  [%next %f q.p.u.riot /[from]/[to]]
    [%pass wire %arvo %c %warp our.bowl desk `rave]~
  ==
::
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-fail   on-fail:def
--
