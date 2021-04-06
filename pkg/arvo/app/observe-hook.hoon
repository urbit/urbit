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
+$  versioned-state
  $%  [%0 observers=(map serial observer:sur)]
      [%1 observers=(map serial observer:sur)]
      [%2 observers=(map serial observer:sur)]
      [%3 observers=(map serial observer:sur)]
      [%4 observers=(map serial observer:sur)]
      [%5 observers=(map serial observer:sur) warm-cache=_|]
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
=|  [%5 observers=(map serial observer:sur) warm-cache=_|]
=*  state  -
::
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init
  |^  ^-  (quip card _this)
  :_  this
  :~  (act [%watch %invite-store /invitatory/graph %invite-accepted-graph])
      (act [%watch %group-store /groups %group-on-leave])
      (act [%watch %group-store /groups %group-on-remove-member])
      (act [%watch %metadata-store /updates %md-on-add-group-feed])
      (act [%warm-cache-all ~])
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
      %5
    [cards this(state old-state)]
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
    %watch           (watch observer vals)
    %ignore          (ignore observer vals)
    %warm-cache-all  warm-cache-all
    %cool-cache-all  cool-cache-all
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
    =/  =mood:clay  [%t da+now.bowl /mar]
    [%pass /warm-cache %arvo %c %warp our.bowl %home `[%sing mood]]~
  ::
  ++  cool-cache-all
    ?.  warm-cache
      ~|('cannot cool down cache that is already cool' !!)
    [~ this(warm-cache %.n)]
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
              !>([~ `tid thread.observer (slop !>(~) q.cage.sign)])
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
  ?+    wire  (on-arvo:def wire sign-arvo)
      [%warm-cache ~]
    ?>  ?=([%clay %writ *] sign-arvo)
    =*  riot  p.sign-arvo
    ?~  riot
      ~|('should always have data in %/mar directory' !!)
    :_  this
    %+  turn  !<((list path) q.r.u.riot)
    |=  pax=path
    ^-  card
    =.  pax  `path`(snip (slag 1 `(list @)`pax))
    =/  mark=@ta
      %+  roll  pax
      |=  [=term mark=term]
      ?:  =(0 (lent (trip mark)))
        term
      %-  crip
      %-  zing
      :~  (trip mark)
          "-"
          (trip term)
      ==
    =/  =rave:clay  [%sing %b [%da now.bowl] [mark ~]]
    [%pass [%mar mark ~] %arvo %c %warp our.bowl [%home `rave]]
  ::
      [%mar ^]
    ?.  warm-cache
      [~ this]
    ?>  ?=([%clay %writ *] sign-arvo)
    =*  riot  p.sign-arvo
    =*  mark  t.wire
    ?~  riot
      ~|('mark build failed for {<mark>}' !!)
    :_  this
    =/  =rave:clay  [%next %b [%da now.bowl] mark]
    [%pass wire %arvo %c %warp our.bowl [%home `rave]]~
  ==
::
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-fail   on-fail:def
--
