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
=|  [%2 observers=(map serial observer:sur)]
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
        !>  ^-  action:sur
        action
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
  ?:  ?=(%2 -.old-state)
    =.  cards
      :_  cards
      (act [%watch %group-store /groups %group-on-leave])
    [cards this(state old-state)]
  ?:  ?=(%1 -.old-state)
    =.  cards
      :_  cards
      (act [%watch %group-store /groups %group-on-leave])
    $(-.old-state %2)
  =.  cards
    :_  cards
    (act [%watch %group-store /groups %group-on-remove-member])
  $(-.old-state %1)
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
        !>  ^-  action:sur
        action
    ==
  --
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?>  (team:title our.bowl src.bowl)
  ?.  ?=(%observe-action mark)
    (on-poke:def mark vase)
  =/  =action:sur  !<(action:sur vase)
  =*  observer  observer.action
  =/  vals  (silt ~(val by observers))
  ?-  -.action
      %watch
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
      %ignore
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
  ==
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
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
