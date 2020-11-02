::  thread-watch-hook:
::
::  helper that observes an app at a particular path and forwards all facts 
::  to a particular thread. kills the subscription if the thread crashes
::
/-  sur=thread-watch-hook
/+  default-agent, dbug
::
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state-0
  ==
::
+$  state-0   [%0 observers=(map time observer:sur)]
++  got-by-val
  |=  [a=(map time observer:sur) b=observer:sur]
  ^-  time
  %-  need
  %+  roll  ~(tap by a)
  |=  [[key=time val=observer:sur] output=(unit time)]
  ?:(=(val b) `key output)
--
::
%-  agent:dbug
=|  state-0
=*  state  -
::
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init  on-init:def
++  on-save   !>(state)
++  on-load
  |=  old-vase=vase
  ^-  (quip card _this)
  `this(state !<(state-0 old-vase))
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?>  (team:title our.bowl src.bowl)
  ?.  ?=(%thread-watch-action mark)
    (on-poke:def mark vase)
  =/  =action:sur  !<(action:sur vase)
  =*  observer  observer.action
  =/  vals  (silt ~(val by observers))
  ?-  -.action
      %watch
    ?:  ?|(=(app.observer %spider) =(app.observer %thread-watch-hook)) 
      ~|('we avoid infinite loops' !!)
    ?:  (~(has in vals) observer)
      ~|('duplicate observer' !!)
    :_  this(observers (~(put by observers) now.bowl observer))  
    :_  ~
    :*  %pass
        /observer/(scot %da now.bowl)
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
        /observer/(scot %da key)
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
  ~&  [wire sign]
  ?-    -.sign
      %kick
    ?+    wire  (on-agent:def wire sign)
        [%thread-result @ ~]  [~ this]
        [%observer @ ~]
      =/  =time  (slav %da i.t.wire)
      =/  =observer:sur  (~(got by observers) time)
      :_  this
      :_  ~
      :*  %pass
          wire
          %agent
          [our.bowl app.observer]
          %watch
          path.observer
      == 
    ==
  ::
      %poke-ack
    ?.  ?=([%thread-start @ @ ~] wire)
      (on-agent:def wire sign)
    ~&  sign
    ?~  p.sign  [~ this]
    =/  =time  (slav %da i.t.wire)
    =/  tid  (slav %uv i.t.t.wire)
    =/  =observer:sur  (~(got by observers) time)
    :_  this(observers (~(del by observers) time))
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
        == 
    ==
  ::
      %watch-ack
    ?~  p.sign  [~ this]
    ?+    wire  (on-agent:def wire sign)
        [%thread-result @ ~]  [~ this]
        [%observer @ ~]
      =/  =time  (slav %da i.t.wire)
      [~ this(observers (~(del by observers) time))]
    ==
  ::
      %fact
    ?+    wire  (on-agent:def wire sign)
        [%thread-result @ ~]
      ?.  =(p.cage.sign %thread-fail)
        [~ this]
      =/  =time  (slav %da i.t.wire)
      =/  =observer:sur  (~(got by observers) time)
      :_  this(observers (~(del by observers) time))
      :_  ~
      :*  %pass
          [%observer i.t.wire ~]
          %agent
          [our.bowl app.observer]
          %leave
          ~
      == 
    ::
        [%observer @ ~]
      =/  =time  (slav %da i.t.wire)
      =/  =observer:sur  (~(got by observers) time)
      =/  tid  (scot %uv (sham eny.bowl))
      :_  this
      :~  :*  %pass
              [%thread-start i.t.wire tid ~]
              %agent
              [our.bowl %spider]
              %poke
              %spider-start
              !>([~ `tid thread.observer (slop !>(cage.sign) !>(~))])
          ==
          :*  %pass
              [%thread-result tid ~]
              %agent
              [our.bowl %spider]
              %watch
              [%thread-result tid ~]
      ==  == 
    ==
  ==
::
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
