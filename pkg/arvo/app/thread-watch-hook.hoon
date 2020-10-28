::  thread-watch-hook:
::
::  watch an app at a particular path and forward all facts to a
::  particular thread
::
/+  default-agent, dbug
::
|%

+$  card  card:agent:gall
+$  versioned-state
  $%  state-0
  ==
+$  observer  [app=term =path input=mark thread=term]
+$  state-0   [%0 observers=(map time observer)]
+$  action
  $%  [%watch =observer]
      [%ignore =observer]
  ==
::
++  got-by-val
  |=  [a=(map time observer) b=observer]
  ^-  time
  %-  need
  %+  roll  ~(tap by a)
  |=  [[key=time val=observer] output=(unit time)]
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
++  on-save   !>([%0 ~])
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
  =/  =action  !<(action vase)
  =/  vals  (silt ~(val by observers))
  ?-  -.action
      %watch
    ?:  ?|(=(app.action %spider) =(app.action %thread-watch-hook)) 
      ~|('we avoid infinite loops' !!)
    ?:  (~(has in vals) observer.action)
      ~|('duplicate observer' !!)
    :_  this(observers.state (~(put by observers) now.bowl observer.action))  
    :_  ~
    :*  %pass
        /observer/(scot %da now.bowl)
        %agent
        [our.bowl app.action]
        %watch
        path.action
    == 
  ::
      %ignore
    ?.  (~(has in vals) observer.action)
      ~|('cannot remove nonexistent observer' !!)
    =/  key  (got-by-val observers observer.action)
    :_  this(observers.state (~(del by observers) key))
    :_  ~
    :*  %pass
        /observer/(scot %da key)
        %agent
        [our.bowl app.action]
        %leave
        ~
    == 
  ==
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  ?+    -.sign  (on-agent:def wire sign)
      %kick
    ::  TODO: handle this by resubscribing... unless it's a
    ::  thread-result kick
    [~ this]
  ::
      %poke-ack
    ::  if thread start fails, delete observer!
    [~ this]
  ::
      %watch-ack
    ::  TODO: handle nack by deleting observer
    [~ this]
  ::
      %fact
    ?+    wire  (on-agent wire sign)
        [%thread-result @ ~]
      ::  TODO: process whether the thread succeeded or failed and
      ::  respond  accordingly
      ::  spider: if we get a %thread-fail on a %thread-result fact... delete the
      ::  observer
      [~ this]
    ::
        [%observer @ ~]
      ::  TODO: spin up a thread to process input
      [~ this]
    ==
  ==
::
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
