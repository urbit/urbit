/-  *settings
/+  verb, dbug, default-agent, agentio
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state-0
      state-1
      state-2
  ==
+$  state-0  [%0 settings=settings-0]
+$  state-1  [%1 settings=settings-1]
+$  state-2  [%2 =settings] 
--
=|  state-2
=*  state  -
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
=<
  |_  bol=bowl:gall
  +*  this  .
      do    ~(. +> bol)
      def   ~(. (default-agent this %|) bol)
      io    ~(. agentio bol)
  ::
  ++  on-init
    ^-  (quip card _this)
    =^  cards  state
      (put-entry:do q.byk.bol %tutorial %seen b+|)
    [cards this]
  ::
  ++  on-save  !>(state)
  ::
  ++  on-load
    |=  =old=vase
    ^-  (quip card _this)
    =/  old  !<(versioned-state old-vase)
    |-
    ?-  -.old
      %0  $(old [%1 +.old])
      %1  $(old [%2 (~(put by *^settings) q.byk.bol settings.old)])
      %2  `this(state old)
    ==
  ::
  ++  on-poke
    |=  [mar=mark vas=vase]
    ^-  (quip card _this)
    ?>  (team:title our.bol src.bol)
    ?.  ?=(%settings-event mar)
      (on-poke:def mar vas)
    =/  evt=event  !<(event vas)
    =^  cards  state
      ?-  -.evt
        %put-bucket  (put-bucket:do [desk key bucket]:evt)
        %del-bucket  (del-bucket:do [desk key]:evt)
        %put-entry   (put-entry:do [desk buc key val]:evt)
        %del-entry   (del-entry:do [desk buc key]:evt)
      ==
    [cards this]
  ::
  ++  on-watch
    |=  pax=path
    ^-  (quip card _this)
    ?>  (team:title our.bol src.bol)
    ?+  pax  (on-watch:def pax)
        [%all ~]
      [~ this]
    ::
        [%desk @ ~]
      =*  desk  i.t.pax
      ?>  (~(has by settings) desk)
      [~ this]
    ::
        [%bucket @ @ ~]
      =*  desk        i.t.pax
      =*  bucket-key  i.t.t.pax
      ?>  (~(has bi settings) desk bucket-key)
      [~ this]
    ::
        [%entry @ @ @ ~]
      =*  desk        i.t.pax
      =*  bucket-key  i.t.t.pax
      =*  entry-key   i.t.t.t.pax
      =/  bucket  (~(got bi settings) desk bucket-key)
      ?>  (~(has by bucket) entry-key)
      [~ this]
    ==
  ::
  ++  on-peek
    |=  pax=path
    ^-  (unit (unit cage))
    ?+  pax  (on-peek:def pax)
        [%x %all ~]
      ``settings-data+!>(`data`all+settings)
    ::
        [%x %desk @ ~]
      =*  desk  i.t.t.pax
      ?~  desk-settings=(~(get by settings) desk)  [~ ~]
      ``settings-data+!>(desk+u.desk-settings)
    ::
        [%x %bucket @ @ ~]
      =*  desk  i.t.t.pax
      =*  buc   i.t.t.t.pax
      =/  bucket=(unit bucket)  (~(get bi settings) desk buc)
      ?~  bucket  [~ ~]
      ``settings-data+!>(`data`bucket+u.bucket)
    ::
        [%x %entry @ @ @ ~]
      =*  desk  i.t.t.pax
      =*  buc   i.t.t.t.pax
      =*  key   i.t.t.t.t.pax
      =/  =bucket  (~(gut bi settings) desk buc *bucket)
      =/  entry=(unit val)  (~(get by bucket) key)
      ?~  entry  [~ ~]
      ``settings-data+!>(`data`entry+u.entry)
    ::
        [%x %has-bucket @ @ ~]
      =/  desk  i.t.t.pax
      =/  buc   i.t.t.t.pax
      =/  has-bucket=?  (~(has bi settings) desk buc)
      ``noun+!>(`?`has-bucket)
    ::
        [%x %has-entry @ @ @ ~]
      =*  desk  i.t.t.pax
      =*  buc   i.t.t.t.pax
      =*  key   i.t.t.t.t.pax
      =/  =bucket  (~(gut bi settings) desk buc *bucket)
      =/  has-entry=?  (~(has by bucket) key)
      ``noun+!>(`?`has-entry)
    ==
  ::
  ++  on-agent  on-agent:def
  ++  on-leave  on-leave:def
  ++  on-arvo   on-arvo:def
  ++  on-fail   on-fail:def
  --
::
|_  bol=bowl:gall
::
::  +put-bucket: put a bucket in the top level settings map, overwriting if it
::               already exists
::
++  put-bucket
  |=  [=desk =key =bucket]
  ^-  (quip card _state)
  =/  pas=(list path)
    :~  /all
        /desk/[desk]
        /bucket/[desk]/[key]
    ==
  :-  [(give-event pas %put-bucket desk key bucket)]~
  state(settings (~(put bi settings) desk key bucket))
::
::  +del-bucket: delete a bucket from the top level settings map
::
++  del-bucket
  |=  [=desk =key]
  ^-  (quip card _state)
  =/  pas=(list path)
    :~  /all
        /desk/[desk]
        /bucket/[key]
    ==
  :-  [(give-event pas %del-bucket desk key)]~
  state(settings (~(del bi settings) desk key))
::
::  +put-entry: put an entry in a bucket, overwriting if it already exists
::              if bucket does not yet exist, create it
::
++  put-entry
  |=  [=desk buc=key =key =val]
  ^-  (quip card _state)
  =/  pas=(list path)
    :~  /all
        /desk/[desk]
        /bucket/[desk]/[buc]
        /entry/[desk]/[buc]/[key]
    ==
  =/  =bucket  (~(put by (~(gut bi settings) desk buc *bucket)) key val)
  :-  [(give-event pas %put-entry desk buc key val)]~
  state(settings (~(put bi settings) desk buc bucket))
::
::  +del-entry: delete an entry from a bucket, fail quietly if bucket does not
::              exist
::
++  del-entry
  |=  [=desk buc=key =key]
  ^-  (quip card _state)
  =/  pas=(list path)
    :~  /all
        /desk/[desk]
        /bucket/[desk]/[buc]
        /entry/[desk]/[buc]/[key]
    ==
  =/  bucket=(unit bucket)  (~(get bi settings) desk buc)
  ?~  bucket
    [~ state]
  =.  u.bucket   (~(del by u.bucket) key)
  :-  [(give-event pas %del-entry desk buc key)]~
  state(settings (~(put bi settings) desk buc u.bucket))
::
++  give-event
  |=  [pas=(list path) evt=event]
  ^-  card
  [%give %fact pas %settings-event !>(evt)]
--
