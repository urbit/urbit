/-  *settings
/+  verb, dbug, default-agent, agentio
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state-0
      state-1
  ==
+$  state-0  [%0 settings=settings-0]
+$  state-1  [%1 =settings]
--
=|  state-1
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
      (put-entry:do %tutorial %seen b+|)
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
      %1  [~ this(state old)]
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
        %put-bucket  (put-bucket:do key.evt bucket.evt)
        %del-bucket  (del-bucket:do key.evt)
        %put-entry   (put-entry:do buc.evt key.evt val.evt)
        %del-entry   (del-entry:do buc.evt key.evt)
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
        [%bucket @ ~]
      =*  bucket-key  i.t.pax
      ?>  (~(has by settings) bucket-key)
      [~ this]
    ::
        [%entry @ @ ~]
      =*  bucket-key  i.t.pax
      =*  entry-key   i.t.t.pax
      =/  bucket  (~(got by settings) bucket-key)
      ?>  (~(has by bucket) entry-key)
      [~ this]
    ==
  ::
  ++  on-peek
    |=  pax=path
    ^-  (unit (unit cage))
    ?+  pax  (on-peek:def pax)
        [%x %all ~]
      ``settings-data+!>(all+settings)
    ::
        [%x %bucket @ ~]
      =*  buc  i.t.t.pax
      =/  bucket=(unit bucket)  (~(get by settings) buc)
      ?~  bucket  [~ ~]
      ``settings-data+!>(bucket+u.bucket)
    ::
        [%x %entry @ @ ~]
      =*  buc  i.t.t.pax
      =*  key  i.t.t.t.pax
      =/  =bucket  (fall (~(get by settings) buc) ~)
      =/  entry=(unit val)  (~(get by bucket) key)
      ?~  entry  [~ ~]
      ``settings-data+!>(entry+u.entry)
    ::
        [%x %has-bucket @ ~]
      =*  buc  i.t.t.pax
      =/  has-bucket=?  (~(has by settings) buc)
      ``noun+!>(has-bucket)
    ::
        [%x %has-entry @ @ ~]
      =*  buc  i.t.t.pax
      =*  key  i.t.t.t.pax
      =/  =bucket  (fall (~(get by settings) buc) ~)
      =/  has-entry=?  (~(has by bucket) key)
      ``noun+!>(has-entry)
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
  |=  [=key =bucket]
  ^-  (quip card _state)
  =/  pas=(list path)
    :~  /all
        /bucket/[key]
    ==
  :-  [(give-event pas %put-bucket key bucket)]~
  state(settings (~(put by settings) key bucket))
::
::  +del-bucket: delete a bucket from the top level settings map
::
++  del-bucket
  |=  =key
  ^-  (quip card _state)
  =/  pas=(list path)
    :~  /all
        /bucket/[key]
    ==
  :-  [(give-event pas %del-bucket key)]~
  state(settings (~(del by settings) key))
::
::  +put-entry: put an entry in a bucket, overwriting if it already exists
::              if bucket does not yet exist, create it
::
++  put-entry
  |=  [buc=key =key =val]
  ^-  (quip card _state)
  =/  pas=(list path)
    :~  /all
        /bucket/[buc]
        /entry/[buc]/[key]
    ==
  =/  =bucket  (fall (~(get by settings) buc) ~)
  =.  bucket   (~(put by bucket) key val)
  :-  [(give-event pas %put-entry buc key val)]~
  state(settings (~(put by settings) buc bucket))
::
::  +del-entry: delete an entry from a bucket, fail quietly if bucket does not
::              exist
::
++  del-entry
  |=  [buc=key =key]
  ^-  (quip card _state)
  =/  pas=(list path)
    :~  /all
        /bucket/[buc]
        /entry/[buc]/[key]
    ==
  =/  bucket=(unit bucket)  (~(get by settings) buc)
  ?~  bucket
    [~ state]
  =.  u.bucket   (~(del by u.bucket) key)
  :-  [(give-event pas %del-entry buc key)]~
  state(settings (~(put by settings) buc u.bucket))
::
++  give-event
  |=  [pas=(list path) evt=event]
  ^-  card
  [%give %fact pas %settings-event !>(evt)]
--
