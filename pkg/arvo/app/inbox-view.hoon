/-  *inbox
/+  *inbox-json
::
|%
+$  state
  $%  [%0 inbox=(set path)]
  ==
::
+$  move  [bone card]
::
+$  card
  $%  [%peer wire dock path]
      [%quit ~]
      [%pull wire dock ~]
      [%diff diff]
  ==
::
+$  diff
  $%  [%inbox-update inbox-update]
      [%inbox-initial inbox-initial]
      [%json json]
  ==
--
::
|_  [bol=bowl:gall state]
::
++  this  .
::
++  prep
  |=  old=(unit state)
  ^-  (quip move _this)
  ?~  old
    :_  this
    [ost.bol %peer /keys [our.bol %inbox] /keys]~
  [~ this(+<+ u.old)]
::
++  diff-inbox-update
  |=  [wir=wire diff=inbox-update]
  ^-  (quip move _this)
  ?-  -.diff
      %keys
    ?~  inbox
      :_  this(inbox keys.diff)
      ^-  (list move)
      %+  turn  ~(tap in keys.diff)
      |=  =path
      [ost.bol %peer [%mailbox path] [our.bol %inbox] [%mailbox path]]
    [~ this(inbox keys.diff)]
  ::
      %create
    :_  this
    :-  [ost.bol %peer [%mailbox path.diff] [our.bol %inbox] [%mailbox path.diff]]
    (send-inbox-update diff)
  ::
      %delete
    :_  this
    :-  [ost.bol %pull [%mailbox path.diff] [our.bol %inbox] ~]
    (send-inbox-update diff)
  ::
      %read
    [(send-inbox-update diff) this]
  ::
      %message
    [(send-inbox-update diff) this]
  ::
  ==
::
++  peer-primary
  |=  wir=wire
  ^-  (quip move _this)
  ?.  =(src.bol our.bol)
    [[ost.bol %quit ~]~ this]
  :_  this
  [ost.bol %diff %json (inbox-to-json (inbox-scry /all))]~
::
++  send-inbox-update
  |=  upd=inbox-update
  ^-  (list move)
  %+  turn  (prey:pubsub:userlib /primary bol)
  |=  [=bone *]
  [bone %diff %inbox-update upd]
::
++  quit
  |=  wir=wire
  ^-  (quip move _this)
  ?~  wir
    (mean [leaf+"invalid wire for diff: {(spud wir)}"]~)
  [~ this]
::
++  inbox-scry
  |=  pax=path
  ^-  inbox-initial
  =.  pax  ;:  weld
    `path`/=inbox/(scot %da now.bol)
    pax
    `path`/noun
  ==
  (need .^((unit inbox-initial) %gx pax))
::
--
