::  permission-store: track black- and whitelists of ships
::
/-  *permission-store
/+  default-agent, verb, dbug
::
|%
+$  card  card:agent:gall
::
+$  versioned-state
  $%  state-zero
  ==
::
+$  state-zero
  $:  %0
      permissions=permission-map
  ==
--
=|  state-zero
=*  state  -
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
=<
  |_  =bowl:gall
  +*  this             .
      permission-core  +>
      pc               ~(. permission-core bowl)
      def              ~(. (default-agent this %|) bowl)
  ::
  ++  on-init  on-init:def
  ++  on-save  !>(state)
  ++  on-load
    |=  old=vase
    `this(state !<(state-zero old))
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?>  (team:title our.bowl src.bowl)
    =^  cards  state
      ?:  ?=(%permission-action mark)
        (poke-permission-action:pc !<(permission-action vase))
      (on-poke:def mark vase)
    [cards this]
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    ?>  (team:title our.bowl src.bowl)
    |^
    =/  cards=(list card)
      ?+  path  (on-watch:def path)
          [%all ~]            (give %permission-initial !>(permissions))
          [%updates ~]        ~
          [%permission @ *]
        =/  =vase  !>([%create t.path (~(got by permissions) t.path)])
        (give %permission-update vase)
      ==
    [cards this]
    ::
    ++  give
      |=  =cage
      ^-  (list card)
      [%give %fact ~ cage]~
    --
  ::
  ++  on-leave  on-leave:def
  ++  on-peek
    |=  =path
    ^-  (unit (unit cage))
    ?+    path  (on-peek:def path)
        [%x %keys ~]         ``noun+!>(~(key by permissions))
        [%x %permission *]
      ?~  t.t.path  ~
      ``noun+!>((~(get by permissions) t.t.path))
    ::
        [%x %permitted @ *]
      ?~  t.t.t.path  ~
      =/  pem  (~(get by permissions) t.t.t.path)
      ?~  pem  ~
      =/  who  (slav %p i.t.t.path)
      =/  has  (~(has in who.u.pem) who)
      ``noun+!>(?-(kind.u.pem %black !has, %white has))
    ==
  ++  on-agent  on-agent:def
  ++  on-arvo   on-arvo:def
  ++  on-fail   on-fail:def
  --
::
|_  bol=bowl:gall
::
++  poke-permission-action
  |=  action=permission-action
  ^-  (quip card _state)
  ?>  (team:title our.bol src.bol)
  ?-  -.action
      %add     (handle-add action)
      %remove  (handle-remove action)
      %create  (handle-create action)
      %delete  (handle-delete action)
      %allow   (handle-allow action)
      %deny    (handle-deny action)
  ==
::
++  handle-add
  |=  act=permission-action
  ^-  (quip card _state)
  ?>  ?=(%add -.act)
  ?~  path.act
    [~ state]
  ::  TODO: calculate diff
  ::  =+  new=(~(dif in who.what.action) who.u.pem)
  ::  ?~(new ~ `what.action(who new))
  ?.  (~(has by permissions) path.act)
    [~ state]
  :-  (send-diff path.act act)
  =/  perm  (~(got by permissions) path.act)
  =.  who.perm  (~(uni in who.perm) who.act)
  state(permissions (~(put by permissions) path.act perm))
::
++  handle-remove
  |=  act=permission-action
  ^-  (quip card _state)
  ?>  ?=(%remove -.act)
  ?~  path.act
    [~ state]
  ?.  (~(has by permissions) path.act)
    [~ state]
  =/  perm  (~(got by permissions) path.act)
  =.  who.perm  (~(dif in who.perm) who.act)
  ::  TODO: calculate diff
  ::  =+  new=(~(int in who.what.action) who.u.pem)
  ::  ?~(new ~ `what.action(who new))
  :-  (send-diff path.act act)
  state(permissions (~(put by permissions) path.act perm))
::
++  handle-create
  |=  act=permission-action
  ^-  (quip card _state)
  ?>  ?=(%create -.act)
  ?~  path.act
    [~ state]
  ?:  (~(has by permissions) path.act)
    [~ state]
  :: TODO: calculate diff
  :-  (send-diff path.act act)
  state(permissions (~(put by permissions) path.act permission.act))
::
++  handle-delete
  |=  act=permission-action
  ^-  (quip card _state)
  ?>  ?=(%delete -.act)
  ?~  path.act
    [~ state]
  ?.  (~(has by permissions) path.act)
    [~ state]
  :-  (send-diff path.act act)
  state(permissions (~(del by permissions) path.act))
::
++  handle-allow
  |=  act=permission-action
  ^-  (quip card _state)
  ?>  ?=(%allow -.act)
  ?~  path.act
    [~ state]
  =/  perm  (~(get by permissions) path.act)
  ?~  perm
    [~ state]
  ?:  =(kind.u.perm %white)
    (handle-add [%add +.act])
  (handle-remove [%remove +.act])
::
++  handle-deny
  |=  act=permission-action
  ^-  (quip card _state)
  ?>  ?=(%deny -.act)
  ?~  path.act
    [~ state]
  =/  perm  (~(get by permissions) path.act)
  ?~  perm
    [~ state]
  ?:  =(kind.u.perm %black)
    (handle-add [%add +.act])
  (handle-remove [%remove +.act])
::
++  update-subscribers
  |=  [pax=path upd=permission-update]
  ^-  (list card)
  [%give %fact ~[pax] %permission-update !>(upd)]~
::
++  send-diff
  |=  [pax=path upd=permission-update]
  ^-  (list card)
  %-  zing
  :~  (update-subscribers /all upd)
      (update-subscribers /updates upd)
      (update-subscribers [%permission pax] upd)
  ==
--
