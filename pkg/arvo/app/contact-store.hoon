:: contact-store: data store that holds group-based contact data
::
/+  *contact-json, default-agent, dbug
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state-zero
  ==
::
+$  state-zero
  $:  %0
      =rolodex
  ==
+$  diff
  $%  [%contact-update contact-update]
  ==
--
::
=|  state-zero
=*  state  -
%-  agent:dbug
^-  agent:gall
=<
  |_  =bowl:gall
  +*  this       .
      contact-core  +>
      cc         ~(. contact-core bowl)
      def        ~(. (default-agent this %|) bowl)
  ::
  ++  on-init   on-init:def
  ++  on-save   !>(state)
  ++  on-load
    |=  old=vase
    `this(state !<(state-zero old))
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?>  (team:title our.bowl src.bowl)
    =^  cards  state
      ?+  mark  (on-poke:def mark vase)
        ::%json            (poke-json:cc !<(json vase))
        %contact-action  (poke-contact-action:cc !<(contact-action vase))
      ==
    [cards this]
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    ?>  (team:title our.bowl src.bowl)
    |^
    =/  cards=(list card)
      ?+    path  (on-watch:def path)
          [%all ~]      (give %contact-update !>([%rolodex rolodex]))
          [%updates ~]  ~
          [%contacts @ *]
        %+  give  %contact-update
        !>([%contacts t.path (~(got by rolodex) t.path)])
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
    ?+  path  (on-peek:def path)
        [%x %all ~]       ``noun+!>(rolodex)
        [%x %contacts *]
      ?~  t.t.path
        ~
      ``noun+!>((~(get by rolodex) t.t.path))
    ::
        [%x %contact *]
      ::  /:path/:ship
      =/  pax  `^path`(flop t.t.path)
      ?~  pax  ~
      =/  =ship  (slav %p i.pax)
      ?~  t.pax  ~
      =>  .(pax `(list @ta)`(flop t.pax))
      =/  contacts=(unit contacts)  (~(get by rolodex) pax)
      ?~  contacts
        ~
      ``noun+!>((~(get by u.contacts) ship))
    ==
  ::
  ++  on-agent  on-agent:def
  ++  on-arvo   on-arvo:def
  ++  on-fail   on-fail:def
  --
::
::
|_  bol=bowl:gall
::
::++  poke-json
::  |=  =json
::  ^-  (quip move _this)
::  ?>  (team:title our.bol src.bol)
::  (poke-contact-action (json-to-action json))
::
++  poke-contact-action
  |=  action=contact-action
  ^-  (quip card _state)
  ?>  (team:title our.bol src.bol)
  ?-  -.action
      %create   (handle-create +.action)
      %delete   (handle-delete +.action)
      %add      (handle-add +.action)
      %remove   (handle-remove +.action)
      %edit     (handle-edit +.action)
  ==
::
++  handle-create
  |=  =path
  ^-  (quip card _state)
  ?<  (~(has by rolodex) path)
  :-  (send-diff path [%create path])
  state(rolodex (~(put by rolodex) path *contacts))
::
++  handle-delete
  |=  =path
  ^-  (quip card _state)
  ?.  (~(has by rolodex) path)  [~ state]
  :-  (send-diff path [%delete path])
  state(rolodex (~(del by rolodex) path))
::
++  handle-add
  |=  [=path =ship =contact]
  ^-  (quip card _state)
  =/  contacts  (~(got by rolodex) path)
  ?<  (~(has by contacts) ship)
  =.  contacts  (~(put by contacts) ship contact)
  :-  (send-diff path [%add path ship contact])
  state(rolodex (~(put by rolodex) path contacts))
::
++  handle-remove
  |=  [=path =ship]
  ^-  (quip card _state)
  =/  contacts  (~(got by rolodex) path)
  ?>  (~(has by contacts) ship)
  =.  contacts  (~(del by contacts) ship)
  :-  (send-diff path [%remove path ship])
  state(rolodex (~(put by rolodex) path contacts))
::
++  handle-edit
  |=  [=path =ship =edit-field]
  ^-  (quip card _state)
  =/  contacts  (~(got by rolodex) path)
  =/  contact  (~(got by contacts) ship)
  =.  contact  (edit-contact contact edit-field)
  =.  contacts  (~(put by contacts) ship contact)
  :-  (send-diff path [%edit path ship edit-field])
  state(rolodex (~(put by rolodex) path contacts))
::
++  edit-contact
  |=  [con=contact edit=edit-field]
  ^-  contact
  ?-  -.edit
      %nickname  con(nickname nickname.edit)
      %email     con(email email.edit)
      %phone     con(phone phone.edit)
      %website   con(website website.edit)
      %notes     con(notes notes.edit)
      %color     con(color color.edit)
      %avatar    con(avatar avatar.edit)
  ==
::
++  send-diff
  |=  [pax=path upd=contact-update]
  ^-  (list card)
  :~  :*  
    %give  %fact
    ~[/all /updates [%contacts pax]]
    %contact-update  !>(upd)
  ==  ==
--
