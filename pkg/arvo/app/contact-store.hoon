:: contact-store [landscape]:
::
:: data store that holds group-based contact data
::
/+  *contact-json, default-agent, dbug
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state-zero
      state-one
      state-two
      state-three
  ==
::
+$  rolodex-0  (map path contacts-0)
+$  contacts-0  (map ship contact-0)
+$  avatar-0  [content-type=@t octs=[p=@ud q=@t]]
+$  contact-0
  $:  nickname=@t
      email=@t
      phone=@t
      website=@t
      notes=@t
      color=@ux
      avatar=(unit avatar-0)
  ==
::
+$  state-zero
  $:  %0
      rolodex=rolodex-0
  ==
+$  state-one
  $:  %1
      =rolodex
  ==
+$  state-two
  $:  %2
     =rolodex
  ==
+$  state-three
  $:  %3
     =rolodex
  ==
--
::
=|  state-three
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
    |=  old-vase=vase
    =/  old  !<(versioned-state old-vase)
    =|  cards=(list card)
    |-
    ?:  ?=(%3 -.old)
      [cards this(state old)]
    ?:  ?=(%2 -.old)
      %_    $
        -.old  %3
      ::
          rolodex.old
        =/  def
          (~(get by rolodex.old) /ship/~/default)
        ?~  def
          rolodex.old
        =.  rolodex.old
          (~(del by rolodex.old) /ship/~/default)
        =.  rolodex.old
          (~(put by rolodex.old) /~/default u.def)
        rolodex.old
      ==
    ?:  ?=(%1 -.old)
      =/  new-rolodex=^rolodex
        %-  malt
        %+  turn
          ~(tap by rolodex.old)
        |=  [=path =contacts]
        [ship+path contacts]
      %_    $
        old  [%2 new-rolodex]
        ::
          cards
        =/  paths
          %+  turn
            ~(val by sup.bol)
          |=([=ship =path] path)
        ?~  paths  cards
        :_  cards
        [%give %kick paths ~ ~]
      ==

    =/  new-rolodex=^rolodex
      %-  ~(run by rolodex.old)
      |=  cons=contacts-0
      ^-  contacts
      %-  ~(run by cons)
      |=  con=contact-0
      ^-  contact
      :*  nickname.con
          email.con
          phone.con
          website.con
          notes.con
          color.con
          ~
      ==
    $(old [%1 new-rolodex])
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?>  (team:title our.bowl ship.src.bowl)
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
    ?>  (team:title our.bowl ship.src.bowl)
    |^
    =/  cards=(list card)
      ?+    path  (on-watch:def path)
          [%all ~]      (give %contact-update !>([%initial rolodex]))
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
    |=  [prov=path =path]
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
::  ?>  (team:title our.bol ship.src.bol)
::  (poke-contact-action (json-to-action json))
::
++  poke-contact-action
  |=  action=contact-action
  ^-  (quip card _state)
  ?>  (team:title our.bol ship.src.bol)
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
  ?.  (~(has by contacts) ship)  [~ state]
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
