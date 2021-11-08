:: contact-store [landscape]:
::
:: data store that holds individual contact data
::
/-  store=contact-store, *resource
/+  default-agent, dbug, *migrate, contact, verb
|%
+$  card  card:agent:gall
+$  state-4
  $:  %4
      =rolodex:store
      allowed-groups=(set resource)
      allowed-ships=(set ship)
      is-public=_|
  ==
+$  versioned-state
  $%  [%0 *]
      [%1 *]
      [%2 *]
      [%3 *]
      state-4
  ==
--
::
=|  state-4
=*  state  -
%-  agent:dbug
%+  verb  |
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
    con   ~(. contact bowl)
::
++  on-init
  =.  rolodex  (~(put by rolodex) our.bowl *contact:store)
  [~ this(state state)]
::
++  on-save   !>(state)
++  on-load
  |=  old-vase=vase
  ^-  (quip card _this)
  =/  old  !<(versioned-state old-vase)
  ?+    -.old
    =.  rolodex  (~(put by rolodex) our.bowl *contact:store)
    [~ this(state state)]
  ::
    %4  [~ this(state old)]
  ==
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  ?>  (team:title our.bowl src.bowl)
  |^
  =/  cards=(list card)
    ?+  path  (on-watch:def path)
      [%all ~]      (give [%initial rolodex is-public])
      [%updates ~]  ~
    ::
        [%our ~]
      %-  give
      :+  %add
        our.bowl
      =/  contact=(unit contact:store)  (~(get by rolodex) our.bowl)
      ?~  contact  *contact:store
      u.contact
    ==
  [cards this]
  ::
  ++  give
    |=  =update:store
    ^-  (list card)
    [%give %fact ~ [%contact-update-0 !>(update)]]~
  --
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?>  (team:title our.bowl src.bowl)
  |^
  =^  cards  state
    ?+  mark  (on-poke:def mark vase)
      %contact-update-0  (update !<(update:store vase))
      %import          (import q.vase)
    ==
  [cards this]
  ::
  ++  update
    |=  =update:store
    ^-  (quip card _state)
    |^
    ?-  -.update
      %initial     (handle-initial +.update)
      %add         (handle-add +.update)
      %remove      (handle-remove +.update)
      %edit        (handle-edit +.update)
      %allow       (handle-allow +.update)
      %disallow    (handle-disallow +.update)
      %set-public  (handle-set-public +.update)
    ==
    ::
    ++  handle-initial
      |=  [rolo=rolodex:store *]
      ^-  (quip card _state)
      =/  our-contact  (~(got by rolodex) our.bowl)
      =/  diff-rolo=rolodex:store
        %-  ~(gas by *rolodex:store)
        %+  skim  ~(tap in rolo)
        |=  [=ship =contact:store]
        ?~  local-con=(~(get by rolodex) ship)  %.y
        (gth last-updated.contact last-updated.u.local-con)
      =/  new-rolo=rolodex:store
        (~(uni by rolodex) diff-rolo)
      =.  new-rolo  (~(put by new-rolo) our.bowl our-contact)
      ?:  =(new-rolo rolodex)  `state
      :_  state(rolodex new-rolo)
      (send-diff [%initial new-rolo is-public] %.n)
    ::
    ++  handle-add
      |=  [=ship =contact:store]
      ^-  (quip card _state)
      ::  ensure difference
      =/  old=(unit contact:store)  (~(get by rolodex) ship)
      ?.  ?|  ?=(~ old)
              !=(contact(last-updated *@da) u.old(last-updated *@da))
          ==
        [~ state]
      ~|  "cannot add a data url to cover!"
      ?>  ?|  ?=(~ cover.contact)
              !=('data:' (cut 3 [0 5] u.cover.contact))
          ==
      ~|  "cannot add a data url to avatar!"
      ?>  ?|  ?=(~ avatar.contact)
              !=('data:' (cut 3 [0 5] u.avatar.contact))
          ==
      :-  (send-diff [%add ship contact] =(ship our.bowl))
      state(rolodex (~(put by rolodex) ship contact))
    ::
    ++  handle-remove
      |=  =ship
      ^-  (quip card _state)
      ?.  (~(has by rolodex) ship)
        [~ state]
      :-  (send-diff [%remove ship] =(ship our.bowl))
      ?:  =(ship our.bowl)
        state(rolodex (~(put by rolodex) our.bowl *contact:store))
      state(rolodex (~(del by rolodex) ship))
    ::
    ++  handle-edit
      |=  [=ship =edit-field:store timestamp=@da]
      |^
      ^-  (quip card _state)
      =/  old  (fall (~(get by rolodex) ship) *contact:store)
      ?:  (lte timestamp last-updated.old)
        [~ state]
      =/  contact  (edit-contact old edit-field)
      ?:  =(old contact)
        [~ state]
      ~|  "cannot add a data url to cover!"
      ?>  ?|  ?=(~ cover.contact)
              !=('data:' (cut 3 [0 5] u.cover.contact))
          ==
      ~|  "cannot add a data url to avatar!"
      ?>  ?|  ?=(~ avatar.contact)
              !=('data:' (cut 3 [0 5] u.avatar.contact))
          ==
      =.  last-updated.contact  timestamp
      :-  (send-diff [%edit ship edit-field timestamp] =(ship our.bowl))
      state(rolodex (~(put by rolodex) ship contact))
      ::
      ++  edit-contact
        |=  [=contact:store edit=edit-field:store]
        ^-  contact:store
        ?-  -.edit
          %nickname  contact(nickname nickname.edit)
          %bio       contact(bio bio.edit)
          %status    contact(status status.edit)
          %color     contact(color color.edit)
          %avatar    contact(avatar avatar.edit)
          %cover     contact(cover cover.edit)
        ::
            %add-group
          contact(groups (~(put in groups.contact) resource.edit))
        ::
            %remove-group
          contact(groups (~(del in groups.contact) resource.edit))
        ==
      --
    ::
    ++  handle-allow
      |=  =beings:store
      ^-  (quip card _state)
      :-  (send-diff [%allow beings] %.n)
      ?-  -.beings
        %group  state(allowed-groups (~(put in allowed-groups) resource.beings))
        %ships  state(allowed-ships (~(uni in allowed-ships) ships.beings))
      ==
    ::
    ++  handle-disallow
      |=  =beings:store
      ^-  (quip card _state)
      :-  (send-diff [%disallow beings] %.y)
      ?-  -.beings
        %group  state(allowed-groups (~(del in allowed-groups) resource.beings))
        %ships  state(allowed-ships (~(dif in allowed-ships) ships.beings))
      ==
    ::
    ++  handle-set-public
      |=  public=?
      ^-  (quip card _state)
      :_  state(is-public public)
      (send-diff [%set-public public] %.n)
    ::
    ++  send-diff
      |=  [=update:store our=?]
      ^-  (list card)
      =/  paths=(list path)
        ?:  our
          [/updates /our /all ~]
        [/updates /all ~]
      [%give %fact paths %contact-update-0 !>(update)]~
    --
  ::
  ++  import
    |=  arc=*
    ^-  (quip card _state)
    ::  note: we are purposefully wiping all state before state-4
    [~ *state-4]
  --
::
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  ?+    path  (on-peek:def path)
    [%x %all ~]     ``noun+!>(`rolodex:store`rolodex)
  ::
      [%x %contact @ ~]
    =/  =ship  (slav %p i.t.t.path)
    =/  contact=(unit contact:store)  (~(get by rolodex) ship)
    ?~  contact  [~ ~]
    :-  ~  :-  ~  :-  %contact-update-0
    !>  ^-  update:store
    [%add ship u.contact]
  ::
      [%x %allowed-ship @ ~]
    =/  =ship  (slav %p i.t.t.path)
    ``noun+!>(`?`(~(has in allowed-ships) ship))
  ::
      [%x %is-public ~]
    ``noun+!>(`?`is-public)
  ::
      [%x %allowed-groups ~]
    ``noun+!>(`(set resource)`allowed-groups)
  ::
      [%x %is-allowed @ @ @ @ ~]
    =/  is-personal  =(i.t.t.t.t.t.path 'true')
    =/  =resource
      ?:  is-personal
        [our.bowl %'']
      [(slav %p i.t.t.path) i.t.t.t.path]
    =/  =ship  (slav %p i.t.t.t.t.path)
    ``json+!>(`json`b+(is-allowed:con resource ship))
  ==
::
++  on-leave  on-leave:def
++  on-agent  on-agent:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
