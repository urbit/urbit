/-  launch
/+  default-agent, dbug
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  [%0 *]
      [%1 *]
      [%2 *]
      [%3 *]
      [%4 state-zero]
  ==
::
+$  state-zero
  $:  =tiles:launch
      =tile-ordering:launch
  ==
--
::
=|  [%4 state-zero]
=*  state  -
%-  agent:dbug
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init
  ^-  (quip card _this)
  =/  new-state  *state-zero
  =.  new-state
    %_  new-state
        tiles
      %-  ~(gas by *tiles:launch)
      %+  turn  `(list term)`[%chat %publish %link %dojo %weather %clock ~]
      |=  =term
      :-  term
      ^-  tile:launch
      ?+  term      [[%custom ~] %.y]
          %chat     [[%basic 'Chat' '/~landscape/img/Chat.png' '/~chat'] %.y]
          %links    [[%basic 'Links' '/~landscape/img/Links.png' '/~link'] %.y]
          %dojo     [[%basic 'Dojo' '/~landscape/img/Dojo.png' '/~dojo'] %.y]
          %publish
        [[%basic 'Publish' '/~landscape/img/Publish.png' '/~publish'] %.y]
      ==
    ==
  [~ this(state [%4 new-state])]
::
++  on-save  !>(state)
++  on-load
  |=  old=vase
  ^-  (quip card _this)
  =/  old-state  !<(versioned-state old)
  ?:  ?=(%4 -.old-state)
    [~ this(state old-state)]
  =/  new-state  *state-zero
  =.  new-state
    %_  new-state
        tiles
      %-  ~(gas by *tiles:launch)
      %+  turn  `(list term)`[%chat %publish %link %dojo %weather %clock ~]
      |=  =term
      :-  term
      ^-  tile:launch
      ?+  term      [[%custom ~] %.y]
          %chat     [[%basic 'Chat' '/~landscape/img/Chat.png' '/~chat'] %.y]
          %links    [[%basic 'Links' '/~landscape/img/Links.png' '/~link'] %.y]
          %dojo     [[%basic 'Dojo' '/~landscape/img/Dojo.png' '/~dojo'] %.y]
          %publish
        [[%basic 'Publish' '/~landscape/img/Publish.png' '/~publish'] %.y]
      ==
    ==
  :_  this(state [%4 new-state])
  :-  [%pass / %arvo %e %disconnect [~ /]]
  %+  turn  ~(tap by wex.bowl)
  |=  [[=wire =ship =term] *]
  ^-  card
  [%pass wire %agent [ship term] %leave ~]
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  |^
  =^  cards  state
    ?+  mark  (on-poke:def mark vase)
        %launch-action  (poke-action !<(action:launch vase))
    ==
  [cards this]
  ::
  ++  poke-action
    |=  =action:launch
    ^-  (quip card _state)
    ~&  action
    ?-  -.action
        %add
      ?<  (~(has by tiles) name.action)
      :-  (give [/all /keys ~] action)
      %_  state
          tiles          (~(put by tiles) name.action tile.action)
          tile-ordering  (snoc tile-ordering name.action)
      ==
    ::
        %remove
      :-  (give [/all /keys ~] action)
      %_  state
          tiles          (~(del by tiles) name.action)
          tile-ordering
        %+  skip  tile-ordering
        |=(=term =(term name.action))
      ==
    ::
        %change-order
      ?>  =(~(key by tiles) (silt tile-ordering.action))
      :-  (give [/all]~ action)
      state(tile-ordering tile-ordering.action)
    ::
        %change-is-shown
      =/  =tile:launch  (~(got by tiles) name.action)
      ?.  =(is-shown.tile is-shown.action)  [~ state]
      =.  is-shown.tile  is-shown.action
      :-  (give [/all]~ action)
      state(tiles (~(put by tiles) name.action tile))
    ==
  ::
  ++  give
    |=  [paths=(list path) =update:launch]
    ^-  (list card)
    [%give %fact paths [%launch-update !>(update)]]~
  --
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  |^
  ?>  (team:title our.bowl src.bowl)
  =/  cards=(list card)
    ?+  path  (on-watch:def path)
        [%all ~]   (give [%initial tiles tile-ordering])
        [%keys ~]  (give [%keys ~(key by tiles)])
    ==
  [cards this]
  ::
  ++  give
    |=  =update:launch
    ^-  (list card)
    [%give %fact ~ [%launch-update !>(update)]]~
  --
::
++  on-peek   on-peek:def
::
++  on-arvo
  |=  [wir=wire sin=sign-arvo]
  ^-  (quip card:agent:gall _this)
  ?:  ?=(%bound +<.sin)  [~ this]
  (on-arvo:def wir sin)
::
++  on-agent  on-agent:def
++  on-leave  on-leave:def
++  on-fail   on-fail:def
--
