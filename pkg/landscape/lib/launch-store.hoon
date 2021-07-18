/-  sur=launch-store
^?
=<  [sur .]
=,  sur
|%
++  enjs
  =,  enjs:format
  |%
  ++  update
    |=  upd=^update
    ^-  json
    |^  (frond %launch-update (pairs ~[(encode upd)]))
    ::
    ++  encode
      |=  upd=^update
      ^-  [cord json]
      ?-  -.upd
          %add
        :-  %add
        %-  pairs
        :~  [%name s+name.upd]
            [%tile (tile tile.upd)]
        ==
      ::
          %remove             [%remove s+name.upd]
          %change-order       [%'changeOrder' (terms tile-ordering.upd)]
          %change-first-time  [%'changeFirstTime' b+first-time.upd]
          %change-is-shown
        :-  %'changeIsShown'
        %-  pairs
        :~  [%name s+name.upd]
            [%'isShown' b+is-shown.upd]
        ==
      ::
          %initial
        :-  %initial
        %-  pairs
        :~  [%tiles (tiles tiles.upd)]
            [%'tileOrdering' (terms tile-ordering.upd)]
            [%'firstTime' b+first-time.upd]
        ==
      ::
          %keys  [%keys (terms ~(tap in keys.upd))]
      ==
    ::
    ++  tile
      |=  =^tile
      ^-  json
      %-  pairs
      :~  [%type (tile-type type.tile)]
          [%'isShown' b+is-shown.tile]
      ==
    ::
    ++  tiles
      |=  =^tiles
      ^-  json
      %-  pairs
      %+  turn  ~(tap by tiles)
      |=  [=term til=^tile]
      [term (tile til)] 
    ::
    ++  tile-type
      |=  type=^tile-type
      ^-  json
      ?-  -.type
          %basic
        %+  frond  %basic
        %-  pairs
        :~  [%title s+title.type]
            [%'iconUrl' s+icon-url.type]
            [%'linkedUrl' s+linked-url.type]
        ==
      ::
          %custom
        %+  frond  %custom
        %-  pairs
        :~  [%'linkedUrl' ?~(linked-url.type ~ s+u.linked-url.type)]
            [%'image' ?~(image.type ~ s+u.image.type)]
        ==
      ==
    ::
    ++  terms
      |=  terms=(list term)
      ^-  json
      [%a (turn terms |=(=term s+term))]
    --
  --
::
++  dejs
  =,  dejs:format
  |%
  ++  action
    |=  =json
    ^-  ^action
    |^  (decode json)
    ++  decode
      %-  of
      :~  [%add (ot [[%name (su sym)] [%tile tile] ~])]
          [%remove (su sym)]
          [%change-order (ar (su sym))]
          [%change-first-time bo]
          [%change-is-shown (ot [[%name (su sym)] [%'isShown' bo] ~])]
      ==
    --
  ::
  ++  tile
    |^
    %-  ot
    :~  [%type tile-type]
        [%'isShown' bo]
    ==
    ::
    ++  tile-type
      %-  of
      :~  [%basic basic]
          [%custom (ot [%'linkedUrl' (mu so)] [%'image' (mu so)] ~)]
      ==
    ::
    ++  basic
      %-  ot
      :~  [%title so]
          [%'iconUrl' so]
          [%'linkedUrl' so]
      ==
    --
  --
--
