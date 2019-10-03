/+  *permission-json
|_  upd=permission-update
::
++  grow
  |%
  ++  json
    =,  enjs:format
    ^-  ^json
    %+  frond  %permission-update
    %-  pairs
    :~
      ::
      ::  %create
      ?:  =(%create -.upd)
        ?>  ?=(%create -.upd)
        :-  %create
        %-  pairs
        :~  [%path (path path.upd)]
            [%kind s+kind.permission.upd]
            [%who [%a (turn ~(tap in who.permission.upd) ship)]]
        ==
      ::
      ::  %delete
      ?:  =(%delete -.upd)
        ?>  ?=(%delete -.upd)
        [%delete (path path.upd)]
      ::
      ::  %add
      ?:  =(%add -.upd)
        ?>  ?=(%add -.upd)
        :-  %add
        %-  pairs
        :~  [%path (path path.upd)]
            [%who [%a (turn ~(tap in who.upd) ship)]]
        ==
      ::
      ::  %remove
      ?:  =(%remove -.upd)
        ?>  ?=(%remove -.upd)
        :-  %remove
        %-  pairs
        :~  [%path (path path.upd)]
            [%who [%a (turn ~(tap in who.upd) ship)]]
        ==
      ::
      ::  %noop
      [*@t *^json]
    ==
  --
::
++  grab
  |%
  ++  noun  permission-update
  --
::
--
