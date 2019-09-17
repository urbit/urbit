/-  *permission-store
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
      ::  %message
      ?:  =(%create -.upd)
        ?>  ?=(%create -.upd)
        :-  %create
        %-  pairs
        :~  [%path (pa path.upd)]
            [%kind s+kind.permission.upd]
            [%who [%a (turn ~(tap in who.permission.upd) ship)]]
        ==
      ::
      ::  %delete
      ?:  =(%delete -.upd)
        ?>  ?=(%delete -.upd)
        [%delete (pa path.upd)]
      ::
      ::  %add
      ?:  =(%add -.upd)
        ?>  ?=(%add -.upd)
        :-  %add
        %-  pairs
        :~  [%path (pa path.upd)]
            [%who [%a (turn ~(tap in who.upd) ship)]]
        ==
      ::
      ::  %remove
      ?:  =(%remove -.upd)
        ?>  ?=(%remove -.upd)
        :-  %remove
        %-  pairs
        :~  [%path (pa path.upd)]
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
