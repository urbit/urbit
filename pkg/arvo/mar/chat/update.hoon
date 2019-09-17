/+  *chat-json
|_  upd=chat-update
++  grow
  |%
  ++  json
    =,  enjs:format
    ^-  ^json
    %+  frond  %chat-update
    %-  pairs
    :~
      ::
      ::  %message
      ?:  =(%message -.upd)
        ?>  ?=(%message -.upd)
        :-  %message
        %-  pairs
        :~  [%path (pa path.upd)]
            [%envelope (enve envelope.upd)]
        ==
      ::
      ::  %read
      ?:  =(%read -.upd)
        ?>  ?=(%read -.upd)
        :-  %read
        %-  pairs
        :~  [%path (pa path.upd)]
            [%read (numb read.upd)]
        ==
      ::
      ::  %create
      ?:  =(%create -.upd)
        ?>  ?=(%create -.upd)
        :-  %create
        %-  pairs
        :~  [%path (pa path.upd)]
            [%owner (ship:enjs:format owner.upd)]
        ==
      ::
      ::  %delete
      ?:  =(%delete -.upd)
        ?>  ?=(%delete -.upd)
        [%delete (pairs [%path (pa path.upd)]~)]
      ::
      ::  %noop
      [*@t *^json]
    ==
  --
::
++  grab
  |%
  ++  noun  chat-update
  --
::
--
