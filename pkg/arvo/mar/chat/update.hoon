::
::
/?  309
::
/-  chat, hall
/+  hall-json
::
|_  upd=update:chat
++  grow
  |%
  ++  json
    =,  enjs:format
    ^-  ^json
    %+  frond  %update
    %-  pairs
    :~
      ::
      ::  %inbox
      ?:  =(%inbox -.upd)
        ?>  ?=(%inbox -.upd)
        [%inbox (conf:enjs:hall-json con.upd)]
      ::
      ::  %message
      ?:  =(%message -.upd)
        ?>  ?=(%message -.upd)
        :-  %message
        %-  pairs
        :~
          [%circle (circ:enjs:hall-json cir.upd)]
          [%envelope (enve:enjs:hall-json env.upd)]
        ==
      ::
      ::  %messages
      ?:  =(%messages -.upd)
        ?>  ?=(%messages -.upd)
        :-  %messages
        %-  pairs
        :~
          [%circle (circ:enjs:hall-json cir.upd)]
          [%start (numb start.upd)]
          [%end (numb end.upd)]
          [%envelopes [%a (turn env.upd enve:enjs:hall-json)]]
        ==
      ::
      ::  %config
      ?:  =(%config -.upd)
        ?>  ?=(%config -.upd)
        :-  %config
        %-  pairs
        :~
          [%circle (circ:enjs:hall-json cir.upd)]
          [%config (conf:enjs:hall-json con.upd)]
        ==
      ::
      ::  %circles
      ?:  =(%circles -.upd)
        ?>  ?=(%circles -.upd)
        :-  %circles
        %-  pairs
        :~
        :-  %circles
        :-  %a
        %+  turn  ~(tap in cir.upd)
          |=  nom=name:hall
          [%s nom]
        ==
      ::
      ::  %peers
      ?:  =(%peers -.upd)
        ?>  ?=(%peers -.upd)
        :-  %peers
        %-  pairs
        :~
          [%circle (circ:enjs:hall-json cir.upd)]
          [%peers [%a (turn ~(tap in per.upd) ship:enjs:format)]]
        ==
      ::
      ::  %delete
      ?:  =(%delete -.upd)
        ?>  ?=(%delete -.upd)
        :-  %delete
        %-  pairs
        :~
          [%circle (circ:enjs:hall-json cir.upd)]
        ==
      ::
      ::  %noop
      [*@t *^json]
    ==
  --
::
++  grab
  |%
  ++  noun  update:chat
  --
--
