::
::
/?  309
::
/-  hall
/+  chat, hall-json
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
      ::  %config
      ?:  =(%config -.upd)
        ?>  ?=(%config -.upd)
        :-  %config
        %-  pairs
        :~
          [%circle (circ:enjs:hall-json cir.upd)]
          [%config (conf:enjs:hall-json con.upd)]
        ==
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
