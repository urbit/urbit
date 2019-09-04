|%
+$  move  [bone card]
+$  card
  $%  [%conf-mall wire dock dock]
      [%deal-mall wire sock internal-task:mall]
  ==
--
|_  [=bowl:gall ~]
++  this  .
++  poke-noun
  |=  arg=*
  ^-  (quip move _this)
  :_  this  :_  ~
  ?+  arg  ~|(%bad-arg !!)
    %conf  `move`[ost.bowl %conf-mall / [our.bowl %first] [our.bowl %home]]
    %poke  `move`[ost.bowl %deal-mall / [our.bowl our.bowl] %first %poke %noun !>(%hey)]
  ==
::
++  onto
  |=  [wire res=(each suss:mall tang)]
  ?:  ?=(%& -.res)
    ~&  %ontoad
    `this
  %-  (slog >'conf-failed'< p.res)
  `this
::
++  unto
  |=  [wire res=internal-gift:mall]
  ?-  -.res
      %diff           !!
      %quit           !!
      %reap           !!
      %http-response  !!
      %coup
    ?~  p.res
      ~&  %mall-coup-good
      `this
    %-  (slog >'mall-coup-failed'< u.p.res)
    `this
  ==
--
