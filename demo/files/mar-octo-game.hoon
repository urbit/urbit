::
::::  /hook/door/octo-game/mar
  ::
/?    310
!:
|%
++  rip9
  =+  b=0
  |=  a=@  ^-  (list ,@A)
  ?:  =(b 9)  ~
  [(cut 0 [b 1] a) $(b +(b))]
--
!:
|_  cod=[who=? box=@ boo=@]                             ::  game state
::
++  grab                                                ::  convert from
  |%
  ++  noun  ,[who=? box=@ boo=@]                        ::  clam from %noun
  --
++  grow
  |%
  ++  json  ^-  ^json
    =>  |=(bor=@ `^json`a/(turn (rip9 bor) |=(a=@A [%b =(1 a)])))
    (jobe who/s/?:(who.cod %x %o) box/(. box.cod) boo/(. boo.cod) ~)
  --
--
