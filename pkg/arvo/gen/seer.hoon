::  an example monadic scry
::
/+  seer
=,  seer
::::  /hoon/seer/gen
  ::
:-  %say
|=  *
:-  %noun
::
=/  z=(seer @t @)  [%| /foo |=(r=@t &+42)]
=/  rof=(sky @t)
  |=  =path
  ^-  (boon @t)
  ?+  path  %blok
    [%foo ~]     done+'foo-result'
    [%bar ~]     done+'whatever'
    [%number ~]  done+'12'
    [%'42' ~]    done+'yes'
    [%tomb ~]    %mute
  ==
:: ((consult @t @) gat rof z)
::
=/  do-add=(seer @t @)
  ;<  a=@t  rapt
