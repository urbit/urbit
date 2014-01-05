!:
::  /=main=/fun/reset/hoon
::
=>  .(-< `who=@p`-<)
|=  [est=time *]
|=  arg=*
=+  ^=  lok  ^-  case
    ?:  =(~ arg)  [%da est]
    ?>  =(~ +.arg)
    ((hard case) -.arg)
=+  cav=(scot (dime lok))
=+  top=`path`[(scot %p who) %arvo cav ~]
=+  pax=`path`(weld top `path`[%hoon ~])
:_  ~
:-  [%xx %vega pax]
^-  (list gift)
=+  ^=  vay  ^-  (list ,[p=@tas q=@tas])
    :~  [%$ %zuse]
        [%a %ames]
        [%b %batz]
        [%c %clay]
        [%d %dill]
        [%e %eyre]
    ==
%+  turn  vay
|=  [a=@tas b=@tas]
=+  pax=(weld top `path`[b ~])
=+  txt=((hard ,@) .^(%cx (weld pax `path`[%hoon ~])))
`gift`[%xx %veer a pax txt]
