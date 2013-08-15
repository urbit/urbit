!:
::  /=main=/fun/reboot/hoon
::
|=  [who=seat est=time eny=@uw was=path]
|=  *
|=  arg=*
=+  ^=  lok  ^-  case
    ?:  =(~ arg)  [%da est]
    ?>  =(~ +.arg)
    ((hard case) -.arg)
=+  pre=`path`[(scot %p who) %arvo ~(rent co %% (dime lok)) ~]
=+  ^=  vay  ^-  (list ,[p=@tas q=@tas])
    :~  [%% %zuse]
        [%a %ames]
        [%b %behn]
        [%c %clay]
        [%d %dill]
        [%e %eyre]
    ==
:_  ~
%+  turn  vay
|=  [saw=@tas nam=@tas]
=+  pax=(weld pre `path`[nam ~])
=+  txt=.^(%cx (weld pax `path`[%hoon ~]))
[%xx [%veer saw pax txt]]
