!:
::  /=main=/bin/liquid/hoon
::
=>  .(- `[who=@p how=path]`-)
|=  [est=time eny=@uw]
|=  arg=*
=+  ^=  lok  ^-  case
    ?:  =(~ arg)  [%da est]
    ?>  =(~ +.arg)
    ((hard case) -.arg)
=+  cav=(scot (dime lok))
=+  top=`path`[(scot %p who) %arvo cav ~]
=+  pax=`path`(weld top `path`[%hoon ~])
~&  %liquid-start
=+  gen=(reck pax)
~&  %liquid-parsed
=+  ken=q:(~(mint ut %noun) %noun gen)
~&  %liquid-compiled
:_  ~  :_  ~
[%xx %sage [%dummy %pill ~] [ken 0]]
