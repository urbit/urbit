!:
::  /=main=/bin/mast/hoon
::
=>  %=    .    
        -  [who=`@p`-< how=`path`->]
        +
      =>  +
      =>  ^/===/lib/pony
      |%                    ::  ...types and stuff...
      ==
    ==
|=  [est=time eny=@uw]
|=  arg=~
^-  bowl
=+  :*  bos=(sein who)
        nyd=(~(gas by *(map ,@tas)) `(list ,[p=@tas q=@ud]) ~[fool/0])
    ==
=<  apex
|%
++  apex
  ^-  bowl
  ?~  nyd  done
  :-  ~  :-  ~
  :-  bite
  |=  [now=@da how=path wat=note]
  ?>  &(?=([%es *] wat) ?=([%pull @ ~] how))
  (bice i.t.how p.wat)
::
++  bice
  |=  [syd=@tas rot=riot]
  %=    apex
      nyd
    ?~  rot
      ~&  [%bice-done syd]
      (~(del by nyd) syd)
    ~&  [%bice-more syd rot]
    =+  saq=(need (~(get by nud) syd))
    ?>  =(u.rot [%w [%ud +(saq)] syd])
    (~(put by nyd) +(saq))
  ==
::
++  bite
  ^-  (list slip)
  :-  [/prompt [%up %none "<who>: installing from <bos> at <est>...]
  =+  dyn=(~(tap by nyd) ~)
  %+  turn  dyn
  |=  [a=@tas b=@ud]
  `slip`[[%pull a ~] %es %bos a [%| [%ud a] [%da est]]]
::
++  done
  ^-  bowl
  :_  ~
  :~  [%la %leaf "done"]
  ==
==
