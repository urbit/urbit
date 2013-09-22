!:
::  /=main=/bin/mast/hoon
::
=>  %=  .  +
      =>  +
      =>  ^/===/lib/pony
      |%
      ++  mind
        $:  bos=ship                                    ::  download master
            seg=(map ,[p=ship q=disc] dome)             ::  repositories
        ==
      --
    ==
=>  %=  .  - 
    :*  [who=`@p`-< how=`path`->]
        *mind
    ==
|=  [est=time eny=@uw]
|=  ~
=<  ahoy
|%  
  ++  ahoy                                              ::  introductory loop
    !!
  ++  apex
    !!
==

=+  hem=*mind
=.  bos.hem  (sein who)
=.  seg.hem  
  %+  ~(gas by seg.hem) 
  :~  [[bos %main] *dome]
      [[bos %arvo] *dome]
  ==
=+  ^=  hem  ^-  mind
    %*  .  *mind
      bos  (sein who)
      arv  *dome
      myn  *dome
    ==
^-  bowl
=<  (update est)
|%
++  update
  =+  guf=(~(tap by seg.hem) ~)
  %+  
  |=  now=@da
  %+
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
