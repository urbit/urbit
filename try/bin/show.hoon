!:
::  /=try=/bin/revert/hoon
::  Call with desk & change to revert
::
=>  .(-< `who=@p`-<)
=>  .(+ =>(+ ^/=/main/=/lib/pony))
|=  [est=time eny=@uw]
|=  $:  des=span 
        cas=@ud
        ~
    ==
=+  dom=((hard dome) .^(%cv /=/[des]/=))                ::  should be instant
::%-  (posh (add ~s1 est))
::|=  tim=@da
=+  tim=est
^-  bowl
:_  ~
^-  (list gift)
=+  ran=((hard rang) .^(%cu /(scot %p who)/main/(scot %da tim)))
=+  hed=(~(got by hut.ran) (~(got by hit.dom) cas))
?~  p.hed  !!
=+  par=(~(got by hut.ran) i.p.hed)
=+  ^=  dif
    %-  %~  zerg  ze
        :+  est  dom  ran
    [par hed]
=+  ^=  sla
    |=  pat=path
    %+  roll  %+  turn  pat  |=  t=@t  (cat 3 '/' t)
    |=  [t=@t a=@t]
    (cat 3 a t)
%-  |=  lin=(list ,@t)
    %+  turn  lin
    |=  lan=@t  ^-  gift
    [%la %leaf (trip lan)]
%+  roll  (~(tap by dif) ~)
|=  [[pat=path mis=miso] lin=(list ,@t)]
?-  -.mis
  %del  [(cat 3 '%del ' (sla pat)) lin]
  %ins  [(cat 3 '%ins ' (sla pat)) lin]
  %mut  ?>  ?=(%c -.q.p.mis)
        =+  ^=  con
            %-  lore
            %-  (hard ,@)
            %-  %~  zaul  ze
                :+  est  dom  ran
            (~(got by q.par) pat)
        %+  welp  lin
        %+  welp  [(cat 3 '%mut ' (sla pat)) ~]
        ^-  (list ,@t)
        =<  q
        %+  roll  ((hard (urge ,@t)) p.q.p.mis)
        |=  [p=(unce ,@t) [q=(list ,@t) r=@ud]]
        ?-  -.p
          %&  [q (add r p.p)]
          %|  =+  old=((hard (list ,@t)) p.p)
              =+  new=((hard (list ,@t)) q.p)
              =+  ^=  pre
                  ?:  =(r 0)  ~
                  (slag (dec r) con)
              :_  (add r (lent p.p))
              ^-  (list ,@t)
              %+  welp  q
              %+  welp  :_  ~  (cat 3 '%chunk ' (scot %u r))
              %+  welp  ?~  pre  ~  :_  ~  (cat 3 '||  ' i.pre)
              %+  welp  %-  flop  %+  turn  old  |=  t=@t  (cat 3 '-<  ' t)
              %-  flop  %+  turn  new  |=  t=@t  (cat 3 '+>  ' t)
        ==
==
::`gift`[%og des [let hit hut lat]]
