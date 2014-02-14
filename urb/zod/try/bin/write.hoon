!:  ::  /=try=/bin/env/hoon
!?      164
::::
=>  .(- `[who=@p how=path]`-)
=>  .(- [woh=(scot %p who) -])
=>  .(+ =>(+ ^/=main=/lib/pony))
::::
|=  [est=time eny=@]  
|=  [loc=[mih=span dez=span caz=span sup=path] ~]
::::
?.  =(woh mih.loc)
  ~|("you are not {(trip mih.loc)}!" !!)
=+  yaz=(zu ((hard ankh) .^(%cz mih.loc dez.loc caz.loc ~)))
%+  pomp  "appending to {(dart:ut loc)}..."
=<  work
|%
++  pend
  |=  den=@t
  ^-  [(list gift) _+>]
  =+  nyp=`soba`[*cart (turn (plan den) |=(p=miso [sup.loc p]))]
  :-  [[%ok dez.loc `nori`[%& ~ nyp]] ~]
  =.  yaz  (durn:yaz nyp)
  +>.$
::
++  plan
  |=  den=@t
  ^-  (list miso)
  =+  cur=q:ank:(deny:yaz sup.loc)
  ?~  cur
    [[%ins den] ~]
  ?^  q.u.cur
    [[%del q.u.cur] [%ins den] ~]
  [[%mut ((diff %c) q.u.cur (cat 3 q.u.cur den))] ~]
::
++  work  
  |-  ^-  bowl
  %+  (polo %text "> " ~ ~)  
    (plus alp)
  |=  [now=@da txt=tape]
  ^-  bowl
  =^  giv  +.^$  (pend (rap 3 (weld txt `tape`[`@`10 ~])))
  (pome giv ^$)
--
