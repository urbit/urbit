::
::::  /hoon/ask/app
  ::
/?    314
/+    sole
[. sole]
|%
  ++  card
    $%  {$diff $sole-effect sole-effect}
    ==
--
!:
|_  {bow/bowl adr/(set cord) sos/(map bone sole-share)}
++  peer-sole
  |=  path
  ^-  (quip {bone card} +>)
  ~|  [%not-in-whitelist src.bow]
  ?>  (~(has in (sa (limo ~zod our.bow ~talsur-todres ~))) src.bow)
  :_  +>.$(sos (~(put by sos) ost.bow *sole-share))
  =-  [(effect %mor pro+[& %$ "<listening>"] -)]~
  =+  all=adrs
  [tan+(turn all message) (turn all put-mail)]
::
++  adrs  (sort (~(tap by adr)) aor)
++  effect  |=(fec/sole-effect [ost.bow %diff %sole-effect fec])
++  message  |=(ask/@t leaf+"ask: {(trip ask)}")
++  put-mail   |=(ask/@t =+(pax=(rash ask unix-path) [%sav pax '']))
++  unix-path  ::  split into path of "name" and "extension"
  ;~  (glue dot)
    (cook crip (star ;~(less dot next)))
    ;~(plug (cook crip (star next)) (easy ~))
  ==
::
++  poke-ask-mail
  |=  ask/@t
  ^-  (quip {bone card} +>)
  ~|  have-mail+ask
  ?<  (~(has in adr) ask)
  :_  +>.$(adr (~(put in adr) ask))
  =+  [mez=[(message ask)]~ sav=(put-mail ask)]
  %+  turn  (prey /sole bow)
  |=({ost/bone ^} (effect(ost.bow ost) %mor tan+mez sav ~))
::
++  poke-sole-action
  |=  act/sole-action
  ^-  (quip {bone card} +>)  
  ?-  -.act
    $clr  `+>.$
    $ret  [[(effect tan+(turn adrs message))]~ +>.$]    :: re-print list
    $det                              :: reject all input
      =+  som=(~(got by sos) ost.bow) ::  XX this code belongs in a library
      =^  inv  som  (~(transceive sole som) +.act)
      =^  det  som  (~(transmit sole som) inv)
      =.  sos  (~(put by sos) ost.bow som)
      [[(effect det+det)]~ +>.$]
  ==
--
