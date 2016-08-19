::
::::  /hoon/ask/app
  ::
/?    310
/+    sole
[. sole]
|%
  ++  card
    $%  {$diff $sole-effect sole-effect}
    ==
  ++  invited  ?($new $sent $ignored)
  ++  email  @t
--
!:
|_  {bow/bowl adr/(map email {time invited}) sos/(map bone sole-share)}
++  peer-sole
  |=  path
  ^-  (quip {bone card} +>)
  ~|  [%not-in-whitelist src.bow]
  ?>  (~(has in (sy ~zod our.bow ~talsur-todres ~)) src.bow)
  :_  +>.$(sos (~(put by sos) ost.bow *sole-share))
  =-  [(effect %mor pro+[& %$ "<listening> [n,a,?]"] -)]~
  =+  all=adrs
  [tan+(turn all message) (turn all put-mail)]
::
++  adrs  (sort (turn (~(tap by adr)) |=({a/email b/time c/invited} [b a c])) lor)
++  effect  |=(fec/sole-effect [ost.bow %diff %sole-effect fec])
++  message
  |=  {now/time ask/@t inv/invited}  ^-  tank
  =.  now  (sub now (mod now ~s1))
  leaf+"ask: {<inv>} {<now>} {(trip ask)}"
::
++  put-mail   |=({@ ask/@t inv/invited} =+(pax=(rash ask unix-path) [%sav pax `@t`inv]))
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
  ?<  (~(has by adr) ask)
  :_  +>.$(adr (~(put by adr) ask now.bow %new)) :: XX electroplating
  =/  new  [now.bow ask %new]
  =+  [mez=[(message new)]~ sav=(put-mail new)]
  %+  turn  (prey /sole bow)
  |=({ost/bone ^} (effect(ost.bow ost) %mor tan+mez sav ~))
::
++  poke-sole-action
  |=  act/sole-action
  ^-  (quip {bone card} +>)  
  ?-  -.act
    $clr  `+>.$
    $ret  [[(effect mor+help)]~ +>.$]    :: re-print list
    $det                              :: reject all input
      =+  som=(~(got by sos) ost.bow) ::  XX this code belongs in a library
      =^  inv  som  (~(transceive sole som) +.act)
      =/  buf  buf.som
      =^  det  som  (~(transmit sole som) inv)
      =.  sos  (~(put by sos) ost.bow som)
      =+  mor=`(list sole-effect)`[det+det]~
      =.  mor
        ?:  =(`*`"?" buf)  (welp mor help)
        ?:  =(`*`"a" buf)  (welp mor tan+(turn adrs message) ~)
        ?:  =(`*`"n" buf)
          =;  new  (welp mor tan+(turn new message) ~)
          (skim adrs |=({@ @ inv/invited} =(%new inv)))
        mor
      [[(effect mor+mor)]~ +>.$]
  ==
++  help
  ^-  (list sole-effect)
  =-  (scan - (more (just '\0a') (stag %txt (star prn))))
  """
  n - list new asks
  a - list all asks
  ? - print help
  """
--
