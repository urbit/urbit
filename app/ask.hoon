::
::::  /hoon/ask/app
  ::
/?    310
/+    sole, womb
[. sole]
|%
  ++  card
    $%  {$diff $sole-effect sole-effect}
    ==
  ++  invited  ?($new $sent $ignored)
  ++  email  @t
--
!:
|_  {bow/bowl adr/(map email {time invited}) sos/(map bone sole-share) wom/(unit ship)}
++  prompt
  ?~  wom  [& %ask-ship ":womb-ship? ~"]
  [& %$ "<listening> [l,a,?]"]
::
++  peer-sole
  |=  path
  ^-  (quip {bone card} +>)
  ~|  [%not-in-whitelist src.bow]
  ?>  (~(has in (sy ~zod our.bow ~wisdyr-holpeg ~)) src.bow)
  :_  +>.$(sos (~(put by sos) ost.bow *sole-share))
  =-  [(effect %mor pro+prompt -)]~
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
  =/  som  (~(got by sos) ost.bow)
  ?-    -.act
      $clr  `+>.$
      $ret
    ?^  wom  [[(effect mor+help)]~ +>.$]    :: show help
    ?~  buf.som  [[(effect txt+"Please enter womb ship")]~ +>.$]
    =/  try  (rose (tufa buf.som) fed:ag)
    ?.  ?=({$& ^} try)
      [[(effect bel+~)]~ +>.$]
    =>  .(wom p.try)  :: XX TMI
    [[(effect pro+prompt)]~ +>.$]  :: XX handle multiple links?
  ::
      $det                              :: reject all input
    =^  inv  som  (~(transceive sole som) +.act)
    ?~  wom
      =/  try  (rose (tufa buf.som) fed:ag)
      ?:  -.try  `+>.$(sos (~(put by sos) ost.bow som))
      =^  det  som  (~(transmit sole som) inv)
      =.  sos  (~(put by sos) ost.bow som)
      [[(effect mor+~[det+det bel+~])]~ +>.$]
    =/  mor/(list sole-effect)
      ?:  =(`*`"?" buf.som)  help
      ?:  =(`*`"a" buf.som)  [tan+(turn adrs message)]~
      ?:  =(`*`"l" buf.som)
        =;  new  [tan+(turn new message)]~
        (skim adrs |=({@ @ inv/invited} =(%new inv)))
      ~
    =^  det  som  (~(transmit sole som) inv)
    =.  sos  (~(put by sos) ost.bow som)
    [[(effect mor+[det+det mor])]~ +>.$]
  ==
::
++  help
  ^-  (list sole-effect)
  =-  (scan - (more (just '\0a') (stag %txt (star prn))))
  """
  l - list new asks
  a - list all asks
  ? - print help
  """
::
++  invite
  |=  who/email
  :-  %womb-invite
  ^-  {cord reference invite}:womb
  =+  inv=(scot %uv (end 7 1 eny.bow))
  [inv `&+our.bow [who 1 0 "You have been invited to Urbit: {(trip inv)}" ""]]
--
