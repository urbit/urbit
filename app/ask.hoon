::
::::  /hoon/ask/app
  ::
/?    310
/+    sole, womb
[. sole]
|%
  ++  card
    $%  {$diff $sole-effect sole-effect}
        {$poke wire {ship $hood} $womb-invite {cord reference invite}:womb}
    ==
  ++  invited  ?($new $sent $ignored)
  ++  email  @t
--
!:
|_  {bow/bowl adr/(map email {time invited}) sos/(map bone sole-share) wom/(unit ship)}
++  prompt
  ^-  sole-prompt
  ?~  wom  [& %ask-ship ":womb-ship? ~"]
  =/  new  new-adrs
  ?~  new  [& %$ "<listening> (0) [l,a,w,?]"]
  [& %$ ": approve {<ask.i.new>}? ({<(lent new)>}) [y,n,l,a,w,?]"]
::
++  peer-sole
  |=  path
  ^-  (quip {bone card} +>)
  ~|  [%not-in-whitelist src.bow]
  ?>  (~(has in (sy ~zod our.bow ~wisdyr-holpeg ~)) src.bow)
  :_  +>.$(sos (~(put by sos) ost.bow *sole-share))
  =-  [(effect %mor pro+prompt -)]~
  =+  all=adrs
  [tan+(turn (flop all) message) (turn all put-mail)]
::
++  adrs
  =-  (sort - lor)
  %+  turn  (~(tap by adr))
  |=({a/email b/time c/invited} [tym=b ask=a inv=c])
::
++  new-adrs  (skim adrs |=({@ @ inv/invited} =(%new inv)))
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
  =.  adr  (~(put by adr) ask now.bow %new) :: XX electroplating
  :_  +>.$
  =/  new  [now.bow ask %new]
  =+  [mez=[(message new)]~ pro=prompt sav=(put-mail new)]
  %+  turn  (prey /sole bow)
  |=({ost/bone ^} (effect(ost.bow ost) %mor tan+mez pro+prompt sav ~))
::
++  poke-sole-action
  |=  act/sole-action
  ^-  (quip {bone card} +>)
  =/  som  (~(got by sos) ost.bow)
  ?-    -.act
      $clr  `+>.$
      $ret
    ?^  wom  [[(effect mor+help)]~ +>.$]    :: show help
    ?:  =(~ buf.som)  [[(effect txt+"Please enter womb ship")]~ +>.$]
    =/  try  (rose (tufa buf.som) fed:ag)
    ?.  ?=({$& ^} try)
      [[(effect bel+~)]~ +>.$]
    =>  .(wom p.try)  :: XX TMI
    (transmit set+~ pro+prompt ~)   :: XX handle multiple links?
  ::
      $det                              :: reject all input
    =^  inv  som  (~(transceive sole som) +.act)
    =.  sos  (~(put by sos) ost.bow som)
    ?~  wom
      =/  try  (rose (tufa buf.som) fed:ag)
      ?:  -.try  `+>.$
      (transmit inv bel+~ ~)
    ?:  =(`*`"?" buf.som)  (transmit inv help)
    ?:  =(`*`"a" buf.som)  (transmit inv tan+(turn (flop adrs) message) ~)
    ?:  =(`*`"l" buf.som)  (transmit inv tan+(turn (flop new-adrs) message) ~)
    ?:  =(`*`"n" buf.som)
      =/  new  new-adrs
      ?~  new  (transmit inv bel+~ ~)
      =.  inv.i.new  %ignored
      =.  adr  (~(put by adr) ask.i.new [tym inv]:i.new)
      (transmit inv tan+[(message i.new)]~ pro+prompt ~)
    ?:  =(`*`"y" buf.som)
      =/  new  new-adrs
      ?~  new  (transmit inv bel+~ ~)
      =.  inv.i.new  %sent  :: XX pending
      =-  [[(invite ask.i.new) -<] ->]
      =.  adr  (~(put by adr) ask.i.new [tym inv]:i.new)
      (transmit inv tan+[(message i.new)]~ pro+prompt ~)
    ?:  =(`*`"w" buf.som)
      =>  .(wom ~)  :: XX TMI
      (transmit inv pro+prompt ~)
    (transmit inv bel+~ ~)
  ==
++  transmit
  |=  {inv/sole-edit mor/(list sole-effect)}
  =/  som  (~(got by sos) ost.bow)
  =^  det  som  (~(transmit sole som) inv)
  =.  sos  (~(put by sos) ost.bow som)
  [[(effect mor+[det+det mor])]~ +>.$]
::
++  help
  ^-  (list sole-effect)
  =-  (scan - (more (just '\0a') (stag %txt (star prn))))
  %+  welp
    ?~  [new-adrs]  ""
    """
    y - invite current ask
    n - ignore current ask
    
    """
  """
  l - list new asks
  a - list all asks
  w - reset womb ship
  ? - print help
  """
::
++  invite
  |=  ask/email
  :-  ost.bow
  ^-  card
  :^  %poke  /invite/(scot %t ask)  [(need wom) %hood]  
  :-  %womb-invite
  ^-  {cord reference invite}:womb
  =+  inv=(scot %uv (end 7 1 eny.bow))
  [inv `&+our.bow [ask 1 0 "You have been invited to Urbit: {(trip inv)}" ""]]
--
