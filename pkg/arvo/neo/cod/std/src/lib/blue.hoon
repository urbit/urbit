/@  renderer
/-  manx-utils
/-  html-utils
|%
::
::  Create a placeholder div and renderer for this child.
++  recur
  |=  [renderer=stud:neo =bowl:neo sesh=road:neo]
  |=  [pax=pith *]
  ^-  [manx card:neo]
  =/  id  #/[uv/(end 3^4 eny.bowl)]
  :-  
  ;div.fr.g2
    =name  (trip (snag 0 (pout id)))
    Child
  ==
  :+  :(welp #/neo/blue sesh id)
    %make
  :+  renderer 
    `[%renderer !>([id ~])]
  (~(gas by *crew:neo) src/pax ~)
::
::  If gift is from an immediate child, swap.
++  render-child
  |=  [=gift:neo =bowl:neo =vase]
  ^-  (quip card:neo pail:neo)
  =/  state  !<(renderer vase)
  =/  ui  (need ui.state)
  =/  sesh  (gift-session gift)
  ?~  sesh
    [~ renderer/vase]
  =/  sub  (session-ui [bowl u.sesh])
  =/  new  (swap [ui sub u.sesh])
  [~ renderer/!>([session.state new])]
::
::  Find the div in main with this id,
::  and replace it with sub.
++  swap
  |=  [main=manx sub=manx id=road:neo]
  ^-  manx
  =/  name  (trip (snag 0 (pout id)))
  =/  hu  ~(. mx:html-utils main)
  =/  c=con:hu
    |=  [=path =manx]
    ^-  ?
    =/  mu  ~(. manx-utils manx)
    =(name (got:mu %name))
  =/  t=tan:hu
    |=  [=path =manx]
    sub
  (wit:hu [c t])
::
::  Get the current UI from the renderer matching this session
++  session-ui
  |=  [=bowl:neo sesh=road:neo]
  ^-  manx
  =/  =idea:neo  (~(got of:neo kids.bowl) sesh)
  =/  =pail:neo  q.saga.idea
  (need ui:!<(renderer q.pail))
::
::  Find which immediate child this gift came from.
::  If not from an immediate child, return null.
++  gift-session
  |=  =gift:neo
  ^-  (unit road:neo)
  =-  ?~  -  
        ~
      `-<-
  %+  skim
    ~(tap of:neo gift)
  |=  [=road:neo =loot:neo]
  =(1 (lent road))
--