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
::  A child renderer has updated its state.
::  Plug its new manx into the matching div.
++  render-child
  |=  [=gift:neo =bowl:neo =vase]
  ^-  (quip card:neo pail:neo)
  =/  ui  (need ui:!<(renderer vase))
  =/  sesh  (gift-session gift)
  =/  sub  (session-ui [bowl sesh])
  =/  new  (swap [ui sub sesh])
  [~ renderer/!>([sesh new])]
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
++  gift-ui
  |=  [=gift:neo =bowl:neo]
  ^-  manx
  =/  sesh  (gift-session gift)
  (session-ui [bowl sesh])
::
::  Get the current UI from the renderer matching this session
++  session-ui
  |=  [=bowl:neo sesh=road:neo]
  ^-  manx
  =/  =idea:neo  (~(got of:neo kids.bowl) sesh)
  =/  =pail:neo  q.saga.idea
  (need ui:!<(renderer q.pail))
::
::  Find which top-level session this gift
::  (or series of gifts) came from
++  gift-session
  |=  =gift:neo
  ^-  road:neo
  =-  ?~  -  !!  :: if no top-level gifts, ignore request
      -<-
  %+  skim
    ~(tap of:neo gift)
  |=  [=road:neo =loot:neo]
  =(1 (lent road))
--