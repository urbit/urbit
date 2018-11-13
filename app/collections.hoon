::
::::  /app/collections/hoon
  ::
/?  309
/-  hall
/+  collections
::
::  cols:
::
::    run collections-item renderer on children of /web/collections
::    combine with a bunted config in a +collection structure defined in 
::    /lib/collections because the top level collection has no config file
::
::    whenever any of the clay files that compose this renderer change, this app
::    will recompile and the +prep arm will fire. we then check which files 
::    changed and notify the corresponding hall circle of that change
::
/=  cols
  /^  collection:collections
  /;  |=  a=(map knot item:collections)
      [*config:collections a]
  /:  /===/web/collections  /_  /collections-item/
::
=,  collections
=,  space:userlib
::
::  state: 
::    
::    stores the collection built by above by :cols so that we can compare old
::    and new versions whenever the rendered data changes
::
~%  %landscape  ..^is  ~
|_  [bol=bowl:gall sta=state]
::
::  +this: app core subject
::
++  this  .
::
::  +prep:
::
::    on initial boot, create top level hall circle for collections, called %c
::
::    on subsequent compiles, call +ta-update:ta on the old collection data,
::    then update state to store the new collection data
::
++  prep
  ~/  %land-prep
  |=  old=(unit state)
  ^-  (quip move _this)
  ?~  old
    :_  this
    ;:  welp
      =<  ta-done
      (~(ta-hall-create-circle ta ~ bol) /c 'collections')
    ::
      :~  [ost.bol %peer /circles [our.bol %hall] /circles/[(scot %p our.bol)]]
          [ost.bol %peer /inbox [our.bol %hall] /circle/inbox/config/grams]
          [ost.bol %peer /invites [our.bol %hall] /circle/i/grams]
      ==
    ==
  ?-    -.u.old
      %0
    =/  mow=(list move)
      =<  ta-done
      (~(ta-update ta ~ bol) col.u.old cols)
    :-  mow
    %=  this
      sta  [%0 cols str.u.old]
    ==
  ==
::
::  +mack: 
::
::    recieve acknowledgement for permissions changes, print error if it failed
::
++  mack
  |=  [wir=wire err=(unit tang)]
  ^-  (quip move _this)
  ?~  err
    [~ this]
  (mean u.err)
::
::  +coup: recieve acknowledgement for poke, print error if it failed
::
++  coup
  |=  [wir=wire err=(unit tang)]
  ^-  (quip move _this)
  ?~  err
    [~ this]
  (mean u.err)
::
::  +poke-collections-action:
::
::    the main interface for creating and deleting collections and items
::
++  poke-collections-action
  ~/  %coll-poke-collections-action
  |=  act=action
  ^-  (quip move _this)
  ?:  =(who.act our.bol)
    :_  this
    =<  ta-done
    (~(ta-act ta ~ bol) act)
  ::  forward poke if its not meant for us
  ::
  :_  this
  :_  ~
  :*  ost.bol  %poke  
      /forward-collections-action  
      [who.act %collections]
      %collections-action  act
  ==
::
::  +poke-json
::
::    utility for setting whether or not to display the onboarding page
::
++  poke-json
  ~/  %coll-poke-json
  |=  jon=json
  ^-  (quip move _this)
  ?:  ?=([%o [[%onboard %b ?] ~ ~]] jon)
    :_  this
    =<  ta-done
    (~(ta-write ta ~ bol) /web/landscape/onboard/json [%json !>(jon)])
  [~ this]
::
::
::
::
::
++  peer
  |=  wir=wire
  ^-  (quip move _this)
  [~ this]
::
::  +reap: recieve acknowledgement for peer, retry on failure
::
++  reap
  |=  [wir=wire err=(unit tang)]
  ^-  (quip move _this)
  ?~  err
    [~ this]
  ?~  wir
    (mean [leaf+"invalid wire for diff: {(spud wir)}"]~)
  ?+  i.wir
    (mean [leaf+"invalid wire for diff: {(spud wir)}"]~)
  ::
      %circles
    :_  this
    [ost.bol %peer /circles [our.bol %hall] /circles/[(scot %p our.bol)]]~
  ::
      %inbox
    :_  this
    [ost.bol %peer /inbox [our.bol %hall] /circle/inbox/config/grams]~
  ::
      %invites
    :_  this
    [ost.bol %peer /invites [our.bol %hall] /circle/i/grams]~
  ::
      %our
    [~ this]
  ::
      %sub
    [~ this]
  ==

:: :~  [ost.bol %peer /circles [our.bol %hall] /circles/[(scot %p our.bol)]]
::     [ost.bol %peer /inbox [our.bol %hall] /circle/inbox/config/grams]
::     [ost.bol %peer /invites [our.bol %hall] /circle/i/grams]
:: ==


::
::  +quit: 
::
++  quit
  |=  [wir=wire err=(unit tang)]
  ^-  (quip move _this)
  ?~  err
    [~ this]
  ?~  wir
    (mean [leaf+"invalid wire for diff: {(spud wir)}"]~)
  ?+  i.wir
    (mean [leaf+"invalid wire for diff: {(spud wir)}"]~)
  ::
      %circles
    :_  this
    [ost.bol %peer /circles [our.bol %hall] /circles/[(scot %p our.bol)]]~
  ::
      %inbox
    :_  this
    [ost.bol %peer /inbox [our.bol %hall] /circle/inbox/config/grams]~
  ::
      %invites
    :_  this
    [ost.bol %peer /invites [our.bol %hall] /circle/i/grams]~
  ::
      %our
    [~ this]
  ::
      %sub
    [~ this]
  ==
::
::  +diff-hall-prize:
::
::
::
++  diff-hall-prize
  |=  [wir=wire piz=prize:hall]
  ^-  (quip move _this)
  ?~  wir
    (mean [leaf+"invalid wire for diff: {(spud wir)}"]~)
  ?+  i.wir
    (mean [leaf+"invalid wire for diff: {(spud wir)}"]~)
  ::
  ::  %circles: subscribe to the configuration of each of our circles
  ::
      %circles
    ?>  ?=(%circles -.piz) 
    =/  circs=(set name:hall)  (~(dif in cis.piz) (sy ~[%inbox %i %public]))
    ::
    :_  this
    ^-  (list move)
    %+  turn  ~(tap in circs)
    |=  circ=name:hall
    ^-  move
    [ost.bol %peer /our/[circ] [our.bol %hall] /circle/[circ]/config] 
  ::
  ::  %inbox:
  ::
      %inbox
    [~ this]
  ::
  ::  %invites
  ::
      %invites
    [~ this]
  ::
      %our
    [~ this]
  ::
      %sub
    [~ this]
  ==

:: :~  [ost.bol %peer /circles [our.bol %hall] /circles/[(scot %p our.bol)]]
::     [ost.bol %peer /inbox [our.bol %hall] /circle/inbox/config/grams]
::     [ost.bol %peer /invites [our.bol %hall] /circle/i/grams]
:: ==

::
::  +diff-hall-rumor
::
::
::
++  diff-hall-rumor
  |=  [wir=wire rum=rumor:hall]
  ^-  (quip move _this)
  ?~  wir
    (mean [leaf+"invalid wire for diff: {(spud wir)}"]~)
  ?+  i.wir
    (mean [leaf+"invalid wire for diff: {(spud wir)}"]~)
  ::
      %circles
    [~ this]
  ::
      %inbox
    [~ this]
  ::
      %invites
    [~ this]
  ::
      %our
    [~ this]
  ::
      %sub
    [~ this]
  ==
--






























