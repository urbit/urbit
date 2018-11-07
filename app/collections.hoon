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
  |=  old=(unit state)
  ^-  (quip move _this)
  ?~  old
    :_  this
    =<  ta-done
    (~(ta-hall-create-circle ta ~ bol) /c 'collections')
  ?-    -.u.old
      %0
    =/  mow=(list move)
      =<  ta-done
      (~(ta-update ta ~ bol) col.u.old cols)
    [mow this(sta [%0 cols])]
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
  |=  jon=json
  ^-  (quip move _this)
  ?:  ?=([%o [[%onboard %b ?] ~ ~]] jon)
    :_  this
    =<  ta-done
    (~(ta-write ta ~ bol) /web/landscape/onboard/json [%json !>(jon)])
  [~ this]
--
