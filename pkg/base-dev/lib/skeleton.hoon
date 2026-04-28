::  Similar to default-agent except crashes everywhere
^-  agent:gall
:-  %new-agent
|_  bowl:gall
++  on-init
  ^-  (quip card:agent:gall form:agent:gall)
  !!
::
++  on-save
  ^-  vase
  !!
::
++  on-load
  |~  old-state=vase
  ^-  (quip card:agent:gall form:agent:gall)
  !!
::
++  on-poke
  |~  in-poke-data=cage
  ^-  (quip card:agent:gall form:agent:gall)
  !!
::
++  on-watch
  |~  path
  ^-  (quip card:agent:gall form:agent:gall)
  !!
::
++  on-leave
  |~  path
  ^-  (quip card:agent:gall form:agent:gall)
  !!
::
++  on-peek
  |~  path
  ^-  (unit (unit cage))
  !!
::
++  on-agent
  |~  [wire sign:agent:gall]
  ^-  (quip card:agent:gall form:agent:gall)
  !!
::
++  on-arvo
  |~  [wire gift-user-v1:gall]
  ^-  (quip card:agent:gall form:agent:gall)
  !!
::
++  on-fail
  |~  [term tang]
  ^-  (quip card:agent:gall form:agent:gall)
  !!
--
