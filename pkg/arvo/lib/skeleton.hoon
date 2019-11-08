::  Similar to default-agent except crashes everywhere
^-  agent:mall
|_  bowl:mall
++  on-init
  ^-  (quip card:agent:mall agent:mall)
  !!
::
++  on-save
  ^-  vase
  !!
::
++  on-load
  |~  old-state=vase
  ^-  (quip card:agent:mall agent:mall)
  !!
::
++  on-poke
  |~  in-poke-data=cage
  ^-  (quip card:agent:mall agent:mall)
  !!
::
++  on-watch
  |~  path
  ^-  (quip card:agent:mall agent:mall)
  !!
::
++  on-leave
  |~  path
  ^-  (quip card:agent:mall agent:mall)
  !!
::
++  on-peek
  |~  path
  ^-  (unit (unit cage))
  !!
::
++  on-agent
  |~  [wire gift:agent:mall]
  ^-  (quip card:agent:mall agent:mall)
  !!
::
++  on-arvo
  |~  [wire =sign-arvo]
  ^-  (quip card:agent:mall agent:mall)
  !!
::
++  on-fail
  |~  [term tang]
  ^-  (quip card:agent:mall agent:mall)
  !!
--
