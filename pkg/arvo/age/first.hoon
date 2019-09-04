/+  agent
|%
+$  in-poke-data  %hoy
++  agent  (^agent ,~ in-poke-data ,*)
--
%-  agent-to-mall-agent:agent
^-  agent:agent
|_  [bowl:mall state=~]
++  handle-init
  `~
::
++  handle-prep
  |~  old-state=vase
  *step:agent:agent
::
++  handle-poke
  |=  =a=in-poke-data
  ~&  >>  'ouchers!'
  ~&  >>>  a-in-poke-data
  *step:agent:agent
::
++  handle-peer
  |~  path
  *step:agent:agent
::
++  handle-pull
  |~  path
  *step:agent:agent
::
++  handle-peek
  |~  path
  *(unit (unit cage))
::
++  handle-mall
  |~  [wire internal-gift:mall]
  *step:agent:agent
::
++  handle-take
  |~  [wire *]
  *step:agent:agent
::
++  handle-lame
  |~  [term tang]
  *step:agent:agent
--
