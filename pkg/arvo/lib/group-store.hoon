/-  *group, sur=group-store
^?
=<  [. sur]
=,  sur
|%
::  +en-path: transform into path
::
++  en-path
  |%
  ::
  ++  group-id
    |=  ^group-id
    ^-  path
    /[(scot %p ship)]/[term]
  --
::  +de-path: transform from path
::
++  de-path
  |%
  ::
  ++  group-id
    |=  =path
    ^-  (unit ^group-id)
    ?.  ?=([@ @ *] path)
      ~
    =/  ship=(unit ship)
      (slaw %p i.path)
    ?~  ship  ~
    =*  term   i.t.path
    `[u.ship term]
  --
--
