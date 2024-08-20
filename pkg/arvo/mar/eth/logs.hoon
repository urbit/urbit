::  list of ethereum logs
::
/+  ethereum
::
|_  logs=(list event-log:rpc:ethereum)
++  grab
  |%
  ++  noun  (list event-log:rpc:ethereum)
  ++  mime
    |=  [mite =octs]
    (noun (cue q.octs))
  --
::
++  grow
  |%
  ++  mime
    [/application/x-ethereum-logs (as-octs:mimes:html (jam logs))]
  --
++  grad  %mime
--
