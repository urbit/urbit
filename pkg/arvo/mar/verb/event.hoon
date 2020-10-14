/-  verb
=,  dejs:format
|_  =event:verb
++  grad  %noun
++  grab
  |%
  ++  noun  event:verb
  --
::
++  grow
  |%
  ++  noun  event
  ++  json
    =,  enjs:format
    %+  frond  -.event
    ?-  -.event
      %on-init   ~
      %on-load   ~
      %on-poke   s+mark.event
      %on-watch  (path path.event)
      %on-leave  (path path.event)
      %on-agent  (pairs 'wire'^(path wire.event) 'sign'^s+sign.event ~)
      %on-arvo   (pairs 'wire'^(path wire.event) 'vane'^s+vane.event 'sign'^s+sign.event ~)
      %on-fail   s+term.event
    ==
  --
--
