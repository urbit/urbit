|_  s=ship
++  grad  %noun
++  grow
  |%
  ++  noun  s
  ++  json  s+(scot %p s)
  ++  mime
    ^-  ^mime
    [/text/x-ship (as-octt:mimes:html (scow %p s))]

  --
++  grab
  |%
  ++  noun  ship
  ++  json  (su:dejs:format ;~(pfix sig fed:ag))
  ++  mime
    |=  [=mite len=@ tex=@]
    (slav %p (snag 0 (to-wain:format tex)))
  --
--
