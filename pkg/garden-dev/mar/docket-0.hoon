/+  dock=docket
|_  =docket:dock
++  grow
  |%
  ++  mime  
    ^-  ^mime
    [/text/x-docket (as-octt:mimes:html (spit-docket:mime:dock docket))]
  ++  noun  docket
  ++  json  (docket:enjs:dock docket)
  --
++  grab
  |%
  ::
  ++  mime
    |=  [=mite len=@ud tex=@]
    ^-  docket:dock
    %-  need
    %-  from-clauses:mime:dock
    !<((list clause:dock) (slap !>(~) (ream tex)))

  ::
  ++  noun  docket:dock
  --
++  grad  %noun
--
