/+  dock=docket
|_  docket=docket-0:dock
++  grow
  |%
  ++  mime  
    ^-  ^mime
    [/text/x-docket (as-octt:mimes:html (spit-docket-0:mime:dock docket))]
  ++  noun  docket
  ++  json  (docket-0:enjs:dock docket)
  --
++  grab
  |%
  ::
  ++  mime
    |=  [=mite len=@ud tex=@]
    ^-  docket-0:dock
    %-  need
    %-  from-clauses-0:mime:dock
    !<((list clause:dock) (slap !>(~) (ream tex)))
  ::
  ++  noun  docket-0:dock
  --
++  grad  %noun
--
