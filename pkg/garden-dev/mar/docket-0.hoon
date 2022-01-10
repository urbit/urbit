/+  dock=docket
|_  docket=docket-0:dock
++  grow
  |%
  ++  mime  
    ^-  ^mime
    [/text/x-docket (as-octt:mimes:html (spit-docket-0:mime:dock docket))]
  ++  noun  docket
  ++  json  (docket-0:enjs:dock docket)
  ++  docket-1
    =/  ref=href:dock
      ?-    -.href-0.docket
        %glob
      :-  `[base.href-0.docket glob-reference.href-0.docket]
      glob+~
         %site
      [~ href-0.docket]
      ==
    :*  %1
        title.docket
        info.docket
        color.docket
        ref
        image.docket
        version.docket
        website.docket
        license.docket
    ==
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
