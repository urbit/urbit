/-  *docket
|_  dock=docket
++  grow
  |%
  ++  mime  `^mime`[/text/x-docket (as-octt:mimes:html tape)]
  ++  tape
    %-  zing
    %+  join  "\0a"
    ^-  (list ^tape)
    :~  (trip title.dock)
        (scow %ux color.dock)
        (trip url.dock)
        (spud base.dock)
    ==
  ++  noun  dock
  --
++  grab
  |%
  ++  tape
    |=  tap=^tape
    |^  ^-  docket
    %+  scan  tap
    ;~  (glue (just '\0a'))
      str
      (cook |=(@ud `@ux`+<) ;~(pfix (jest '0x') hex:ag))
      str
      stap
    ==
    ::
    ++  str
      (cook |=(t=^tape `cord`(crip t)) (star ;~(less (just '\0a') next)))
    --
  ::
  ++  mime
    |=  [=mite len=@ud tex=@]
    ^-  docket
    %-  tape
    %+  scag  (dec len)
    (trip tex)
  ::
  ++  noun  docket
  --
++  grad  %noun
--
