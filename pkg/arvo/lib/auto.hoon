::  Autocompletion for hoon.
::
|%
+$  ids  (list [=term =type])
++  get-identifiers
  |=  ty=type
  %-  flop
  |-  ^-  ids
  ?-    ty
      %noun      ~
      %void      ~
      [%atom *]  ~
      [%cell *]
    %+  weld
      $(ty p.ty)
    $(ty q.ty)
  ::
      [%core *]
    %-  weld
    :_  $(ty p.ty)
    ^-  (list (pair term type))
    %-  zing
    %+  turn  ~(tap by q.r.q.ty)
    |=  [term =tome]
    %+  turn
      ~(tap by q.tome)
    |=  [name=term =hoon]
    ^-  (pair term type)
    ~|  term=term
    [name ~(play ~(et ut ty) ~[name] ~)]
  ::
      [%face *]
    ?^  p.ty
      ~
    [p.ty q.ty]~
  ::
      [%fork *]
    ~|  %find-fork
    !!  ::  eh, fuse?
  ::
      [%hint *]  $(ty q.ty)
      [%hold *]  $(ty ~(repo ut ty))
  ==
::
++  search-prefix
  |=  [sid=term =ids]
  ^-  (list [term type])
  %+  skim  ids
  |=  [id=term ty=type]
  =(sid (end 3 (met 3 sid) id))
::
++  longest-match
  |=  matches=(list [=term =type])
  ^-  term
  ?~  matches
    ''
  =/  n  1
  =/  last  (met 3 term.i.matches)
  |-  ^-  term
  ?:  (gth n last)
    term.i.matches
  =/  prefix  (end 3 n term.i.matches)
  ?:  |-  ^-  ?
      ?|  ?=(~ t.matches)
          ?&  =(prefix (end 3 n term.i.t.matches))
              $(t.matches t.t.matches)
      ==  ==
    $(n +(n))
  (end 3 (dec n) term.i.matches)
::
++  find-type-mule
  |=  [sut=type gen=hoon]
  ^-  (unit [term type])
  =/  res  (mule |.((find-type sut gen)))
  ?-  -.res
    %&  p.res
    %|  ((slog (flop (scag 1 p.res))) ~)
  ==
::
::  Get the subject type of the wing where you've put the "magic-spoon".
::
++  find-type
  |=  [sut=type gen=hoon]
  =*  loop  $
  |^
  ^-  (unit [term type])
  ?-    gen
      [%cnts [%magic-spoon ~] *]    `['' sut]
      [%cnts [%magic-spoon @ ~] *]  `[i.t.p.gen sut]
      [^ *]      (both p.gen q.gen)
      [%ktcn *]  loop(gen p.gen)
      [%brcn *]  (grow q.gen)
      [%brvt *]  (grow q.gen)
      [%cnts *]
    |-  ^-  (unit [term type])
    =*  inner-loop  $
    ?~  q.gen
      ~
    %+  replace
      loop(gen q.i.q.gen)
    |.  inner-loop(q.gen t.q.gen)
  ::
      [%dtkt *]  (spec-and-hoon p.gen q.gen)
      [%dtls *]  loop(gen p.gen)
      [%rock *]  ~
      [%sand *]  ~
      [%tune *]  ~
      [%dttr *]  (both p.gen q.gen)
      [%dtts *]  (both p.gen q.gen)
      [%dtwt *]  loop(gen p.gen)
      [%hand *]  ~
      [%ktbr *]  loop(gen p.gen)
      [%ktls *]  (both p.gen q.gen)
      [%ktpd *]  loop(gen p.gen)
      [%ktsg *]  loop(gen p.gen)
      [%ktwt *]  loop(gen p.gen)
      [%note *]  loop(gen q.gen)
      [%sgzp *]  (both p.gen q.gen)
      [%sgbn *]  loop(gen q.gen)  ::  should check for hoon in p.gen
      [%tsbn *]  (change p.gen q.gen)
      [%tscm *]
    %+  replace
      loop(gen p.gen)
    |.(loop(gen q.gen, sut (~(busk ut sut) p.gen)))
  ::
      [%wtcl *]  (bell p.gen q.gen r.gen)
      [%fits *]  (both p.gen wing+q.gen)
      [%wthx *]  loop(gen wing+q.gen)
      [%dbug *]  loop(gen q.gen)
      [%zpcm *]  (both p.gen q.gen)
      [%lost *]  loop(gen p.gen)
      [%zpmc *]  (both p.gen q.gen)
      [%zpts *]  loop(gen p.gen)
      [%zpvt *]  (both q.gen r.gen)
      [%zpzp *]  ~
      *
    =+  doz=~(open ap gen)
    ?:  =(doz gen)
      ~_  (show [%c 'hoon'] [%q gen])
      ~>  %mean.'play-open'
      !!
    loop(gen doz)
  ==
  ::
  ++  replace
    |=  [a=(unit [term type]) b=(trap (unit [term type]))]
    ^-  (unit [term type])
    ?~(a $:b a)
  ::
  ++  both
    |=  [a=hoon b=hoon]
    (replace loop(gen a) |.(loop(gen b)))
  ::
  ++  bell
    |=  [a=hoon b=hoon c=hoon]
    (replace loop(gen a) |.((replace loop(gen b) |.(loop(gen c)))))
  ::
  ++  spec-and-hoon
    |=  [a=spec b=hoon]
    (replace (find-type-in-spec sut a) |.(loop(gen b)))
  ::
  ++  change
    |=  [a=hoon b=hoon]
    (replace loop(gen a) |.(loop(gen b, sut (~(play ut sut) a))))
  ::
  ++  grow
    |=  m=(map term tome)
    =/  tomes  ~(tap by m)
    |-  ^-  (unit [term type])
    =*  outer-loop  $
    ?~  tomes
      ~
    =/  arms  ~(tap by q.q.i.tomes)
    |-  ^-  (unit [term type])
    =*  inner-loop  $
    ?~  arms
      outer-loop(tomes t.tomes)
    %+  replace
      loop(gen q.i.arms, sut (~(play ut sut) gen))
    |.  inner-loop(arms t.arms)
  --
::
::  Not implemented yet.  I wonder whether we should modify types found
::  in spec mode such that if it's a mold that produces a type, it
::  should just display the type and not that it's technically a
::  function.
::
++  find-type-in-spec
  |=  [sut=type pec=spec]
  ^-  (unit [term type])
  !!
::
++  replace-hax
  |=  code=tape
  %+  scan  code
  %+  cook
    |=  res=(list $@(@ [~ (unit term)]))
    %-  trip
    %-  crip
    %+  turn  res
    |=  elem=$@(@ [~ (unit term)])
    ?@  elem
      elem
    ?~  +.elem
      'magic-spoon'
    (cat 3 'magic-spoon.' u.+.elem)
  %-  star
  ;~  pose
    ;~  less
      hax
      prn
    ==
  ::
    (stag ~ ;~(pfix hax (punt sym)))
  ==
::
++  autoadvance
  |=  [sut=type gen=hoon]
  %+  bind  (find-type-mule sut gen)
  |=  [id=term typ=type]
  (longest-match (search-prefix id (get-identifiers typ)))
::
++  auto-advance
  |=  [sut=type code=cord]
  (autoadvance sut (scan (replace-hax (trip code)) vest))
::
++  tablist
  |=  [sut=type gen=hoon]
  %+  bind  (find-type-mule sut gen)
  |=  [id=term typ=type]
  (search-prefix id (get-identifiers typ))
::
++  tab-list
  |=  [sut=type code=cord]
  %+  bind  (find-type sut (scan (replace-hax (trip code)) vest))
  |=  [id=term typ=type]
  (search-prefix id (get-identifiers typ))
--
