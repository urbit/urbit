=*  ankh  ankh:clay
=*  blob  blob:clay
=*  lobe  lobe:clay
=*  page  page:clay
=>
|%
::  $ford-cache: memoized builds at a $dome
::
+$  ford-cache
  $:  vases=(map path [res=vase dez=(set path)])
      marks=(map mark [res=dais dez=(set path)])
      casts=(map mars [res=tube dez=(set path)])
  ==
::  $mars: mark conversion request
::  $tube: mark conversion gate
::
+$  mars  [a=mark b=mark]
+$  tube  $-(vase vase)
::  $dais: processed mark core
::
+$  dais
  $_  ^|
  |_  sam=vase
  ++  bunt  sam
  ++  diff  |~(new=_sam *vase)
  ++  form  *mark
  ++  join  |~([a=vase b=vase] *(unit (unit vase)))
  ++  mash
    |~  [a=[ship desk diff=vase] b=[ship desk diff=vase]]
    *(unit vase)
  ++  pact  |~(diff=vase sam)
  ++  vale  |~(noun sam)
  ++  volt  |~(noun sam)
  --
::  $pile: preprocessed hoon source file
::
+$  pile
  $:  =mont
      =hoon
  ==
::  $mont: imports declaration
::
::    /-  imports from /sur
::    /+  imports from /lib
::    /=  imports from some other path
::
+$  mont
  $:  sur=(list taut)
      lib=(list taut)
      raw=(list [face=term =path])
  ==
::  $taut: file import from /lib or /sur
::
+$  taut  [face=(unit term) pax=term]
::  +an: $ankh interface door
::
++  an
  |_  nak=ankh
  ::  +get: produce file at path
  ::
  ++  get
    |=  =path
    ^-  (unit cage)
    ?~  path
      ?~  fil.nak
        ~
      `q.u.fil.nak
    ?~  kid=(~(get by dir.nak) i.path)
      ~
    $(nak u.kid, path t.path)
  ::  +get-fit: produce file at path with /'s maybe converted to -'s
  ::
  ++  get-fit
    |=  pax=path
    ^-  (unit path)
    ?>  ?=([* * ~] pax)
    =-  ?~(- ~ `[i.pax u.-])
    =.  nak  (~(got by dir.nak) i.pax)
    ?^  got=(get t.pax)
      (some t.pax)
    =/  seg=tape  (trip i.t.pax)
    ?~  dex=(find "-" seg)
      ~
    =/  hed  (crip (scag u.dex seg))
    =/  tal  (crip (slag +(u.dex) seg))
    $(pax /[hed]/[tal])
  --
--
|%
++  ford
  |=  $:  =ankh
          deletes=(set path)
          changes=(map path (each page lobe))
          file-store=(map lobe blob)
          =ford-cache
      ==
  =/  state
    :*  baked=*(map path cage)
        cache=ford-cache
        stack=*(list (set path))
    ==
  |%
  ::  +pop-stack: pop build stack, copying deps downward
  ::
  ++  pop-stack
    ^-  [(set path) _stack.state]
    =^  top=(set path)  stack.state  stack.state
    =?  stack.state  ?=(^ stack.state)
      stack.state(i (~(uni in i.stack.state) top))
    [top stack.state]
  ::
  ++  get-value
    |=  =path
    ^-  [(unit cage) _state]
    ?^  cage=(~(get by baked.state) path)
      [cage state]
    ?^  change=(~(get by changes) path)
      =^  page  state
        ?:  ?=(%& -.u.change)
          [p.u.change state]
        ~|  %ugly-lobe^p.u.change^path
        (lobe-to-page p.u.change)
      =^  cage  state  (validate-path path page)
      [`cage state]
    ?:  (~(has in deletes) path)
      [~ state]
    [(~(get an ankh) path) state]
  ::  +get-mark: build a mark definition
  ::
  ::   TODO: convert - to /
  ::
  ++  get-mark
    |=  =mark
    ^-  [dais _state]
    =^  cor=vase  state  (build-file /mar/[mark])
    !!
  ::  
  ++  get-cast
    |=  [a=mark b=mark]
    ^-  [tube _state]
    !!
  ++  lobe-to-page
    |=  =lobe
    ^-  [page _state]
    =/  =blob  (~(got by file-store) lobe)
    |-  ^-  [page _state]
    ?-    -.blob
        %direct  [q.blob state]
        %delta
      =/  [[=mark =parent=^lobe] diff=page]  [q r]:blob
      =^  parent-page  state  $(blob (~(got by file-store) parent-lobe))
      =^  =cage  state  (run-pact parent-page diff)
      [[p q.q]:cage state]
    ==
  ++  validate-path
    |=  [=path =page]
    ^-  [cage _state]
    !!
  ++  cast-path
    |=  [=path =mark]
    ^-  [cage _state]
    !!
  ++  run-pact
    |=  [old=page diff=page]
    ^-  [cage _state]
    !!
  ++  build-file
    |=  =path
    ^-  [vase _state]
    ?^  got=(~(get by vases.cache.state) path)
      [res.u.got state]
    =.  stack.state  [(sy path ~) stack.state]
    =^  cag=(unit cage)  state  (get-value path)
    ?~  cag  ~|(no-file+path !!)
    ?>  =(%hoon p.u.cag)
    =/  tex=tape  (trip !<(@t q.u.cag))
    =/  =pile  (parse-pile path tex)
    =^  sut=vase  state  run-reef
    =^  sut=vase  state  (run-tauts sut %sur sur.mont.pile)
    =^  sut=vase  state  (run-tauts sut %lib lib.mont.pile)
    =^  sut=vase  state  (run-raw sut raw.mont.pile)
    =/  res=vase  (slap sut hoon.pile)
    =^  top  stack.state  pop-stack
    =.  vases.cache.state  (~(put by vases.cache.state) path [res top])
    [res state]
  ::
  ++  parse-pile
    |=  [=path tex=tape]
    ^-  pile
    ~|  parse-fail+path  ::  TODO: better error reporting
    =/  [=mont =nail]  (parse-mont tex)
    =/  [=hair res=(unit [src=(list hoon) ^nail])]
      ((most gap tall:(vang & path)) nail)
    [mont tssg+src:(need res)]
  ::
  ++  parse-mont
    |=  tex=tape
    ^-  [mont nail]
    =/  [=hair res=(unit [=mont =nail])]  (mont-rule [1 1] tex)
    (need res)  ::  TODO: better error reporting
  ::
  ++  mont-rule
    %+  ifix  [gay gay]
    %+  cook  |=(mont +<)
    ;~  plug
      %+  cook  |=((list (list taut)) (zing +<))
      %+  more  gap
      %+  ifix  [;~(plug net hep gap) gap]
      (most ;~(plug com gaw) taut-rule)
    ::
      %+  cook  |=((list (list taut)) (zing +<))
      %+  more  gap
      %+  ifix  [;~(plug net lus gap) gap]
      (most ;~(plug com gaw) taut-rule)
    ::
      %+  cook  |=((list [face=term =path]) +<)
      %+  more  gap
      %+  ifix  [;~(plug net tis gap) gap]
      %+  cook  |=([term path] +<)
      ;~(plug sym ;~(pfix ;~(plug gap net) (more net urs:ab)))
    ==
  ::
  ++  taut-rule
    %+  cook  |=(taut +<)
    ;~  pose
      (stag ~ ;~(pfix tar sym))
      ;~(plug (stag ~ sym) ;~(pfix tis sym))
      (cook |=(a=term [`a a]) sym)
    ==
  ::
  ++  run-tauts
    |=  [sut=vase wer=?(%lib %sur) taz=(list taut)]
    ^-  [vase _state]
    ?~  taz  [sut state]
    =^  pin=vase  state  (build-fit /[wer]/[pax.i.taz])
    =?  p.pin  ?=(^ face.i.taz)  [%face u.face.i.taz p.pin]
    $(sut (slop pin sut), taz t.taz)
  ::
  ++  run-raw
    |=  [sut=vase raw=(list [face=term =path])]
    ^-  [vase _state]
    ?~  raw  [sut state]
    =^  pin=vase  state  (build-file path.i.raw)
    =.  p.pin  [%face face.i.raw p.pin]
    $(sut (slop pin sut), raw t.raw)
  ::
  ++  run-reef
    ^-  [vase _state]
    [!>(..zuse) state]  ::  TODO implement
  ::
  ++  build-fit
    |=  pax=path
    ^-  [vase _state]
    (build-file ~|(no-file+pax (need (~(get-fit an ankh) pax))))
  --
--
