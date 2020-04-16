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
    ::  add the hoon extension to the result
    ::
    =-  ?~(- ~ `(snoc u.- %hoon))
    |-  ^-  (unit path)
    ?>  ?=([* * ~] pax)
    ::  put the prefix back on the result
    ::
    =-  ?~(- ~ `[i.pax u.-])
    =.  nak  (~(got by dir.nak) i.pax)
    ?^  got=(get (snoc t.pax %hoon))
      (some t.pax)
    =/  seg=tape  (trip i.t.pax)
    ?~  dex=(find "-" seg)
      ~
    =/  hed  (crip (scag u.dex seg))
    =/  tal  (crip (slag +(u.dex) seg))
    $(pax /[hed]/[tal])
  --
++  with-face  |=([face=@tas =vase] vase(p [%face face p.vase]))
++  with-faces
  =|  res=(unit vase)
  |=  vaz=(list [face=@tas =vase])
  ^-  vase
  ?~  vaz  (need res)
  =/  faz  (with-face i.vaz)
  =.  res  `?~(res faz (slop faz u.res))
  $(vaz t.vaz)
--
|%
++  ford
  =>  |%
      +$  state
        $:  baked=(map path cage)
            cache=ford-cache
            stack=(list (set path))
        ==
      --
  |=  $:  =ankh
          deletes=(set path)
          changes=(map path (each page lobe))
          file-store=(map lobe blob)
          =ford-cache
      ==
  ::  nub: internal mutable state for this computation
  ::
  =|  nub=state
  =.  cache.nub  ford-cache
  |%
  ::  +pop-stack: pop build stack, copying deps downward
  ::
  ++  pop-stack
    ^-  [(set path) _stack.nub]
    =^  top=(set path)  stack.nub  stack.nub
    =?  stack.nub  ?=(^ stack.nub)
      stack.nub(i (~(uni in i.stack.nub) top))
    [top stack.nub]
  ::
  ++  get-value
    |=  =path
    ^-  [(unit cage) state]
    ?^  cage=(~(get by baked.nub) path)
      [cage nub]
    ?^  change=(~(get by changes) path)
      =^  page  nub
        ?:  ?=(%& -.u.change)
          [p.u.change nub]
        ~|  %ugly-lobe^p.u.change^path
        (lobe-to-page p.u.change)
      =^  cage  nub  (validate-path path page)
      [`cage nub]
    ?:  (~(has in deletes) path)
      [~ nub]
    [(~(get an ankh) path) nub]
  ::  +get-mark: build a mark definition
  ::
  ++  get-mark
    |=  mak=mark
    ^-  [dais state]
    ?^  got=(~(get by marks.cache.nub) mak)
      =?  stack.nub  ?=(^ stack.nub)
        stack.nub(i (~(uni in i.stack.nub) dez.u.got))
      [res.u.got nub]
    =.  stack.nub  [~ stack.nub]
    =^  cor=vase  nub  (build-fit /mar/[mak])
    =;  res=[=dais nub=state]
      =.  nub  nub.res
      =^  top  stack.nub  pop-stack
      =.  marks.cache.nub  (~(put by marks.cache.nub) mak [dais.res top])
      [dais.res nub]
    =/  gad=vase  (slap cor %limb %grad)
    ?@  q.gad
      =+  !<(mok=mark gad)
      =^  deg=dais  nub  $(mak mok)
      =^  tub=tube  nub  (get-cast mak mok)
      =^  but=tube  nub  (get-cast mok mak)
      :_  nub
      ^-  dais
      |_  sam=vase
      ++  bunt  (slap cor $+6)
      ++  diff
        |=  new=vase
        ^-  vase
        (~(diff deg (tub sam)) (tub new))
      ++  form  form:deg
      ++  join  join:deg
      ++  mash  mash:deg
      ++  pact
        |=  diff=vase
        ^+  sam
        (but (~(pact deg (tub sam)) diff))
      ++  vale
        |=  =noun
        ^+  sam
        (slam (slap cor ^~((ream 'noun:grab'))) !>(noun))
      ++  volt
        |=  =noun
        ^+  sam
        [p:bunt noun]
      --
    :_  nub
    =+  !<(fom=mark (slap gad %limb %form))
    ^-  dais
    |_  sam=vase
    ++  bunt  (slap cor $+6)
    ++  diff
      |=  new=vase
      ^-  vase
      %+  slap
        (with-faces cor+cor sam+sam new+new ~)
      ^~((ream '(diff:~(grad cor sam) new)'))
    ++  form  fom
    ++  join
      |=  [a=vase b=vase]
      ^-  (unit (unit vase))
      ?:  =(q.a q.b)
        ~
      =;  res  `?~(q.res ~ `(slap res ^~((ream '?~(. !! u)'))))
      (slam (slap cor ^~((ream 'join:grad'))) (slop a b))
    ++  mash
      |=  [a=[=ship =desk diff=vase] b=[=ship =desk diff=vase]]
      ^-  (unit vase)
      ?:  =(q.diff.a q.diff.b)
        ~
      :-  ~
      %+  slam  (slap cor ^~((ream 'mash:grad')))
      %+  slop
        :(slop !>(ship.a) !>(desk.a) diff.a)
      :(slop !>(ship.b) !>(desk.b) diff.b)
    ++  pact
      |=  diff=vase
      ^+  sam
      %+  slap
        (with-faces cor+cor sam+sam diff+diff ~)
      ^~((ream '(pact:~(grad cor sam) diff)'))
    ++  vale
      |=  =noun
      ^+  sam
      (slam (slap cor ^~((ream 'noun:grab'))) !>(noun))
    ++  volt
      |=  =noun
      ^+  sam
      [p:bunt noun]
    --
  ::  
  ++  get-cast
    |=  [a=mark b=mark]
    ^-  [tube state]
    !!
  ++  lobe-to-page
    |=  =lobe
    ^-  [page state]
    =/  =blob  (~(got by file-store) lobe)
    |-  ^-  [page state]
    ?-    -.blob
        %direct  [q.blob nub]
        %delta
      =/  [[=mark =parent=^lobe] diff=page]  [q r]:blob
      =^  parent-page  nub  $(blob (~(got by file-store) parent-lobe))
      =^  =cage  nub  (run-pact parent-page diff)
      [[p q.q]:cage nub]
    ==
  ++  validate-path
    |=  [=path =page]
    ^-  [cage state]
    =^  =dais  nub  (get-mark p.page)
    =/  res=vase  (vale:dais q.page)
    !!
  ++  cast-path
    |=  [=path =mark]
    ^-  [cage state]
    !!
  ++  run-pact
    |=  [old=page diff=page]
    ^-  [cage state]
    !!
  ++  build-file
    |=  =path
    ^-  [vase state]
    ?^  got=(~(get by vases.cache.nub) path)
      =?  stack.nub  ?=(^ stack.nub)
        stack.nub(i (~(uni in i.stack.nub) dez.u.got))
      [res.u.got nub]
    =.  stack.nub  [(sy path ~) stack.nub]
    =^  cag=(unit cage)  nub  (get-value path)
    ?~  cag  ~|(no-file+path !!)
    ?>  =(%hoon p.u.cag)
    =/  tex=tape  (trip !<(@t q.u.cag))
    =/  =pile  (parse-pile path tex)
    =^  sut=vase  nub  run-reef
    =^  sut=vase  nub  (run-tauts sut %sur sur.mont.pile)
    =^  sut=vase  nub  (run-tauts sut %lib lib.mont.pile)
    =^  sut=vase  nub  (run-raw sut raw.mont.pile)
    =/  res=vase  (slap sut hoon.pile)
    =^  top  stack.nub  pop-stack
    =.  vases.cache.nub  (~(put by vases.cache.nub) path [res top])
    [res nub]
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
    ^-  [vase state]
    ?~  taz  [sut nub]
    =^  pin=vase  nub  (build-fit /[wer]/[pax.i.taz])
    =?  p.pin  ?=(^ face.i.taz)  [%face u.face.i.taz p.pin]
    $(sut (slop pin sut), taz t.taz)
  ::
  ++  run-raw
    |=  [sut=vase raw=(list [face=term =path])]
    ^-  [vase state]
    ?~  raw  [sut nub]
    =^  pin=vase  nub  (build-file path.i.raw)
    =.  p.pin  [%face face.i.raw p.pin]
    $(sut (slop pin sut), raw t.raw)
  ::
  ++  run-reef
    ^-  [vase state]
    [!>(..zuse) nub]  ::  TODO implement
  ::
  ++  build-fit
    |=  pax=path
    ^-  [vase state]
    (build-file ~|(no-file+pax (need (~(get-fit an ankh) pax))))
  --
--
