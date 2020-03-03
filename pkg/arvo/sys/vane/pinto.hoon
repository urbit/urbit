=,  contain
=*  care  care:clay
=*  case  case:clay
=>
|%
::  TODO: move to zuse
::
+$  plan
  $^  [hed=plan tal=plan]
  $%  [%ride sut=plan gen=hoon]
      [%call gat=plan sam=plan]
      [%load =spar]
      [%grok =path]
      [%$ =cage]
  ==
+$  pile  (list pike)
+$  pike
  $%  [%'/-' =taut]                           ::  surface import
      [%'/+' =taut]                           ::  library import
      [%'/=' lal=term =pike]                  ::  wrap face around .pike
      [%'/~' =hoon]                           ::  hoon against subject
      [%'/;' gat=hoon =pike]                  ::  call gate on .pike
      [%'//' marks=(list mark) =pike]         ::  mark conversion sequence
      [%'/^' typ=hoon =pike]                  ::  cast .pike result
      [%'/$' ~]                               ::  read renderer arg
      [%'/!' pax=hoon]                        ::  compile hoon file
      [%'/@' pax=hoon]                        ::  load file
      [%'/*' pax=hoon ren=term]               ::  renderer on file tree
      [%'/%' pax=hoon marks=(list mark)]      ::  file tree through marks
  ==
+$  taut  [face=(unit term) pax=term]
::  $hoon-cache: cache for compiler operations
::
+$  hoon-cache  (clock hoon-cache-key vase)
::  +hoon-cache-key: cache key for a compiler operation
::
::    %call: +slam (vase of) gate on (vase of) sample
::    %hood: parse file into $scaffold; use !<(scaffold val)
::    %ride: +slap $hoon against $vase
::    %slim: compile +slap; use !<([type nock] val)
::    %slit: infer +slam product type; use !<(type val)
::
+$  hoon-cache-key
  $%  [%call gate=vase sample=vase]
      [%hood =beam txt=@t]
      [%ride subject=vase formula=hoon]
      [%slim subject-type=type formula=hoon]
      [%slit gate=type sample=type]
  ==
+$  product  (unit (each cage tang))
+$  build
  $:  live=?
      =desk
      =case
      =plan
  ==
+$  build-state
  $:  fum=(unit [=spar on-load=(fume-form-raw ,cage)])
      cur=(map spar (unit cage))
      pre=(unit [=case:clay resources=(map spar (unit cage))])
  ==
::  $spar: clay scry request on unspecified $beak
::
+$  spar  [=care =path]
::  $fume-input: input to a monadic build operation
::
::    in: ~ if initial input to kick off computation
::
+$  fume-input  [in=(unit (unit cage)) s=fume-state]
+$  fume-state  hoon-cache
++  fume-output-raw
  |*  a=mold
  $~  [*fume-state %done *a]
  $:  s=fume-state
      $=  next
      $%  [%done value=a]
          [%fail =tang]
          [%load =spar on-load=(fume-form-raw a)]
  ==  ==
++  fume-form-raw
  |*  a=mold
  $-(fume-input (fume-output-raw a))
::
++  fume
  |*  a=mold
  |%
  ++  output  (fume-output-raw a)
  ++  form    (fume-form-raw a)
  ++  pure
    |=  [arg=a]
    ^-  form
    |=  fume-input
    [s %done arg]
  ::
  ++  fail
    |=  =tang
    ^-  form
    |=  fume-input
    [s %fail tang]
  ::
  ++  on-fail
    |=  [print=(trap tang) run=form]
    ^-  form
    |=  in=fume-input
    =*  this  .
    =/  res  (run in)
    ?-  -.next.res
      %done  res
      %fail  res(tang.next (welp (print) tang.next.res))
      %load  res(on-load.next this(run on-load.next.res))
    ==
  ::
  ++  bind
    |*  b=mold
    |=  [m-b=(fume-form-raw b) fun=$-(b form)]
    ^-  form
    |=  fin=fume-input
    =*  this  .
    =/  b-res  (m-b fin)
    ^-  output
    ?-    -.next.b-res
      %fail  b-res
      %load  b-res(on-load.next this(m-b on-load.next.b-res))
      %done  ((fun value.next.b-res) [in.fin s.b-res])
    ==
  ::
  ++  fmap
    |*  b=mold
    |=  [fun=$-(b a) m-b=(fume-form-raw b)]
    ^-  form
    ;<  res=b  bind  m-b
    (pure (fun res))
  ::
  ++  try
    |=  [x=form y=form]
    ^-  form
    |=  in=fume-input
    =*  this  .
    =/  x-res  (x in)
    ?-    -.next.x-res
        %done  x-res
        %load  x-res(on-load.next this(x on-load.next.x-res))
        %fail
      =/  y-res  (y in)
      :-  s.y-res
      ?-  -.next.y-res
        %done  next.y-res
        %fail  [%fail (welp tang.next.x-res tang.next.y-res)]
        %load  next.y-res(on-load this(y on-load.next.y-res))
      ==
    ==
  --
::
++  lift-vase
  =/  mc  (fume ,cage)
  =/  mv  (fume ,vase)
  |=  run=form:mv
  ^-  form:mc
  ((fmap:mc ,vase) vase-to-cage run)
::
++  vase-to-cage  (bake (with-mark %noun) vase)
++  with-mark  |=(=mark |*(* [mark +<]))
::
++  parse-pile
  |=  =path
  =/  hoon-parser  (vang & path)
  %+  ifix  [gay gay]
  %+  cook  |=((list pile) (zing +<))
  %+  most  gap
  %+  cook  |=(pile +<)
  ;~  pose
    %+  ifix  [;~(plug net hep gap) gap]
    (most ;~(plug com gaw) (stag %'/-' parse-taut))
  ::
    %+  ifix  [;~(plug net lus gap) gap]
    (most ;~(plug com gaw) (stag %'/+' parse-taut))
  ::
    %+  cook  |=(pike `pile`~[+<])
    (stag %'/~' tall:hoon-parser)
  ==
++  parse-taut
  %+  cook  |=(taut +<)
  ;~  pose
    (stag ~ ;~(pfix tar sym))
    (cook |=([face=term tis=@ file=term] [`face file]) ;~(plug sym tis sym))
    (cook |=(a=term [`a a]) sym)
  ==
--
|=  =hoon-cache
|=  [our=ship =desk now=@da scry=sley]
|%
++  run-root-build
  |=  [=build state=build-state in=(unit (unit cage))]
  ^-  [=product =build-state =^hoon-cache]
  =/  m  (fume ,cage)
  ::  fresh build should have no response; rerun must have one
  ::
  ?>  =(=(~ fum.state) =(~ in))
  ::  if no attempt has been made already, try to run the build
  ::
  =^  pro=output:m  in
    ?^  fum.state
      [[hoon-cache %load u.fum.state] in]
    [((make plan.build) in hoon-cache) ~]
  ::
  |-  ^-  [product build-state ^hoon-cache]
  =.  hoon-cache  s.pro
  ?-    -.next.pro
      %done  [`&+value.next.pro state hoon-cache]
      %fail  [`|+tang.next.pro state hoon-cache]
      %load
    ::  run .on-load on clay response if we have one
    ::
    ?^  in
      =.  cur.state  (~(put by cur.state) spar.next.pro u.in)
      $(pro (on-load.next.pro in hoon-cache), in ~)
    ::  run .on-load on previously loaded resource if we have one
    ::
    ?^  got=(~(get by cur.state) spar.next.pro)
      $(pro (on-load.next.pro got hoon-cache))
    ::  run .on-load on result of scry if it completes synchronously
    ::
    ?^  res=(scry-for-spar spar.next.pro)
      =.  cur.state  (~(put by cur.state) spar.next.pro u.res)
      $(pro (on-load.next.pro res hoon-cache))
    ::  block, storing .on-load in .fum.state
    ::
    [~ state(fum `[spar on-load]:next.pro) hoon-cache]
  ==
::
++  make
  |=  =plan
  =/  m  (fume ,cage)
  ^-  form:m
  ?-  -.plan
    ^      (make-cell plan)
    %$     (pure:m cage.plan)
    %call  (make-call +.plan)
    %grok  (make-grok +.plan)
    %load  (make-load +.plan)
    %ride  (make-ride +.plan)
  ==
::
++  make-cell
  |=  [a=plan b=plan]
  =/  m  (fume ,cage)
  ^-  form:m
  ;<  [mark hed=vase]  bind:m  (make a)
  ;<  [mark tal=vase]  bind:m  (make b)
  (pure:m noun+(slop hed tal))
::
++  make-call  |=([gat=plan sam=plan] (lift-vase (run-call gat sam)))
++  run-call
  |=  [gat=plan sam=plan]
  =/  m  (fume ,vase)
  ^-  form:m
  ;<  [mark got=vase]  bind:m  (make gat)
  ;<  [mark som=vase]  bind:m  (make sam)
  %+  with-cache-key  [%call got som]
  ;<  sit=vase  bind:m
    %+  with-cache-key  [%slit p.got p.som]
    %+  on-fail:m  |.([leaf+"ford: slit-fail"]~)
    =/  lap  (mule |.((slit p.got p.som)))
    ?-  -.lap
      %&  (pure:m !>(p.lap))
      %|  (fail:m p.lap)
    ==
  =+  !<(gol=type sit)
  =/  vap  (mong [q.got q.som] (sloy scry))
  ?-  -.vap
    %0  (pure:m [gol p.vap])
    %1  (fail:m leaf+"ford: scry-block {<p.vap>}" ~)
    %2  (fail:m leaf+"ford: call-fail" p.vap)
  ==
::
++  make-grok
  |=  =path
  =/  m  (fume ,cage)
  ((fmap:m ,pile) |=(p=pile noun+!>(p)) (run-grok path))
::
++  run-grok
  |=  =path
  =/  m  (fume ,pile)
  ^-  form:m
  ;<  [=mark xet=vase]  bind:m  (make-load %x path)
  ?>  =(%hoon mark)
  =+  !<(tex=@t xet)
  =/  par  ((full (parse-pile path)) [1 1] (trip tex))
  ?~  q.par
    (fail:m [leaf+"syntax error at TODO"]~)
  (pure:m p.u.q.par)
::
++  make-load
  |=  =spar
  =/  m  (fume ,cage)
  ^-  form:m
  |=  fin0=fume-input
  :^  s.fin0  %load  spar
  ^-  form:m
  |=  fin1=fume-input
  ?>  ?=(^ in.fin1)
  ?~  u.in.fin1
    [s.fin1 %fail ~[leaf+"ford: load-fail {<spar>}"]]
  [s.fin1 %done u.u.in.fin1]
::
++  run-reef
  =/  m  (fume ,vase)
  ^-  form:m
  ;<  hun=vase  bind:m  (load-hoon-raw /sys/hoon/hoon !>(~))
  ;<  rav=vase  bind:m  (load-hoon-raw /sys/arvo/hoon (slot 7 hun))
  (load-hoon-raw /sys/zuse/hoon (slap rav ^~((ream '..is'))))
::
++  load-hoon-raw
  |=  [=path sut=vase]
  =/  m  (fume ,vase)
  ^-  form:m
  ;<  [=mark xet=vase]  bind:m  (make-load %x path)
  ?>  ?=(%hoon mark)
  =/  tex  !<(@t xet)
  =/  gen  (rain path tex)
  (run-ride sut gen)
::
++  make-ride
  |=  [sut=plan gen=hoon]
  =/  m  (fume ,cage)
  ;<  [mark vut=vase]  bind:m  (make sut)
  %-  lift-vase
  (run-ride vut gen)
::
++  run-ride
  |=  [sut=vase gen=hoon]
  =/  m  (fume ,vase)
  ^-  form:m
  %+  with-cache-key  [%ride sut gen]
  ;<  sim=vase  bind:m
    %+  with-cache-key  [%slim p.sut gen]
    %+  on-fail:m  |.([leaf+"ford: slim-fail"]~)
    =/  lap  (mule |.((~(mint ut p.sut) %noun gen)))
    ?-  -.lap
      %&  (pure:m !>(p.lap))
      %|  (fail:m p.lap)
    ==
  =+  !<([gol=type fol=nock] sim)
  =/  nap  (mock [q.sut fol] (sloy scry))
  ?-  -.nap
    %0  (pure:m [gol p.nap])
    %1  (fail:m leaf+"ford: scry-block {<p.nap>}" ~)
    %2  (fail:m leaf+"ford: ride-fail" p.nap)
  ==
::
++  with-cache-key
  =/  m  (fume ,vase)
  |=  [key=hoon-cache-key run=form:m]
  ^-  form:m
  |=  in=fume-input
  =*  this  .
  =/  ca  (by-clock hoon-cache-key vase)
  =^  val  s.in  (get:ca key)
  ?^  val
    [s.in %done u.val]
  =/  ran  (run in)
  ?-  -.next.ran
    %fail  ran
    %done  ran(s (~(put ca s.ran) key value.next.ran))
    %load  ran(on-load.next this(run on-load.next.ran))
  ==
::
++  scry-for-spar
  |=  =spar
  ^-  (unit (unit cage))
  =/  =term  (cat 3 %c care.spar)
  =/  =beam  [[our desk da+now] (flop path.spar)]
  (scry ** ~ term beam)
--
