=,  contain
=*  case  case:clay
=>
|%
::  TODO: use $care:clay
::
+$  care  ?(%s %t %u %v %w %x %y %z)
::  TODO: move to zuse
::
++  able
  |%
  ::  $task: build request
  ::
  ::    %make: build hoon files
  ::    %mark: perform a mark operation
  ::    %drop: cancel build
  ::
  +$  task
    $%  [%make =desk case=(unit case:clay) all=(set path)]
        [%mark =desk all=(map path plan)]
        [%drop ~]
    ==
  ::  $gift: build result
  ::
  ::    %made: %make build results
  ::    %mark: %mark build results
  ::
  +$  gift
    $%  [%make all=(map path (each vase tang))]
        [%mark all=(map path (each cage tang))]
    ==
  --
::  $plan: mark-related build plan
::
::    %$:    literal $cage
::    %bunt: default value for mark
::    %cast: convert .arg from mark .a to .b
::    %diff: calculate diff from .old to .new
::    %join: merge two diffs into one
::    %mash: annotate merge conflicts
::    %pact: apply a diff
::    %vale: validate .noun to .mark
::    %volt: .noun as .mark, unvalidated
::
+$  plan
  $%  [%$ =cage]
      [%bunt =mark]
      [%cast =mark =plan]
      [%diff start=plan end=plan]
      [%join =mark one=plan two=plan]
      [%mash =mark a=[=desk =plan] b=[=desk =plan]]
      [%pact sut=plan dif=plan]
      [%vale =mark =noun]
      [%volt =mark =noun]
  ==
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
::  $hoon-cache: cache for compiler operations
::
+$  hoon-cache  (clock hoon-cache-key vase)
::  $make-cache-key: unique cache key for part of a %make build
::
::    %slap: eval (+slap) a hoon against a subject vase
::    %mint: compile (+mint) a hoon against a subject type
::    %rain: parse (+rain) hoon source at a filepath
::
::    TODO: do we need anything other than %slap and a file build cache?
::    TODO: use %rain cache
::
+$  hoon-cache-key
  $%  [%slap sut=vase gen=hoon]
      [%mint sut=type gen=hoon]
      [%rain pax=path =nail]
  ==
::
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
    |=  arg=a
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
  |=  [=path tex=tape]
  =/  m  (fume ,pile)
  ^-  form:m
  ;<  [=mont =nail]  bind:m  (parse-mont tex)
  =/  [=hair res=(unit [src=(list hoon) ^nail])]
    ((most gap tall:(vang & path)) nail)
  ?^  res
    (pure:m [mont tssg+src.u.res])
  (fail:m leaf+"syntax error {<hair>}" ~)
::
++  parse-mont
  |=  tex=tape
  =/  m  (fume ,[mont nail])
  ^-  form:m
  =/  [=hair res=(unit [=mont =nail])]  (mont-parser [1 1] tex)
  ?^  res
    (pure:m u.res)
  (fail:m leaf+"syntax error {<hair>}" ~)
::
++  mont-parser
  %+  ifix  [gay gay]
  %+  cook  |=(mont +<)
  ;~  plug
    %+  cook  |=((list (list taut)) (zing +<))
    %+  most  gap
    %+  ifix  [;~(plug net hep gap) gap]
    (most ;~(plug com gaw) taut-parser)
  ::
    %+  cook  |=((list (list taut)) (zing +<))
    %+  most  gap
    %+  ifix  [;~(plug net lus gap) gap]
    (most ;~(plug com gaw) taut-parser)
  ::
    %+  cook  |=((list [face=term =path]) +<)
    %+  most  gap
    %+  ifix  [;~(plug net tis gap) gap]
    %+  cook  |=([term path] +<)
    ;~(plug sym ;~(pfix ;~(plug gap net) (more net urs:ab)))
  ==
::
++  taut-parser
  %+  cook  |=(taut +<)
  ;~  pose
    (stag ~ ;~(pfix tar sym))
    ;~(plug (stag ~ sym) ;~(pfix tis sym))
    (cook |=(a=term [`a a]) sym)
  ==
--
::  TODO vane interface
::
=|  =hoon-cache
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
    %$     (pure:m cage.plan)
    %bunt  (make-bunt +.plan)
    %cast  (make-cast +.plan)
    %diff  (make-diff +.plan)
    %join  (make-join +.plan)
    %mash  !!  ::  TODO: write this
    %pact  (make-pact +.plan)
    %vale  (make-vale +.plan)
    %volt  (make-volt +.plan)
  ==
::
++  make-bunt
  |=  =mark
  =/  m  (fume ,cage)
  ^-  form:m
  ;<  cor=vase  bind:m  (load-mark mark)
  (pure:m [mark (slap cor ^~((ream '+<')))])
::  TODO: redo caching, maybe in some other way
::
++  do-slam
  |=  [gat=vase sam=vase]
  =/  m  (fume ,vase)
  ^-  form:m
  ::%+  with-cache-key  [%slam gat sam]
  ;<  sit=vase  bind:m
    ::%+  with-cache-key  [%slit p.gat p.sam]
    %+  on-fail:m  |.([leaf+"ford: slit-fail"]~)
    =/  lap  (mule |.((slit p.gat p.sam)))
    ?-  -.lap
      %&  (pure:m !>(p.lap))
      %|  (fail:m p.lap)
    ==
  =+  !<(gol=type sit)
  =/  vap  (mong [q.gat q.sam] (sloy scry))
  ?-  -.vap
    %0  (pure:m [gol p.vap])
    %1  (fail:m leaf+"ford: scry-block {<p.vap>}" ~)
    %2  (fail:m leaf+"ford: call-fail" p.vap)
  ==
::
++  make-cast
  |=  [new=mark =plan]
  =/  m  (fume ,cage)
  ^-  form:m
  ;<  [old=mark arg=vase]  bind:m  (make plan)
  %+  on-fail:m  |.([leaf+"ford: cast-fail {<old>} -> {<new>}"]~)
  ;<  cor=vase  bind:m  (load-mark new)
  =/  rab  (mule |.((slap cor (ream (cat 3 'grab:' old)))))
  ?:  ?=(%& -.rab)
    %+  on-fail:m  |.([leaf+"ford: grab-fail {<old>} -> {<new>}"]~)
    ;<  pro=vase  bind:m  (do-slam p.rab arg)
    (pure:m [new pro])
  %+  on-fail:m  |.([leaf+"ford: grow-fail {<old>} -> {<new>}"]~)
  ;<  roc=vase  bind:m  (load-mark old)
  =/  row  (mule |.((slap roc (ream (cat 3 'grow:' new)))))
  ?-  -.row
    %|  (fail:m p.row)
    %&  ;<  pro=vase  bind:m  (do-slam p.row arg)
        (pure:m [new pro])
  ==
::
++  make-diff
  |=  [start=plan end=plan]
  =*  loop  $
  =/  m  (fume ,cage)
  ^-  form:m
  ;<  uno=cage  bind:m  (make start)
  ;<  dos=cage  bind:m  (make end)
  ?:  =([p q.q]:uno [p q.q]:dos)
    (pure:m [%null !>(~)])
  ;<  cor=vase  bind:m  (load-mark p.uno)
  ;<  deg=(each mark vase)  bind:m  (run-grad cor)
  ?:  ?=(%& -.deg)
    loop(start [%cast p.deg $+uno], end [%cast p.deg $+dos])
  =/  sut=vase  (slop p.deg q.uno)
  =/  gat  (mule |.((slap sut ^~((ream 'diff:~(grad - +)')))))
  ?:  ?=(%| -.gat)
    (fail:m leaf+"ford: grad-diff {<p.uno>}" p.gat)
  ;<  fom=mark  bind:m  (run-form p.deg)
  ;<  dif=vase  bind:m  (do-slam p.gat q.dos)
  (pure:m [fom dif])
::
++  run-file
  |=  =path
  =/  m  (fume ,vase)
  ^-  form:m
  ;<  [mark xet=vase]  bind:m  (load-spar %x path)
  =/  tex=tape  (trip !<(@t xet))
  ;<  =pile  bind:m  (parse-pile path tex)
  ;<  sut=vase  bind:m  run-reef
  ;<  sut=vase  bind:m  (run-tauts sut %sur sur.mont.pile)
  ;<  sut=vase  bind:m  (run-tauts sut %lib lib.mont.pile)
  ;<  sut=vase  bind:m  (run-raws sut raw.mont.pile)
  (run-slap sut hoon.pile)
::
++  run-tauts
  |=  [sut=vase wer=?(%lib %sur) taz=(list taut)]
  =/  m  (fume ,vase)
  ^-  form:m
  =*  loop  $
  ?~  taz  (pure:m sut)
  ;<  pin=vase  bind:m  (load-fit /[wer]/[pax.i.taz])
  =?  p.pin  ?=(^ face.i.taz)  [%face u.face.i.taz p.pin]
  loop(sut (slop pin sut), taz t.taz)
::
++  run-raws
  |=  [sut=vase raw=(list [face=term =path])]
  =/  m  (fume ,vase)
  ^-  form:m
  =*  loop  $
  ?~  raw  (pure:m sut)
  ;<  pin=vase  bind:m  (run-file path.i.raw)
  =.  p.pin  [%face face.i.raw p.pin]
  loop(sut (slop pin sut), raw t.raw)
::
++  make-join
  |=  [=mark one=plan two=plan]
  =*  loop  $
  =/  m  (fume ,cage)
  ^-  form:m
  ;<  cor=vase  bind:m  (load-mark mark)
  ;<  gad=(each @tas vase)  bind:m  (run-grad cor)
  ?:  ?=(%& -.gad)
    loop(mark p.gad)
  ;<  uno=cage  bind:m  (make one)
  ;<  dos=cage  bind:m  (make two)
  ;<  fom=@tas  bind:m  (run-form p.gad)
  ?.  &(=(fom p.uno) =(fom p.dos))
    (fail:m leaf+"ford: join-mark" ~)
  ?:  =(q.q.uno q.q.dos)
    (pure:m uno)
  =/  gat  (mule |.((slap p.gad ^~((ream 'join')))))
  ?:  ?=(%| -.gat)
    (fail:m leaf+"ford: join-gate" p.gat)
  ;<  dif=vase  bind:m  (do-slam p.gat (slop q.uno q.dos))
  ?~  q.dif
    (pure:m [%null dif])
  (pure:m [fom dif])
::
++  load-spar
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
++  make-pact
  |=  [sut=plan dif=plan]
  =*  loop  $
  =/  m  (fume ,cage)
  ^-  form:m
  ;<  sot=cage  bind:m  (make sut)
  ;<  cor=vase  bind:m  (load-mark p.sot)
  ;<  gad=(each mark vase)  bind:m  (run-grad cor)
  ?:  ?=(%& -.gad)
    (make %cast p.sot %pact [%cast p.gad $+sot] dif)
  ;<  fom=mark  bind:m  (run-form p.gad)
  ;<  fid=cage  bind:m  (make dif)
  ?.  =(fom p.fid)
    (fail:m leaf+"ford: pact-mark" ~)
  =/  sit=vase  (slop cor q.sot)
  =/  gat  (mule |.((slap sit ^~((ream 'pact:~(grad - +)')))))
  ?:  ?=(%| -.gat)
    (fail:m leaf+"ford: grad-pact {<p.sot>}" p.gat)
  ;<  pac=vase  bind:m  (do-slam p.gat q.fid)
  (pure:m [p.sot pac])
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
  ;<  [=mark tex=vase]  bind:m  (load-spar %x path)
  ?>  ?=(%hoon mark)
  =/  gen  (rain path !<(@t tex))
  (run-slap sut gen)
::
++  run-slap
  |=  [sut=vase gen=hoon]
  =/  m  (fume ,vase)
  ^-  form:m
  %+  with-cache-key  [%slap sut gen]
  ;<  sim=vase  bind:m
    %+  with-cache-key  [%mint p.sut gen]
    %+  on-fail:m  |.([leaf+"ford: mint-fail"]~)
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
    %2  (fail:m leaf+"ford: slap-fail" p.nap)
  ==
::
++  make-vale
  |=  [=mark =noun]
  =/  m  (fume ,cage)
  ^-  form:m
  ;<  cor=vase  bind:m  (load-mark mark)
  =/  gat=vase  (slap cor ^~((ream 'noun:grab')))
  =/  sam=vase  !>(noun)
  ;<  pro=vase  bind:m  (do-slam gat sam)
  (pure:m [mark pro])
::
++  make-volt
  |=  [=mark =noun]
  =/  m  (fume ,cage)
  ^-  form:m
  ;<  cag=cage  bind:m  (make-bunt mark)
  (pure:m cag(q.q noun))
::
++  run-form
  |=  gad=vase
  =/  m  (fume ,mark)
  ^-  form:m
  =/  fom  (mule |.((slap gad ^~((ream '`@tas`form')))))
  ?:  ?=(%| -.fom)
    (fail:m leaf+"ford: form-fail" p.fom)
  (pure:m !<(mark p.fom))
::
++  run-grad
  |=  cor=vase
  =/  m  (fume ,(each mark vase))
  ^-  form:m
  =/  gad  (mule |.((slap cor ^~((ream 'grad')))))
  ?:  ?=(%| -.gad)
    (fail:m leaf+"ford: no +grad" p.gad)
  ?^  q.p.gad
    (pure:m |+p.gad)
  ?~  deg=((sand %tas) q.p.gad)
    (fail:m leaf+"ford: grad-type" ~)
  (pure:m &+u.deg)
::
++  load-mark
  |=  =mark
  =/  m  (fume ,vase)
  ^-  form:m
  %+  on-fail:m  |.([leaf+"ford: load-mark {(trip mark)}"]~)
  (load-fit /[mark])
::
++  load-fit
  |=  pax=path
  =/  m  (fume ,vase)
  ^-  form:m
  ;<  [mark pox=vase]  bind:m  (load-spar %s pax)
  (run-file !<(path pox))
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
