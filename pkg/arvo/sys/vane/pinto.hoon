::  TODO: apply changes to +clay in zuse
::
/-  clay=clay-pinto
=,  contain
=*  care  care:clay
=*  case  case:clay
=>
|%
::  TODO: move to zuse
::
++  able
  |%
  ::  $task: request to ford
  ::
  ::    %make: submit a build request
  ::     desk: filesystem context
  ::     case: ~ if live, or pinned filesystem version
  ::      fiz: hoon files to build
  ::      maz: marks to build into $dais's
  ::      caz: mark conversion gates to construct
  ::    %drop: cancel build
  ::
  +$  task
    $%  $:  %make
            =desk
            case=(unit case:clay)
            fiz=(set path)
            maz=(set mark)
            caz=(set [a=mark b=mark])
        ==
        [%drop ~]
    ==
  ::  $gift: response from ford
  ::
  ::    %made: build result
  ::     case: version at which this result was built
  ::      fiz: (re)built hoon files
  ::      maz: (re)built mark $dais's
  ::      caz: (re)built mark conversion gates
  ::
  +$  gift
    $%  $:  %made
            =case:clay
            fiz=(map path (each vase tang))
            maz=(map mark (each dais tang))
            caz=(map [a=mark b=mark] (each $-(vase vase) tang))
    ==  ==
  --
::  $dais: processed mark core
::
::    TODO: separate +grad core to simplify delegation?
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
::  TODO: fill in
::
+$  move  [=duct =card]
+$  card  (wind note gift:able)
+$  note
  $~  [%c %warp *@p *riff:clay]
  $%  $:  %c
          $>(%warp task:able:clay)
      ==
  ==
+$  sign
  $~  [%c %writ *riot:clay]
  $%  $:  ?(%b %c)
          $>(?(%writ %wris) gift:able:clay)
      ==
  ==
+$  axle
  $:  %0
      =state
  ==
+$  state
  $:  builds=(map duct build-state)
      cache=hoon-cache
  ==
++  build-state
  =/  m-file  (fume ,vase)
  =/  m-mark  (fume ,dais)
  =/  m-cast  (fume ,$-(vase vase))
  $:  build-meta
      sky=(map spar (unit cage))
      $:  fiz=(map path (unit [=spar on-load=form:m-file]))
          fuz=(map path (each vase tang))
      ==
      $:  maz=(map mark (unit [=spar on-load=form:m-mark]))
          muz=(map mark (each dais tang))
      ==
      $:  caz=(map [a=mark b=mark] (unit [=spar on-load=form:m-cast]))
          cuz=(map [a=mark b=mark] (each $-(vase vase) tang))
  ==  ==
::
+$  build-meta  [live=? =desk =case]
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
::  $hoon-cache-key: cache key for a compiler operation
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
::  $line: wire specialized to clay file request identifier type
::
+$  line
  $%  [%fiz =path]
      [%maz =mark ~]
      [%caz a=mark b=mark ~]
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
      $=  o
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
    ?-  -.o.res
      %done  res
      %fail  res(tang.o (welp (print) tang.o.res))
      %load  res(on-load.o this(run on-load.o.res))
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
    ?-    -.o.b-res
      %fail  b-res
      %load  b-res(on-load.o this(m-b on-load.o.b-res))
      %done  ((fun value.o.b-res) [in.fin s.b-res])
    ==
  ::
  ++  fmap
    |*  b=mold
    |=  [fun=$-(b a) m-b=(fume-form-raw b)]
    ^-  form
    ;<  res=b  bind  m-b
    (pure (fun res))
  ::
  +$  eval-res  [out=eval-output state=eval-state]
  ::
  +$  eval-state
    $:  nex=(unit [=spar on-load=form])
        sky=(map spar (unit cage))
    ==
  +$  eval-output
    $%  [%done value=a]
        [%fail =tang]
        [%load =spar]
    ==
  ++  eval
    |=  [our=ship =desk =case scry=sley]
    |=  $:  cache=hoon-cache
            state=eval-state
            fun=form
            fil=(unit (unit cage))
        ==
    ^-  [[out=eval-output state=eval-state] cache=hoon-cache] 
    ::  fresh build should have no response; rerun must have one
    ::
    ?>  =(=(~ nex.state) =(~ fil))
    ::  if no attempt has been made already, try to run the build
    ::
    =^  pro=output  fil
      ?~  nex.state
        [(fun ~ cache) ~]
      [[cache %load u.nex.state] fil]
    ::  loop until %done, %fail, or blocking resource request
    ::
    |^  ^+  [[*eval-output state] cache]
        =.  cache  s.pro
        ?-    -.o.pro
            %done  [[[%done value.o.pro] state] cache]
            %fail  [[[%fail tang.o.pro] state] cache]
            %load
          ::  run .on-load on clay response if we have one
          ::
          ?^  fil
            =.  sky.state  (~(put by sky.state) spar.o.pro u.fil)
            $(pro (on-load.o.pro fil cache), fil ~)
          ::  run .on-load on previously loaded resource if we have one
          ::
          ?^  got=(~(get by sky.state) spar.o.pro)
            $(pro (on-load.o.pro got cache))
          ::  run .on-load on synchronous scry result
          ::
          ?^  res=(scry-for-spar spar.o.pro)
            =.  sky.state  (~(put by sky.state) spar.o.pro u.res)
            $(pro (on-load.o.pro res cache))
          ::  block on asynchronous clay request, storing .on-load
          ::
          [[[%load spar.o.pro] state(nex `[spar on-load]:o.pro)] cache]
        ==
    ++  scry-for-spar
      |=  =spar
      ^-  (unit (unit cage))
      =/  =term  (cat 3 %c care.spar)
      =/  =beam  [[our desk case] (flop path.spar)]
      (scry ** ~ term beam)
    --
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
=>
=-  [at=- .]
|=  [our=ship =desk scry=sley]
=>
::  %make execution routines
::
|%
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
++  parse-pile
  |=  [=path tex=tape]
  =/  m  (fume ,pile)
  ^-  form:m
  ;<  [=mont =nail]  bind:m  (parse-mont tex)
  =/  [=hair res=(unit [src=(list hoon) ^nail])]
    ((most gap tall:(vang & path)) nail)
  ?^  res
    (pure:m [mont tssg+src.u.res])
  (fail:m leaf+"ford: syntax error {<hair>}" ~)
::
++  parse-mont
  |=  tex=tape
  =/  m  (fume ,[mont nail])
  ^-  form:m
  =/  [=hair res=(unit [=mont =nail])]  (mont-rule [1 1] tex)
  ?^  res
    (pure:m u.res)
  (fail:m leaf+"ford: mont syntax error {<hair>}" ~)
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
  ?-  -.o.ran
    %fail  ran
    %done  ran(s (~(put ca s.ran) key value.o.ran))
    %load  ran(on-load.o this(run on-load.o.ran))
  ==
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
      %1  (fail:m leaf+"ford: scry-block {<;;((list path) p.nap)>}" ~)
      %2  (fail:m leaf+"ford: slap-fail" p.nap)
  ==
--
::  %mark execution routines
::
|%
++  run-mark
  |=  =mark
  =/  m  (fume ,dais)
  ^-  form:m
  ;<  cor=vase  bind:m  (load-mark mark)
  ;<  gad=(each ^mark vase)  bind:m  (run-grad cor)
  ?:  ?=(%& -.gad)
    ;<  deg=dais  bind:m  (run-mark p.gad)
    ;<  cas=$-(vase vase)  bind:m  (run-cast mark p.gad)
    ;<  sac=$-(vase vase)  bind:m  (run-cast p.gad mark)
    %-  pure:m
    ^-  dais
    |_  sam=vase
    ++  bunt  (slap cor ^~((ream '+<')))
    ++  diff
      |=  new=vase
      ^-  vase
      (~(diff deg (cas sam)) (cas new))
    ::
    ++  form  form:deg
    ++  join  join:deg
    ++  mash  mash:deg
    ++  pact
      |=  diff=vase
      ^+  sam
      (sac (~(pact deg (cas sam)) diff))
    ::
    ++  vale
      |=  =noun
      ^+  sam
      (slam (slap cor ^~((ream 'noun:grab'))) !>(noun))
    ::
    ++  volt
      |=  =noun
      ^+  sam
      [p:bunt noun]
    --
  ;<  fom=^mark  bind:m  (run-form p.gad)
  %-  pure:m
  ^-  dais
  |_  sam=vase
  ++  bunt  (slap cor ^~((ream '+<')))
  ++  diff
    |=  new=vase
    ^-  vase
    %+  slap
      (with-faces cor+cor sam+sam new+new ~)
    ^~((ream '(diff:~(grad cor sam) new)'))
  ::
  ++  form  fom
  ::
  ++  join
    |=  [a=vase b=vase]
    ^-  (unit (unit vase))
    ?:  =(q.a q.b)
      ~
    =;  res  `?~(q.res ~ `(slap res ^~((ream '?~(. !! u)'))))
    (slam (slap cor ^~((ream 'join:grad'))) (slop a b))
  ::
  ++  mash
    |=  [a=[=ship =^desk diff=vase] b=[=ship =^desk diff=vase]]
    ^-  (unit vase)
    ?:  =(q.diff.a q.diff.b)
      ~
    :-  ~
    %+  slam  (slap cor ^~((ream 'mash:grad')))
    %+  slop
      :(slop !>(ship.a) !>(desk.a) diff.a)
    :(slop !>(ship.b) !>(desk.b) diff.b)
  ::
  ++  pact
    |=  diff=vase
    ^+  sam
    %+  slap
      (with-faces cor+cor sam+sam diff+diff ~)
    ^~((ream '(pact:~(grad cor sam) diff)'))
  ::
  ++  vale
    |=  =noun
    ^+  sam
    (slam (slap cor ^~((ream 'noun:grab'))) !>(noun))
  ::
  ++  volt
    |=  =noun
    ^+  sam
    [p:bunt noun]
  --
::
++  run-cast
  |=  [a=mark b=mark]
  =/  m  (fume ,$-(vase vase))
  ^-  form:m
  !!
::
++  run-cast-old
  |=  [new=mark old=mark arg=vase]
  =/  m  (fume ,cage)
  ^-  form:m
  %+  on-fail:m  |.([leaf+"ford: cast-fail {<old>} -> {<new>}"]~)
  ;<  cor=vase  bind:m  (load-mark new)
  =/  rab  (mule |.((slap cor (ream (cat 3 'grab:' old)))))
  ?:  ?=(%& -.rab)
    %+  on-fail:m  |.([leaf+"ford: grab-fail {<old>} -> {<new>}"]~)
    =/  pro=vase  (slam p.rab arg)
    (pure:m [new pro])
  %+  on-fail:m  |.([leaf+"ford: grow-fail {<old>} -> {<new>}"]~)
  ;<  roc=vase  bind:m  (load-mark old)
  =/  row  (mule |.((slap roc (ream (cat 3 'grow:' new)))))
  ?-  -.row
    %|  (fail:m p.row)
    %&  =/  pro=vase  (slam p.row arg)
        (pure:m [new pro])
  ==
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
  (load-fit /mar/[mark])
--
=<
|=  pit=vase
=|  ax=axle
|=  [our=ship now=@da eny=@ scry-gate=sley]
=*  ford-gate  .
|%
++  call
  |=  [=duct type=* wrapped-task=(hobo task:able)]
  ^-  [(list move) _ford-gate]
  =/  =task:able  ((harden task:able) wrapped-task)
  ?-    -.task
      %make
    =/  live=?  =(~ case.task)
    =/  =case  (fall case.task da+now)
    ::
    =/  m  (fume ,vase)
    =/  =build-state
      =/  fiz
        %+  roll  ~(tap in fiz.task)
        =/  m  (fume ,vase)
        |=  [=path acc=(map path (unit [spar form:m]))]
        (~(put by acc) path ~)
      =/  maz
        %+  roll  ~(tap in maz.task)
        =/  m  (fume ,dais)
        |=  [=mark acc=(map mark (unit [spar form:m]))]
        (~(put by acc) mark ~)
      =/  caz
        %+  roll  ~(tap in caz.task)
        =/  m  (fume ,$-(vase vase))
        |=  [[a=mark b=mark] acc=(map [mark mark] (unit [spar form:m]))]
        (~(put by acc) [a b] ~)
      [[live desk.task case] sky=~ [[fiz fuz=~] [maz muz=~] [caz cuz=~]]]
    =.  builds.state.ax  (~(put by builds.state.ax) duct build-state)
    =/  build-core  (per-build scry-gate cache.state.ax duct build-state)
    =^  remove  build-core  make:build-core 
    =.  cache.state.ax  cache.build-core
    =.  builds.state.ax
      ?:  remove
        (~(del by builds.state.ax) duct)
      (~(put by builds.state.ax) duct build-state:build-core) 
    [fex.build-core ford-gate]
  ::
      %drop
    =.  builds.state.ax  (~(del by builds.state.ax) duct)
    [~ ford-gate]
  ==
++  take
  |=  [=wire =duct type=* wrapped-sign=(hobo sign)]
  ^-  [(list move) _ford-gate]
  =/  =sign  ((harden sign) wrapped-sign)
  ^-  [(list move) _ford-gate]
  ?>  ?=(^ wire)
  ?+    i.wire  ~|(ford-bad-wire+wire !!)
      %load
    =+  ;;(=line t.wire)
    ?>  ?=([%c %writ *] sign)
    =/  fil=(unit cage)  ?~(p.sign ~ `r.u.p.sign)
    =/  =build-state  (~(got by builds.state.ax) duct)
    =/  build-core  (per-build scry-gate cache.state.ax duct build-state)
    =.  build-core
      ?-    -.line
          %fiz
        =/  m  (fume ,vase)
        =/  [=spar on-load=form:m]
          (need (~(got by fiz.build-state) path.line))
        =.  sky.build-state  (~(put by sky.build-state) spar fil)
        =/  =eval-state:m  [`[spar on-load] sky.build-state]
        (make-file:build-core path.line eval-state `fil)
      ::
          %maz
        =/  m  (fume ,dais)
        =/  [=spar on-load=form:m]
          (need (~(got by maz.build-state) mark.line))
        =.  sky.build-state  (~(put by sky.build-state) spar fil)
        =/  =eval-state:m  [`[spar on-load] sky.build-state]
        (make-mark:build-core mark.line eval-state `fil)
      ::
          %caz
        =/  m  (fume ,$-(vase vase))
        =/  [=spar on-load=form:m]
          (need (~(got by caz.build-state) [a b]:line))
        =.  sky.build-state  (~(put by sky.build-state) spar fil)
        =/  =eval-state:m  [`[spar on-load] sky.build-state]
        (make-cast:build-core [a b]:line eval-state `fil)
      ==
    =^  remove  build-core  finalize:build-core
    =.  cache.state.ax  cache.build-core
    =.  builds.state.ax
      ?:  remove
        (~(del by builds.state.ax) duct)
      (~(put by builds.state.ax) duct build-state:build-core) 
    [fex.build-core ford-gate]
  ::
      %live
    ?>  ?=([@tas %wris *] sign)
    =/  =build-state  (~(got by builds.state.ax) duct)
    =.  build-state
      %_  build-state
        case  p.sign
        sky  ~
        fiz  (~(run by fiz.build-state) |=(* ~))
        fuz  ~
        maz  (~(run by maz.build-state) |=(* ~))
        muz  ~
        caz  (~(run by caz.build-state) |=(* ~))
        cuz  ~
      ==
    =/  build-core  (per-build scry-gate cache.state.ax duct build-state)
    =^  remove  build-core  make:build-core
    =.  cache.state.ax  cache.build-core
    =.  builds.state.ax
      ?:  remove
        (~(del by builds.state.ax) duct)
      (~(put by builds.state.ax) duct build-state:build-core)
    [fex.build-core ford-gate]
  ==
++  load
  |=  old=axle
  ford-gate(ax axle)
++  stay  ax
++  scry  |=(* [~ ~])
--
|%
++  per-build
  =|  fex=(list move)
  |=  $:  scry=sley
          cache=hoon-cache
          =duct
          build-state
      ==
  |%
  ++  build-core  .
  ++  build-state  |3.+<.$
  ++  make  =~(make-files make-marks make-casts finalize)
  ::
  ++  finalize
    ^+  [remove=*? build-core]
    ?.  &(=(~ fiz) =(~ maz) =(~ caz))
      [%.n build-core]
    =.  fex  :_(fex [duct %give %made case fuz muz cuz])
    =?  fex  live
      :_  fex
      =/  =rave:clay  [%mult case ~(key by sky)]
      [duct %pass /live %c %warp our desk `rave]
    [!live build-core]
  ::
  ++  make-files
    =/  m  (fume ,vase)
    =/  biz=(list [=path nex=(unit [=spar form:m])])
      ~(tap by fiz)
    |-  ^+  build-core
    ?~  biz  build-core
    =/  =eval-state:m  [nex.i.biz sky]
    $(biz t.biz, build-core (make-file path.i.biz eval-state fil=~))
  ::
  ++  make-marks
    =/  m  (fume ,dais)
    =/  biz=(list [=mark nex=(unit [=spar form:m])])
      ~(tap by maz)
    |-  ^+  build-core
    ?~  biz  build-core
    =/  =eval-state:m  [nex.i.biz sky]
    =.  build-core  (make-mark mark.i.biz eval-state fil=~)
    $(biz t.biz)
  ::
  ++  make-casts
    =/  m  (fume ,$-(vase vase))
    =/  biz=(list [[a=mark b=mark] nex=(unit [=spar form:m])])
      ~(tap by caz)
    |-  ^+  build-core
    ?~  biz  build-core
    =/  =eval-state:m  [nex.i.biz sky]
    =.  build-core  (make-cast [a b]:i.biz eval-state fil=~)
    $(biz t.biz)
  ::
  ++  make-file
    =/  m  (fume ,vase)
    |=  [=path =eval-state:m fil=(unit (unit cage))]
    ^+  build-core
    =/  fun=form:m
      ?^  nex.eval-state
        on-load.u.nex.eval-state
      (run-file:(at our desk scry) path)
    =^  res=eval-res:m  cache
      ((eval:m our desk case scry) cache eval-state fun fil)
    =.  sky  sky.state.res
    ?:  ?=(%load -.out.res)
      =.  fex  :_(fex (pass-load fiz+path spar.out.res))
      =.  fiz  (~(put by fiz) path nex.state.res)
      build-core
    =.  fiz  (~(del by fiz) path)
    =.  fuz
      %+  ~(put by fuz)  path
      ?-  -.out.res
        %done  &+value.out.res
        %fail  |+tang.out.res
      ==
    build-core
  ::
  ++  make-mark
    =/  m  (fume ,dais)
    |=  [=mark =eval-state:m fil=(unit (unit cage))]
    ^+  build-core
    =/  fun=form:m
      ?^  nex.eval-state
        on-load.u.nex.eval-state
      (run-mark:(at our desk scry) mark)
    =^  res=eval-res:m  cache
      ((eval:m our desk case scry) cache eval-state fun fil)
    =.  sky  sky.state.res
    ?:  ?=(%load -.out.res)
      =.  fex  :_(fex (pass-load maz+/[mark] spar.out.res))
      =.  maz  (~(put by maz) mark nex.state.res)
      build-core
    =.  maz  (~(del by maz) mark)
    =.  muz
      %+  ~(put by muz)  mark
      ?-  -.out.res
        %done  &+value.out.res
        %fail  |+tang.out.res
      ==
    build-core
  ::
  ++  make-cast
    =/  m  (fume ,$-(vase vase))
    |=  [[a=mark b=mark] =eval-state:m fil=(unit (unit cage))]
    ^+  build-core
    =/  fun=form:m
      ?^  nex.eval-state
        on-load.u.nex.eval-state
      (run-cast:(at our desk scry) a b)
    =^  res=eval-res:m  cache
      ((eval:m our desk case scry) cache eval-state fun fil)
    =.  sky  sky.state.res
    ?:  ?=(%load -.out.res)
      =.  fex  :_(fex (pass-load caz+/[a]/[b] spar.out.res))
      =.  caz  (~(put by caz) [a b] nex.state.res)
      build-core
    =.  caz  (~(del by caz) [a b])
    =.  cuz
      %+  ~(put by cuz)  [a b]
      ?-  -.out.res
        %done  &+value.out.res
        %fail  |+tang.out.res
      ==
    build-core
  ::
  ++  pass-load
    |=  [=line =spar]
    ^-  move
    =/  =rave:clay  [%sing care.spar case path.spar]
    [duct %pass line %c %warp our desk `rave]
  --
--
