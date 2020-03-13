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
  ::  $task: build request
  ::
  ::    %make: build hoon files
  ::    %mark: build mark cores
  ::    %cast: build mark-conversion gates
  ::    %drop: cancel build
  ::
  +$  task
    $%  [%make =desk case=(unit case:clay) all=(set path)]
        [%mark =desk case=(unit case:clay) all=(set mark)]
        [%cast =desk case=(unit case:clay) all=(set [a=mark b=mark])]
        [%drop ~]
    ==
  ::  $gift: build result
  ::
  ::    %made: a vase for each filepath
  ::    %mark: a $dais for each mark
  ::    %cast: a gate for each pair of marks
  ::
  +$  gift
    $%  [%make =cass:clay all=(map path (each vase tang))]
        [%mark =cass:clay all=(map mark (each dais tang))]
        [%cast =cass:clay all=(map [a=mark b=mark] (each $-(vase vase) tang))]
    ==
  --
::  $dais: processed mark core
::
+$  dais
  $_  ^|
  |_  sam=vase
  ++  bunt  sam
  ++  diff  |~(new=_sam [form=form diff=*vase])
  ++  form  *mark
  ++  join  |~([a=vase b=vase] *(unit [form=_form diff=vase]))
  ++  mash  |~([a=[ship desk diff=vase] b=[ship desk diff=vase]] diff=*vase)
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
  $:  makes=(map duct [live=? job=make-build])
      marks=(map duct mark-build)
      =hoon-cache
  ==
++  make-build
  =/  m  (fume ,vase)
  $:  =desk
      =case
      left=@ud
      states=(map path [out=eval-output:m state=eval-state:m])
  ==
++  mark-build
  =/  m  (fume ,cage)
  $:  =desk
      =case
      left=@ud
      states=(map path [out=eval-output:m state=eval-state:m])
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
::
+$  build-state
  $:  fum=(unit [=spar on-load=(fume-form-raw ,cage)])
      cur=(map spar (unit cage))
      pre=(unit [=case resources=(map spar (unit cage))])
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
++  run-plan
  |=  =plan
  =/  m  (fume ,cage)
  ^-  form:m
  ?-  -.plan
    %$     (pure:m cage.plan)
    %bunt  (run-bunt +.plan)
    %cast  (run-cast +.plan)
    %diff  (run-diff +.plan)
    %join  (run-join +.plan)
    %mash  (run-mash +.plan)
    %pact  (run-pact +.plan)
    %vale  (run-vale +.plan)
    %volt  (run-volt +.plan)
  ==
::
++  run-bunt
  |=  =mark
  =/  m  (fume ,cage)
  ^-  form:m
  ;<  cor=vase  bind:m  (load-mark mark)
  (pure:m [mark (slap cor ^~((ream '+<')))])
::
++  run-cast
  |=  [new=mark =plan]
  =/  m  (fume ,cage)
  ^-  form:m
  ;<  [old=mark arg=vase]  bind:m  (run-plan plan)
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
++  run-diff
  |=  [start=plan end=plan]
  =*  loop  $
  =/  m  (fume ,cage)
  ^-  form:m
  ;<  uno=cage  bind:m  (run-plan start)
  ;<  dos=cage  bind:m  (run-plan end)
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
  =/  dif=vase  (slam p.gat q.dos)
  (pure:m [fom dif])
::
++  run-join
  |=  [=mark one=plan two=plan]
  =*  loop  $
  =/  m  (fume ,cage)
  ^-  form:m
  ;<  cor=vase  bind:m  (load-mark mark)
  ;<  gad=(each @tas vase)  bind:m  (run-grad cor)
  ?:  ?=(%& -.gad)
    loop(mark p.gad)
  ;<  uno=cage  bind:m  (run-plan one)
  ;<  dos=cage  bind:m  (run-plan two)
  ;<  fom=@tas  bind:m  (run-form p.gad)
  ?.  &(=(fom p.uno) =(fom p.dos))
    (fail:m leaf+"ford: join-mark" ~)
  ?:  =(q.q.uno q.q.dos)
    (pure:m uno)
  =/  gat  (mule |.((slap p.gad ^~((ream 'join')))))
  ?:  ?=(%| -.gat)
    (fail:m leaf+"ford: join-gate" p.gat)
  =/  dif=vase  (slam p.gat (slop q.uno q.dos))
  ?~  q.dif
    (pure:m [%null dif])
  (pure:m [fom dif])
::
++  run-mash
  |=  [=mark a=[=^desk =plan] b=[=^desk =plan]]
  =*  loop  $
  =/  m  (fume ,cage)
  ;<  cor=vase  bind:m  (load-mark mark)
  ;<  gad=(each ^mark vase)  bind:m  (run-grad cor)
  ?:  ?=(%& -.gad)
    loop(mark p.gad)
  ;<  fom=@tas  bind:m  (run-form p.gad)
  ;<  uno=cage  bind:m  (run-plan plan.a)
  ;<  dos=cage  bind:m  (run-plan plan.b)
  ?.  &(=(fom p.uno) =(fom p.dos))
    (fail:m leaf+"ford: mash-mark {<[mark p.uno p.dos]>}" ~)
  ?:  =(q.q.uno q.q.dos)
    (pure:m null+!>(~))
  %+  pure:m  fom
  %+  slam  (slap p.gad ^~((ream 'mash')))
  %+  slop
    :(slop !>(our) !>(desk.a) q.uno)
  :(slop !>(our) !>(desk.b) q.dos)
::
++  run-pact
  |=  [sut=plan dif=plan]
  =*  loop  $
  =/  m  (fume ,cage)
  ^-  form:m
  ;<  sot=cage  bind:m  (run-plan sut)
  ;<  cor=vase  bind:m  (load-mark p.sot)
  ;<  gad=(each mark vase)  bind:m  (run-grad cor)
  ?:  ?=(%& -.gad)
    (run-plan %cast p.sot %pact [%cast p.gad $+sot] dif)
  ;<  fom=mark  bind:m  (run-form p.gad)
  ;<  fid=cage  bind:m  (run-plan dif)
  ?.  =(fom p.fid)
    (fail:m leaf+"ford: pact-mark" ~)
  =/  sit=vase  (slop cor q.sot)
  =/  gat  (mule |.((slap sit ^~((ream 'pact:~(grad - +)')))))
  ?:  ?=(%| -.gat)
    (fail:m leaf+"ford: grad-pact {<p.sot>}" p.gat)
  =/  pac=vase  (slam p.gat q.fid)
  (pure:m [p.sot pac])
::
++  run-vale
  |=  [=mark =noun]
  =/  m  (fume ,cage)
  ^-  form:m
  ;<  cor=vase  bind:m  (load-mark mark)
  =/  gat=vase  (slap cor ^~((ream 'noun:grab')))
  =/  sam=vase  !>(noun)
  =/  pro=vase  (slam gat sam)
  (pure:m [mark pro])
::
++  run-volt
  |=  [=mark =noun]
  =/  m  (fume ,cage)
  ^-  form:m
  ;<  cag=cage  bind:m  (run-bunt mark)
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
  =|  fex=(list move)
  =/  =task:able  ((harden task:able) wrapped-task)
  ?-    -.task
      %make
    =/  live=?  =(~ case.task)
    =/  =case  (fall case.task da+now)
    =/  m  (fume ,vase)
    =|  states=(map path [out=eval-output:m state=eval-state:m])
    =/  paz  ~(tap in all.task)
    =/  dun=?  %.y
    ::  run each file build as far until done or blocked on resource
    ::
    |-  ^+  [fex ford-gate]
    ?~  paz
      ::  we've tried all paths; store if live or unfinished
      ::
      =?  makes.state.ax  |(live !dun)
        %+  ~(put by makes.state.ax)  duct
        [live desk.task case states]
      ::  if not live, delete build on completion
      ::
      =?  makes.state.ax  &(dun !live)
        (~(del by makes.state.ax) duct)
      ::  send result if done
      ::
      =?  fex  dun
        :_  fex
        :^  duct  %give  %make
        %-  ~(gas by *(map path (each vase tang)))
        %+  turn  ~(tap by states)
        |=  [=path out=eval-output:m state=eval-state:m]
        ?-  -.out
          %done  [path &+value.out]
          %fail  [path |+tang.out]
          %load  ~|(ford-not-done+path !!)
        ==
      ::  subscribe to dependencies each time a live build completes
      ::
      =?  fex  &(dun live)
        :_  fex
        =/  sky=(set spar)
          %+  roll  ~(val by states)
          |=  [[* * sky=(map spar (unit cage))] acc=(set spar)]
          (~(uni in acc) ~(key by sky))
        =/  =rave:clay  [%mult case sky]
        [duct %pass /live %c %warp our desk.task `rave]
      [fex ford-gate]
    ::  run the build on this path, .i.paz
    ::
    =^  res  hoon-cache.state.ax
      %:  (eval:m our desk.task case scry-gate)
        hoon-cache.state.ax
        *eval-state:m
        fun=(run-file:(at our desk scry-gate) i.paz)
        fil=~
      ==
    ::  accumulate result; clear the .dun flag if blocked
    ::
    =.  states  (~(put by states) i.paz res)
    =?  dun  ?=(%load -.out.res)  %.n
    ::  request the resource the build blocked on
    ::
    =?  fex  ?=(%load -.out.res)
      :_  fex
      =/  =wire  load+path.spar.out.res
      =/  =rave:clay  [%sing care.spar.out.res case path.spar.out.res]
      [duct %pass wire %c %warp our desk.task `rave]
    ::  recurse on next path
    ::
    $(paz t.paz)
  ::
      %mark
    !!
  ::
      %drop
    =.  makes.state.ax  (~(del by makes.state.ax) duct)
    =.  marks.state.ax  (~(del by marks.state.ax) duct)
    [~ ford-gate]
  ==
++  take
  |=  [=wire =duct type=* wrapped-sign=(hobo sign)]
  ^-  [(list move) _ford-gate]
  =/  =sign  ((harden sign) wrapped-sign)
  |^  ^-  [(list move) _ford-gate]
      ?>  ?=(^ wire)
      ?+    i.wire  ~(ford-bad-wire+wire !!)
          %load
        ?>  ?=(%writ -.sign)
        =/  =make-build  (~(got by makes.state.ax) duct)
        (on-load t.wire p.sign make-build)
      ::
          %live
        (on-live t.wire)
      ==
  ++  on-load
    |=  [=path =riot =make-build]
    ^-  [(list move _ford-gate]
    =/  m  (fume ,vase)
    =/  fil=(unit cage)  ?~(riot ~ `r.u.riot)
    =/  state  (~(got by states.make-build) path)
    ?>  ?=(%load -.out.eval-output.state)
    ?>  ?=(^ nex.eval-state.state)
    =/  [=spar on-load=form:m]  u.nex.eval-state.state
    ?>  =(path path.spar)
    ::  run the continuation for the file
    ::
    =^  res  hoon-cache.state.ax
      %:  (eval:m our desk.task case scry-gate)
        hoon-cache.state.ax
        eval-state.state
        fun=on-load
        fil
      ==
    =.  states.make-build  (~(put by states.make-build) path res)

  ++  on-live
    |=  id=wire
    ^-  [(list move _ford-gate]
    !!
  --
++  load
  |=  old=axle
  ford-gate(ax axle)
++  stay  ax
++  scry  |=(* [~ ~])
--
|%
++  per-event
  |=  [our=ship =desk =case:clay =duct scry=sley]
  |%
  +*  event-core  .
  ++  on-make-done
    !!
  --
--
