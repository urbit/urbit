=,  contain
=>
|%
::  $hoon-cache: cache for compiler operations
::
+$  hoon-cache  (clock hoon-cache-key vase)
::  +hoon-cache-key: cache key for a compiler operation
::
::    %call: +slam (vase of) gate on (vase of) sample
::    %hood: parse file into $scaffold; use !<(scaffold val)
::    %ride: +slap $hoon against $vase
::    %slim: compile +slap; use !<([type nock] val)
::    %slit: infer +slam product type; use .p.val
::
+$  hoon-cache-key
  $%  [%call gate=vase sample=vase]
      [%hood =beam txt=@t]
      [%ride formula=hoon subject=vase]
      [%slim subject-type=type formula=hoon]
      [%slit gate=type sample=type]
  ==
+$  product  (unit (each cage tang))
+$  build
  $:  live=?
      =desk
      =case:clay
      =plan
  ==
+$  build-state
  $:  fum=(unit [=spar on-load=(fume-form-raw ,cage)])
      cur=(map spar (unit cage))
      pre=(unit [=case:clay resources=(map spar (unit cage))])
  ==
::  $spar: clay scry request on unspecified $beak
::
+$  spar  [=care:clay =path]
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
    |=  in=fume-input
    =*  this  .
    =/  b-res  (m-b in)
    ^-  output
    ?-    -.next.b-res
        %fail  b-res
        %load  b-res(on-load.next this(m-b on-load.next.b-res))
        %done  (fun value.next.b-res)
    ==
  ::
  ++  fmap
    |*  b=mold
    |=  [m-b=(fume-form-raw b) fun=$-(b a)]
    ^-  form
    ((bind b) |=(b (pure (fun +<))))
  ::
  ++  try
    |=  [x=form y=form]
    ^-  form
    |=  in=fume-input
    =*  this  .
    =/  x-res  (x in)
    ?-    -.next.x-res
        %done  x-res
        %load  x-res(self.next this(x self.next.x-res))
        %fail
      =/  y-res  (y in)
      :-  s.y-res
      ?-  -.next.y-res
        %done  next.y-res
        %fail  [%fail (welp tang.x-res tang.y-res)]
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
  ((fmap:form:mc ,vase) vase-to-cage run)
::
++  vase-to-cage  (bake (with-mark %noun) vase)
++  with-mark  |=(=mark |*(* [mark +<])))
--
|=  =hoon-cache  ::  TODO: include relevant ford state
|=  [our=ship syd=desk now=@da scry=sley]
|%
++  run-root-build
  |=  [=build =build-state in=(unit (unit cage))]
  ^-  [=product =^build-state =^hoon-cache]
  =/  m  (fume ,cage)
  ::  fresh build should have no response; rerun must have one
  ::
  ?>  =(=(~ fum.build-state) =(~ in))
  ::  if no attempt has been made already, try to run the build
  ::
  =^  pro  in
    ?^  fum.build-state
      [[%load `fum.build-state] in]
    [((make plan.build) in) ~]
  ::
  |-  ^-  [product ^build-state ^hoon-cache]
  ?-    -.next.pro
      %done  [`&+value.next.pro build-state s.pro]
      %fail  [`|+tang.next.pro build-state s.pro]
      %load
    ?^  in
      $(pro ((make plan.build) in), hoon-cache s.pro, in ~)
    ?^  got=(~(get by cur.build-state) spar.next.pro)
      $(pro ((make plan.build) got), hoon-cache s.pro)
    ?^  res=(scry-for-spar spar.next.pro)
      $(pro ((make plan.build) res), hoon-cache s.pro)
    [~ build-state(fum `[spar on-load]:next.pro) s.pro]
  ==
::
++  make
  |=  =plan
  =/  m  (fume ,cage)
  ^-  form:m
  ?-  -.plan
    ^      make-cell
    %reef  make-reef
    %ride  make-ride
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
++  make-reef  (lift-vase run-reef)
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
  ;<  [=mark xet=vase]  bind:m  (load-spar %x path)
  ?>  ?=(%hoon mark)
  =/  tex  !<(@t xet)
  =/  gen  (rain path tex)
  (run-ride gen sut)
::
++  make-ride  |=([sut=vase gen=hoon] (lift-vase (run-ride +<)))
++  run-ride
  |=  [sut=vase gen=hoon]
  =/  m  (fume ,vase)
  ^-  form:m
  %+  with-cache-key  [%ride gen sut]
  ;<  sim=vase  bind:m
    %+  with-cache-key  [%slim gen p.sut]
    %+  on-fail  |.([leaf+"ford: slim-fail"]~)
    =/  lap  (mule |.((~(mint ut sut) %noun gen)))
    ?-  -.lap
      %&  (pure:m !>(p.lap))
      %|  (fail:m p.lap)
    ==
  %+  on-fail  |.([leaf+"ford: ride-fail"]~)
  =+  !<([gol=type fol=nock] sim)
  =/  nap  (mock [q.sut fol] scry)
  ?-  -.nap
    %&  (pure:m [gol nap])
    %|  (fail:m p.nap)
  ==
::
++  with-cache
  =,  m  (fume ,vase)
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
    %done  ran(s (put:ca key value.next.ran))
    %load  ran(on-load this(run on-load.next.ran))
  ==
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
  [s.fin1 %done u.in.fin1]
--
