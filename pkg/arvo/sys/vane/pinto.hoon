=>
|%
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
+$  fume-input  [in=(unit clay-response) s=fume-state]
+$  fume-state  hoon-cache
+$  clay-response  (unit cage)
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
++  fume-fail
  |=  =tang
  |=  fume-input
  [s %fail err]
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
  ++  bind
    |*  b=mold
    |=  [m-b=(fume-form-raw b) fun=$-(b form)]
    ^-  form
    |=  =fume-input
    =*  this  .
    =/  b-res  (m-b fume-input)
    ^-  output
    ?-    -.next.b-res
        %fail  b-res
        %load  b-res(on-load.next this(m-b on-load.next.b-res))
        %done  (fun value.next.b-res)
    ==
  ::
  ++  try
    |=  [x=form y=form]
    ^-  form
    |=  =fume-input
    =*  this  .
    =/  x-res  (x fume-input)
    ?-    -.next.x-res
        %done  x-res
        %load  x-res(self.next this(x self.next.x-res))
        %fail
      =/  y-res  (y fume-input)
      :-  s.y-res
      ?-  -.next.y-res
        %done  next.y-res
        %fail  [%fail (welp tang.x-res tang.y-res)]
        %load  next.y-res(on-load this(y on-load.next.y-res))
      ==
    ==
  --
--
|=  =hoon-cache  ::  TODO: include relevant ford state
|=  [our=ship syd=desk now=@da hen=duct]
|%
++  get-cached
  |=  =hoon-cache-key
  =/  m  (fume ,(unit vase))
  ^-  form:m
  |=  s=fume-state
  !!
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
::
++  load-hoon
  |=  =spar
  =/  m  (fume ,vase)
  ^-  form:m
  |=  s=fume-state
  ;<  [=mark vase=vase]  bind:m  (load spar)
  ?>  ?=(%hoon mark)
  =/  tex  !<(@t vase)
  =/  gen  (rain path.spar tex)  ::  TODO: parse (list pike)
  ;<  sut=vase  bind:m  make-reef
  ;<  vax  bind:m  (get-cached %ride gen sut)
  ?^  vax
    (pure:m vax)
  ;<  sim  bind:m  (get-cached %slim gen p.sut)
  ?^  sim
    (pure:m p.sim .*(q.sut q.sim))
  (slap sut gen)
::
++  cons
  |=  [a=plan b=plan]
  =/  m  (fume ,cage)
  ^-  form:m
  ;<  [mark hed=vase]  bind:m  (make a)
  ;<  [mark tal=vase]  bind:m  (make b)
  (pure:m noun+(slop hed tal))
::
++  run-root-build
  |=  [=build =build-state in=(unit clay-response)]
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
    [((make build build-state) in) ~]
  ::
  |-  ^-  [product ^build-state ^hoon-cache]
  ?-    -.next.pro
      %done  [`&+value.next.pro build-state s.pro]
      %fail  [`|+tang.next.pro build-state s.pro]
      %load
    ?^  in
      $(pro ((make build build-state) in), hoon-cache s.pro, in ~)
    ?^  got=(~(get by cur.build-state) spar.next.pro)
      $(pro ((make build build-state) got), hoon-cache s.pro)
    ?^  res=(scry-for-spar spar.next.pro)
      $(pro ((make build build-state) res), hoon-cache s.pro)
    [~ build-state(fum `[spar on-load]:next.pro) s.pro]
  ==
::
++  make
  |=  [=build =build-state]
  =/  m  (fume ,cage)
  ^-  form:m
  !!
::
++  make-reef
  =,  m  (fume ,vase)
  ^-  form:m
  ::
  !!
--
