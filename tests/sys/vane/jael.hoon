/+  *test
::
/=  jael-raw  /:  /===/sys/vane/jael
              /!noun/
=/  type-spear  -:!>(jael-raw)
::
=/  test-pit=vase  !>(.)
=/  jael-gate  (jael-raw test-pit)
::
|%
::  tests that booting into %jael inits other vanes
::
++  test-boot-dawn
  :: +key ~nul
  =/  key=ring
    0w8O.k5Ry4.QsKQq.1k~uj.DBOU4.numfq.nXOwa.cSk7B.VcHVm.
    -8~kX.3ALiG.rQjOi.HZ9hj.84b6G.P5pCZ.UtNtt.Lh9TE.2DQJ2
  =/  url  (de-purl:html 'http://localhost:8545')
  =/  dan
    [`seed:able:jael`[~nul 1 key ~] ~nul ~ [/org/urbit ~] 0 url ~]
  ::
  =^  results1  jael-gate
    =/  hen=duct
      [/ /term/1 / ~]
    %-  jael-call
    :*  jael-gate
        now=~1234.5.6
        call-args=[hen type=*type %dawn dan]
        :~  [hen %slip %a %init ~nul]
            [hen %slip %c %init ~nul]
            [hen %slip %g %init ~nul]
            [hen %slip %d %init ~nul]
            [hen %slip %e %init ~nul]
            [hen %give %init ~nul]
            [hen %pass /~nul/init %b %wait +(~1234.5.6)]
    ==  ==
  ::
  =^  results2  jael-gate
    =/  wir=wire
      /our/~nul/now/(scot %da (add ~s1 ~1234.5.6))
    %-  jael-call-with-comparator  :*
      jael-gate
      now=(add ~s1 ~1234.5.6)
      call-args=[duct=[/ /term/1 wir ~] type=*type %vein ~]
      ^=  comparator
        |=  moves=(list move:jael-gate)
        ;:  weld
          %+  expect-eq
            !>  1
            !>  (lent moves)
        ::
          %+  expect-eq
            !>  [%give %vein 1 (my [1 key] ~)]
            !>  q:(head moves)
    ==  ==
  ::
  =^  results3  jael-gate
    =/  hiss-httr=hiss:eyre
      %+  json-request:rpc:ethereum
        (need url)
      %+  request-to-json:rpc:ethereum
        `'block number'
      [%eth-block-number ~]
    %-  jael-take-with-comparator  :*
      jael-gate
      now=(add ~s2 ~1234.5.6)
      take-args=[wire=/~nul/init duct=[/ /term/1 ~] type=*type %b %wake ~]
      ^=  comparator
        |=  moves=(list move:jael-gate)
        ;:  weld
          %+  expect-eq
            !>  1
            !>  (lent moves)
        ::
          %+  expect-eq
            !>  hiss-httr
            ?>  ?=(^ moves)
            ?>  ?=([* %pass * %e %hiss *] i.moves)
            q.r.q.q.i.moves
    ==  ==
  ::
  :(weld results1 results2 results3)
::
++  jael-call
  |=  $:  jael-gate=_jael-gate
          now=@da
          call-args=[=duct wrapped-task=(hypo (hobo task:able:jael-gate))]
          expected-moves=(list move:jael-gate)
      ==
  ^-  [tang _jael-gate]
  ::
  =/  jael  (jael-gate our=~nul now=now eny=`@`0xdead.beef scry=*sley)
  ::
  =^  moves  jael-gate
    %-  call:jael  call-args
  ::
  =/  output=tang
    %+  expect-eq
      !>  expected-moves
      !>  moves
  ::
  [output jael-gate]
::
++  jael-call-with-comparator
  |=  $:  jael-gate=_jael-gate
          now=@da
          call-args=[=duct wrapped-task=(hypo (hobo task:able:jael-gate))]
          move-comparator=$-((list move:jael-gate) tang)
      ==
  ^-  [tang _jael-gate]
  ::
  =/  jael  (jael-gate our=~nul now=now eny=`@`0xdead.beef scry=*sley)
  ::
  =^  moves  jael-gate
    %-  call:jael  call-args
  ::
  =/  output=tang  (move-comparator moves)
  ::
  [output jael-gate]
::
++  jael-take
  |=  $:  jael-gate=_jael-gate
          now=@da
          take-args=[=wire =duct wrapped-task=(hypo sign:able:jael-gate)]
          expected-moves=(list move:jael-gate)
      ==
  ^-  [tang _jael-gate]
  ::
  =/  jael  (jael-gate our=~nul now=now eny=`@`0xdead.beef scry=*sley)
  ::
  =^  moves  jael-gate
    %-  take:jael  take-args
  ::
  =/  output=tang
    %+  expect-eq
      !>  expected-moves
      !>  moves
  ::
  [output jael-gate]
::
++  jael-take-with-comparator
  |=  $:  jael-gate=_jael-gate
          now=@da
          take-args=[=wire =duct wrapped-task=(hypo sign:able:jael-gate)]
          move-comparator=$-((list move:jael-gate) tang)
      ==
  ^-  [tang _jael-gate]
  ::
  =/  jael  (jael-gate our=~nul now=now eny=`@`0xdead.beef scry=*sley)
  ::
  =^  moves  jael-gate
    %-  take:jael  take-args
  ::
  =/  output=tang  (move-comparator moves)
  ::
  [output jael-gate]
--
