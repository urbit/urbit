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
::  tests that galaxies try to listen to an ethereum node
::
++  test-init-as-galaxy  ^-  tang
  ::
  =/  hiss-httr=hiss:eyre
    %+  json-request:ethereum
      =-  -(p.p |)
      (need (de-purl:html 'http://localhost:8545'))
    %+  request-to-json:ethereum  `'new filter'
    :*  %eth-new-filter
        `[%number 1]
        ~
        ~[ships:contracts:constitution:ethe]
        ~
    ==
  ::
  =^  results1  jael-gate
    %-  jael-call-with-comparator  :*
      jael-gate
      now=~1234.5.6
      call-args=[duct=~ type=*type %init ~nul]
      ^=  comparator
        |=  moves=(list move:jael-gate)
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %pass * %e %hiss *] i.moves)
        ::
        %+  expect-eq
          !>(hiss-httr)
          q.r.q.q.i.moves
    ==
  ::
  results1
::
++  jael-call
  |=  $:  jael-gate=_jael-gate
          now=@da
          call-args=[=duct wrapped-task=(hypo (hobo task:able:jael-gate))]
          expected-moves=(list move:jael-gate)
      ==
  ^-  [tang _jael-gate]
  ::
  =/  jael  (jael-gate now=now eny=`@e`0xdead.beef scry=*sley)
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
  =/  jael  (jael-gate now=now eny=`@e`0xdead.beef scry=*sley)
  ::
  =^  moves  jael-gate
    %-  call:jael  call-args
  ::
  =/  output=tang  (move-comparator moves)
  ::
  [output jael-gate]
--

