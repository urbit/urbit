::
/=  jael-raw  /:  /===/sys/vane/jael
              /!noun/
=/  type-spear  -:!>(jael-raw)
::
::  this is parts of tester.hoon copied in manually because /+ is broken on research-fjord!?
::
|%
++  expect-eq
  |=  a=vase
  ^-  tang
  ?@  q.a  [palm+[": " ~ ~ ~]^~[>%ex-expected-pair< (sell a)]]~
  ?:  =(-.q.a +.q.a)
    ~
  :~  palm+[": " ~ ~ ~]^~[leaf+"expected" (sell (slot 2 a))]
      palm+[": " ~ ~ ~]^~[leaf+"actual" (sell (slot 3 a))]
  ==
--
::
::
:-  %say
|=  [[now=@da eny=@ bek=beak] ~ ~]
:-  %noun
::
=/  test-pit=vase  !>(.)
=/  jael-gate  (jael-raw test-pit)
::
|^
=-  ((slog -) ~)
^-  tang
;:  weld
  test-init-as-galaxy
==
::  tests that galaxies try to listen to an ethereum node
::
++  test-init-as-galaxy
  :-  `tank`leaf+"test-init-as-galaxy"
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
    %-  test-jael-call-with-comparator  :*
      jael-gate
      now=~1234.5.6
      call-args=[duct=~ type=*type %init ~nul]
      ^=  comparator
        |=  moves=(list move:jael-gate)
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %pass * %e %hiss *] i.moves)
        ::  the response contains a vase, check both the value and that it nests
        ::
        %+  weld
          %-  expect-eq  (slop !>(hiss-httr) q.r.q.q.i.moves)
        ::
        %-  expect-eq  !>
        :-  &
        (~(nest ut p.q.r.q.q.i.moves) | -:!>(hiss-httr))
    ==
  ::
  results1
::
++  test-jael-call
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
    %-  expect-eq  !>
    :-  expected-moves
    moves
  ::
  [output jael-gate]
::
++  test-jael-call-with-comparator
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
