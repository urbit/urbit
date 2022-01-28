/+  *test
/=  khan-raw  /sys/vane/khan
=/  khan-gate  (khan-raw ~nul)
|%
++  test-khan-fyrd-start-args
  =^  results1  khan-gate
    %-  khan-call  :*
      khan-gate
      now=~1111.1.1
      scry=scry-provides-mark
      call-args=[duct=~[/initial-born-duct] ~ [%born ~]]
      ^=  moves-check
        |=  mev=(list move:khan-gate)
        (expect-eq !>(~) !>(mev))
    ==
  =/  =fyrd:khan  [%base %nonexistent %noun %noun ~]
  =/  now=@da  (add ~1111.1.1 ~s1)
  =/  =dais:clay  dais-noun
  =/  args
    :*  ~  `%'khan-fyrd--0vsome.ductt'  [~nul %base %da now]
        %nonexistent  (vale.dais ~)
    ==
  =^  results2  khan-gate
    %-  khan-call
      :*  khan-gate
          now
          scry=scry-provides-mark
          ^=  call-args
            :*  duct=~[//khan/1/0vsome.ductt]  ~
                %fyrd  fyrd
            ==
          ^=  moves-check
            |=  mev=(list move:khan-gate)
            ^-  tang
            =/  r0  (expect-eq !>(2) !>((lent mev)))
            =/  r1
              %+  expect-eq
                !>  :*  ~[//khan/1/0vsome.ductt]
                        %pass  //g  %g  %deal
                        [~nul ~nul]  %spider  %watch
                        /thread-result/'khan-fyrd--0vsome.ductt'
                    ==
                !>  (head mev)
            ::  XX this is somewhat verbose because we do not
            ::  have a +expect-eq for recursive vases, and spider
            ::  takes a double-nested vase. so we first test that
            ::  the poke is correct aside from the vase; then we
            ::  check that the outer vase is correct aside from
            ::  the inner vase; then we check that the inner vase
            ::  is correct.
            ::
            =/  rem  (rear mev)
            =/  r2
              %+  expect-eq
                !>  :*  ~[//khan/1/0vsome.ductt]
                        %pass  //g  %g  %deal
                        [~nul ~nul]  %spider  %poke
                        %spider-start  ~
                    ==
                !>  rem(+1023 ~)
            =/  vez=*  +1023:(rear mev)
            =/  r3
              ;;  tang
              %+  slum  expect-eq
              :-  !>  args(+31 ~)
                  !>  +.vez(+31 ~)
            =/  r4
              ;;  tang
              %+  slum  expect-eq
              :-  +31.args
                  +31.+.vez
            ;:  weld
              r0
              r1
              r2
              r3
              r4
      ==    ==
  (weld results1 results2)
::  ++  test-khan-take-dud
::    !!
::  ++  test-khan-take-watch-fail
::    !!
::  ++  test-khan-take-poke-fail
::    !!
::  ++  test-khan-take-full-run
::    !!
++  khan-call
  |=  $:  khan-gate=_khan-gate
          now=@da
          scry=roof
          $=  call-args
            $:  =duct
                dud=(unit goof)
                wrapped-task=(hobo task:khan)
            ==
          $=  moves-check
            $-  (list move:khan-gate)  tang
      ==
  ^-  [tang _khan-gate]
  =/  khan-core
    (khan-gate now eny=`@uvJ`0xdead.beef scry=scry)
  =^  moves  khan-gate
    (call:khan-core [duct dud wrapped-task]:call-args)
  =/  output=tang
    (moves-check moves)
  [output khan-gate]
++  dais-noun  ^-  dais:clay
  |_  sam=vase
  ++  diff  !!
  ++  form  !!
  ++  join  !!
  ++  mash  !!
  ++  pact  !!
  ++  vale  |=(=noun !>(;;(^noun noun)))
  --
++  scry-provides-mark  ^-  roof
  |=  [gang =view =beam]
  ^-  (unit (unit cage))
  ?:  &(=(%cb view) =(/noun s.beam))
    :^  ~  ~  %dais
    !>  ^-  dais:clay
    dais-noun
  ~
--
