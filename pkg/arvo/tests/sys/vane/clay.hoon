/+  *test
/=  clay-raw  /sys/vane/clay
/*  gen-hello     %hoon  /gen/hello/hoon
/*  lib-strandio  %hoon  /lib/strandio/hoon
/*  lib-strand    %hoon  /lib/strand/hoon
/*  sur-spider    %hoon  /sur/spider/hoon
/*  mar-mime      %hoon  /mar/mime/hoon
::
!:
=,  format
::
=/  bud=vase  !>(..zuse)
=/  clay-gate  (clay-raw ~nul)
=/  fusion  fusion:clay-gate
::
|%
++  test-parse-pile  ^-  tang
  %+  expect-eq
    !>  ^-  pile:fusion
        :*  ~  ~  ~  ~
            tssg+[%dbug [/sur/foo/hoon [[1 1] [1 2]]] [%cnts ~[[%.y 1]] ~]]~
        ==
    !>  (parse-pile:(ford):fusion /sur/foo/hoon ".")
::
++  test-parse-multiline-faslus  ^-  tang
  =/  src
    """
    ::                                                      ::  ::
    ::::  /hoon/hood/app                                    ::  ::
      ::                                                    ::  ::
    /?    310                                               ::  zuse version
    /-  *sole
    /+  sole                                                ::  libraries
        ::  XX these should really be separate apps, as
        ::     none of them interact with each other in
        ::     any fashion; however, to reduce boot-time
        ::     complexity and work around the current
        ::     non-functionality of end-to-end acknowledgments,
        ::     they have been bundled into :hood
        ::
        ::  |command handlers
    /+  hood-helm, hood-kiln, hood-drum, hood-write
    ::                                                      ::  ::
    .
    """
  %+  expect-eq
    !>  ^-  pile:fusion
        :*  sur=`(list taut:fusion)`[~ %sole]~
            ^=  lib  ^-  (list taut:fusion)
            :~  [`%sole %sole]
                [`%hood-helm %hood-helm]
                [`%hood-kiln %hood-kiln]
                [`%hood-drum %hood-drum]
                [`%hood-write %hood-write]
            ==
            raw=~  bar=~
            hoon=tssg+[p:(need q:(tall:(vang & /app/hood/hoon) [17 1] "."))]~
        ==
    !>  (parse-pile:(ford):fusion /app/hood/hoon src)
::
++  test-cycle  ^-  tang
  =/  source=@t
    '''
    /+  self
    .
    '''
  =/  =ankh:clay
    :-  fil=~
    %-  ~(gas by *(map @tas ankh:clay))
    :~  :+  %lib  fil=~
        %-  ~(gas by *(map @tas ankh:clay))
        :~  :+  %self  fil=~
            %-  ~(gas by *(map @tas ankh:clay))
            :~  :+  %hoon  fil=`[*lobe:clay hoon+!>(source)]  dir=~
    ==  ==  ==
  %-  expect-fail
  |.
  =/  ford
    %:  ford:fusion
      bud
      ankh
      deletes=~
      changes=~
      file-store=~
      *ford-cache:fusion
    ==
  (build-file:ford /lib/self/hoon)
::
++  test-parse-fail  ^-  tang
  %-  expect-fail
  |.  (parse-pile:(ford):fusion /sur/foo/hoon "[")
::
++  test-mar-mime  ^-  tang
  =/  =ankh:clay
    :-  fil=~
    %-  ~(gas by *(map @tas ankh:clay))
    :~  :+  %mar  fil=~
        %-  ~(gas by *(map @tas ankh:clay))
        :~  :+  %hoon  fil=`[*lobe:clay hoon+!>(mar-mime)]  dir=~
    ==  ==
  =/  ford
    %:  ford:fusion
      bud
      ankh
      deletes=~
      changes=(my [/mar/mime/hoon &+hoon+mar-mime]~)
      file-store=~
      *ford-cache:fusion
    ==
    =/  [res=vase nub=state:ford:fusion]  (get-nave:ford %mime)
    ;:  weld
      %+  expect-eq
        !>(*mime)
        (slap res limb/%bunt)
    ::
      %+  expect-eq
        !>  (~(gas in *(set path)) /mar/mime/hoon ~)
        !>  dez:(~(got by vases.cache.nub) /mar/mime/hoon)
    ==
::
++  test-gen-hello  ^-  tang
  =/  =ankh:clay
    :-  fil=~
    %-  ~(gas by *(map @tas ankh:clay))
    :~  :+  %gen  fil=~
        %-  ~(gas by *(map @tas ankh:clay))
        :~  :+  %hello  fil=~
            %-  ~(gas by *(map @tas ankh:clay))
            :~  :+  %hoon  fil=`[*lobe:clay hoon+!>(gen-hello)]  dir=~
    ==  ==  ==
  =/  ford
    %:  ford:fusion
      bud
      ankh
      deletes=~
      changes=(my [/gen/hello/hoon &+hoon+gen-hello]~)
      file-store=~
      *ford-cache:fusion
    ==
  =/  [res=vase nub=state:ford:fusion]  (build-file:ford /gen/hello/hoon)
  ;:  weld
    %+  expect-eq
      !>  noun+'hello, bob'
      (slap res (ream '(+ [*^ [%bob ~] ~])'))
  ::
    %+  expect-eq
      !>  (~(gas in *(set path)) /gen/hello/hoon ~)
      !>  dez:(~(got by vases.cache.nub) /gen/hello/hoon)
  ==
::
++  test-lib-strandio  ^-  tang
  =/  =ankh:clay
    :-  fil=~
    %-  ~(gas by *(map @tas ankh:clay))
    :~  :+  %lib  fil=~
        %-  ~(gas by *(map @tas ankh:clay))
        :~  :+  %strandio  fil=~
            %-  ~(gas by *(map @tas ankh:clay))
            :~  :+  %hoon  fil=`[*lobe:clay hoon+!>(lib-strandio)]  dir=~
            ==
        ::
            :+  %strand  fil=~
            %-  ~(gas by *(map @tas ankh:clay))
            :~  :+  %hoon  fil=`[*lobe:clay hoon+!>(lib-strand)]  dir=~
        ==  ==
    ::
        :+  %sur  fil=~
        %-  ~(gas by *(map @tas ankh:clay))
        :~  :+  %spider  fil=~
            %-  ~(gas by *(map @tas ankh:clay))
            :~  :+  %hoon  fil=`[*lobe:clay hoon+!>(sur-spider)]  dir=~
    ==  ==  ==
  =/  ford
    %:  ford:fusion
      bud
      ankh
      deletes=~
      changes=~
      file-store=~
      *ford-cache:fusion
    ==
  =/  [res=vase nub=state:ford:fusion]  (build-file:ford /lib/strandio/hoon)
  ;:  weld
    %-  expect
    !>((slab %read %get-our -.res))
  ::
    %+  expect-eq
      !>  %-  ~(gas in *(set path))
          :~  /lib/strandio/hoon
              /lib/strand/hoon
              /sur/spider/hoon
          ==
      !>  dez:(~(got by vases.cache.nub) /lib/strandio/hoon)
  ==
::
::  |utilities: helper functions for testing
::
::    TODO: make these utilities generic for any vane
::
::+|  utilities
::
::  +clay-call: have clay run a +task and assert it produces :expected-moves7890
::
++  clay-call
  |=  $:  clay-gate=_clay-gate
          now=@da
          scry=roof
          call-args=[=duct wrapped-task=(hobo task:clay)]
          expected-moves=(list move:clay-gate)
      ==
  ^-  [tang _clay-gate]
  ::
  =/  clay-core  (clay-gate now=now eny=`@`0xdead.beef scry=scry)
  ::
  =^  moves  clay-gate  (call:clay-core [duct ~ wrapped-task]:call-args)
  ::
  =/  output=tang
    %+  expect-eq
      !>  expected-moves
      !>  moves
  ::
  [output clay-gate]
::  +clay-call-with-comparator: run a clay +task and test output moves
::
++  clay-call-with-comparator
  |=  $:  clay-gate=_clay-gate
          now=@da
          scry=roof
          call-args=[=duct wrapped-task=(hobo task:clay)]
          move-comparator=$-((list move:clay-gate) tang)
      ==
  ^-  [tang _clay-gate]
  ::
  =/  clay-core  (clay-gate now=now eny=`@`0xdead.beef scry=scry)
  ::
  =^  moves  clay-gate  (call:clay-core [duct ~ wrapped-task]:call-args)
  ::
  =/  output=tang  (move-comparator moves)
  ::
  [output clay-gate]
::  +clay-take: have clay receive a +note and assert output moves
::
++  clay-take
  |=  $:  clay-gate=_clay-gate
          now=@da
          scry=roof
          take-args=[=wire =duct =sign:clay-gate]
          expected-moves=(list move:clay-gate)
      ==
  ^-  [tang _clay-gate]
  ::
  =/  clay-core  (clay-gate now=now eny=`@`0xdead.beef scry=scry)
  ::
  =^  moves  clay-gate  (take:clay-core [wire duct ~ sign]:take-args)
  ::
  =/  output=tang
    %+  expect-eq
      !>  expected-moves
      !>  moves
  ::
  [output clay-gate]
::  +clay-take-with-comparator: have clay receive a +note and test output moves
::
++  clay-take-with-comparator
  |=  $:  clay-gate=_clay-gate
          now=@da
          scry=roof
          take-args=[=wire =duct =sign:clay-gate]
          move-comparator=$-((list move:clay-gate) tang)
      ==
  ^-  [tang _clay-gate]
  ::
  =/  clay-core  (clay-gate now=now eny=`@`0xdead.beef scry=scry)
  ::
  =^  moves  clay-gate  (take:clay-core [wire duct ~ sign]:take-args)
  ::
  =/  output=tang  (move-comparator moves)
  ::
  [output clay-gate]
--
