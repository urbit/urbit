/+  *test
/=  clay-raw  /sys/vane/clay
/*  hello-gen     %hoon  /gen/hello/hoon
/*  strandio-lib  %hoon  /lib/strandio/hoon
/*  strand-lib    %hoon  /lib/strand/hoon
/*  spider-sur    %hoon  /sur/spider/hoon
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
++  test-hello-gen  ^-  tang
  =/  =ankh:clay
    :-  fil=~
    %-  ~(gas by *(map @tas ankh:clay))
    :~  :+  %gen  fil=~
        %-  ~(gas by *(map @tas ankh:clay))
        :~  :+  %hello  fil=~
            %-  ~(gas by *(map @tas ankh:clay))
            :~  :+  %hoon  fil=`[*lobe:clay hoon+!>(hello-gen)]  dir=~
    ==  ==  ==
  =/  ford
    %:  ford:fusion
      bud
      ankh
      deletes=~
      changes=(my [/gen/hello/hoon &+hoon+hello-gen]~)
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
++  test-strandio-lib  ^-  tang
  =/  =ankh:clay
    :-  fil=~
    %-  ~(gas by *(map @tas ankh:clay))
    :~  :+  %lib  fil=~
        %-  ~(gas by *(map @tas ankh:clay))
        :~  :+  %strandio  fil=~
            %-  ~(gas by *(map @tas ankh:clay))
            :~  :+  %hoon  fil=`[*lobe:clay hoon+!>(strandio-lib)]  dir=~
            ==
        ::
            :+  %strand  fil=~
            %-  ~(gas by *(map @tas ankh:clay))
            :~  :+  %hoon  fil=`[*lobe:clay hoon+!>(strand-lib)]  dir=~
        ==  ==
    ::
        :+  %sur  fil=~
        %-  ~(gas by *(map @tas ankh:clay))
        :~  :+  %spider  fil=~
            %-  ~(gas by *(map @tas ankh:clay))
            :~  :+  %hoon  fil=`[*lobe:clay hoon+!>(spider-sur)]  dir=~
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
