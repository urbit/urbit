/+  *test
/=  clay-raw  /sys/vane/clay
/*  gen-hello     %hoon  /gen/hello/hoon
/*  lib-cram      %hoon  /lib/cram/hoon
/*  lib-strandio  %hoon  /lib/strandio/hoon
/*  lib-strand    %hoon  /lib/strand/hoon
/*  sur-spider    %hoon  /sur/spider/hoon
/*  mar-html      %hoon  /mar/html/hoon
/*  mar-mime      %hoon  /mar/mime/hoon
/*  mar-udon      %hoon  /mar/udon/hoon
/*  mar-txt       %hoon  /mar/txt/hoon
/*  mar-txt-diff  %hoon  /mar/txt-diff/hoon
::
!:
=,  format
::
=/  bud=vase  !>(..zuse)
=/  clay-gate  (clay-raw ~nul)
=/  fusion  fusion:clay-gate
::
=>  |%
    ++  leak-to-deps
      |=  =leak:fusion
      ^-  (set mist:fusion)
      %-  sy
      |-  ^-  (list mist:fusion)
      %-  zing
      %+  turn  ~(tap in deps.leak)
      |=  l=leak:fusion
      :-  (pour-to-mist:fusion pour.l)
      ^$(leak l)
    --
|%
++  test-parse-pile  ^-  tang
  =/  src  '.'
  %+  expect-eq
    !>  ^-  pile:fusion
        :*  ~  ~  ~  ~  ~  ~  ~
            tssg+[%dbug [/sur/foo/hoon [[1 1] [1 2]]] [%cnts ~[[%.y 1]] ~]]~
        ==
    !>  (parse-pile:(ford):fusion /sur/foo/hoon src)
::
++  test-parse-fascen  ^-  tang
  =/  src  '/%  moo  %mime\0a.'
  %+  expect-eq
    !>  ^-  pile:fusion
        :*  sur=~  lib=~  raw=~  raz=~
            maz=[face=%moo mark=%mime]~
            caz=~  bar=~
            tssg+[%dbug [/sur/foo/hoon [[2 1] [2 2]]] [%cnts ~[[%.y 1]] ~]]~
        ==
    !>  (parse-pile:(ford):fusion /sur/foo/hoon src)
::
++  test-parse-fasbuc  ^-  tang
  =/  src  '/$  goo  %mime  %txt\0a.'
  %+  expect-eq
    !>  ^-  pile:fusion
        :*  sur=~  lib=~  raw=~  raz=~  maz=~
            caz=[face=%goo from=%mime to=%txt]~
            bar=~
            tssg+[%dbug [/sur/foo/hoon [[2 1] [2 2]]] [%cnts ~[[%.y 1]] ~]]~
        ==
    !>  (parse-pile:(ford):fusion /sur/foo/hoon src)
::
++  test-parse-multiline-faslus  ^-  tang
  =/  src
    '''
    ::
    /?    310                                               ::  zuse version
    ::
    /-  *sole
    ::
    /+  sole                                                ::  libraries
    ::
    /+  hood-helm, hood-kiln, hood-drum, hood-write
    ::
    .
    '''
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
            raw=~  raz=~  maz=~  caz=~  bar=~
            tssg+[%dbug [/sur/foo/hoon [[10 1] [10 2]]] [%cnts ~[[%.y 1]] ~]]~
        ==
    !>  (parse-pile:(ford):fusion /sur/foo/hoon src)
::
++  test-cycle  ^-  tang
  =/  source=@t  '/+  self\0a.'
  %-  expect-fail
  |.
  =/  ford
    %:  ford:fusion
      files=(my [/lib/self/hoon &+hoon+source]~)
      file-store=~
      0
      *flow:fusion
      *flue:fusion
    ==
  (build-file:ford /lib/self/hoon)
::
++  test-parse-fail  ^-  tang
  %-  expect-fail
  |.  (parse-pile:(ford):fusion /sur/foo/hoon '[')
::
++  test-mar-mime  ^-  tang
  =/  ford
    %:  ford:fusion
      files=(my [/mar/mime/hoon &+hoon+mar-mime]~)
      file-store=~
      0
      *flow:fusion
      *flue:fusion
    ==
    =/  [res=vase nub=state:ford:fusion]  (build-nave:ford %mime)
    =/  =leak:fusion  leak:(~(got by sprig.nub) file+/mar/mime/hoon)
    ;:  weld
      %+  expect-eq
        !>(*mime)
        (slap res !,(*hoon *vale))
    ::
      %+  expect-eq
        !>  (~(gas in *(set mist:fusion)) vale+/mar/mime/hoon ~)
        !>  (leak-to-deps leak)
    ==
::
++  test-mar-udon  ^-  tang
  =/  ford
    %:  ford:fusion
      ^=  files
      %-  my
      :~  [/mar/udon/hoon &+hoon+mar-udon]
          [/lib/cram/hoon &+hoon+lib-cram]
          [/mar/txt/hoon &+hoon+mar-txt]
          [/mar/txt-diff/hoon &+hoon+mar-txt-diff]
      ==
      file-store=~
      0
      *flow:fusion
      *flue:fusion
    ==
    =/  [res=vase nub=state:ford:fusion]  (build-nave:ford %udon)
    =/  =leak:fusion  leak:(~(got by sprig.nub) file+/mar/udon/hoon)
    ;:  weld
      %+  expect-eq
        !>(*@t)
        (slap res !,(*hoon *vale))
    ::
      %+  expect-eq
        !>  %-  ~(gas in *(set mist:fusion))
        :~  vale+/mar/udon/hoon
            vale+/lib/cram/hoon
            file+/lib/cram/hoon
        ==
        !>  (leak-to-deps leak)
    ==
::
++  test-cast-html-mime  ^-  tang
  =/  files
    %-  my
    :~  [/mar/mime/hoon &+hoon+mar-mime]
        [/mar/html/hoon &+hoon+mar-html]
    ==
  =/  ford
    %:  ford:fusion
      files
      file-store=~
      0
      *flow:fusion
      *flue:fusion
    ==
  =/  [res=vase nub=state:ford:fusion]  (build-cast:ford %html %mime)
  %+  expect-eq
    (slam res !>('<html></html>'))
    !>  `mime`[/text/html 13 '<html></html>']
::
++  test-fascen  ^-  tang
  =/  files
    %-  my
    :~  [/mar/mime/hoon &+hoon+mar-mime]
        [/lib/foo/hoon &+hoon+'/%  moo  %mime\0a*vale:moo']
    ==
  =/  ford
    %:  ford:fusion
      files
      file-store=~
      0
      *flow:fusion
      *flue:fusion
    ==
  =/  [res=vase nub=state:ford:fusion]  (build-file:ford /lib/foo/hoon)
  %+  expect-eq
    res
    !>  *mime
::
++  test-fasbuc  ^-  tang
  =/  files
    %-  my
    :~  [/mar/mime/hoon &+hoon+mar-mime]
        [/mar/html/hoon &+hoon+mar-html]
        [/lib/foo/hoon &+hoon+'/$  foo  %mime  %html\0a*foo']
    ==
  =/  ford
    %:  ford:fusion
      files
      file-store=~
      0
      *flow:fusion
      *flue:fusion
    ==
  =/  [res=vase nub=state:ford:fusion]  (build-file:ford /lib/foo/hoon)
  %+  expect-eq
    res
    !>  ''
::
++  test-gen-hello  ^-  tang
  =/  ford
    %:  ford:fusion
      files=(my [/gen/hello/hoon &+hoon+gen-hello]~)
      file-store=~
      0
      *flow:fusion
      *flue:fusion
    ==
  =/  [res=vase nub=state:ford:fusion]  (build-file:ford /gen/hello/hoon)
  =/  =leak:fusion  leak:(~(got by sprig.nub) file+/gen/hello/hoon)
  ;:  weld
    %+  expect-eq
      !>  noun+'hello, bob'
      (slap res (ream '(+ [*^ [%bob ~] ~])'))
  ::
    %+  expect-eq
      !>  (~(gas in *(set mist:fusion)) vale+/gen/hello/hoon ~)
      !>  (leak-to-deps leak)
  ==
::
++  test-lib-strandio  ^-  tang
  =/  ford
    %:  ford:fusion
      ^=  files
      %-  my
      :~  [/lib/strand/hoon &+hoon+lib-strand]
          [/lib/strandio/hoon &+hoon+lib-strandio]
          [/sur/spider/hoon &+hoon+sur-spider]
      ==
      file-store=~
      0
      *flow:fusion
      *flue:fusion
    ==
  =/  [res=vase nub=state:ford:fusion]  (build-file:ford /lib/strandio/hoon)
  =/  =leak:fusion  leak:(~(got by sprig.nub) file+/lib/strandio/hoon)
  ;:  weld
    %-  expect
    !>((slab %read %get-our -.res))
  ::
    %+  expect-eq
      !>  %-  ~(gas in *(set mist:fusion))
          :~  vale+/lib/strandio/hoon
              file+/lib/strand/hoon
              vale+/lib/strand/hoon
              file+/sur/spider/hoon
              vale+/sur/spider/hoon
          ==
      !>  (leak-to-deps leak)
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
