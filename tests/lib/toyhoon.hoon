::  toyhoon: tests
::
/+  *test, th=toyhoon
::
=>  |%
    ++  test-slap
      |=  [sut=vase:th gol=type:th =naty:th expect=vase:th]
      =/  res=vase:th  (slap:th sut gol naty)
      |-  ^-  tang
      ?:  &(?=(^ p.res) =(%hold -.p.res))  ::NOTE  tmi!
        ?>  ?=(%hold -.p.res)
        $(p.res (drop:th p.res))
      (expect-eq !>(expect) !>(res))
    ::
    ++  pre
      |%
      ++  loob  ^-  type:th
        [%bcwt (my [%& %f] [%| %f] ~)]
      ++  numb  ^-  type:th
        [%atom %ud ~]
      --
    --
|%
++  test-noun
  ::  "42"
  =/  =naty:th  [%noun numb:pre 42]
  (test-slap [%noun ~] %noun naty numb:pre 42)
::
++  test-dttr
  ::  ".*(42 [0 1])"
  =/  =naty:th
    :+  %dttr
      [%noun %noun 42]
    [%noun %noun [0 1]]
  (test-slap [%noun ~] %noun naty %noun 42)
::
++  test-dtwt
  %+  weld
    ::  ".?(42)"
    =/  =naty:th  [%dtwt %noun numb:pre 42]
    (test-slap [%noun ~] %noun naty loob:pre |)
  ::  ".?([1 2])"
  =/  =naty:th  [%dtwt %noun [%cell [. .]:numb:pre] 1 2]
  (test-slap [%noun ~] %noun naty loob:pre &)
::
++  test-dtls
  ::  ".+(42)"
  =/  =naty:th  [%dtls %noun numb:pre 42]
  (test-slap [%noun ~] %noun naty [%atom %$ ~] 43)
::
++  test-dtts
  %+  weld
    ::  ".=(42 42)"
    =/  =naty:th  [%dtts [%noun %noun 42] [%noun [%atom %$ ~] 42]]
    (test-slap [%noun ~] %noun naty loob:pre &)
  ::  ".=(1 2)"
  =/  =naty:th  [%dtts [%noun %noun 1] [%noun %noun 2]]
  (test-slap [%noun ~] %noun naty loob:pre |)
::
++  test-wtcl
  %+  weld
    ::  "?:(& 'yes' 'no')"
    =/  =naty:th  [%wtcl [%noun loob:pre &] [%noun %noun 'yes'] [%noun %noun 'no']]
    (test-slap [%noun ~] %noun naty %noun 'yes')
  ::  "?:(| 'yes' 'no')"
  =/  =naty:th  [%wtcl [%noun loob:pre |] [%noun %noun 'yes'] [%noun %noun 'no']]
  (test-slap [%noun ~] %noun naty %noun 'no')
  ::TODO  mb test for non-loob conditional
::
++  test-cnts
  ::  ".", two flavors
  %+  weld
    =/  =naty:th  [%cnts ~ ~]
    (test-slap [numb:pre 42] %noun naty numb:pre 42)
  =/  =naty:th  [%cnts ~[&+1] ~]
  (test-slap [numb:pre 42] %noun naty numb:pre 42)
::
++  test-tsgr
  ::  "=>(42 .)"
  =/  =naty:th  [%tsgr [%noun numb:pre 42] [%cnts ~ ~]]
  (test-slap [%noun ~] %noun naty numb:pre 42)
::
++  test-tsls
  ::  "=+(42 .)"
  =/  =naty:th  [%tsls [%noun numb:pre 42] [%cnts ~ ~]]
  (test-slap [%noun 0] %noun naty [%cell numb:pre %noun] [42 0])
::
++  test-sggr
  ::  "~>(%hint .)"
  =/  =naty:th  [%sggr %hint [%cnts ~ ~]]
  =/  [=type:th =nock]  (mint:th %noun %noun naty)
  (expect-eq !>([%11 %hint %0 1]) !>(nock))
::
++  test-brcn
  ::  "|%  ++  $  42  --"
  =/  arm=naty:th  [%noun numb:pre 42]
  =/  =naty:th  [%brcn %gold [%$ arm] ~ ~]
  %:  test-slap
    [%noun ~]
    %noun
    naty
    `type:th`[%core %noun %gold %noun [%$ arm] ~ ~]
    [[1 42] 0]
  ==
::
++  test-cnts-pull
  ::  "=>  |%  ++  $  42  --  $"
  =/  =naty:th
    :+  %tsgr
      [%brcn %gold [%$ [%noun numb:pre 42]] ~ ~]
    [%cnts `wing`[%| 0 `%$]~ ~]
  (test-slap [%noun ~] %noun naty numb:pre 42)
::
++  test-parse-nuck
  ;:  weld
    (expect-eq !>([%noun numb:pre 42]) !>((scan "42" nuck:parse:th)))
    (expect-eq !>([%noun [%atom %ud `42] 42]) !>((scan "%42" nuck:parse:th)))
  ::
    (expect-eq !>([%noun %noun [1 2]]) !>((scan "~04hh" nuck:parse:th)))
    (expect-eq !>([%noun [%cell [. .]:[%atom %$ ~]] [1 2]]) !>((scan "%~04hh" nuck:parse:th)))
  ::
    (expect-eq !>([%noun %noun 1]) !>((scan "._1__" nuck:parse:th)))
    (expect-eq !>([%noun %noun [1 2]]) !>((scan "._1_2__" nuck:parse:th)))
    (expect-eq !>([%noun %noun [1 2 3]]) !>((scan "._1_2_3__" nuck:parse:th)))
    (expect-eq !>([%noun [%cell [%atom %ud `1] [%atom %ud `2]] [1 2]]) !>((scan "%._1_2__" nuck:parse:th)))
  ==
::
++  test-parse-and-run-42
  (expect-eq !>([numb:pre 42]) !>((parse-and-run:th '42')))
::
++  test-parse-and-run-constant-cell
  (expect-eq !>([[%cell [. .]:numb:pre] [42 43]]) !>((parse-and-run:th '[42 43]')))
::
++  test-parse-and-run-dtls-41
  (expect-eq !>([[%atom %$ ~] 42]) !>((parse-and-run:th '.+(41)')))
::
++  test-flatten-naty
  %+  expect-eq
    !>  ^-  naty:th
    :*  [%noun [%cell [. .]:numb:pre] [1 2]]
        [%dtls %noun numb:pre 3]
        [%noun [%cell [. .]:numb:pre] [4 5]]
    ==
  !>((scan "[[1 2] .+(3) 4 5]" apex:parse:th))
--
