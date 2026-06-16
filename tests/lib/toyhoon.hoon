::  toyhoon: tests
::
/+  *test, th=toyhoon, tp=toyhoon-parse
::
=/  p  parser:tp
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
    ++  test-nest-sut
      |=  [sut=vase:th gol=type:th]
      |=  [a=naty:th b=naty:th]
      ^-  tang
      =+  va=(slap:th sut gol a)
      =+  vb=(slap:th sut gol b)
      ?:  (nest:th p.va p.vb)  ~
      :~  'b fails to nest in a'
          'b:'  >p.vb<
          'a:'  >p.va<
      ==
    ::
    ++  test-nest
      (test-nest-sut [%noun ~] %noun)
    ::
    ++  pre
      |%
      ++  loob  ^-  type:th
        [%bcwt (my [%& %f] [%| %f] ~)]
      ++  numb  ^-  type:th
        [%atom %ud ~]
      ++  cell  ^-  type:th
        [%cell numb numb]
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
  =/  =naty:th  [%brcn %gold ~ [%$ arm] ~ ~]
  %:  test-slap
    [%noun ~]
    %noun
    naty
    `type:th`[%core %noun %gold %noun arm [%$ 1] ~ ~]
    [[1 42] 0]
  ==
::
++  test-cnts-pull
  ::  "=>  |%  ++  $  42  --  $"
  =/  =naty:th
    :+  %tsgr
      [%brcn %gold ~ [%$ [%noun numb:pre 42]] ~ ~]
    [%cnts `wing`[%| 0 `%$]~ ~]
  (test-slap [%noun ~] %noun naty numb:pre 42)
::
++  test-core-nesting-match
  ::TODO  same names & geometries nest
  =/  c1=naty:th
    :^  %brcn  %gold  ~
    %-  my
    :~  [%a %noun numb:pre 42]
        [%b %noun cell:pre [42 42]]
    ==
  =/  c2=naty:th
    :^  %brcn  %gold  `[%b %a]
    %-  my
    :~  [%a %noun numb:pre 42]
        [%b %noun cell:pre [42 42]]
    ==
  %+  weld  (test-nest c1 c2)
  (test-nest c2 c1)
::
::TODO  same names & different geometries don't nest
::TODO  dif names & same geometries don't nest
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
::
++  make-primitive-test
  |=  [input=@t match=[tag=tag:tp i=@ud]]
  =+  res=(gate:tp (init-cord-cursor:tp input))
  ?~  res  `tang`~['failed to tokenize' input]
  %+  weld
    (expect-eq !>(tag.match) !>(tag.res))
  (expect-eq !>(i.match) !>(i.cur.res))
::
++  test-primitive-lexer
  ;:  weld
    (make-primitive-test '.*(123)' |+%dttr 3)
    (make-primitive-test '123' |+[%atom | %ud] 3)
    (make-primitive-test '1.234' |+[%atom | %ud] 5)
  ::
    (make-primitive-test '+2' |+%axis 2)
    (make-primitive-test '+>' |+%lark 2)
    (make-primitive-test '$' |+%skip 1)
    (make-primitive-test '^$' |+%skip 2)
    (make-primitive-test 'a.b' |+%skip 1)
  ==
::
::TODO  test +proc
::
++  test-cursor-tracking
  =/  input=@t  '.+\0a1'
  =+  res=~(tall p & (init-cord-cursor:tp input) [1 1] ~)
  ?~  res  ~['failed to parse' input]
  %+  expect-eq
    !>([row=2 col=2])
  !>(~(here parser:tp & s.res))
::
++  make-parser-test
  |=  [input=@t =naty:th]
  =+  res=~(tall p & (init-cord-cursor:tp input) [1 1] ~)
  ?~  res  ~['failed to parse' input]
  %+  weld
    (expect-eq !>(naty) !>(u.res))
  (expect-eq !>(i.cur.s.res) !>(len.cur.s.res))  ::  fully parsed
::
++  make-parser-fail-test
  |=  input=@t
  ^-  tang
  =+  res=~(tall p & (init-cord-cursor:tp input) [1 1] ~)
  ?^  res  ~['parsed unexpectedly' input]
  ~
::
++  test-parser
  =/  z=naty:th  [%noun [%atom %ud ~] 0]
  =/  o=naty:th  [%noun [%atom %ud ~] 1]
  =/  t=naty:th  [%noun [%atom %ud ~] 2]
  =/  w=wing:th  [%| 0 `%$]~
  ;:  weld
    (make-parser-test '123' [%noun [%atom %ud ~] 123])
    (make-parser-test '%123' [%noun [%atom %ud `123] 123])
    (make-parser-test '%foo' [%noun [%atom %tas `%foo] %foo])
    (make-parser-test '.+(1)' [%dtls %noun [%atom %ud ~] 1])
    (make-parser-test '.+  2' [%dtls %noun [%atom %ud ~] 2])
    (make-parser-test '.+  .+(3)' [%dtls %dtls %noun [%atom %ud ~] 3])
  ::
    (make-parser-test ':*(0 1 2)' [z o t])
    (make-parser-test '[0 1 2]' [z o t])
    (make-parser-test ':*  0  1  2  ==' [z o t])
  ::
    (make-parser-test '$' [%cnts [%| 0 `%$]~ ~])
    (make-parser-test '^$' [%cnts [%| 1 `%$]~ ~])
    (make-parser-test 'a.b' [%cnts ~[[%| 0 `%a] [%| 0 `%b]] ~])
  ::
    (make-parser-test '.*(0 0)' [%dttr z z])
    (make-parser-test '.*  0  0' [%dttr z z])
    (make-parser-test '.=(0 0)' [%dtts z z])
    (make-parser-test '.=  0  0' [%dtts z z])
    (make-parser-test '?:(0 0 0)' [%wtcl z z z])
    (make-parser-test '?:  0  0  0' [%wtcl z z z])
    (make-parser-test '=>(0 0)' [%tsgr z z])
    (make-parser-test '=>  0  0' [%tsgr z z])
    (make-parser-test '=+(0 0)' [%tsls z z])
    (make-parser-test '=+  0  0' [%tsls z z])
  ::
    (make-parser-test '^+(0 0)' [%ktls z z])
    (make-parser-test '^+  0  0' [%ktls z z])
  ::
    (make-parser-test '?@($ 0 0)' [%wtpt w z z])
    (make-parser-test '?@  $  0  0' [%wtpt w z z])
    (make-parser-test '?%($ 97 0 0)' [%wtcn w 97 z z])
    (make-parser-test '?%  $  97  0  0' [%wtcn w 97 z z])
    (make-parser-test '?^($ 0 0)' [%wtkt w z z])
    (make-parser-test '?^  $  0  0' [%wtkt w z z])
  ::
    (make-parser-test '~>(%slog 0)' [%sggr %slog z])
    (make-parser-test '~>(%slog.0 0)' [%sggr [%slog z] z])
    (make-parser-test '~>  %slog  0' [%sggr %slog z])
    (make-parser-test '~>  %slog.0  0' [%sggr [%slog z] z])
  ::
    (make-parser-test '%=($ $ 0)' [%cnts w [w z] ~])
    (make-parser-test '%=  $  $  0  ==' [%cnts w [w z] ~])
    (make-parser-test '%=($ $ 0, $ 0)' [%cnts w [w z] [w z] ~])
    (make-parser-test '%=  $  $  0  $  0  ==' [%cnts w [w z] [w z] ~])
  ::
    (make-parser-test '|@  ++  a  0  --' [%brpt ~ (my [%a z] ~)])
    (make-parser-test '|@  ++  a  0  ++  b  1  --' [%brpt ~ (my [%a z] [%b o] ~)])
    (make-parser-test '|@  [a b]  ++  a  0  ++  b  1  --' [%brpt `[%a %b] (my [%a z] [%b o] ~)])
    (make-parser-test '|@  [a *]  ++  a  0  ++  b  1  --' [%brpt `[%a `~] (my [%a z] [%b o] ~)])
    (make-parser-test '|@  :*(a b)  ++  a  0  ++  b  1  --' [%brpt `[%a %b] (my [%a z] [%b o] ~)])
    (make-parser-test '|@  :*(a *)  ++  a  0  ++  b  1  --' [%brpt `[%a `~] (my [%a z] [%b o] ~)])
    (make-parser-test '|@  :*  a  b  ==  ++  a  0  ++  b  1  --' [%brpt `[%a %b] (my [%a z] [%b o] ~)])
    (make-parser-test '|@  :*  a  *  ==  ++  a  0  ++  b  1  --' [%brpt `[%a `~] (my [%a z] [%b o] ~)])
    (make-parser-test '|@  [[a b] c]  ++  a  0  ++  b  1  ++  c  2  --' [%brpt `[[%a %b] %c] (my [%a z] [%b o] [%c t] ~)])
    (make-parser-test '|@  :*  [a b]  c  ==  ++  a  0  ++  b  1  ++  c  2  --' [%brpt `[[%a %b] %c] (my [%a z] [%b o] [%c t] ~)])
    (make-parser-fail-test '|@  [a a]  ++  a  0  ++  b  1  --')
    (make-parser-fail-test '|@  [a b]  ++  a  0  ++  a  1  --')
    (make-parser-fail-test '|@  [a c]  ++  a  0  ++  b  1  --')
    (make-parser-fail-test '|@  [a c]  ++  a  0  ++  b  1  ++  c  2  --')
    (make-parser-fail-test '|@  [a * *]  ++  a  0  ++  b  1  ++  c  2  --')
  ==
--
