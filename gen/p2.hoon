/?  310
::
/+  pprint
::
!:
::
:-  %say
::
=<  |=  {^ {{=arg ~} ~}}
    ^-  [%txt wain]
    ::
    =/  v=vase
      ?-  target.arg
        ^        target.arg
        %all     !>(all-examples)
        %demo    !>(demo-example)
        %test    !>(test-example)
        %type    !>(type-example)
        %xml     !>(xml-example)
        %kernel  !>(xray-the-kernel-example)
        %parser  !>(xray-the-parser-example)
      ==
    ::
    :-  %txt
    ?-  print.arg
      %type  (render-type:pprint p.v)
      %val   (render-vase:pprint v)
      %both  (render-vase-with-type:pprint v)
    ==
::
|%
::
+$  arg
  $:  print=?(%type %val %both)
      target=$@(?(%all %demo %test %type %xml %kernel %parser) vase)
  ==
::
+$  option  $?(%a %b %c)
::
+$  junct  $@(@ {@ cord})
::
+$  union  $%([%list (list ~)] [%unit (unit ~)])
::
+$  conjunct  $^  [[@ @] cord]
              [@ cord]
::
+$  misjunct  $^([~ @] [cord @])
::
++  forks-example
  :*  :-  %junct     ^-  (list junct)     ~[3 [4 '5']]
      :-  %conjunct  ^-  (list conjunct)  ~[[3 '4'] [[5 6] '7']]
      :-  %union     ^-  (list union)     ~[[%list [~ ~]] [%unit [~ ~]]]
      :-  %option    ^-  (list option)    ~[%a %a %b %c]
      :-  %misjunct  ^-  (list misjunct)  ~[[~ 3] [~ 4]]
      %nice
  ==
::
++  all-examples
  :*
    :-  %type   type-example
    :-  %cores  core-example
    :-  %add    ..add
    :-  zuse-example
    :-  %demo   demo-example
    :-  %forks  forks-example
    %eof
  ==
::
++  type-example
  ^-  type
  -:!>(`(map ? (unit (list cord)))`~)
::
++  xray-the-parser-example
  =>  ..musk
  |%  ++  x  ~  --
::
++  xray-the-kernel-example
  |%  ++  x  ~  --
::
++  zuse-example
  [%zuse ..zuse]
::
++  cores-example
  |^  :*
          [%trivial trivial-core-example]
          [%gate gate-example]
          [%core core-example]
      ==
  ::
  --
::
++  trivial-core-example
  =>  ~
  |%  ++  x  3  --
::
++  core-example
  =>  [=gate-example]
  |%
  ++  dup  gate-example
  ++  const
    |=  x=*  ^-  $-(* *)
    |=  *    ^-  *
    x
  --
::
++  gate-example
  =>  ~
  |=  x=@ud
  ^-  [@ud @ud]
  [x x]
::
++  test-example
  :*
    `(list ?)`~[%.y %.n]
    `(list ~)`~[~ ~]
    `(unit ~)``~
    /a/path
  ==
::
++  hoon-example
  ^-  hoon
  :+  %brcn  ~
  %-  ~(gas by *(map term tome))
  ^-  (list (pair term tome))
  :_  ~
  ^-  (pair term tome)
  :-  'chapter'
  ^-  tome
  :-  `what`~
  %-  ~(gas by *(map term hoon))
  ^-  (list (pair term hoon))
  :_  ~
  :-  'arm'
  :+  %brts  `spec`[%bsts 'x' [%base [%atom ~.ud]]]
  :-  %clsg
  ~[[%wing ~['x']] [%$ 0]]
::
++  demo-example
  :*  [~ %.y %.n 1 0x2 ~ ~.knot 'cord' %const]
      :*  [%tape "a tape"]
          [%path /path/literal `path`/typed/path]
          [%unit `(unit @)`[~ 9]]
          [%list [`?`%.y `(list ?)`~[%.y %.n %.y]]]
          %nice
      ==
      [%hoon hoon-example]
      [%type -:!>(`(unit (list tape))`~)]
      [%json-and-xml json-example xml-example]
      %cool
  ==
  ::
++  xml-example
  |^  ^-  manx
      :-  ['json' ~]
      :~  (json-to-xml json-example)
      ==
  ++  json-to-xml
    |=  j=json
    ^-  manx
    ?-  j
      ~       [['nil' ~] ~]
      [%a *]  [['array' ~] (turn p.j json-to-xml)]
      [%b *]  [['bool' ~[['' ?:(p.j "true" "false")]]] ~]
      [%o *]  [['obj' ~] (turn ~(tap by p.j) pair)]
      [%n *]  [['num' ~[[['n' 'val'] (trip p.j)]]] ~]
      [%s *]  [['str' ~[['' (trip p.j)]]] ~]
    ==
  ++  pair
    |=  [t=@t j=json]
    ^-  manx
    [['slot' ~[['key' (trip t)]]] ~[(json-to-xml j)]]
  --
::
++  json-example
  ^-  json
  |^  ob2
  ++  nil  ~
  ++  yes  [%b %.y]
  ++  nah  [%b %.n]
  ++  str  [%s 'Very long test string. Test test test test test test test.']
  ++  foo  'foo'
  ++  bar  'bar'
  ++  baz  'baz'
  ++  one  [%n '1']
  ++  ten  [%n '10']
  ++  mil  [%n '100000']
  ++  arr  [%a ~[one ten mil]]
  ++  ar2  [%a ~[arr yes nah nil str]]
  ++  obj  [%o (~(gas by *(map @t json)) ~[[foo mil] [baz arr]])]
  ++  ob2  [%o (~(gas by *(map @t json)) ~[[foo ar2] [bar obj] [baz yes]])]
  ++  ar3  [%a ~[arr obj ob2 one ten mil yes nah nil]]
  --
::
--
