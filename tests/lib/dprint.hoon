::  This library contains unit tests for doccords.
::
/+  *test, *dprint
::
=>  =>
    ::    core-summary
    ::
    ::  core-description
    |%
    ::    chapter-summary
    ::
    ::  chapter-description
    +|  %chapter-test
    ++  empty  ~
    --
  ::
  ::    arms used for testing doccords
  |%
  +|  %types
  +$  arm-dox      [tape what what what]
  +$  core-dox     what
  +$  chapter-dox  [tape what]
  ::
  +|  %helper-functions
  ++  get-item
    |=  a=(list term)
    ^-  item
    +:(find-item-in-type a -:!>(.))
  ::
  ++  get-arm-dox
    |=  a=(list term)
    ^-  arm-dox
    =/  itm=item  (get-item a)
    ?>  ?=([%arm *] itm)
    [name adoc pdoc cdoc]:itm
  ::
  ++  get-core-dox
    |=  a=(list term)
    ^-  core-dox
    =/  itm=item  (get-item a)
    ?>  ?=([%core *] itm)
    docs:itm
  ::
  ++  get-chapter-dox
    |=  a=(list term)
    ^-  chapter-dox
    =/  itm=item  (get-item a)
    ?>  ?=([%chapter *] itm)
    [name docs]:itm
  ::
  ++  arm-check
    |=  [arms=(list term) wat=(trel ? ? ?) docs=(trel what what what)]
    ^-  tang
    =|  res=tang
    |-
    ?~  arms  res
    %=  $
      arms  t.arms
      res   %+  weld
              res
            %+  expect-eq
              !>  ^-  arm-dox
              :*  (trip i.arms)
                ?:(p.wat p.docs *what)
                ?:(q.wat q.docs *what)
                ?:(r.wat r.docs *what)
              ==
            ::
              !>  (get-arm-dox ~[i.arms])
    ==
  ::
  ++  run-arm-tests
    |=  [wat=(trel ? ? ?)]
    ^-  tang
    =/  num  (sub 3 :(add p.wat q.wat r.wat))
    =/  prefix=term
      ;:  (cury cat 3)
        %arm-
        ?:(p.wat %adoc- ~)
        ?:(q.wat %pdoc- ~)
        ?:(r.wat %cdoc- ~)
      ==
    =/  arms=(list term)
        %-  turn
        :_  |=  [postfix=term]
            `term`(cat 3 prefix postfix)
        ^-  (list term)
        ?+  num  ~
          %1  ~[%pre %post]
          %2  ~[%pre-pre %post-pre %post-post]
          %3  :~  %pre-pre-pre
                  %pre-pre-post
                  %pre-post-pre
                  %post-pre-pre
                  %pre-post-post
                  %post-pre-post
                  %post-post-pre
               ==
         ==
    %^    arm-check
        arms
      wat
    [`['arm-doc' ~] `['product-doc' ~] `['core-doc' ~]]
  ::
  +|  %batch-comments
  ::  +b-foo: a foo
  ::  $b-baz: a baz
  ::    +b-bar: a bar
  ::
  ::  a very bar foo
  ::
  ::    $b-boz: a boz
  ::
  ::  a very boz baz
  ::
  ++  b-foo  ~
  ++  b-bar  ~
  +$  b-baz  *
  +$  b-boz  *
  ::
  +|  %docs-for-arms
  ++  no-doc  ~
  ::
  ::  +arm-adoc-pre: arm-doc
  ++  arm-adoc-pre  ~
  ::
  ++  arm-adoc-post  ::  arm-doc
    ~
  ::
  ++  arm-pdoc-pre
    ::    product-doc
    'foo'
  ++  arm-pdoc-post
    'foo'  ::  product-doc
  ::
  ++  arm-cdoc-pre
    |%
    ::    core-doc
    ++  $  ~
    --
  ::
  ++  arm-cdoc-post
    |%
    ++  $  ::  core-doc
      ~
    --
  ::
  ::  +arm-adoc-pdoc-pre-pre: arm-doc
  ++  arm-adoc-pdoc-pre-pre
    ::    product-doc
    'foo'
  ::
  ++  arm-adoc-pdoc-post-pre  ::  arm-doc
    ::    product-doc
    'foo'
  ::
  ++  arm-adoc-pdoc-post-post  ::  arm-doc
    'foo'  ::  product-doc
  ::
  ::  +arm-adoc-cdoc-pre-pre: arm-doc
  ++  arm-adoc-cdoc-pre-pre
    |%
    ::    core-doc
    ++  $  ~
    --
  ::
  ++  arm-adoc-cdoc-post-pre  ::  arm-doc
    |%
    ::    core-doc
    ++  $  ~
    --
  ::
  ++  arm-adoc-cdoc-post-post  ::  arm-doc
    |%
    ++  $  ::  core-doc
      ~
    --
  ::
  ++  arm-pdoc-cdoc-pre-pre
    ::    product-doc
    |%
    ::    core-doc
    ++  $  ~
    --
  ++  arm-pdoc-cdoc-post-pre
    |%
    ::    core-doc
    ++  $  ~
    --  ::  product-doc
  ::
  ++  arm-pdoc-cdoc-post-post
    |%
    ++  $  ::  core-doc
      ~
    --  ::  product-doc
  ::
  ::  +arm-adoc-pdoc-cdoc-pre-pre-pre: arm-doc
  ++  arm-adoc-pdoc-cdoc-pre-pre-pre
    ::    product-doc
    |%
    ::    core-doc
    ++  $  ~
    --
  ::
  ++  arm-adoc-pdoc-cdoc-post-pre-pre  ::  arm-doc
    ::    product-doc
    |%
    ::    core-doc
    ++  $  ~
    --
  ::
  ::  +arm-adoc-pdoc-cdoc-pre-post-pre: arm-doc
  ++  arm-adoc-pdoc-cdoc-pre-post-pre
    |%
    ::    core-doc
    ++  $  ~
    --  ::  product-doc
  ::
  ::  +arm-adoc-pdoc-cdoc-pre-pre-post: arm-doc
  ++  arm-adoc-pdoc-cdoc-pre-pre-post
    ::    product-doc
    |%
    ++  $  ::  core-doc
      ~
    --
  ::
  ++  arm-adoc-pdoc-cdoc-post-post-pre  ::  arm-doc
    |%
    ::    core-doc
    ++  $  ~
    --  ::  product-doc
  ::
  ++  arm-adoc-pdoc-cdoc-post-pre-post  ::  arm-doc
    ::    product-doc
    |%
    ++  $  ::  core-doc
      ~
    --
  ::
  ::  +arm-adoc-pdoc-cdoc-pre-post-post: arm-doc
  ++  arm-adoc-pdoc-cdoc-pre-post-post
    |%
    ++  $  ::  core-doc
      ~
    --  ::  product-doc
  ::
  ++  arm-adoc-pdoc-cdoc-post-post-post  ::  arm-doc
    |%
    ++  $  ::  core-doc
      ~
    --  ::  product-doc
  ::
  --
::
::    contains the actual tests to be run by -test
|%
+|  %batch-tests
  ++  test-b-foo
    %+  expect-eq
      !>  ^-  arm-dox
      ["b-foo" `['a foo' ~] *what *what]
    ::
      !>  (get-arm-dox ~[%b-foo])
  ::
  ++  test-b-bar
    %+  expect-eq
      !>  ^-  arm-dox
      ["b-bar" `['a bar' ~[~[[& 'a very bar foo']]]] *what *what]
    ::
      !>  (get-arm-dox ~[%b-bar])
  ::
  ++  test-b-baz
    %+  expect-eq
      !>  ^-  arm-dox
      ["b-baz" `['a baz' ~] *what *what]
    ::
      !>  (get-arm-dox ~[%b-baz])
  ::
  ++  test-b-boz
    %+  expect-eq
      !>  ^-  arm-dox
      ["b-boz" `['a boz' ~[~[[& 'a very boz baz']]]] *what *what]
    ::
      !>  (get-arm-dox ~[%b-boz])
  ::
+|  %arm-tests
  ::
++  test-no-doc
  %+  expect-eq
    !>  ^-  arm-dox
    ["no-doc" *what *what *what]
  ::
    !>  (get-arm-dox ~[%no-doc])
::
++  test-adoc  (run-arm-tests & | |)
++  test-pdoc  (run-arm-tests | & |)
++  test-cdoc  (run-arm-tests | | &)
++  test-adoc-pdoc  (run-arm-tests & & |)
++  test-adoc-cdoc  (run-arm-tests & | &)
++  test-pdoc-cdoc  (run-arm-tests | & &)
++  test-adoc-pdoc-cdoc  (run-arm-tests & & &)
::
::  +|  %core-tests
::  ++  test-core
::    %+  expect-eq
::      !>  ^-  core-dox
::      `['core-summary' ~[~[[%.y 'core-description']]]]
::    ::
::      !>  (get-core-dox ~[%core-summary])
::  ::
::  ++  test-chapter
::    %+  expect-eq
::      !>  ^-  chapter-dox
::      ["chapter-test" `['chapter-summary' ~[~[[%.y 'chapter-description']]]]]
::    ::
::      !>  (get-chapter-dox ~[%core-summary %chapter-test])
::
--
