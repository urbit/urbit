::  This library contains unit tests for doccords.
::
/+  *test, *dprint
::
=>  =>
    ::  This core is for testing core and chapter docs.
    :>  #  %core-test
    :>
    :>    core-summary
    :>
    :>  core-description
    |%
    :>    chapter-summary
    :>
    :>  chapter-description
    +|  %chapter-test
    ++  empty  ~
    --
  ::
  :>  #  %test-arms
  :>
  :>    arms used for testing arm documentation
  |%
  +|  %types
  +$  arm-dox  [tape what what what]
  +$  core-dox  [tape what]
  +$  chapter-dox  [tape what]
  ::
  +|  %helper-functions
  ++  get-item
    |=  a=(list term)
    ^-  item
    +:(find-item-in-type a -:!>(.) %.y)
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
    [name docs]:itm
  ::
  ++  get-chapter-dox
    |=  a=(list term)
    ^-  chapter-dox
    =/  itm=item  (get-item a)
    ?>  ?=([%chapter *] itm)
    [name docs]:itm
  ::
  +|  %docs-for-arms
  ++  no-doc  ~
  ::
  :>  arm-doc
  ++  arm-adoc-pre  ~
  ::
  ++  arm-adoc-post  :<  arm-doc
    ~
  ::
  ++  arm-pdoc-pre
    :>  product-doc
    'foo'
  ++  arm-pdoc-post
    'foo'  :<  product-doc
  ::
  ++  arm-cdoc-pre
    |%
    :>  core-doc
    ++  $  ~
    --
  ::
  ++  arm-cdoc-post
    |%
    ++  $  :<  core-doc
      ~
    --
  ::
  :>  arm-doc
  ++  arm-adoc-pdoc-pre-pre
    :>  product-doc
    'foo'
  ::
  ++  arm-adoc-pdoc-post-pre  :<  arm-doc
    :>  product-doc
    'foo'
  ::
  ++  arm-adoc-pdoc-post-post  :<  arm-doc
    'foo'  :<  product-doc
  ::
  :>  arm-doc
  ++  arm-adoc-cdoc-pre-pre
    |%
    :>  core-doc
    ++  $  ~
    --
  ::
  ++  arm-adoc-cdoc-post-pre  :<  arm-doc
    |%
    :>  core-doc
    ++  $  ~
    --
  ::
  ++  arm-adoc-cdoc-post-post  :<  arm-doc
    |%
    ++  $  :<  core-doc
      ~
    --
  ::
  ++  arm-pdoc-cdoc-pre-pre
    :>  product-doc
    |%
    :>  core-doc
    ++  $  ~
    --
  ++  arm-pdoc-cdoc-post-pre
    |%
    :>  core-doc
    ++  $  ~
    --  :<  product-doc
  ::
  ++  arm-pdoc-cdoc-post-post
    |%
    ++  $  :<  core-doc
      ~
    --  :<  product-doc
  ::
  :>  arm-doc
  ++  arm-adoc-pdoc-cdoc-pre-pre-pre
    :>  product-doc
    |%
    :>  core-doc
    ++  $  ~
    --
  ::
  ++  arm-adoc-pdoc-cdoc-post-pre-pre  :<  arm-doc
    :>  product-doc
    |%
    :>  core-doc
    ++  $  ~
    --
  ::
  :>  arm-doc
  ++  arm-adoc-pdoc-cdoc-pre-post-pre
    |%
    :>  core-doc
    ++  $  ~
    --  :<  product-doc
  ::
  :>  arm-doc
  ++  arm-adoc-pdoc-cdoc-pre-pre-post
    :>  product-doc
    |%
    ++  $  :<  core-doc
      ~
    --
  ::
  ++  arm-adoc-pdoc-cdoc-post-post-pre  :<  arm-doc
    |%
    :>  core-doc
    ++  $  ~
    --  :<  product-doc
  ::
  ++  arm-adoc-pdoc-cdoc-post-pre-post  :<  arm-doc
    :>  product-doc
    |%
    ++  $  :<  core-doc
      ~
    --
  ::
  :>  arm-doc
  ++  arm-adoc-pdoc-cdoc-pre-post-post
    |%
    ++  $  :<  core-doc
      ~
    --  :<  product-doc
  ::
  ++  arm-adoc-pdoc-cdoc-post-post-post  :<  arm-doc
    |%
    ++  $  :<  core-doc
      ~
    --  :<  product-doc
  ::
  --
::
:>  #  %dprint-tests
:>
:>    contains the actual tests to be run by -test
|%
++  test-no-doc
  %+  expect-eq
    !>  (get-arm-dox ~[%no-doc])
  ::
    !>  ^-  arm-dox
    ["no-doc" *what *what *what]
::
++  test-adoc
  ;:  weld
    %+  expect-eq
      !>  (get-arm-dox ~[%arm-adoc-pre])
    ::
      !>  ^-  arm-dox
      ["arm-adoc-pre" `['arm-doc' ~] *what *what]
    ::
    %+  expect-eq
      !>  (get-arm-dox ~[%arm-adoc-post])
    ::
      !>  ^-  arm-dox
      ["arm-adoc-post" `['arm-doc' ~] *what *what]
  ==
::
++  test-pdoc
  ;:  weld
    %+  expect-eq
      !>  (get-arm-dox ~[%arm-pdoc-pre])
    ::
      !>  ^-  arm-dox
      ["arm-pdoc-pre" *what `['product-doc' ~] *what]
    ::
    %+  expect-eq
      !>  (get-arm-dox ~[%arm-pdoc-post])
    ::
      !>  ^-  arm-dox
      ["arm-pdoc-post" *what `['product-doc' ~] *what]
  ==
::
++  test-cdoc
  ;:  weld
    %+  expect-eq
      !>  (get-arm-dox ~[%arm-cdoc-pre])
    ::
      !>  ^-  arm-dox
      ["arm-cdoc-pre" *what *what `['core-doc' ~]]
    ::
    %+  expect-eq
      !>  (get-arm-dox ~[%arm-cdoc-post])
    ::
      !>  ^-  arm-dox
      ["arm-cdoc-post" *what *what `['core-doc' ~]]
  ==
::
++  test-adoc-pdoc
  ;:  weld
    %+  expect-eq
      !>  (get-arm-dox ~[%arm-adoc-pdoc-pre-pre])
    ::
      !>  ^-  arm-dox
      ["arm-adoc-pdoc-pre-pre" `['arm-doc' ~] `['product-doc' ~] *what]
    ::
    %+  expect-eq
      !>  (get-arm-dox ~[%arm-adoc-pdoc-post-pre])
    ::
      !>  ^-  arm-dox
      ["arm-adoc-pdoc-post-pre" `['arm-doc' ~] `['product-doc' ~] *what]
    ::
    %+  expect-eq
      !>  (get-arm-dox ~[%arm-adoc-pdoc-post-post])
    ::
      !>  ^-  arm-dox
      ["arm-adoc-pdoc-post-post" `['arm-doc' ~] `['product-doc' ~] *what]
  ==
::
++  test-adoc-cdoc
  ;:  weld
    %+  expect-eq
      !>  (get-arm-dox ~[%arm-adoc-cdoc-pre-pre])
    ::
      !>  ^-  arm-dox
      ["arm-adoc-cdoc-pre-pre" `['arm-doc' ~] *what `['core-doc' ~]]
    ::
    %+  expect-eq
      !>  (get-arm-dox ~[%arm-adoc-cdoc-post-pre])
    ::
      !>  ^-  arm-dox
      ["arm-adoc-cdoc-post-pre" `['arm-doc' ~] *what `['core-doc' ~]]
    ::
    %+  expect-eq
      !>  (get-arm-dox ~[%arm-adoc-cdoc-post-post])
    ::
      !>  ^-  arm-dox
      ["arm-adoc-cdoc-post-post" `['arm-doc' ~] *what `['core-doc' ~]]
  ==
::
++  test-pdoc-cdoc
  ;:  weld
    %+  expect-eq
      !>  (get-arm-dox ~[%arm-pdoc-cdoc-pre-pre])
    ::
      !>  ^-  arm-dox
      ["arm-pdoc-cdoc-pre-pre" *what `['product-doc' ~] `['core-doc' ~]]
    ::
    %+  expect-eq
      !>  (get-arm-dox ~[%arm-pdoc-cdoc-post-pre])
    ::
      !>  ^-  arm-dox
      ["arm-pdoc-cdoc-post-pre" *what `['product-doc' ~] `['core-doc' ~]]
    ::
    %+  expect-eq
      !>  (get-arm-dox ~[%arm-pdoc-cdoc-post-post])
    ::
      !>  ^-  arm-dox
      ["arm-pdoc-cdoc-post-post" *what `['product-doc' ~] `['core-doc' ~]]
  ==
::
++  test-adoc-pdoc-cdoc
  ;:  weld
    %+  expect-eq
      !>  (get-arm-dox ~[%arm-adoc-pdoc-cdoc-pre-pre-pre])
    ::
      !>  ^-  arm-dox
      :^    "arm-adoc-pdoc-cdoc-pre-pre-pre"
          `['arm-doc' ~]
        `['product-doc' ~]
      `['core-doc' ~]
    ::
    %+  expect-eq
      !>  (get-arm-dox ~[%arm-adoc-pdoc-cdoc-post-pre-pre])
    ::
      !>  ^-  arm-dox
      :^    "arm-adoc-pdoc-cdoc-post-pre-pre"
          `['arm-doc' ~]
        `['product-doc' ~]
      `['core-doc' ~]
    ::
    %+  expect-eq
      !>  (get-arm-dox ~[%arm-adoc-pdoc-cdoc-pre-post-pre])
    ::
      !>  ^-  arm-dox
      :^    "arm-adoc-pdoc-cdoc-pre-post-pre"
          `['arm-doc' ~]
        `['product-doc' ~]
      `['core-doc' ~]
    ::
    %+  expect-eq
      !>  (get-arm-dox ~[%arm-adoc-pdoc-cdoc-pre-pre-post])
    ::
      !>  ^-  arm-dox
      :^    "arm-adoc-pdoc-cdoc-pre-pre-post"
          `['arm-doc' ~]
        `['product-doc' ~]
      `['core-doc' ~]
    ::
    %+  expect-eq
      !>  (get-arm-dox ~[%arm-adoc-pdoc-cdoc-post-post-pre])
    ::
      !>  ^-  arm-dox
      :^    "arm-adoc-pdoc-cdoc-post-post-pre"
          `['arm-doc' ~]
        `['product-doc' ~]
      `['core-doc' ~]
    ::
    %+  expect-eq
      !>  (get-arm-dox ~[%arm-adoc-pdoc-cdoc-post-pre-post])
    ::
      !>  ^-  arm-dox
      :^    "arm-adoc-pdoc-cdoc-post-pre-post"
          `['arm-doc' ~]
        `['product-doc' ~]
      `['core-doc' ~]
    ::
    %+  expect-eq
      !>  (get-arm-dox ~[%arm-adoc-pdoc-cdoc-pre-post-post])
    ::
      !>  ^-  arm-dox
      :^    "arm-adoc-pdoc-cdoc-pre-post-post"
          `['arm-doc' ~]
        `['product-doc' ~]
      `['core-doc' ~]
  ==
::
++  test-core
  %+  expect-eq
    !>  (get-core-dox ~[%core-test])
  ::
    !>  ^-  core-dox
    ["core-test" `['core-summary' ~[~[[%.y 'core-description']]]]]
::
++  test-chapter
  %+  expect-eq
    !>  (get-chapter-dox ~[%core-test %chapter-test])
  ::
    !>  ^-  chapter-dox
    ["chapter-test" `['chapter-summary' ~[~[[%.y 'chapter-description']]]]]
::
--
