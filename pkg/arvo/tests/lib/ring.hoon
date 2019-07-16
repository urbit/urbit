/+  *test, ring
::
=/  eny=@uvJ               `@`0xdead.beef
::
=/  hello-world            (shaz (jam "Hello World"))
=/  not-hello-world        (shaz (jam "Goodbye World"))
::
=/  empty-scope=(unit @)   ~
=/  test-scope=(unit @)    `(shaz (jam [%scope 5]))
=/  test-scope-2=(unit @)  `(shaz (jam [%scope 6]))
::
=/  our-privkey=@udscalar  3
=/  our-pubkey=@udpoint    (scalarmult-base:ed:crypto our-privkey)
::
=/  two-privkey=@udscalar  2
=/  two-pubkey=@udpoint    (scalarmult-base:ed:crypto two-privkey)
::
=/  public-key-set         (sy (turn (gulf 1 5) scalarmult-base:ed:crypto))
|%
++  test-basic-unlinked
  =/  unlinked
    %-  sign:raw:ring  :*
      hello-world
      empty-scope
      public-key-set
      our-pubkey
      our-privkey
      eny
    ==
  ::
  %+  expect-eq
    !>  %.y
    !>  (verify:raw:ring hello-world empty-scope public-key-set unlinked)
::
++  test-linked-different-message-same-key-same-scope
  =/  linked-hello-world
    %-  sign:raw:ring  :*
      hello-world
      test-scope
      public-key-set
      our-pubkey
      our-privkey
      eny
    ==
  ::
  =/  linked-not-hello-world
    %-  sign:raw:ring  :*
      not-hello-world
      test-scope
      public-key-set
      our-pubkey
      our-privkey
      eny
    ==
  ::
  ;:  weld
    %+  expect-eq
      !>  %.y
      !>  (verify:raw:ring hello-world test-scope public-key-set linked-hello-world)
  ::
    %+  expect-eq
      !>  %.y
      !>  (verify:raw:ring not-hello-world test-scope public-key-set linked-not-hello-world)
  ::
    (expect-eq !>(%.y) !>(?=(^ y.linked-hello-world)))
    (expect-eq !>(%.y) !>(?=(^ y.linked-not-hello-world)))
  ::
    %+  expect-eq
      !>  y.linked-hello-world
      !>  y.linked-not-hello-world
  ==
::  if we use the same key to sign the same message in two different scopes, we
::  must have different resulting linkage tags.
::
++  test-linked-same-message-same-key-different-scope
  =/  scope-one-hello-world
    %-  sign:raw:ring  :*
      hello-world
      test-scope
      public-key-set
      our-pubkey
      our-privkey
      eny
    ==
  ::
  =/  scope-two-hello-world
    %-  sign:raw:ring  :*
      hello-world
      test-scope-2
      public-key-set
      our-pubkey
      our-privkey
      eny
    ==
  ::
  ;:  weld
    %+  expect-eq
      !>  %.y
      !>  (verify:raw:ring hello-world test-scope public-key-set scope-one-hello-world)
  ::
    %+  expect-eq
      !>  %.y
      !>  (verify:raw:ring hello-world test-scope-2 public-key-set scope-two-hello-world)
  ::
    (expect-eq !>(%.y) !>(?=(^ y.scope-one-hello-world)))
    (expect-eq !>(%.y) !>(?=(^ y.scope-two-hello-world)))
  ::
    %+  expect-eq
      !>  %.n
      !>  =(y.scope-one-hello-world y.scope-two-hello-world)
  ==
::  the same message signed by two different keys should have different linkage
::
++  test-linked-same-message-different-key
  =/  our-hello-world
    %-  sign:raw:ring  :*
      hello-world
      test-scope
      public-key-set
      our-pubkey
      our-privkey
      eny
    ==
  ::
  =/  two-hello-world
    %-  sign:raw:ring  :*
      hello-world
      test-scope
      public-key-set
      two-pubkey
      two-privkey
      eny
    ==
  ::
  ;:  weld
    %+  expect-eq
      !>  %.y
      !>  (verify:raw:ring hello-world test-scope public-key-set our-hello-world)
  ::
    %+  expect-eq
      !>  %.y
      !>  (verify:raw:ring hello-world test-scope public-key-set two-hello-world)
  ::
    (expect-eq !>(%.y) !>(?=(^ y.our-hello-world)))
    (expect-eq !>(%.y) !>(?=(^ y.two-hello-world)))
  ::
    %+  expect-eq
      !>  %.n
      !>  =(y.our-hello-world y.two-hello-world)
  ==
--
