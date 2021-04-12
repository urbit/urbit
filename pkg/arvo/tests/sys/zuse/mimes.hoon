::  tests for |mimes:html
::
/+  *test
=,  mimes:html
::  helpers
::
|%
++  octn
  |%
  ++  en  |=(a=@u `octs`[(met 3 a) (swp 3 a)])
  ++  de  |=(a=octs `@u`(rev 3 p.a q.a))
  --
++  en-base64url
  ~(en base64 | &)
++  de-base64url
  ~(de base64 | &)
--
::
|%
++  test-en-base16
  ;:  weld
    %+  expect-eq
      !>  'aa'
      !>  (en:base16 (as-octs 0xaa))
  ::
    %+  expect-eq
      !>  '1234'
      !>  (en:base16 (as-octs 0x1234))
  ::
    %+  expect-eq
      !>  'f012'
      !>  (en:base16 (as-octs 0xf012))
  ::
    %+  expect-eq
      !>  '010b'
      !>  (en:base16 (as-octs 0x10b))
  ::
    %+  expect-eq
      !>  '001234'
      !>  (en:base16 3 0x1234)
  ::
    %+  expect-eq
      !>  '34'
      !>  (en:base16 1 0x1234)
  ==
::
++  test-de-base16
  ;:  weld
    %+  expect-eq
      !>  `[1 0xaa]
      !>  ^-  (unit [@ud @ux])
          (de:base16 'aa')
  ::
    %+  expect-eq
      !>  `[2 0x1234]
      !>  `(unit [@ud @ux])`(de:base16 '1234')
  ::
    %+  expect-eq
      !>  `[2 `@`0xf012]
      !>  ^-  (unit [@ud @ux])
          (de:base16 'f012')
  ::
    %+  expect-eq
      !>  lz=`[2 0x10b]
      !>  ^-  (unit [@ud @ux])
          (de:base16 '010b')
  ::
    %+  expect-eq
      !>  nlz=`[2 0x10b]
      !>  ^-  (unit [@ud @ux])
          (de:base16 '10b')
  ::
    %+  expect-eq
      !>  `[3 0x1234]
      !>  ^-  (unit [@ud @ux])
          (de:base16 '001234')
  ==
::
++  test-explode-bytes
  ;:  weld
    %+  expect-eq
      !>  ~
      !>  (flop (explode-bytes:base64 [0 `@`0]))
  ::
    %+  expect-eq
      !>  ~[0x0 0x0 0x0]
      !>  (flop (explode-bytes:base64 [3 `@`0]))
  ::
    %+  expect-eq
      !>  ~[0x1 0x1 0x1]
      !>  (flop (explode-bytes:base64 [3 `@`0x1.0101]))
  ==
::
++  test-explode-words
  ;:  weld
    %+  expect-eq
      !>  ~
      !>  (flop (explode-words:base64 1 [0 `@`0]))
  ::
    %+  expect-eq
      !>  ~[0 0 0 0 0 0 0 1]
      !>  (flop (explode-words:base64 3 [3 `@`1]))
  ::
    %+  expect-eq
      !>  ~[0x0 0x12.3456 0x78.9abc 0xde.f012 0x34.5678]
      !>  =/  inp  [15 `@`0x1234.5678.9abc.def0.1234.5678]
          (flop (explode-words:base64 24 inp))
  ==
::
++  test-base64
  ;:  weld
    %+  expect-eq
      !>  'AQAB'
      !>  (en-base64url (en:octn 65.537))
  ::
    %+  expect-eq
      !>  65.537
      !>  (de:octn (need (de-base64url 'AQAB')))
  ::
    :: echo "hello" | base64
    %+  expect-eq
      !>  'aGVsbG8K'
      !>  (en:base64 (as-octs:mimes:html 'hello\0a'))
  ::
    %+  expect-eq
      !>  'hello\0a'
      !>  +:(need (de:base64 'aGVsbG8K'))
  ::
    :: echo -n -e "\x01\x01\x02\x03" | base64
    %+  expect-eq
      !>  'AQECAw=='
      !>  (en:base64 (en:octn 0x101.0203))
  ::
    %+  expect-eq
      !>  `@t`0x302.0101
      !>  +:(need (de:base64 'AQECAw=='))
  ==
--
