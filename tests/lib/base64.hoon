/+  base64, tester
:: XX move to zuse
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
|_  _tester:tester
++  test-base64
  ;:  weld
    %+  expect-nu-eq
      !>  'AQAB'
      !>  (en-base64url (en:octn 65.537))
  ::
    %+  expect-nu-eq
      !>  65.537
      !>  (de:octn (need (de-base64url 'AQAB')))
  ::
    :: echo "hello" | base64
    %+  expect-nu-eq
      !>  'aGVsbG8K'
      !>  (en:base64 (as-octs:mimes:html 'hello\0a'))
  ::
    %+  expect-nu-eq
      !>  'hello\0a'
      !>  +:(need (de:base64 'aGVsbG8K'))
  ::
    :: echo -n -e "\x01\x01\x02\x03" | base64
    %+  expect-nu-eq
      !>  'AQECAw=='
      !>  (en:base64 (en:octn 0x101.0203))
  ::
    %+  expect-nu-eq
      !>  `@t`0x302.0101
      !>  +:(need (de:base64 'AQECAw=='))
  ==
--

