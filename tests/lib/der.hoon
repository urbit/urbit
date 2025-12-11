/-  asn1
/+  der, *test
|%
++  test-asn1-der
  =/  nul=spec:asn1  [%nul ~]
  =/  int=spec:asn1  [%int 187]
  =/  obj=spec:asn1  [%obj sha-256:obj:asn1]
  =/  oct=spec:asn1  [%oct 32 (shax 'hello\0a')]
  =/  seq=spec:asn1  [%seq [%seq obj nul ~] oct ~]
  ;:  weld
    %+  expect-eq
      !>  [0x5 0x0 ~]
      !>  ~(ren raw:en:der nul)
  ::
    %+  expect-eq
      !>  nul
      !>  (scan ~(ren raw:en:der nul) parse:de:der)
  ::
    %+  expect-eq
      !>  [0x2 0x2 0x0 0xbb ~]
      !>  ~(ren raw:en:der int)
  ::
    %+  expect-eq
      !>  int
      !>  (scan ~(ren raw:en:der int) parse:de:der)
  ::
    %+  expect-eq
      !>  [0x6 0x9 0x60 0x86 0x48 0x1 0x65 0x3 0x4 0x2 0x1 ~]
      !>  ~(ren raw:en:der obj)
  ::
    %+  expect-eq
      !>  obj
      !>  (scan ~(ren raw:en:der obj) parse:de:der)
  ::
    %+  expect-eq
      !>  0x420.5891.b5b5.22d5.df08.6d0f.f0b1.10fb.
           d9d2.1bb4.fc71.63af.34d0.8286.a2e8.46f6.be03
      !>  `@ux`(swp 3 +:(en:der oct))
  ::
    %+  expect-eq
      !>  oct
      !>  (scan ~(ren raw:en:der oct) parse:de:der)
  ::
    %+  expect-eq
      !>  0x30.3130.0d06.0960.8648.0165.0304.0201.0500.0420.5891.b5b5.22d5.
          df08.6d0f.f0b1.10fb.d9d2.1bb4.fc71.63af.34d0.8286.a2e8.46f6.be03
      !>  `@ux`(swp 3 +:(en:der seq))
  ::
    %+  expect-eq
      !>  seq
      !>  (scan ~(ren raw:en:der seq) parse:de:der)
  ==
--
