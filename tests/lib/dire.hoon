/+  *test, dire
=/  dire-zod  (dire ~zod)
=/  dire-nec  (dire ~nec)
=>  |%
    ++  dbug  `?`|
    ++  make-roof
      |=  [pax=path val=cage]
      ^-  roof
      |=  [lyc=gang pov=path vis=view bem=beam]
      ^-  (unit (unit cage))
      ?.  ?&(=(s.bem pax) =(vis [%g %x]))  [~ ~]
      ``val
    ++  reply
      |=  [d=_dire-zod =roof pac=(list move:dire)]
      ^-  [(list move:dire) _dire-zod]
      ?>  ?=([[* [%give [%send *]]] ~] pac)
      %-  call:(d *@da *@uvJ roof)
      [`duct`[/foo ~] ~ %soft `task:dire`[%hear *lane:pact:dire q.gift.q.i.pac]]
    ++  expect-msg
      |=  [pac=(list move:dire) exp=@]
      ?>  ?=([[* [%give [%response *]]] ~] pac)
      ?>  ?=([@tas @] r.gift.q.i.pac)
      (expect-eq !>(q.r.gift.q.i.pac) !>(exp))
    --
::
|%
++  test-dire-peek-no-proof
  ::  tell ~zod to ask ~nec for a 1-fragment message
  ::
  =/  dat=@  'hi'
  =/  bex-roof  (make-roof //hello/atom atom+!>(dat))
  =^  mov1  dire-zod
    %-  call:(dire-zod *@da *@uvJ bex-roof)
    =/  pat  /publ/0/g/x/0/dap//hello/atom
    [`duct`[/foo ~] ~ %soft `task:dire`[%make-peek ~nec pat]]
  =^  mov2  dire-nec  (reply dire-nec bex-roof mov1) :: "ok, here is the 1st fragment"
  =^  mov3  dire-zod  (reply dire-zod bex-roof mov2) :: "ok, here is the complete message"
  (expect-msg mov3 dat)
::
++  test-dire-peek-inline-proof
  ::  3-fragment message; proof is inlined into first response
  ::
  =/  dat  (bex (bex 14))
  =/  bex-roof  (make-roof //some/data/atom atom+!>(dat))
  =^  mov1  dire-zod
    %-  call:(dire-zod *@da *@uvJ bex-roof)
    =/  pat  /chum/0/~nec/0/g/x/0/dap//some/data/atom
    [`duct`[/foo ~] ~ %soft `task:dire`[%make-peek ~nec pat]]
  =^  mov2  dire-nec  (reply dire-nec bex-roof mov1) :: "ok, here is the 1st fragment"
  =^  mov3  dire-zod  (reply dire-zod bex-roof mov2) :: "ok, give me the 2nd fragment"
  =^  mov4  dire-nec  (reply dire-nec bex-roof mov3) :: "ok, here is the 2nd fragment"
  =^  mov5  dire-zod  (reply dire-zod bex-roof mov4) :: "ok, give me the 3rd fragment"
  =^  mov6  dire-nec  (reply dire-nec bex-roof mov5) :: "ok, here is the 3rd fragment"
  =^  mov7  dire-zod  (reply dire-zod bex-roof mov6) :: "ok, here is the complete message"
  (expect-msg mov7 dat)
::
++  test-dire-peek-standalone-proof
  ::  5-fragment message; proof is sent standalone as first response
  ::
  =/  dat  (bex (bex 15))
  =/  bex-roof  (make-roof //shrek/atom atom+!>(dat))
  =^  mov1  dire-zod
    %-  call:(dire-zod *@da *@uvJ bex-roof)
    =/  pat  /shut/0/g/x/0/dap//shrek/atom
    [`duct`[/foo ~] ~ %soft `task:dire`[%make-peek ~nec pat]]
  =^  mov2  dire-nec  (reply dire-nec bex-roof mov1) :: "ok, here is the proof"
  =^  mov3  dire-zod  (reply dire-zod bex-roof mov2) :: "ok, give me the 1st fragment"
  =^  mov4  dire-nec  (reply dire-nec bex-roof mov3) :: "ok, here is the 1st fragment"
  =^  mov5  dire-zod  (reply dire-zod bex-roof mov4) :: "ok, give me the 2nd fragment"
  =^  mov6  dire-nec  (reply dire-nec bex-roof mov5) :: "ok, here is the 2nd fragment"
  =^  mov7  dire-zod  (reply dire-zod bex-roof mov6) :: "ok, give me the 3rd fragment"
  =^  mov8  dire-nec  (reply dire-nec bex-roof mov7) :: "ok, here is the 3rd fragment"
  =^  mov9  dire-zod  (reply dire-zod bex-roof mov8) :: "ok, give me the 4th fragment"
  =^  mova  dire-nec  (reply dire-nec bex-roof mov9) :: "ok, here is the 4th fragment"
  =^  movb  dire-zod  (reply dire-zod bex-roof mova) :: "ok, give me the 5th fragment"
  =^  movc  dire-nec  (reply dire-nec bex-roof movb) :: "ok, here is the 5th fragment"
  =^  movd  dire-zod  (reply dire-zod bex-roof movc) :: "ok, here is the complete message"
  (expect-msg movd dat)
::
++  test-dire-ns
  =/  bex-roof  (make-roof //some/data/atom atom+!>((bex (bex 14))))
  =/  res
    %-  scry:(dire-zod *@da *@uvJ bex-roof)
    =-  [~ / %x [[~zod %$ ud+1] -]]
    /mess/0/pact/13/pure/init/publ/0/g/x/0/dap//some/data/atom
  ?~  res
    ~&  %sig  ~
  ?~  u.res
    ~&  %inner-sig  ~
  ~&  ;;([@tas pact:pact:dire] [p q.q]:u.u.res)
  ~
::
++  test-dire-chacha
  ;:  weld
    %+  expect-eq
    !>  64^0x8665.eeb2.69b6.87c3.1ca1.1815.f4b8.436a.374a.d8b8.3fe0.2477.8d48.5751.7c59.41da.c70d.778b.ccef.36a8.1aed.8da0.b819.d2bd.28bd.8653.e56a.5d40.903d.f1a0.ade0.b876
    !>  (chacha:dire 20 0x0 0x0 0 64^0)
  ::
    %+  expect-eq
    !>  64^0x45e.75c4.d115.1f45.44e8.1525.ff4e.f862.6ef2.d08d.c90b.8e6f.82b2.20d3.e462.7b0a.4da3.a1da.6b76.4da0.3832.0408.1c8c.5092.1701.bbae.a97d.80e3.f914.1301.b39f.5011
    !>  (chacha:dire 20 0x8 0x0 0 64^0)
  ::
    %+  expect-eq
    !>  64^0x6f4d.794b.1f0a.e1ac.45fb.0a51.281f.ed31.d539.d874.b033.71d5.434e.e69c.7621.b729.ed7a.ee32.3e53.c612.6965.e348.a029.0fcb.0d08.2d73.7c97.ba98.7a38.5155.bee7.079f
    !>  (chacha:dire 20 0x0 0x0 1 64^0)
  ::
    %+  expect-eq
    !>  64^0x5a2b.6209.cf28.02f5.618a.4603.b9b5.8206.af59.61c9.17cd.81c2.9096.9899.9846.1958.3a68.dc3b.87e3.80b6.c9d5.4369.0017.9a9c.d54b.e2e6.25f2.e65d.2829.d3a0.3a1d.b43d
    !>  (chacha:dire 20 0x0 0x0 (bex 32) 64^0)
  ::
    =*  key  (rev 3 32 0x120.0000.0000.0000.0000.0000.0000.0070.0000.0000.0000.0000.0000.0000.0000.0def)
    %+  expect-eq
    !>  64^0xf8d0.2ab0.95f4.c33e.68e9.817f.70d2.3760.740a.5153.fc50.107e.a3ec.c4cb.ec20.b8a8.be8d.9b5b.8aa4.81a4.6b1f.aa78.6349.f59f.54f0.0f84.53ec.7f5b.819c.f7c4.79ce.6bba
    !>  (chacha:dire 20 key 0x0 0 64^0)
  ::
    =*  key  (rev 3 32 0x1.0203.0405.0607.0809.0a0b.0c0d.0e0f.1011.1213.1415.1617.1819.1a1b.1c1d.1e1f)
    =*  nonce  (rev 3 8 0x1.0203.0405.0607)
    %+  expect-eq
    !>  64^0x7379.5390.2219.1fb6.980d.b776.c8ed.acf9.df6e.1b0a.b7ea.ef80.63c1.3267.140e.0bbf.fb37.3bd1.aab1.1b3f.5d9a.bf24.e8b0.47ce.5dc0.fe28.b78e.b128.aa3b.841c.eaaa.e140
    !>  (chacha:dire 8 key nonce 0 64^0)
  ::
  ::  double-xor should cancel
  ::
    %+  expect-eq
    !>  6^0
    !>  (chacha:dire 12 0x3 0x4 5 (chacha:dire 12 0x3 0x4 5 6^0))
  ::
  ::  ietf
  ::
    =*  key  (rev 3 32 0x2be0.7734.9f80.bf45.faa1.f427.e81d.90db.dc90.a1d8.d421.2c1d.acf2.bd87.0000.bfdf)
    =*  nonce  (rev 3 12 0xddfa.6904.1ecc.3fee.b077.cf45)
    =/  i  (ietf:chacha:dire nonce)
    %+  expect-eq
    !>  5^(rev 3 5 0x41.5a3e.498d)
    !>  `[@ @ux]`(chacha:dire 20 key nonce.i counter.i 5^(rev 3 5 0x23.dbad.0780))
  ::
  ::  xchacha
  ::
    %+  expect-eq
    !>  [0x8665.eeb2.69b6.87c3.1ca1.1815.f4b8.436a.bd9d.20df.6c08.300e.5d1d.8d32.4c70.4011 0x0]
    !>  (xchacha:chacha:dire 0x0 0x0)
  ::
    =*  key  (rev 3 32 0x9d23.bd41.49cb.979c.cf3c.5c94.dd21.7e98.08cb.0e50.cd0f.6781.2235.eaaf.601d.6232)
    =*  nonce  (rev 3 24 0xc047.5482.66b7.c370.d335.66a2.425c.bf30.d82d.1eaf.5294.109e)
    =/  x  (xchacha:chacha:dire key nonce)
    %+  expect-eq
    !>  32^0x352d.69ec.6f39.cd82.470e.21df.54d0.0641.743f.d93a.d1b1.6756.8cde.9465.0909.12a2
    !>  (chacha:dire 20 key.x nonce.x 0 32^0)
  ==
--
