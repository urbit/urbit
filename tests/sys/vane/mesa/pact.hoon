::  test %pact
::
/+  *test, v=test-mesa-gall
::
=+  [nec-life=1 bud-life=3]
=+  (nec-bud:v [nec-life bud-life] nec=0 bud=0)
::
=>  |%
    ++  dbug  `?`&
    ++  make-roof
      |=  [pax=path val=cage]
      ^-  roof
      |=  [lyc=gang pov=path vis=view bem=beam]
      ^-  (unit (unit cage))
      ?.  ?&(=(s.bem pax) |(=(vis %x) =(vis [%g %x]) =(vis %gx) =(vis %ax)))
        [~ ~]
      ``val
    ::
    --
|%
++  test-mesa-peek-no-proof
  ~?  >  dbug  'test-mesa-peek-no-proof'
  ::  tell ~nec to ask ~nec for a 1-fragment message
  ::
  =/  dat=@        'hi'
  =/  =space:ames  publ/bud-life
  =/  pat          /g/x/0/dap//hello/atom
  =/  bex-roof     (make-roof //hello/atom atom+!>(dat))
  =^  moves-1  ames.nec
    (ames-call:v ames.nec [~[/pact] [%keen ~ ~bud pat] bex-roof])
  =^  moves-2  ames.bud  (ames-reply:v ames.bud ~[/pact] moves-1 bex-roof) :: "ok, here is the 1st fragment"
  =^  moves-3  ames.nec  (ames-reply:v ames.nec ~[/pact] moves-2 bex-roof) :: "ok, here is the complete message"
  (ames-expect-msg:v moves-3 dat)
::
++  test-mesa-peek-inline-proof
  ~?  >  dbug  'test-mesa-peek-inline-proof'
  ::  3-fragment message; proof is sent standalone as first response
  ::
  =/  dat          (bex (bex 14))
  =/  =space:ames  publ/bud-life
  =/  bex-roof     (make-roof //some/data/atom atom+!>(dat))
  =/  pat          /g/x/0/dap//some/data/atom
  =^  moves-1  ames.nec
      (ames-call:v ames.nec [~[/pact] [%keen ~ ~bud pat] bex-roof])
  =^  moves-2  ames.bud  (ames-reply:v ames.bud ~[/pact] moves-1 bex-roof) :: "ok, here is the 1st fragment (auth)"
  =^  moves-3  ames.nec  (ames-reply:v ames.nec ~[/pact] moves-2 bex-roof) :: "ok, give me the 1st fragment (data)"
  =^  moves-4  ames.bud  (ames-reply:v ames.bud ~[/pact] moves-3 bex-roof) :: "ok, here is the 1st fragment (data)"
  =^  moves-5  ames.nec  (ames-reply:v ames.nec ~[/pact] moves-4 bex-roof) :: "ok, give me the 2rd fragment (data)"
  =^  moves-6  ames.bud  (ames-reply:v ames.bud ~[/pact] moves-5 bex-roof) :: "ok, here is the 2rd fragment (data)"
  =^  moves-7  ames.nec  (ames-reply:v ames.nec ~[/pact] moves-6 bex-roof) :: "ok, give me the 3rd fragment (data)"
  =^  moves-8  ames.bud  (ames-reply:v ames.bud ~[/pact] moves-7 bex-roof) :: "ok, here is the 3rd fragment (data)"
  =^  moves-9  ames.nec  (ames-reply:v ames.nec ~[/pact] moves-8 bex-roof) :: "ok, here is the complete message"
  (ames-expect-msg:v moves-9 dat)
::
++  test-mesa-peek-standalone-proof
  ~?  >  dbug  'test-mesa-peek-standalone-proof'
  ::  5-fragment message; proof is sent standalone as first response
  ::
  =/  dat          (bex (bex 15))
  =/  bex-roof     (make-roof //shrek/atom atom+!>(dat))
  =/  =space:ames  publ/bud-life
  =/  pat          /g/x/0/dap//shrek/atom
  =^  moves-1  ames.nec
      (ames-call:v ames.nec [~[/pact] [%keen ~ ~bud pat] bex-roof])
  =^  moves-2  ames.bud  (ames-reply:v ames.bud ~[/pact] moves-1 bex-roof) :: "ok, here is the proof"
  =^  moves-3  ames.nec  (ames-reply:v ames.nec ~[/pact] moves-2 bex-roof) :: "ok, give me the 1st fragment"
  =^  moves-4  ames.bud  (ames-reply:v ames.bud ~[/pact] moves-3 bex-roof) :: "ok, here is the 1st fragment"
  =^  moves-5  ames.nec  (ames-reply:v ames.nec ~[/pact] moves-4 bex-roof) :: "ok, give me the 2nd fragment"
  =^  moves-6  ames.bud  (ames-reply:v ames.bud ~[/pact] moves-5 bex-roof) :: "ok, here is the 2nd fragment"
  =^  moves-7  ames.nec  (ames-reply:v ames.nec ~[/pact] moves-6 bex-roof) :: "ok, give me the 3rd fragment"
  =^  moves-8  ames.bud  (ames-reply:v ames.bud ~[/pact] moves-7 bex-roof) :: "ok, here is the 3rd fragment"
  =^  moves-9  ames.nec  (ames-reply:v ames.nec ~[/pact] moves-8 bex-roof) :: "ok, give me the 4th fragment"
  =^  moves-10  ames.bud  (ames-reply:v ames.bud ~[/pact] moves-9 bex-roof) :: "ok, here is the 4th fragment"
  =^  moves-11  ames.nec  (ames-reply:v ames.nec ~[/pact] moves-10 bex-roof) :: "ok, give me the 5th fragment"
  =^  moves-12  ames.bud  (ames-reply:v ames.bud ~[/pact] moves-11 bex-roof) :: "ok, here is the 5th fragment"
  =^  moves-13  ames.nec  (ames-reply:v ames.nec ~[/pact] moves-12 bex-roof) :: "ok, here is the complete message"
  (ames-expect-msg:v moves-13 dat)
::
++  test-mesa-ns
  =/  bex-roof  (make-roof //some/data/atom atom+!>((bex (bex 14))))
  =/  res
    %-  scry:(ames.nec ~1111.1.10 `@`0xdead.beef bex-roof)
    =-  [~ / %x [[~nec %$ ud+1] -]]
    %+  weld
      /mess/0/pact/13/pure/init
    /publ/[(scot %ud nec-life)]/g/x/0/dap//some/data/atom
  ?~  res
    ~
  ?~  u.res
    ~
  =+  !<([=pact:pact:ames.nec (list (unit [l=@ux r=@ux]))] q.u.u.res)
  ~&  >  test-mesa-ns/-.pact
  ~
::
++  test-mesa-verify-msg-fail
  ~?  >  dbug  'test-mesa-verify-msg-fail'
  ::  4-fragment message
  ::
  =/  dat          (crip (reap 3.500 'A'))
  =/  =space:ames  publ/bud-life
  =/  bex-roof     (make-roof //some/data/atom atom+!>(dat))
  =/  pat          /g/x/0/dap//some/data/atom
  =^  moves-1  ames.nec
      (ames-call:v ames.nec [~[/pact] [%keen ~ ~bud pat] bex-roof])
  =^  moves-2  ames.bud  (ames-reply:v ames.bud ~[/pact] moves-1 bex-roof)  :: "ok, here is the proof"
  =^  moves-3  ames.nec  (ames-reply:v ames.nec ~[/pact] moves-2 bex-roof)  :: "ok, give me the 1st fragment"
  =^  moves-4  ames.bud  (ames-reply:v ames.bud ~[/pact] moves-3 bex-roof)  :: "ok, here is the 1st fragment"
  =^  moves-5  ames.nec  (ames-reply:v ames.nec ~[/pact] moves-4 bex-roof)  :: "ok, give me the 2nd fragment"
  =^  moves-6  ames.bud  (ames-reply:v ames.bud ~[/pact] moves-5 bex-roof)  :: "ok, here is the 2nd fragment"
  =^  moves-7  ames.nec  (ames-reply:v ames.nec ~[/pact] moves-6 bex-roof)  :: "ok, give me the 3rd fragment"
  =^  moves-8  ames.bud  (ames-reply:v ames.bud ~[/pact] moves-7 bex-roof)  :: "ok, here is the 3rd fragment"
  =^  moves-9  ames.nec  (ames-reply:v ames.nec ~[/pact] moves-8 bex-roof)  :: "ok, give me the 4th fragment"
  =^  moves-10  ames.bud  (ames-reply:v ames.bud ~[/pact] moves-9 bex-roof)  :: "ok, here is the 4th fragment"
  =^  moves-11  ames.nec  (ames-reply:v ames.nec ~[/pact] moves-10 bex-roof)  :: "ok, here is the complete message"
  (ames-expect-msg:v moves-11 dat)
::
--
