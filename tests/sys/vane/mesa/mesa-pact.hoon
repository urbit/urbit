::  test %pact
::
/+  *test, v=test-mesa-gall
::
=+  [nec-life=2 bud-life=3]
=+  (nec-bud:v [nec-life bud-life] nec=0 bud=0)
::
=>  |%
    ++  dbug  `?`&
    ++  make-roof
      |=  [pax=path val=cage]
      ^-  roof
      |=  [lyc=gang pov=path vis=view bem=beam]
      ^-  (unit (unit cage))
      ?.  ?&(=(s.bem pax) |(=(vis %x) =(vis [%g %x])))  [~ ~]
      ``val
    ::
    --
|%
++  test-dire-peek-no-proof
  ~?  >  dbug  'test-dire-peek-no-proof'
  ::  tell ~nec to ask ~nec for a 1-fragment message
  ::
  =/  dat=@        'hi'
  =/  =space:mesa  publ/bud-life
  =/  bex-roof     (make-roof //hello/atom atom+!>(dat))
  =/  pat          /g/x/0/dap//hello/atom
  =^  moves-1  mesa.nec
    (mesa-call:v mesa.nec [~[/pact] [%make-peek space ~bud pat] bex-roof])
  =^  moves-2  mesa.bud  (mesa-reply:v mesa.bud ~[/pact] moves-1 bex-roof) :: "ok, here is the 1st fragment"
  =^  moves-3  mesa.nec  (mesa-reply:v mesa.nec ~[/pact] moves-2 bex-roof) :: "ok, here is the complete message"
  (mesa-expect-msg:v moves-3 dat)
::
++  test-dire-peek-inline-proof
  ~?  >  dbug  'test-dire-peek-no-proof'
  ::  3-fragment message; proof is inlined into first response
  ::
  =/  dat          (bex (bex 14))
  =/  =space:mesa  publ/bud-life
  =/  bex-roof     (make-roof //some/data/atom atom+!>(dat))
  =/  pat          /g/x/0/dap//some/data/atom
  =^  moves-1  mesa.nec
      (mesa-call:v mesa.nec [~[/pact] [%make-peek space ~bud pat] bex-roof])
  =^  moves-2  mesa.bud  (mesa-reply:v mesa.bud ~[/pact] moves-1 bex-roof) :: "ok, here is the 1st fragment"
  =^  moves-3  mesa.nec  (mesa-reply:v mesa.nec ~[/pact] moves-2 bex-roof) :: "ok, give me the 2nd fragment"
  =^  moves-4  mesa.bud  (mesa-reply:v mesa.bud ~[/pact] moves-3 bex-roof) :: "ok, here is the 2nd fragment"
  =^  moves-5  mesa.nec  (mesa-reply:v mesa.nec ~[/pact] moves-4 bex-roof) :: "ok, give me the 3rd fragment"
  =^  moves-6  mesa.bud  (mesa-reply:v mesa.bud ~[/pact] moves-5 bex-roof) :: "ok, here is the 3rd fragment"
  =^  moves-7  mesa.nec  (mesa-reply:v mesa.nec ~[/pact] moves-6 bex-roof) :: "ok, here is the complete message"
  (mesa-expect-msg:v moves-7 dat)
::
++  test-dire-peek-standalone-proof
  ~?  >  dbug  'test-dire-peek-standalone-proof'
  ::  5-fragment message; proof is sent standalone as first response
  ::
  =/  dat          (bex (bex 15))
  =/  bex-roof     (make-roof //shrek/atom atom+!>(dat))
  =/  =space:mesa  publ/bud-life
  =/  pat          /g/x/0/dap//shrek/atom
  =^  moves-1  mesa.nec
      (mesa-call:v mesa.nec [~[/pact] [%make-peek space ~bud pat] bex-roof])
  =^  moves-2  mesa.bud  (mesa-reply:v mesa.bud ~[/pact] moves-1 bex-roof) :: "ok, here is the proof"
  =^  moves-3  mesa.nec  (mesa-reply:v mesa.nec ~[/pact] moves-2 bex-roof) :: "ok, give me the 1st fragment"
  =^  moves-4  mesa.bud  (mesa-reply:v mesa.bud ~[/pact] moves-3 bex-roof) :: "ok, here is the 1st fragment"
  =^  moves-5  mesa.nec  (mesa-reply:v mesa.nec ~[/pact] moves-4 bex-roof) :: "ok, give me the 2nd fragment"
  =^  moves-6  mesa.bud  (mesa-reply:v mesa.bud ~[/pact] moves-5 bex-roof) :: "ok, here is the 2nd fragment"
  =^  moves-7  mesa.nec  (mesa-reply:v mesa.nec ~[/pact] moves-6 bex-roof) :: "ok, give me the 3rd fragment"
  =^  moves-8  mesa.bud  (mesa-reply:v mesa.bud ~[/pact] moves-7 bex-roof) :: "ok, here is the 3rd fragment"
  =^  moves-9  mesa.nec  (mesa-reply:v mesa.nec ~[/pact] moves-8 bex-roof) :: "ok, give me the 4th fragment"
  =^  moves-a  mesa.bud  (mesa-reply:v mesa.bud ~[/pact] moves-9 bex-roof) :: "ok, here is the 4th fragment"
  =^  moves-b  mesa.nec  (mesa-reply:v mesa.nec ~[/pact] moves-a bex-roof) :: "ok, give me the 5th fragment"
  =^  moves-c  mesa.bud  (mesa-reply:v mesa.bud ~[/pact] moves-b bex-roof) :: "ok, here is the 5th fragment"
  =^  moves-d  mesa.nec  (mesa-reply:v mesa.nec ~[/pact] moves-c bex-roof) :: "ok, here is the complete message"
  (mesa-expect-msg:v moves-d dat)

++  test-dire-ns
  =/  bex-roof  (make-roof //some/data/atom atom+!>((bex (bex 14))))
  =/  res
    %-  scry:(mesa.nec ~1111.1.10 `@`0xdead.beef bex-roof)
    =-  [~ / %x [[~nec %$ ud+1] -]]
    %+  weld
      /mess/0/pact/13/pure/init
    /publ/[(scot %ud nec-life)]/g/x/0/dap//some/data/atom
  ?~  res
    ~&  %sig  ~
  ?~  u.res
    ~&  %inner-sig  ~
  ~&  >  test-dire-ns/;;([@tas pact:pact:mesa.nec] [p q.q]:u.u.res)
  ~
--
