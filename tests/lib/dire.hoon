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
    =/  pat  /publ/0/g/x/0/dap//some/data/atom
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
    =/  pat  /publ/0/g/x/0/dap//shrek/atom
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
--
