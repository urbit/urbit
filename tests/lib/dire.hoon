/+  *test, dire
=/  dire-zod  (dire ~zod)
=/  dire-nec  (dire ~nec)
=>  |%
    ++  dbug  `?`|
    ++  bex-roof
      ^-  roof
      |=  [lyc=gang pov=path vis=view bem=beam]
      ^-  (unit (unit cage))
      ?>  =(s.bem //some/data/atom)
      ?+  vis  ~
        [%g %x]  ``atom+!>((bex (bex 14)))
      ==
    ++  reply
      |=  [d=_dire-zod pac=(list move:dire)]
      ^-  [(list move:dire) _dire-zod]
      ?>  ?=([[* [%give [%send *]]] ~] pac)
      %-  call:(d *@da *@uvJ bex-roof)
      [`duct`[/foo ~] ~ %soft `task:dire`[%hear *lane:pact:dire q.gift.q.i.pac]]
    --
::
|%
++  test-dire-peek
  ::  tell ~zod to ask ~nec for some data
  ::
  =^  mov1  dire-zod
    %-  call:(dire-zod *@da *@uvJ bex-roof)
    =/  pat  /publ/0/g/x/0/dap//some/data/atom
    [`duct`[/foo ~] ~ %soft `task:dire`[%make-peek ~nec pat]]
  =^  mov2  dire-nec  (reply dire-nec mov1) :: "ok, here is the 1st fragment"
  =^  mov3  dire-zod  (reply dire-zod mov2) :: "ok, give me the 2nd fragment"
  =^  mov4  dire-nec  (reply dire-nec mov3) :: "ok, here is the 2nd fragment"
  =^  mov5  dire-zod  (reply dire-zod mov4) :: "ok, give me the 3rd fragment"
  =^  mov6  dire-nec  (reply dire-nec mov5) :: "ok, here is the 3rd fragment"
  =^  mov7  dire-zod  (reply dire-zod mov6) :: "ok, here is the complete message"
  ~&  mov7+mov7
  ~
++  test-dire-ns
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
