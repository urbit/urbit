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
    --
::
|%
++  test-dire-peek
  =^  mov1  dire-zod
    %-  call:(dire-zod *@da *@uvJ bex-roof)
    =/  pat  /publ/0/g/x/0/dap//some/data/atom
    [`duct`[/foo ~] ~ %soft `task:dire`[%make-peek ~nec /foo/bar]]
  :: =^  mov2  dire-nec
  ::   %-  call:(dire-nec *@da *@uvJ bex-roof)
  ::   [`duct`[/foo ~] ~ %soft `task:dire`[%hear *lane:pact:dire PACKET_FROM_MOV1]]
  ~&  mov1
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
