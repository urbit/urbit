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
  =^  mov  dire-zod
    %-  call:(dire-zod *@da *@uvJ bex-roof)
    [`duct`[/foo ~] ~ %soft `task:dire`[%make-peek ~nec /foo/bar]]
  ~&  mov
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
