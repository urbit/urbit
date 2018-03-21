/+  ford-turbo, tester
::
:-  %say
|=  [[now=@da eny=@ =beak] ~ ~]
:-  %noun
=+  our=p.beak
=+  tester:tester
=/  ford  (ford-turbo now=~1234.5.6 eny=0xdead.beef scry=*sley)
|^  
^-  wall
;:  weld
  test-compiles
  test-unify-jugs
  test-dependency-wire-encoding
  test-literal
  test-autocons-same
  test-autocons-different
  test-scry-clay-succeed
  test-scry-clay-fail
  test-scry-clay-block
==
++  test-compiles
  ~&  %test-compiles
  %-  expect-eq  !>
  [ford-turbo ford-turbo]
::
++  test-unify-jugs
  ~&  %test-unify-jugs
  %-  expect-eq  !>
  :-  %+  unify-jugs:ford
        `(jug @tas @ud)`(my ~[[%a (sy 1 2 ~)] [%b (sy 3 4 ~)]])
      `(jug @tas @ud)`(my ~[[%b (sy 5 6 ~)] [%c (sy 7 8 ~)]])
  ::
  `(jug @tas @ud)`(my ~[[%a (sy 1 2 ~)] [%b (sy 3 4 5 6 ~)] [%c (sy 7 8 ~)]])
::
++  test-dependency-wire-encoding
  ~&  %test-dependency-wire-encoding
  ;:  welp
    %-  expect-eq  !>
    :-  `path`(to-wire:ford [%clay-live care=%x bel=[[~nul %desk] /foo/bar]])
    /c/x/.y/~nul/desk/0/bar/foo
  ::
    %-  expect-eq  !>
    :-  `dependency:ford`[%clay-live care=%x bel=[[~nul %desk] /foo/bar]]
    (from-wire:ford /c/x/.y/~nul/desk/0/bar/foo)
  ::
    %-  expect-eq  !>
    :-  ^-  path
      (to-wire:ford [%clay-once care=%x beam=[[~nul %desk %ud 42] /foo/bar]])
    /c/x/.n/~nul/desk/42/bar/foo
  ::
    %-  expect-eq  !>
    :-  ^-  dependency:ford
      [%clay-once care=%x beam=[[~nul %desk %ud 42] /foo/bar]]
    (from-wire:ford /c/x/.n/~nul/desk/42/bar/foo)
  ::
    %-  expect-eq  !>
    :-  ^-  path
      (to-wire:ford [%gall-live care=%x bel=[[~nul %desk] /foo/bar]])
    /g/x/.y/~nul/desk/0/bar/foo
  ::
    %-  expect-eq  !>
    :-  ^-  dependency:ford
      [%gall-live care=%x bel=[[~nul %desk] /foo/bar]]
    (from-wire:ford /g/x/.y/~nul/desk/0/bar/foo)
  ::
    %-  expect-eq  !>
    :-  ^-  path
      %-  to-wire:ford
      [%gall-once care=%x beam=[[~nul %desk %da ~1234.5.6] /foo/bar]]
    /g/x/.n/~nul/desk/~1234.5.6/bar/foo
  ::
    %-  expect-eq  !>
    :-  ^-  dependency:ford
      [%gall-once care=%x beam=[[~nul %desk %da ~1234.5.6] /foo/bar]]
    (from-wire:ford /g/x/.n/~nul/desk/~1234.5.6/bar/foo)
  ::
  ==
::
++  test-literal
  ~&  %test-literal
  =^  moves  ford
    %-  call:ford
    :*  duct=~
        type=~
        %make
        ~nul
        plan=[%$ %noun !>(**)]
        date=`~1234.5.6
    ==
  %+  welp
    %-  expect-eq  !>
    :-  moves
    [duct=~ %give %made ~1234.5.6 %complete %result %$ %noun !>(**)]~
  ::
  %-  expect-eq  !>
  :-  state-by-ship.+>+<.ford
  (my [~nul *ford-state:ford-turbo]~)
::
++  test-autocons-same
  ~&  %test-autocons-same
  =^  moves  ford
    %-  call:ford
    :*  duct=~
        type=~
        %make
        ~nul
        plan=[[%$ %noun !>(**)] [%$ %noun !>(**)]]
        date=`~1234.5.6
    ==
  %+  welp
    %-  expect-eq  !>
    :-  moves
    :~  :*  duct=~  %give  %made  ~1234.5.6  %complete
            %result
            [%result %$ %noun !>(**)]
            [%result %$ %noun !>(**)]
    ==  ==
  ::
  %-  expect-eq  !>
  :-  state-by-ship.+>+<.ford
  (my [~nul *ford-state:ford-turbo]~)
::
++  test-autocons-different
  ~&  %test-autocons-different
  =^  moves  ford
    %-  call:ford
    :*  duct=~
        type=~
        %make
        ~nul
        plan=[[%$ %noun !>(42)] [%$ %noun !>(43)]]
        date=`~1234.5.6
    ==
  %+  welp
    %-  expect-eq  !>
    :-  moves
    :~  :*  duct=~  %give  %made  ~1234.5.6  %complete
            %result
            [%result %$ %noun !>(42)]
            [%result %$ %noun !>(43)]
    ==  ==
  ::
  %-  expect-eq  !>
  :-  state-by-ship.+>+<.ford
  (my [~nul *ford-state:ford-turbo]~)
::
++  test-scry-clay-succeed
  ~&  %test-scry-clay-succeed
  =/  scry
    |=  [* (unit (set monk)) =term =beam]
    ^-  (unit (unit cage))
    ::
    ?>  =(term %cx)
    ?>  =(beam [[~nul %desk %da ~1234.5.6] /foo/bar])
    ::
    [~ ~ %noun !>(42)]
  ::
  =.  ford  (ford-turbo now=~1234.5.6 eny=0xdead.beef scry=scry)
  =^  moves  ford
    %-  call:ford
    :*  duct=~
        type=~
        %make
        ~nul
        plan=[%scry %clay-once ren=%x bem=[[~nul %desk %da ~1234.5.6] /foo/bar]]
        date=`~1234.5.6
    ==
  %+  welp
    %-  expect-eq  !>
    :-  moves
    :~  :*  duct=~  %give  %made  ~1234.5.6  %complete  %result
            [%scry %noun !>(42)]
    ==  ==
  ::
  %-  expect-eq  !>
  :-  state-by-ship.+>+<.ford
  (my [~nul *ford-state:ford-turbo]~)
::
++  test-scry-clay-fail
  ~&  %test-scry-clay-fail
  =/  scry
    |=  [* (unit (set monk)) =term =beam]
    ^-  (unit (unit cage))
    ::
    ?>  =(term %cx)
    ?>  =(beam [[~nul %desk %da ~1234.5.6] /bar/foo])
    ::
    [~ ~]
  ::
  =.  ford  (ford-turbo now=~1234.5.6 eny=0xdead.beef scry=scry)
  =^  moves  ford
    %-  call:ford
    :*  duct=~
        type=~
        %make
        ~nul
        plan=[%scry %clay-once ren=%x bem=[[~nul %desk %da ~1234.5.6] /bar/foo]]
        date=`~1234.5.6
    ==
  %+  welp
    %-  expect-eq  !>
    :-  moves
    :~  :*  duct=~  %give  %made  ~1234.5.6  %complete  %error
        :~  leaf+"clay-live scry failed for"
            leaf+"%cx /~nul/desk/~1234.5.6/foo/bar"
    ==  ==  ==
  ::
  %-  expect-eq  !>
  :-  state-by-ship.+>+<.ford
  (my [~nul *ford-state:ford-turbo]~)
::
++  test-scry-clay-block
  ~&  %test-scry-clay-block
  =/  scry-block
    |=  [* (unit (set monk)) =term =beam]
    ^-  (unit (unit cage))
    ::
    ?>  =(term %cx)
    ?>  =(beam [[~nul %desk %da ~1234.5.6] /bar/foo])
    ::
    ~
  ::
  =/  scry-succeed
    |=  [* (unit (set monk)) =term =beam]
    ^-  (unit (unit cage))
    ::
    ~|  term+term
    ?>  =(term %cx)
    ~|  beam+beam
    ?>  =(beam [[~nul %desk %da ~1234.5.6] /bar/foo])
    ::
    [~ ~ %noun !>(42)]
  ::
  =.  ford  (ford-turbo now=~1234.5.6 eny=0xdead.beef scry=scry-block)
  =^  moves  ford
    %-  call:ford
    :*  duct=~
        type=~
        %make
        ~nul
        plan=[%scry %clay-once ren=%x bem=[[~nul %desk %da ~1234.5.6] /bar/foo]]
        date=`~1234.5.6
    ==
  %+  welp
    %-  expect-eq  !>
    :-  moves
    :~  :*  duct=~  %pass  wire=/c/x/.n/~nul/desk/~1234.5.6/foo/bar
            %c  %warp  [~nul ~nul]  %desk
            ~  %sing  %x  [%da ~1234.5.6]  /bar/foo
    ==  ==
  ::
  =.  ford  (ford now=~1234.5.7 eny=0xbeef.dead scry=scry-succeed)
  ::
  =^  moves2  ford
    %-  take:ford
    :*  wire=/~nul/c/x/.n/~nul/desk/~1234.5.6/foo/bar  duct=~
        ^=  wrapped-sign  ^-  (hypo sign:ford)  :-  *type  ::  ^-  sign:ford
        [%c %writ ~ [%x [%da ~1234.5.6] %desk] /bar/foo %noun !>(42)]
    ==
  ::
  %+  welp
    %-  expect-eq  !>
    :-  moves2
    :~  :*  duct=~  %give  %made  ~1234.5.6  %complete  %result
            [%scry %noun !>(42)]
    ==  ==
  ::
  %-  expect-eq  !>
  :-  state-by-ship.+>+<.ford
  (my [~nul *ford-state:ford-turbo]~)
--
