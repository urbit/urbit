/+  ford-turbo, tester
::
:-  %say
|=  [[now=@da eny=@ =beak] ~ ~]
:-  %noun
=+  our=p.beak
=+  tester:tester
|^  
^-  wall
;:  weld
  test-compiles
  test-call
==
++  test-compiles
  ~&  %test-compiles
  %-  expect-eq  !>
  [ford-turbo ford-turbo]
::
++  test-call
  ~&  %test-call
  =/  ford  (ford-turbo now=~1234.5.6 eny=0xdead.beef scry=*sley)
  =^  moves  ford
    %-  call:ford
    :*  duct=~
        type=~
        %make
        ~nul
        plan=[%$ %noun !>(**)]
        date=`~1234.5.6
    ==
  ::  %+  welp
  ::    %-  expect-eq  !>
  ::    :-  moves
  ::    [duct=~ %give %made now %complete %result %$ %noun !>(**)]~
  ::
  %-  expect-eq  !>
  :-  state-by-ship.+>+<.ford
  (my [~nul *ford-state:ford-turbo]~)
--
