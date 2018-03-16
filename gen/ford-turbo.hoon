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
  %-  expect-eq  !>
  :-  [duct=~ %give %made now %complete %result %$ %noun !>(**)]~
  =-  -.-
  %-  call:(ford-turbo)
  :*  duct=~
      type=~
      %make
      our
      plan=[%$ %noun !>(**)]
      date=`now
  ==
--
