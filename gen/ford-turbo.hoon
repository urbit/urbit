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
  test-literal
  test-autocons-same
  test-autocons-different
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
--
