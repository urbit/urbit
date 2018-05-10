/+  ford-turbo
::
::  testing application for ford-turbo
::
::    To test our integration with clay, we have a minimal app which translates
::    calls from vane move form to gall moves. This proxies filesystem calls
::    back and forth.
::
=,  clay
::
=/  test-pit=vase  !>(.)
=/  ford-gate  (ford-turbo test-pit)
::
|%
++  move  (pair bone card)
++  card
  $%  [%warp wire sock riff]
  ==
--
::
|_  {bol/bowl:gall turbo/_(ford-gate)}
::  +prep: clear the state on each reload
::
++  prep  _`.
::  +poke-noun: invoke with `:ford-turbo &atom 0`
::
++  poke-noun
  |=  a/*
  ^-  [(list move) _+>.$]
  ::
  =.  turbo  (turbo now.bol eny.bol our-scry)
  ::
  =^  vane-moves  turbo
    %-  call:turbo
    :*  duct=~[/ford-test]  type=~  %make  our.bol
        [%scry %c %x rail=[[our.bol %home] /hoon/code/gen]]
    ==
  ::
  (convert-moves vane-moves)
::  clay response to a %multi
::
++  wris
  |=  {way/wire p/[%da @da] q/(set (pair care path))}
  ^-  [(list move) _+>.$]
  ~&  [%wris way p q]
  ::
  =.  turbo  (turbo now.bol eny.bol our-scry)
  ::
  =^  vane-moves  turbo
    %-  take:turbo
    :*  wire=way  duct=~  *type  [%c %wris p q]
    ==
  (convert-moves vane-moves)
::  clay response to a %sing
::
++  writ
  |=  {way/wire rot/riot}
  ^-  [(list move) _+>.$]
  ~&  [%writ way rot]
  ::
  =.  turbo  (turbo now.bol eny.bol our-scry)
  ::
  =^  vane-moves  turbo
    %-  take:turbo
    :*  wire=way  duct=~  *type  [%c %writ rot]
    ==
  (convert-moves vane-moves)
::  +convert-moves: converts vane moves to gall moves
::
::    The moves that come out of a raw call to ford-turbo are set up for
::    arvo. Change them so they're in gall format.
::
++  convert-moves
  |=  vane-moves=(list move:ford-gate)
  ^-  [(list move) _+>.$]
  ::
  =/  gall-moves=(list move)
    %+  murn  vane-moves
    |=  [=duct card=(wind note:ford-gate gift:able:ford-api:ford-gate)]
    ^-  (unit move)
    ::
    ?+    -.card  !!
        %pass
      =*  wire  p.card
      ?+  -.q.card  !!
        %c  `[ost.bol %warp wire sock.q.card riff.q.card]
      ==
    ::
        %give
      ::  print out the result, but don't do anything else.
      ~&  [%give card]
      ~
    ==
  ::
  ~&  [%gall-moves gall-moves]
  ::
  [gall-moves +>.$]
::  +our-scry: scry function for ford to use.
::
::  OK, so maybe we can't just scry here. When we hit .^, we're telling what's
::  interpreting us to block if we can't answer synchronously. So the real deal
::  is to always block, so ford will emit moves asking for everything asynchronously.
++  our-scry
  |=  [one=* two=(unit (set monk)) =term =beam]
  ^-  (unit (unit cage))
  ::
  ~&  [%scrying-for term beam]
  ~
--
