::                                                      ::  ::
::::  /hoon#oct1/lib                                    ::::::  dependencies  
  ::                                                    ::  ::
/?    310                                               ::  arvo version
/-    oct1                                              ::  structures
=>    ^oct1                                             ::  namespace
::                                                      ::  ::
::::                                                    ::::::  semantics
  !:                                                    ::  ::
|%                                                      ::
++  icon   |=(? ?:(+< 'X' 'O'))                         ::  display at
++  bo                                                  ::  per board
  |_  bud/board                                         ::
  ++  bit  |=(@ =(1 (cut 0 [+< 1] bud)))                ::  moved at address
  ++  get  |=(point (bit (off +<)))                     ::  get point
  ++  off  |=(point (add x (mul 3 y)))                  ::  bitfield address
  ++  set  |=(point (con bud (bex (off +<))))           ::  set point
  ++  win  %-  lien  :_  |=(a/@ =(a (dis a bud)))       ::  test for win
           (rip 4 0wl04h0.4A0Aw.4A00s.0e070)            ::  with bitmasks
  --                                                    ::
++  go                                                  ::  play from
  |_  game:oct1                                              ::
  ++  at  |_  point                                     ::  per point
          ++  g  +>+<                                   ::  game
          ++  k  !|(x o)                                ::  legal move
          ++  m  ?.(k [| g] [& g:t:?:(who y p)])        ::  move
          ++  o  (~(get bo boo) +<)                     ::  old at o
          ++  p  .(boo (~(set bo boo) +<))              ::  play at o
          ++  t  .(who !who)                            ::  take turn
          ++  v  ?:(x (icon &) ?:(o (icon |) '.'))      ::  view
          ++  x  (~(get bo box) +<)                     ::  old at x
          ++  y  .(box (~(set bo box) +<))              ::  play at x
          --                                            ::
  ++  new  +<(boo 0, box 0, who &)                      ::  reset game
  ++  res  ?.  |(~(win bo box) ~(win bo boo))           ::  possible result
           ?:  =(511 (con boo box))                     ::
           `"tie :-("  ~  `"{<~[(icon who)]>} wins"     ::
  ++  row  |=  y/@  :-  (add y '1')  %-  zing           ::  print row
           (turn (gulf 0 3) |=(@ ~[' ' ~(v at y +<)]))  ::
  ++  tab  ~["+ 1 2 3" (row 0) (row 1) (row 2)]         ::  print table
  ++  voy   ": {<~[(icon who)]>} (row#col): "           ::  print prompt
  --
--
