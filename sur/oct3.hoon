::                                                      ::  ::
::::  /hoon#oct3/sur                                    ::::::  dependencies
  ::                                                    ::  ::
/?    310                                               ::  arvo version
::                                                      ::  ::
::::                                                    ::::::  semantics
  ::                                                    ::  ::
|%                                                      ::
++  board   @                                           ::  one-player bitfield
++  point   {x+@ y+@}                                   ::  coordinate
++  stage   (pair (unit ship) (unit ship))              ::  players
++  play    (each game tape)                            ::  update
++  game                                                ::  game state
            $:  who+?                                   ::  whose turn
                sag+stage                               ::  who's playing
                aud+(map ship @ud)                      ::  who's watching
                box+board                               ::  X board
                boo+board                               ::  O board
            ==                                          ::
--
