::                                                      ::  ::
::::  /hoon#oct1/sur                                    ::::::  dependencies
  ::                                                    ::  ::
/?    310                                               ::  arvo version
::                                                      ::  ::  
::::                                                    ::::::  semantics
  ::                                                    ::  ::
|%                                                      ::
++  board   @                                           ::  one-player bitfield
++  point   {x/@ y/@}                                   ::  coordinate
++  game                                                ::  game state
            $:  who/?                                   ::  whose turn
                box/board                               ::  X board
                boo/board                               ::  O board
            ==                                          ::
--
