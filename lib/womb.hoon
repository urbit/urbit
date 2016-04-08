::                                                      ::  ::
::::  /hoon/womb/lib                                    ::  ::
  ::                                                    ::  ::
/?    310                                               ::  version
/+    talk
::                                                      ::  ::
::::                                                    ::  ::
  ::                                                    ::  ::
|%
++  foil                                                ::  allocation map
  |*  mold                                              ::  entry mold
  $:  min/@ud                                           ::  minimum entry
      ctr/@ud                                           ::  next allocated
      und/(set @ud)                                     ::  free under counter
      ove/(set @ud)                                     ::  free over counter
      max/@ud                                           ::  maximum entry
      box/(map @ud +<)                                  ::  entries
  ==                                                    ::
--                                                      ::
|%                                                      ::
++  managed                                             ::  managed plot
  |*  mold                                              ::  
  %-  unit                                              ::  virgin
  %+  each  +<                                          ::  subdivided
  mail                                                  ::  delivered
::                                                      ::
++  planet                                              ::  subdivided planet
  (managed (lone (foil moon)))                          ::
::                                                      ::
++  star                                                ::  subdivided star
  (managed (pair (foil moon) (foil planet)))            ::
::                                                      ::
++  galaxy                                              ::  subdivided galaxy
  (managed (trel (foil moon) (foil planet) (foil star)))::
::                                                      ::
++  passcode  @pG                                       ::  64-bit passcode
++  mail  @ta                                           ::  email address
++  balance                                             ::  invitation balance
  $:  planets/@ud                                       ::  planet count
      stars/@ud                                         ::  star count
      owner/mail                                        ::  owner's email
      history/(list mail)                               ::  transfer history
  ==                                                    ::
++  client                                              ::  per email
  $:  sta/@ud                                           ::  unused star refs
      has/(set @p)                                      ::  planets owned
  ==                                                    ::
++  property                                            ::  subdivided plot
  $%  {$galaxy galaxy}                                  ::  galaxy
      {$star star}                                      ::  star
      {$planet planet}                                  ::  planet
  ==                                                    ::
++  invite                                              ::
  $:  who/mail                                          ::  who to send to
      pla/@ud                                           ::  planets to send
      sta/@ud                                           ::  stars to send
      wel/welcome                                       ::  welcome message
  ==                                                    ::
++  welcome                                             ::  welcome message
  $:  intro/tape                                        ::  in invite email
      hello/tape                                        ::  as talk message
  ==                                                    ::
++  reference                                           ::  affiliate credit
  (unit (each @p mail))                                 ::  ship or email
--                                                      ::
::                                                      ::  ::
::::                                                    ::  ::
  ::                                                    ::  ::
|%
++  womb-part  {$womb $0 womb-pith}                     ::  womb state
++  womb-pith                                           ::  womb content
  $:  boss/(unit @p)                                    ::  outside master
      bureau/(map passcode balance)                     ::  active invitations
      office/(map @p property)                          ::  properties managed
      hotel/(map mail client)                           ::  everyone we know
  ==                                                    ::
--                                                      ::
::                                                      ::  ::
::::                                                    ::  ::
  ::                                                    ::  ::
|%                                                      ::  arvo structures
++  card                                                ::
  $%  {$flog wire flog}                                 ::
  ==                                                    ::
++  move  (pair bone card)                              ::  user-level move
--
::                                                    ::  ::
::::                                                  ::  ::
  !:                                                  ::  ::
|=  {bowl womb-part}                                  ::  main womb work
|_  moz/(list move)
++  abet                                              ::  resolve
  [(flop moz) +>+>+<+]
::
++  emit  |=(card %_(+> moz [[ost +<] moz]))          ::  return card
++  emil                                              ::  return cards
  |=  (list card) 
  ^+  +>
  ?~(+< +> $(+< t.+<, +> (emit i.+<)))
::
++  peek
  |=  {ren/@tas tyl/path}
  ^-  (unit (unit (pair mark *)))
  ::
  ::  /shop/planets/@ud   (list @p)   up to 3 planets
  ::  /shop/stars/@ud     (list @p)   up to 3 stars
  ::  /shop/galaxies/@ud  (list @p)   up to 3 galaxies 
  ::  /stats                          general stats dump
  ::  /stats/@p                       what we know about @p
  ::  /invite/passcode                invitation status
  ::  
  ~
::
++  poke-invite                                       ::  create invitation
  |=  {ref/reference inv/invite}
  =<  abet
  ?>  |(=(our src) =([~ src] boss))                   ::  me or boss
  .
::
++  poke-obey                                         ::  set/reset boss
  |=  who/(unit @p)
  =<  abet
  ?>  =(our src)                                      ::  me only
  .
::
++  poke-rekey                                        ::  extend will
  |=  ~
  =<  abet
  ?>  |(=(our src) =([~ src] boss))                   ::  privileged
  .
::
++  poke-report                                       ::  report will
  |=  {her/@p wyl/will}                               ::
  =<  abet
  ?>  =(src src)                                      ::  self-authenticated
  .
::
++  poke-claim                                        ::  claim plot, send ticket
  |=  {aut/@uvH her/@p}                               ::
  =<  abet
  ?>  |=(src src)
  .
::
++  poke-
::
++  poke-release                                      ::  release to subdivide
  |=  {gal/@ud sta/@ud}                               ::
  =<  abet
  ?>  =(our src)                                      ::  privileged
  .
::
++  poke-reinvite                                     ::  split invitation
  |=  $:  aut/@uvH                                    ::  hash w/passcode
          inv/invite                                  ::  further invite
      ==
  ?>  =(src src)                                      ::
  =<  abet
  .
--
