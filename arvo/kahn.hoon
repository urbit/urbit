::    %kahn, social state
::
::::  /hoon/kahn
  ::
!?  164
::::
|=  pit=vase   
=>  =~
::  structures
|%
++  axle                                                ::  %kahn state
          $:  %0                                        ::
              big=(unit ship)                           ::  main ship, freeze
              all=(unit axel)                           ::  state once big
          ==                                            ::
++  axel                                                ::  all social state
          $:  rod=(list ship)                           ::  leadership upward
              myn=(map ship girl)                       ::  contacts
              cir=(map ,@tas clan)                      ::  contact groups
              nac=(map narc ship)                       ::  foreign to home
              wyl=will                                  ::  cryptowill
          ==                                            ::
++  axon                                                ::  all descendants
          $:  [%a p=hulk q=hulk r=hulk s=hulk t=hulk]   ::  zeppelin
              [%b p=hulk q=hulk r=hulk s=hulk]          ::  blimp
              [%c p=hulk q=hulk r=hulk]                 ::  balloon
              [%d p=hulk q=hulk]                        ::  drone
              [%e p=hulk]                               ::  bird
          ==                                            ::
++  clan                                                ::  ranked group
          $:  pec=rank                                  ::  rank conferred
              who=(set ship)                            ::  members
          ==                                            ::
++  gift                                                ::  out result <-$
          $:  [%step p=ship q=girl]                     ::  change contact
          ==                                            ::
++  girl                                                ::  daughter record
          $:  hop=pony                                  ::  status
              tip=rank                                  ::  rank
              fig=(set narc)                            ::  home to foreign
              gor=(set ,@tas)                           ::  in groups
              out=(unit ship)                           ::  exported to
              wyl=will                                  ::  cryptowill
          ==                                            ::
++  hulk  (map ship girl)                               ::  social state
++  kiss                                                ::  change
          $:  [%lead p=(list ship)]                     ::  set leadership
              [%tact p=ship q=girl]                     ::  set contact
              [%will p=will]                            ::  set will
          ==                                            ::
++  mojo  ?(%a %b %c %d %e)                             ::  ship rank
++  trigger 
          $:  (set ship)
              (set clan)
              (set rank)
              (set pony)
          ==
++  action
          $:  %warm -> %cold
              %cold -> %here
              %cold -> %fake
              %cold -> %free
              * -> %dead
              * -> %left
              *: tip
              *: add, subtract fig
              *: add, subtract gor
              adopt: external to free
          ==
++  narc  path                                          ::  contact path
++  pony                                                ::  contacts status
          $?  %cold                                     ::  virginal
              %dead                                     ::  inoperative
              %fake                                     ::  virtual
              %free                                     ::  exported
              %here                                     ::  hosted
              %left                                     ::  divorced
              %warm                                     ::  reserved
          ==                                            ::
++  rank                                                ::  privilege ring
          $?  %0                                        ::  enemy
              %1                                        ::  guest
              %2                                        ::  customer/vendor
              %3                                        ::  member/employee
              %4                                        ::  admin/officer
              %5                                        ::  self/god
          ==                                            ::
--                                                      ::
.  ==
=|  axle
=*  lex  -
|=  [now=@da eny=@ ski=sled]                            ::  activate
^?                                                      ::  opaque core
|%                                                      ::
++  call                                                ::  request
  |=  [hen=duct hic=(hypo (hobo kiss))]
  ^-  [p=(list move) q=_..^$]
  =>  .(q.hic ?.(?=(%soft -.q.hic) q.hic ((hard kiss) p.q.hic)))
  !!
::
++  doze
  |=  [now=@da hen=duct]
  ^-  (unit ,@da)
  ~
::
++  load                                                ::  highly forgiving
  |=  old=*
  =+  lox=((soft axle) old)
  ^+  ..^$
  ?~  lox
    ~&  %lunt-reset
    ..^$
  ..^$(+>- u.lox)
::
++  scry
  |=  [fur=(unit (set monk)) ren=@tas who=ship syd=desk lot=coin tyl=path]
  ^-  (unit (unit (pair mark ,*)))
  ~
::
++  stay                                                ::  save w/o cache
  `axle`+>-.$
::
++  take                                                ::  response
  |=  [tea=wire hen=duct hin=(hypo noun)]
  !!
--

