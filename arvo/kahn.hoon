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
              all=(map ship axel)                       ::  state by owner
          ==                                            ::
++  axel                                                ::  all social state
          $:  rod=(list ship)                           ::  ancestry upward
              myn=(map ship girl)                       ::  daughters
          ==
++  axon                                                ::  all descendants
          $:  [%a p=hulk q=hulk r=hulk]                 ::  carrier
              [%b p=hulk q=hulk]                        ::  cruiser
              [%c p=hulk]                               ::  destroyer
              [%d p=hulk]                               ::  yacht
              [%e p=ship]                               ::  submarine
          ==                                            ::
++  clan                                                ::  ranked group
          $:  pec=rank                                  ::  membership bar 
              who=(set ship)                            ::  members
          ==                                            ::
++  gift                                                ::  out result <-$
          $:  [%notice p=ship q=action]
          ==
++  girl                                                ::  daughter record
          $:  hop=pony                                  ::  status
              tip=rank                                  ::  rank
              fig=(set narc)                            ::  other identities
              gor=(set ,@tas)                           ::  memberships
              out=(unit ship)                           ::  stepmother
              res=(unit ,@da)                           ::  reserved until
          ==                                            ::
++  hulk                                                ::  social state
          $:  rod=(list ship)                           ::  ancestry upward
              myn=(map ship girl)                       ::  daughter status
              cir=(map ,@tas clan)                      ::  daughter groups
          ==                                            ::
++  kiss                                                ::  change
          $:  [%reserve p=@ud q=@ud]                    ::  reserve class/num
              [%renew ~]                                ::  self-renew
              [%modify p=ship q=action]                 ::  
              [%await p=(unit trigger)]                 ::  subscribe actions
          ==
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
++  pony                                                ::  daughter status
          $?  %cold                                     ::  virginal
              %dead                                     ::  inoperative
              %fake                                     ::  virtual
              %free                                     ::  exported
              %here                                     ::  hosted
              %left                                     ::  divorced
              %warm                                     ::  reserved
          ==                                            ::
++  rank                                                ::  privilege
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

