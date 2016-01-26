::    %musk, realm management
::
::::  /hoon+musk
  ::
!?  164
::::
|=  pit=vase   
=>  =~
::  structures
|%
++  axle                                                ::  %musk state
          $:  %0                                        ::
              all=(map ship axil)                       ::  state by owner
          ==                                            ::
++  axil                                                ::
          $:  kid=(map ship girl)                       ::  daughters
              deq=(map narc ship)                       ::  reverse address
              siq=(map ship (list clan))                ::  ship to clans
              kes=(map clan (list ship))                ::  clan to ships
          ==                                            ::
++  clan  ,@tas                                         ::  group identity
++  narc                                                ::  contact address
          $:  [%$ p=ship]                               ::  urbit
              [%m p=@t q=@t]                            ::  email p@q
              [%f p=@t]                                 ::  facebook
              [%g p=@t]                                 ::  google
              [%p p=@t]                                 ::  phone message
              [%t p=@t]                                 ::  twitter
          ==                                            ::
++  pony                                                ::  daughter status
          $%  [%cold ~]                                 ::  virginal
              [%dead ~]                                 ::  written off
              [%fake ~]                                 ::  virtual
              [%free ~]                                 ::  downloaded
              [%here ~]                                 ::  hosted
              [%left p=(unit ship)]                     ::  run away to
          ==                                            ::
++  rank                                                ::  relative privilege
          $?  %0                                        ::  enemy
              %1                                        ::  neighbor
              %2                                        ::  guest+customer
              %3                                        ::  friend+employee
              %4                                        ::  officer+family
              %5                                        ::  self+admin
          ==                                            ::
++  girl                                                ::
          $:  hop=pony                                  ::  status
              tag=(unit ,@tas)                          ::  petname
              tip=rank                                  ::  rank
              fig=(set narc)                            ::  identities
              loc=(unit ,[p=@da q=@ud r=clip])          ::  last position
              sym=(set ,[p=@ q=@uvH])                   ::  symmetric keys?
              wyl=will                                  ::  crypto will
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
++  stay                                                ::  save w+o cache
  `axle`+>-.$
::
++  take                                                ::  response
  |=  [tea=wire hen=duct hin=(hypo noun)]
  !!
--

