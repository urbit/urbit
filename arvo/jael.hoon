::    %jael, secret 
::
::::  /hoon/jael
  ::
!?  164
!:  ::
|=  pit/vase   
=>  =~
::  private structures                                  ::  ::
::::                                                    ::  ::
  ::                                                    ::  ::
|%
++  jael-state                                          ::  all crypto state
  $:  ver/$0                                            ::  %jael version 
      own/jael-private                                  ::  urbit private keys
      urb/jael-public                                   ::  urbit public state
      cap/jael-bearer                                   ::  urbit symmetric keys
      for/jael-embassy                                  ::  foreign secrets
      sec/jael-vault                                    ::  actual secret data
      hut/jael-service                                  ::  waiting ducts
  ==                                                    ::
++  jael-private  (map ship doom)                       ::  private keys
++  jael-public                                         ::  whole pki
  $:  pki/(map ship jael-urbit)                         ::  
      net/(map ship life)                               ::  reverse version
  ==                                                    ::
++  jael-bearer                                         ::  bearer codes
  $:  orp/(map ship hand)                               ::  reverse index
      por/(map hand fist)                               ::  forward index
      ::                                                ::  priority queue?
  ==                                                    ::
++  jael-role                                           ::  token 
  $%  %e
++  jael-public   (map ship gyft)                       ::  public keys
++  jael-embassy  (map term jael-partner)               ::  
++  jael-partner                                        ::  api apps
  $:  api/(map term hand)                               ::  apps by name
      tok/(map @t (map term hand))                      ::  shortlived tokens
  ==                                                    ::
++  jael-task                                           ::  secret operation
  $:  {$auth p/(unit @dr) q/ship r/@uvI}                ::  save capability
      {$link p/
      {$meet p/gree}                                    ::  adopt will
      {$nigh p/ship q/life}                             ::  track neighbor
      {$ring p/life q/ring}                             ::  save private key
      {$wait p/path}                                    ::  wait on desk/spur
      {$west p/sack q/path r/@ud s/*}                   ::  remote request
  ==                                                    ::
++  jael-secret                                         ::  secret by hash
  $:  key/code                                          ::  secret itself
      exp/(unit @da)                                    ::  expiration date
  ==                                                    ::
::                                                      ::
++  jael-service  (map path duct)                       ::
++  jael-vault                                          ::  secret store
  $:  saf/(map hand jael-secret)                        ::
  ==                                                    ::
--                                                      ::
.  ==                                                   ::
=|  lex/jael-state                                      ::  kernel state
|=  {now/@da eny/@ ski/sley}                            ::  current invocation
=<  |%                                                  ::  vane interface
    ++  call                                            ::  request
      |=  $:  hen/duct
              hic/(hypo (hobo kiss-jael))
          ==
      =>  .(q.hic ?.(?=($soft -.q.hic) q.hic ((hard kiss-jael) p.q.hic)))
      ^-  {p/(list move) q/_..^$}
      !!
    ::
    ++  doze                                            ::  sleep
      |=  {now/@da hen/duct}
      ^-  (unit @da)
      ~
    ::
    ++  load                                            ::  upgrade
      |=  old/jael-state
      ^+  ..^$
      ~&  %jael-reload
      ..^$(lex old)
    ::
    ++  scry
      |=  {fur/(unit (set monk)) ren/@tas who/ship syd/desk lot/coin tyl/path}
      ^-  (unit (unit cage))
      ?.  ?=($$ ren)  [~ ~]
      !!
    ::
    ++  stay  lex
    ++  take                                            ::  accept response
      |=  {tea/wire hen/duct hin/(hypo sign-arvo)}
      ^-  {p/(list move) q/_..^$}
      [~ ..^$]
    --
|%
++  foo  %bar
--
