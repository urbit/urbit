::    %jael, secret 
::
::::  /hoon/jael
  ::
!?  164
::::
|=  pit=vase   
=>  =~
::  private structures                                  ::  ::
::::                                                    ::  ::
  ::                                                    ::  ::
|%                                                      ::
++  bitx  $~                                            ::  any crypto wallet
++  prof                                                ::  per service profile
  $:  oat/(map user (safe @))                           ::  auth tokens
      pax/(unit @t)                                     ::  actual password
  ==                                                    ::
++  safe  |*(gate {exp/@da nub/+<})                     ::  secret w/expiration
++  tlsy  $~                                            ::  HTTPS secrets
++  whom  @t                                            ::  foreign identity
++  wapi                                                ::  per api service
  $:  app/(map @tas (safe @))                           ::  per appname
      use/(map whom prof)                               ::  user secrets
  ==                                                    ::
++  land                                                ::  urbit secrets
  $:  lyf/@ud                                           ::  life number
      sym/(map @p (map @uvH (safe @uvI)))               ::  shared keys
      own/(map @ud (safe ring))                         ::  private key per life
  ==                                                    ::  
++  user  @ta                                           ::  user id per service
++  visa  (map ship will)                               ::  meta-will
--
::  system structures
|%
++  axle                                                ::  %jael state
  $:  $0                                                ::  %jael version
      pri/land                                          ::  local crypto state
      pub/(map @p will)                                 ::  will system
      api/(map @tas wapi)                               ::  web services
      tix/(map @pG @p)                                  ::  old style tickets
      tls/tlsy                                          ::  tls keys
      val/(map @tas bitx)                               ::  bitcoin etc
      sub/(map path (set duct))                         ::  subscribers
  ==                                                    ::
++  gift                                                ::  out result <-$
  $%  {$dead p/path}                                    ::  key expired
      {$save p/(each * *)}                              ::  backup
  ==                                                    ::
++  kiss                                                ::  in request ->$
  $%  {$kill p/path}                                    ::  cancel path
      {$know p/visa}                                    ::  learn will (new pki)
      {$knew p/ship q/will}                             ::  learn will (old pki)
      {$next p/ring}                                    ::  update private key
      {$tell p/path q/@da r/@}                          ::  save atomic secret
      {$tick p/@pG q/@p}                                ::  save old ticket
  ==
--
|%                                                      ::
++  call                                                ::  request
  |=  {hen/duct hic/(hypo (hobo kiss))}
  ^-  [p=(list move) q=_..^$]
  !!
::
++  doze
  |=  [now=@da hen=duct]
  ^-  (unit @da)
  ~
::
++  load                                                ::  highly forgiving
  |=  old=*
  =+  lox=((soft axle) old)
  ^+  ..^$
  ?~  lox
    ~&  %jael-reset
    ..^$
  ..^$(+>- u.lox)
::
++  scry
  |=  {fur/(unit (set monk)) ren/@tas who/ship syd/desk lot/coin tyl/path}
  ^-  (unit (unit (pair mark *)))
  ::  actually scry
  ~
::
++  stay                                                ::  save w/o cache
  `axle`+>-.$
::
++  take                                                ::  response
  |=  {tea/wire hen/duct hin/(hypo noun)}
  !!
--
