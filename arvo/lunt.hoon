::    %lunt, fleet job control
::
::::  /hoon/lunt/arvo
  ::
!?  164
::::
|=  pit=vase   
=>  =~
::  structures
|%
++  axle                                                ::  %lunt state
          $:  %0                                        ::
              all=(map ship axil)                       ::  state by owner
          ==                                            ::
++  born  ,[p=brat q=(unit ship)]                       ::  task identity
++  brat  ,@ud                                          ::  task number
++  bulb                                                ::
          $:  p=@p                                      ::  ship identity
              q=home                                    ::  server data
          ==                                            ::
++  home                                                ::  storage access
          $:  pad=@uvH                                  ::  passcode
              huc=husk                                  ::  log server
              sog=hulk                                  ::  storage server
          ==                                            ::
++  hulk                                                ::  checkpoint service
          $%  [%astr p=@ud q=@ud]                       ::  S3
          ==                                            ::
++  husk                                                ::  log server
          $:  pro=@tas                                  ::  protocol
              cap=@uvH                                  ::  access code
              srv=(list (pair ,@ud clip))               ::  server cluster
          ==                                            ::
++  gift                                                ::  result
          $:  [%die p=brat]                             ::  kill
              [%int p=brat]                             ::  interrupt
              [%run p=brat q=@p r=home]                 ::  load 
              [%say p=brat q=(list ovum)]               ::  send events
              [%new p=brat q=@p r=home s=(list ovum)]   ::  create 
          ==                                            ::
++  kiss                                                ::  request
          $:  [%com p=@p]                               ::  toggle compute svr
              [%end p=brat]                             ::  local end
              [%fan p=@ud]                              ::  set local fanout
              [%kil ~]                                  ::  terminate ship
              [%int ~]                                  ::  interrupt ship
              [%new p=@p q=(set ,@p) q=home r=@uvI]     ::  create ship
              [%run p=@p q=home]                        ::  run existing ship
              [%say p=(list ovum)]                      ::  remote events
              [%sto p=husk]                             ::  toggle logger
          ==                                            ::
++  axil                                                ::
          $:  bus=(unit ,@p)                            ::  master
              loc=@ud                                   ::  local resources
              hen=(unit duct)                           ::  effect duct
              ent=@                                     ::  entropy
              seq=@                                     ::  brat sequence
              why=(map duct born)                       ::  hosted ships
              how=(map born duct)                       ::  reverse why
              hut=(map born home)                       ::  storage control
              ham=(set hulk)                            ::  block stores
              sto=(set husk)                            ::  log stores
              com=(set ship)                            ::  compute resources
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

