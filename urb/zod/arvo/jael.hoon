::    %jael, secret storage
::
::::  /hoon/jael
  ::
!?  164
::::
::  %jael is logically homogeneous, but please follow these conventions:
::
::  /cap                    ::  foreign app keys
::    /service              ::  service name, eg %face
::      /appid              ::  your ship's app-id
::        /@uvH             ::  by hash
::        /@ud              ::  by number
::        /@tas             ::  by name
::
::  /key                    ::  foreign user secrets
::    /service              ::  service name, eg %face
::      /userid             ::  user identity

|=  pit=vase   
=>  =~
::  structures
|%
++  axle                                                ::  %jael state
          $:  %0                                        ::
              ent=@uwH                                  ::  entropy
              all=(map ship ,[p=@ q=safe])              ::  entropy, secrets
          ==                                            ::
++  mast  $:  ent=@
               
++  mort  ,[p=@da q=duct r=@]                           ::  a mortal secret
++  gift                                                ::  out result <-$
          $%  [%done p=path q=@]                        ::  key expired
          ==                                            ::
++  kiss                                                ::  in request ->$
          $%  [%kill p=
              [%drop p=@p q=path r=@]                   ::  discard key
              [%junk p=@]                               ::  add entropy
              [%show p=@p q=path]                       ::  read subtree
              [%tell p=@ q=path r=@da s=@]              ::  save key
          ==                                            ::
++  safe  ,[p=(unit ,@) q=(map ,@ta safe)]              ::  secret tree
++  move  ,[p=duct q=[%give p=gift]]                    ::  local move
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
    ~&  %jael-reset
    ..^$
  ..^$(+>- u.lox)
::
++  scry
  |=  [fur=(unit (set monk)) ren=@tas who=ship syd=desk lot=coin tyl=path]
  ^-  (unit (unit (pair mark ,*)))
  ::  actually scry
  ~
::
++  stay                                                ::  save w/o cache
  `axle`+>-.$(pol (~(run by pol) |=(a=baby [tad.a dym.a ~])))
::
++  take                                                ::  response
  |=  [tea=wire hen=duct hin=(hypo noun)]
  !!
--
