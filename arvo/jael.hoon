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
::
::  /urb                    ::  urbit secrets
::    /tok/hash

|=  pit=vase   
=>  =~
::  structures
|%
++  axle                                                ::  %jael state
          $:  %0                                        ::
              ent=@                                     ::  entropy
              sef=safe                                  ::  secret tree
              red=(map duct (set path))                 ::  reverse subscribers
          ==                                            ::
++  gift                                                ::  out result <-$
          $%  [%dead p=path]                            ::  key expired
              [%live p=path q=@]                        ::  key created
          ==                                            ::
++  kiss                                                ::  in request ->$
          $%  [%kill p=path]                            ::  discard secret
              [%make p=@uw q=(unit ,@da) r=@ud s=path]  ::  create secret
              [%nuke ~]                                 ::  erase subscriber
              [%prim p=@uw q=(unit ,@da) r=perm s=path] ::  forge prime
              [%tell p=@uw q=(unit ,@da) r=path]        ::  save secret
          ==                                            ::
++  move  ,[p=duct q=[%give p=gift]]                    ::  local move
++  perm  (pair ,@ud (list ,@ud))                       ::  prime definition
++  safe                                                ::
          $:  nub=@uw                                   ::  secret
              dex=(unit ,@da)                           ::  expiration
              sud=(set duct)                            ::  subscribers
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
