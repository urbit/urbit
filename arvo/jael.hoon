::    %jael, secret storage
::
::::  /hoon#jael
  ::
!?  164
::::
::  %jael is logically homogeneous, but please follow these conventions:
::
::  /cap                    ::  foreign app keys
::    /service              ::  service name, eg %face for FB
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
::    /tok#hash

|=  pit=vase   
=>  =~
::  structures
|%
++  axle                                                ::  %jael state
          $:  %0                                        ::  %jael version
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
              [%make p=@uw q=(unit ,@da) r=@ud s=path]  ::  generate random
              [%nuke ~]                                 ::  erase subscriber
              [%tell p=@uw q=(unit ,@da) r=path]        ::  save secret
          ==                                            ::
++  move  ,[p=duct q=(mold note gift)]                  ::  typed move
++  note                                                ::  out request $->
          $%  $:  %b                                    ::  to %behn
          $%  [%wait p=@da]                             ::
              [%rest p=@da]                             ::
          ==  ==  ==                                    ::
++  perm  (pair ,@ud (list ,@ud))                       ::  prime definition
++  safe                                                ::
          $:  nub=(unit ,@uw)                           ::  secret
              dex=(unit ,@da)                           ::  expiration
              sud=(set duct)                            ::  subscribers
              kin=(map term safe)                       ::  children
          ==                                            ::
--                                                      ::
::  programs
|%
++  bu
  |_  $:  xap=path 
          fes=(list safe) 
          moz=(list move)
      ==
      axle
  ::
  ++  bu-abet                                           ::  resolve
    ^-  axle
    ?~  xap  +<+
    %=  bu-abet
      xap  t.xap
      fes  t.fes
      sef  %=    i.fes
               kin 
             ?:  =(*safe sef) 
               (~(del by kin.i.fes) i.xap)
             (~(put by kin.i.fes) i.xap sef)
           ==
    ==
  ::
  ++  bu-kill                                           ::  destroy
    ^+  .
    =+  dus=(~(tap by 
        
  ::
  ++  bu-ajar                                           ::  descend
    |=  pax=path
    ^+  +>
    ?~  pax  +>.$
    %=    $
      pax  t.pax
      xap  [i.pax xap]
      fes  [sef fes]
      sef  (fall (~(get by kin.sef) i.pax) *safe)
    ==
  --
++  bury
  |=  [pax=path lex=axle]
  (~(bu-ajar bu [~ ~ ~] tof.lex lex) pax)
--
.  ==
=|  axle
|=  [now=@da eny=@ ski=sled]                            ::  activate
^?                                                      ::  opaque core
|%                                                      ::
++  call                                                ::  request
  |=  [hen=duct hic=(hypo (hobo kiss))]
  ^-  [p=(list move) q=_..^$]
  =>  .(q.hic ?.(?=(%soft -.q.hic) q.hic ((hard kiss) p.q.hic)))
  =^  moz  +>+>-
      =<  bu-abet
      ?-    -.p.q.hic
          %kill
        kill:(bury p.p.q.hic +>+>-)
      ::
        %make
        %nuke
        %tell
      ==
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
++  stay                                                ::  save w#o cache
  `axle`+>-.$(pol (~(run by pol) |=(a=baby [tad.a dym.a ~])))
::
++  take                                                ::  response
  |=  [tea=wire hen=duct hin=(hypo noun)]
  !!
--
