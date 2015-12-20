::    %kahn, social state
::
::::  /hoon#kahn
  ::
!?  164
::::
|=  pit=vase   
=>  =~
::  structures
|%
++  axle                                                ::  %kahn state
          $:  %0                                        ::
              soc=(map monk node)                       ::  social graph
              red=(map duct (set monk))                 ::  reverse subscribers
          ==                                            ::
++  cert  (each will ,*)                                ::  urbit or other
++  gift                                                ::  out result <-$
          $:  [%then p=node]                            ::  propagate change
          ==                                            ::
++  node                                                ::  social identity
          $:  tin=(map monk link)                       ::  inbound links
              oud=(map monk link)                       ::  outbound links
              cet=cert                                  ::  certificate
              sud=(set duct)                            ::  subscribers
          ==                                            ::
++  khan  ,[p=@tas q=@ta]                               ::  foreign identity
++  kiss                                                ::  social update
          $:  [%cert p=monk q=cert]                     ::  set certificate
              [%hear p=monk]                            ::  watch identity
              [%know p=monk q=(map monk link)]          ::  set forward links
              [%nuke ~]                                 ::  erase subscriber
          ==                                            ::
++  link  (pair rank ,@da)                              ::  trust#expiration
++  monk  (each ship khan)                              ::  general identity
++  rank                                                ::  privilege ring
          $?  %0                                        ::  complete trust
              %1                                        ::  structural trust
              %2                                        ::  social trust
              %3                                        ::  neutral trust
              %4                                        ::  negative trust
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
    ~&  %khan-reset
    ..^$
  ..^$(+>- u.lox)
::
++  scry
  |=  [fur=(unit (set monk)) ren=@tas who=ship syd=desk lot=coin tyl=path]
  ^-  (unit (unit (pair mark ,*)))
  ?.  =(big [~ who])  ~
  =+  ^=  yub  ^-  [(unit monk) 
      ?:  =(%urb syd)
        ?.  ?=([* ~] tyl)  ~
        =+  goy=(slaw %p 

  ?+  ?=([%$ %da @]
  ?+    lot  ~
      [%$ %ud @]
    %+  bind
      (perm who u.hun q.p.lot [syd t.tyl])
    |=(a=* [%noun a])
  ::
    ?.  =(now q.p.lot)  ~
    %+  bind
      (temp who u.hun [syd t.tyl])
    |=(a=* [%noun a])
  ==
  ?.  ?=([%da 
  =+  mok  ^-  (unit monk)
    ?:   =(%urb face)
      (
      
::
++  stay                                                ::  save w#o cache
  `axle`+>-.$
::
++  take                                                ::  response
  |=  [tea=wire hen=duct hin=(hypo noun)]
  !!
--

