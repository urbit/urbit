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
              soc=(map monk node)                       ::  state once big
          ==                                            ::
++  cert  (each will ,*)                                ::  urbit and others
++  gift                                                ::  out result <-$
          $:  [%then p=node]                            ::  propagate change
          ==                                            ::
++  node                                                ::  social identity
          $:  ven=@ud                                   ::  iteration number
              tin=(map monk link)                       ::  inbound links
              oud=(map monk link)                       ::  outbound links
              cet=cert                                  ::  certificate
          ==                                            ::
++  kiss                                                ::  change
          $:  [%that p=note]                            ::  social update
          ==                                            ::
++  link  (pair rank ,@da)                              ::  graph link
++  note  (qual ,@ud monk (map monk link) cert)         ::
++  rank                                                ::  privilege ring
          $?  %0                                        ::  owner / admin
              %1                                        ::  guardian / employer
              %2                                        ::  partner / employee
              %3                                        ::  friend / customer
              %4                                        ::  neighbor/ contact
              %5                                        ::  zombie
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
++  stay                                                ::  save w/o cache
  `axle`+>-.$
::
++  take                                                ::  response
  |=  [tea=wire hen=duct hin=(hypo noun)]
  !!
--

