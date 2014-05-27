!:  ::  %gall, user-level extension
!?  164
::::
|=  pit=vase
^-  vane
=>  =~                                                  ::  preface
|%                                                      ::  structures
++  axle                                                ::  all %gall state
  $:  ven=%0                                            ::  state version
      pol=(map ship mast)                               ::  apps by ship
  ==                                                    ::
++  bone  ,@ud                                          ::  opaque duct
++  cork  ?(%u %v %w %x %y %z)                          ::  view angle
++  gift                                                ::  out result <-$
  %$  [%back ~]                                         ::  acknowledgment
      [%crud p=(list tank)]                             ::  error notification
      [%seen p=@da q=*]                                 ::  wind update
  ==                                                    ::
++  kiss                                                ::  in request ->$
  $%  [%mess p=ship q=term r=(disk)]                    ::  urbit message
      [%puke p=@tas q=(list tank) r=kiss]               ::  rising error
      [%user p=(unit chop) q=kiss]                      ::  restriction
      [%show p=ship q=term r=wind s=path]               ::  subscription
  ==                                                    ::
++  mast                                                ::  apps by ship
  $:  bum=(map ,@ta seat)                               ::  apps by name
  ==                                                    ::
++  move  ,[p=duct q=(mold note gift)]                  ::  typed move
++  note  curd                                          ::  out request $->
++  seat                                                ::  the living app
  $:  
      huv=(unit vase)                                   ::  current hive
      dep=(set ,[p=ship q=desk])                        ::  dependencies
      orm=(unit ,@da)                                   ::  last buildtime
      eny=@                                             ::  entropy
      lat=@da                                           ::  time of last tick
      tik=@ud                                           ::  tick computed
      vey=(qeu ,[p=bone q=debt])                        ::  blocked queue
      ^=  zam                                           ::  opaque duct system
      $:  p=@ud                                         ::  sequence
          q=(map duct ,[p=bone q=(unit chop)])          ::  by duct
          r=(map bone duct)                             ::  by bone
      ==                                                ::  
  ==                                                    ::
++  wind  ,[p=cork q=@da r=(unit ,@dr)]                 ::  show window
--                                                      ::
|%                                                      ::  functions
++  limp                                                ::  merge authorities
  |=  [a=(unit chop) b=(unit chop)]
  ^-  (unit chop)
  a ::  XX ?
--
.  ==                                                   ::  end preface
=|  all=axle                                            ::  all vane state
|=  $:  now=@da                                         ::  urban time
        eny=@                                           ::  entropy
        $=  ski                                         ::  namespace
        $+  $:  (unit (set monk))                       ::  rights
                path                                    ::  name
        ==  (unit (unit (disk)))                        ::  known/any/value
    ==                                                  ::  opaque core
=<  |%                                                  ::
    ++  take                                            ::  return card
      |=  [pax=path hen=duct hil=(hypo curd)]           ::
      ^-  [(list move) ..^$]
      !! 
    ++  give                                            ::  request card
      |=  [hen=duct hil=(hypo kiss)]
      =+  ^=  ,[p=ship q=term]
          ?-  -.q.hil
            %mess
            %puke
          ==
      ?-  -.hil
    ++  scry                                            ::  observe
      |=  $:  use=(unit (set monk))                     ::  observers
              bid=bead                                  ::  position
          ==                                                ::
      *view
    --                    
++  go
  |_  $:  our=@p                                        ::  application owner
          app=@tas                                      ::  application name
          mat=mast                                      ::  per owner
          sat=seat                                      ::  per application
      ==                                                ::
  ++  abet                                              ::  collapse core
    %_  +>
      lex  %_  lex
              pol  %+  ~(put by pol.lex)  our 
                   %_  mat
                     bum  (~(put by bum.mat) app sat)
    ==     ==      ==
  ::
  ++  bear                                              ::  write backward
    |=  hen=duct
    =+  orf=(need (~(get by q.zam) hen))
    ~(. bo:~(. au p.orf) hen q.orf)
  ::
  ++  beef                                              ::  read in
    |=  [lok=case use=(unit (set monk))
    ~(. au use)
  ::
  ++  boar                                              ::  write forward
    |=  $:  hen=duct                                    ::  cause
            law=(unit chop)                             ::  read permission
        ==
    =^  orf  zam                                        ::  opaque duct
      =+  orf=(~(get by q.zam) hen)
      ?^  orf
        [[p.u.orf (limp law q.u.orf)] zam]
      :^  [[p.zam law] +(p.zam) 
        (~(put by q.zam) hen [p.zam aud])
      (~(put by r.zam) p.zam hen)
    ~(. bo:~(. au ?~(law [~ ~] p.u.law)) hen p.orf ?~(law ~ q.u.law))
  ::
  ++  au                                                ::  read
    |_  use=(unit (set monk))                           ::  write
    ++  lens
      |=  
    ++  bo
      |_  $:  hen=duct                                  ::  system duct
              ost=bone                                  ::  opaque duct
              seg=(unit (set monk))                     ::  write permission
          ==
      ++  abet  +>                                      ::  collapse core
      ++  mess                                          ::
        |=  dur=(disk)                                  ::  general data
        ^-  [(list move) _+>]
        !!                                              ::
      
      ++  more                                          ::  process result
        |=  $:  pax=path 
                hil=(hypo curd)
            ==
        ^-  [(list move) _+>]
      ++  show                                          ::
        |=  $:  win=wind                                ::  version window
                pax=path                                ::  subtree
            ==                                          ::
        ^-  [(list move) +>]
        !!
      --
    ++  abet  +>
    --
  --
::
++  goad                                                ::  take and go
  |=  [our=@p app=@tas]
  =+  mat=(need (~(get by pol.lex our)))
  =+  sat=(need (~(get by bum.mat) app))
  ~.( go [our app mat sat])
::
++  goat                                                ::  call and go
  |=  [our=@p app=@tas]
  =+  ^=  mat  ^-  mast                                 ::  per owner
      =+  mat=(~(get by pol.lex) p.fav)
      ?~(mat *mast u.mat)
  =+  ^=  sat  ^-  seat                                 ::  per app
      =+  sat=(~(get by bum.mat) q.fav)
      ?^  sat  u.sat
      %*  .  *seat
        eny  (shax (mix now eny))
        lat  now
      ==
  ~(. go [our app mat sat])
--
  ==
.  ==                                                   ::  end preface
