!:  ::  %gall, application control
!?  164
::::
|=  pit=vase
^-  vane
=>  =~
|%                                                      ::  structures
++  axle                                                ::  all %gall state
  $:  tad=(map duct task)                               ::  tasks by duct
  ==                                                    ::
++  bone  ,@ud                                          ::  abstract duct
++  claw                                                ::  startup chain
  $:  est=@da                                           ::  startup date
      eny=@                                             ::  entropy
      joy=(unit viol)                                   ::  local context
      ran=(unit viol)                                   ::  arguments
      jiv=(unit viol)                                   ::  app configuration
      kyq=(unit viol)                                   ::  app customization
      gam=(unit viol)                                   ::  app image
  ==                                                    ::
++  crab                                                ::  ascending card
  $%  [%halt ~]                                         ::  interrupt
      [%ride p=(list silk)]                             ::  start/restart task
  ==                                                    ::
++  croc  ,*                                            ::  descending card
++  hawk                                                ::  untyped effect
  $%  [%call p=bone q=wire r=*]                         ::  advance card
      [%give p=bone q=*]                                ::  produce card
      [%play p=bone q=*]                                ::  reprocess card
  ==                                                    ::
++  hide                                                ::  engine state
  $:  own=[p=ship q=@tas]                               ::  static identity
      ^=  seq                                           ::  dynamic sequence
        $:  num=@ud                                     ::  change number
            eny=@                                       ::  entropy
            now=@da                                     ::  date
  ==  ==                                                ::
++  hive                                                ::  typed engine
  |*  $:  cud=,_*                                       ::  events (++card)
          wad=,_*                                       ::  actions (++hawk)
          bag=_,*                                       ::  condensed state
      ==                                                ::
  $_  ^?                                                ::
  |_  hide                                              ::
  ++  pack  *bag                                        ::  save as bag
  ++  poke                                              ::  apply
    |+  $:  kul=(unit keel)                             ::  acting agent
            tea=wire                                    ::  logical place
            fav=cud                                     ::  present event
        ==                                              ::
    [*(list wad) +>]                                    ::  effects
  ++  poll                                              ::  current subchange
    |+  tea=wire                                        ::
    _@ud                                                ::
  ++  peek                                              ::  view
    |+  $:  kyl=keel                                    ::  inspecting agent
            asp=?(%view %diff)                          ::  desired aspect
            tea=wire                                    ::  logical place
            dev=path                                    ::  device identity
        ==                                              ::
    [*?(%view %diff) *vase]                             ::
  ++  prep                                              ::  restore from bag
    |+  bux=bag                                         ::
    +>                                                  ::
  ==                                                    ::
++  keel  (unit ship khan)                              ::  general identity
++  khan  ,[p=@tas q=@tas]                              ::  foreign identity
++  lamp                                                ::  task definition
  $:  wor=writ                                          ::  authority
      sac=(list skit)                                   ::  library structure
      lac=(each path twig)                              ::  indirect/direct
  ==                                                    ::
++  logo  ,@tas                                         ::  logical type
++  silk                                                ::  construction layer
  $%  [%a p=vase]                                       ::  nominal
      [%b p=twig]                                       ::  direct
      [%c p=path]                                       ::  indirect
      [%d p=(list silk) q=(list silk)]                  ::  functional
  ==                                                    ::
++  task                                                ::  execution
  $:  kyl=keel                                          ::  logical owner
      lam=(list silk)                                   ::  task definition
      kit=(map ,@ud kite)                               ::  dependencies
      eve=(qeu ,[p=wire q=card])                        ::  pending cards
      vax=(unit vase)                                   ::  current state
  ==                                                    ::
++  vial  ,*                                            ::  untyped vase
--
|%
++  ye                                                  ::  per event
  =|  $:  $:  $:  kul=(unit keel)                       ::  event authority
                  tea=wire                              ::  event place
                  hen=duct                              ::  event floor
                  fav=card                              ::  event data
              ==                                        ::
              $:  now=@da                               ::  event date
                  eny=@                                 ::  unique entropy
                  sky=$+(* (unit))                      ::  system namespace
              ==                                        ::
              mow=(list move)                           ::  pending actions
          ==                                            ::
          bolo                                          ::  all vane state
      ==                                                ::
  =*  bol  ->
  |%
  ++  abet
    ^-  [(list move) bolo]
    [(flop mow) bol]
.  ==
=|  axle
=*  lex  -
|=  [now=@da eny=@ sky=$+(* (unit))]                    ::  activate
^?                                                      ::  opaque core
|%                                                      ::
++  beat                                                ::  process move
  |=  [wru=(unit writ) tea=wire hen=duct fav=curd]
  =>  .(fav ((hard card) fav))
  ^-  [p=(list move) q=vane]
  =+  ^=  kul  ^-  (unit keel)
      ?:  |(?=(%
  =^  mos  bol
    abet:apex:~(adit ye [[wru tea hen fav] [now eny sky] ~] lex)
  [mos ..^$]
::
++  come
  |=  [sam=? old=vase]
  ^-  vane
  (load old)
::
++  doze
  |=  [now=@da hen=duct]
  ^-  (unit ,@da)
  ~
::
++  load
  |=  old=vase
  ^-  vane
  ?.  (~(nest ut -:!>(`bolo`+>-.^$)) | p.old)
    ~&  %eyre-reset
    ..^$
  ..^$(+>- (bolo q.old))
::
++  raze
  ^-  vane
  ..$(+>- *bolo)
::
++  scry
  |=  [our=ship ren=@tas who=ship syd=disc lot=coin tyl=path]
  ^-  (unit)
  ~
::
++  stay
  `vase`!>((colt `bolo`+>-.$))
++  vern  [164 0]
--
