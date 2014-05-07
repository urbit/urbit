!:  ::  %gall, user-level extension
!?  164
::::
|=  pit=vase
^-  vane
=>  =~
|%                                                      ::  structures
++  axle                                                ::  all %gall state
  $:  ven=%0                                            ::  
      own=(map ship yawl)                               ::  apps by ship
      tad=(map duct task)                               ::  tasks by duct
  ==                                                    ::
++  bone  ,@ud                                          ::  opaque duct
++  moan  
++  seat                                                ::  living app
  $:  hiv=vase                                          ::  the hive
      eny=@                                             ::  entropy
      now=@da                                           ::  hive time
      tik=@ud                                           ::  pokes computed
      zud=bone                                          ::  opaque duct
      zos=(map bone hen)                                ::  duct table
      wok=(unit ,@ud)                                   ::  awaiting / next
      vey=(qeu ,[p=@da q=move])                         ::  blocked queue
  ==                                                    ::
++  yawl                                                ::  apps by ship
  $:  bus=(map term seat)                               ::  apps by name
  ==                                                    ::
++  hawk                                                ::  
  $%  [%call p=bone q=wire r=card]                      ::  advance card
      [%give p=bone q=card]                             ::  produce card
      [%play p=bone q=card]                             ::  revisit card
      [%lose p=bone]                                    ::  tear down duct
  ==                                                    ::
++  hide                                                ::  engine state
  $:  own=[p=ship q=@tas]                               ::  static identity
    ^=  seq                                             ::  dynamic sequence
      $:  num=@ud                                       ::  change number
          eny=@                                         ::  entropy
          now=@da                                       ::  date
  ==  ==                                                ::
++  hive                                                ::  typed engine
  $_  ^?                                                ::
  |_  hide                                              ::
  ++  pack  vase                                        ::  save as bag
  ++  poke                                              ::  apply
    |+  $:  kyz=(unit (set keel))                       ::  authorizers
            tea=wire                                    ::  internal position
            fav=card                                    ::  present event
        ==                                              ::
    [*(list hawk) +>]                                   ::  effects
  ++  peek                                              ::  view
    |+  $:  kyz=(unit (set keel))                       ::  inspectors
            asp=?                                       ::  desired aspect
            tea=wire                                    ::  internal position
        ==                                              ::
    [*?(%view %diff) *vase]                             ::
  ++  prep                                              ::  restore from bag
    |+  bux=vase                                        ::
    +>                                                  ::
  ==                                                    ::
--
|%
++  ye                                                  ::  per event
  =|  $:  $:  $:  wru=(unit writ)                       ::  event authority
                  tea=wire                              ::  event place
                  hen=duct                              ::  event floor
                  fav=card                              ::  event data
              ==                                        ::
              $:  now=@da                               ::  event date
                  eny=@                                 ::  unique entropy
                  ska=$+(* (unit (unit)))               ::  system namespace
              ==                                        ::
              mow=(list move)                           ::  pending actions
          ==                                            ::
          axle                                          ::  all vane state
      ==                                                ::
  =*  lex  ->
  |%
  ++  abet
    ^-  [(list move) axle]
    [(flop mow) lex]
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
  ?>  ?=(
  =+  ^=  kul  ^-  (unit keel)
      ?:  |(?=(%
  =^  mos  lex
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
  ?.  (~(nest ut -:!>(`axle`+>-.^$)) | p.old)
    ~&  %eyre-reset
    ..^$
  ..^$(+>- (axle q.old))
::
++  raze
  ^-  vane
  ..$(+>- *axle)
::
++  scry
  |=  [our=ship ren=@tas who=ship syd=disc lot=coin tyl=path]
  ^-  (unit)
  ~
::
++  stay
  `vase`!>((colt `axle`+>-.$))
++  vern  [164 0]
--
