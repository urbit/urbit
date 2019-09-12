::                                                      ::  ::
::::  /hoon/hood/app                                    ::  ::
  ::                                                    ::  ::
/?    310                                               ::  zuse version
/-  *sole
/+  sole,                                               ::  libraries
    ::  XX these should really be separate apps, as
    ::     none of them interact with each other in
    ::     any fashion; however, to reduce boot-time
    ::     complexity and work around the current
    ::     non-functionality of end-to-end acknowledgments,
    ::     they have been bundled into :hood
    ::
    ::  |command handlers
    hood-helm-mall, hood-kiln-mall, hood-drum-mall, hood-write-mall
::                                                      ::  ::
::::                                                    ::  ::
  ::                                                    ::  ::
|%
++  hood-module
  ::  each hood module follows this general shape
  =>  |%
      +$  part  [%module %0 pith]
      +$  pith  ~
      --
  |=  [bowl:mall own=part]
  |_  moz=(list card:agent:mall)
  ++  abet  [(flop moz) own]
  --
--
::                                                      ::  ::
::::                                                    ::  ::  state handling
  ::                                                    ::  ::
!:
=>  |%                                                  ::
    ++  hood-old                                        ::  unified old-state
      {?($0 $1) lac/(map @tas hood-part-old)}           ::
    ++  hood-1                                          ::  unified state
      {$1 lac/(map @tas hood-part)}                     ::
    ++  hood-good                                       ::  extract specific
      =+  hed=$:hood-head
      |@  ++  $
            |:  paw=$:hood-part
            ?-  hed
              $drum  ?>(?=($drum -.paw) `part:hood-drum-mall`paw)
              $helm  ?>(?=($helm -.paw) `part:hood-helm-mall`paw)
              $kiln  ?>(?=($kiln -.paw) `part:hood-kiln-mall`paw)
              $write  ?>(?=($write -.paw) `part:hood-write-mall`paw)
            ==
      --
    ++  hood-head  _-:$:hood-part                       ::  initialize state
    ++  hood-make                                       ::
      =+  $:{our/@p hed/hood-head}                      ::
      |@  ++  $
            ?-  hed
              $drum  (make:hood-drum-mall our)
              $helm  *part:hood-helm-mall
              $kiln  *part:hood-kiln-mall
              $write  *part:hood-write-mall
            ==
      --
    ++  hood-part-old  hood-part                        ::  old state for ++prep
    ++  hood-port                                       ::  state transition
      |:  paw=$:hood-part-old  ^-  hood-part            ::
      paw                                               ::
    ::                                                  ::
    ++  hood-part                                       ::  current module state
      $%  {$drum $2 pith-2:hood-drum-mall}              ::
          {$helm $0 pith:hood-helm-mall}                ::
          {$kiln $0 pith:hood-kiln-mall}                ::
          {$write $0 pith:hood-write-mall}              ::
      ==                                                ::
    --                                                  ::
::                                                      ::  ::
::::                                                    ::  ::  app proper
  ::                                                    ::  ::
^-  agent:mall
=|  hood-1                                              ::  module states
=>  |%
    ++  help
    |=  hid/bowl:mall
      |%
      ++  able                                          ::  find+make part
        =+  hed=$:hood-head
        |@  ++  $
              =+  rep=(~(get by lac) hed)
              =+  par=?^(rep u.rep `hood-part`(hood-make our.hid hed))
              ((hood-good hed) par)
        --
      ::
      ++  ably                                          ::  save part
        =+  $:{(list) hood-part}
        |@  ++  $
              [+<- (~(put by lac) +<+< +<+)]
        --
      ::                                                ::  ::
      ::::                                              ::  ::  generic handling
        ::                                              ::  ::
      ++  prep
        |=  old/(unit hood-old)  ^-  (quip _!! _+>)
        :-  ~
        ?~  old  +>
        +>(lac (~(run by lac.u.old) hood-port))
      ::
      ++  poke-hood-load                                ::  recover lost brain
        |=  dat/hood-part
        ?>  =(our.hid src.hid)
        ~&  loaded+-.dat
        [~ (~(put by lac) -.dat dat)]
      ::
      ::
      ++  from-module                                   ::  create wrapper
        |*  _[identity=%module start=..$ finish=_abet]:(hood-module)
        =-  [wrap=- *start]                 ::  usage (wrap handle-arm):from-foo
        |*  handle/_finish
        |=  a=_+<.handle
        =.  +>.handle  (start hid (able identity))
        ^-  (quip card:agent:mall _lac)
        (ably (handle a))
      ::  per-module interface wrappers
      ++  from-drum  (from-module %drum [..$ _se-abet]:(hood-drum-mall))
      ++  from-helm  (from-module %helm [..$ _abet]:(hood-helm-mall))
      ++  from-kiln  (from-module %kiln [..$ _abet]:(hood-kiln-mall))
      ++  from-write  (from-module %write [..$ _abet]:(hood-write-mall))
      --
    --
|_  hid/bowl:mall                                       ::  gall environment
++  handle-init
  `..handle-init
::
++  handle-extract-state
  !>([%1 lac])
::
++  handle-upgrade-state
  |=  =old-state=vase
  =/  old-state  !<(hood-1 old-state-vase)
  ?~  old-state
    ~&  %prep-lost
    `..handle-init
  ~&  %prep-found
  `..handle-init(lac lac.u.old-state)
::
++  handle-poke
  |=  [=mark =vase]
  ^-  (quip card:agent:mall agent:mall)
  =/  h  (help hid)
  =^  cards  lac
    ?+  mark  ~|([%poke-hood-bad-mark mark] !!)
      %hood-load              %-  poke-hood-load:h
                              (need !<(hood-part vase))
      %atom                   %-  (wrap poke-atom):from-helm:h
                              (need !<(@ vase))
      %helm-hi                %-  (wrap poke-hi):from-helm:h
                              (need !<(@t vase))
      %helm-mass              %-  (wrap poke-mass):from-helm:h
                              (need !<(~ vase))
      %helm-reload            %-  (wrap poke-reload):from-helm:h
                              (need !<((list term) vase))
      %helm-reload-desk       %-  (wrap poke-reload-desk):from-helm:h
                              (need !<([@t (list term)] vase))
      %helm-reset             %-  (wrap poke-reset):from-helm:h
                              (need !<(~ vase))
      %helm-serve             %-  (wrap poke-serve):from-helm:h
                              (need !<([=binding:eyre =generator:eyre] vase))
      %helm-send-hi           %-  (wrap poke-send-hi):from-helm:h
                              (need !<([ship (unit tape)] vase))
      %helm-verb              %-  (wrap poke-verb):from-helm:h
                              (need !<(~ vase))
      %helm-rekey             %-  (wrap poke-rekey):from-helm:h
                              (need !<(@t vase))
      %helm-moon              %-  (wrap poke-moon):from-helm:h
                              (need !<((unit [ship udiff:point:able:jael]) vase))
      %helm-nuke              %-  (wrap poke-nuke):from-helm:h
                              (need !<(ship vase))
      %helm-automass          %-  (wrap poke-automass):from-helm:h
                              (need !<(@dr vase))
      %helm-cancel-automass   %-  (wrap poke-cancel-automass):from-helm:h
                              (need !<(~ vase))
      %helm-bonk              %-  (wrap poke-bonk):from-helm:h
                              (need !<(~ vase))
      %dill-belt              %-  (wrap poke-dill-belt):from-drum:h
                              (need !<(dill-belt:dill vase))
      %dill-blit              %-  (wrap poke-dill-blit):from-drum:h
                              (need !<(dill-blit:dill vase))
      %drum-put               %-  (wrap poke-put):from-drum:h
                              (need !<([path @] vase))
      %drum-link              %-  (wrap poke-link):from-drum:h
                              (need !<(gill:gall vase))
      %drum-unlink            %-  (wrap poke-unlink):from-drum:h
                              (need !<(gill:gall vase))
      %drum-exit              %-  (wrap poke-exit):from-drum:h
                              (need !<(~ vase))
      %drum-start             %-  (wrap poke-start):from-drum:h
                              (need !<(well:gall vase))
      %drum-set-boot-apps     %-  (wrap poke-set-boot-apps):from-drum:h
                              (need !<(? vase))
      %hood-sync              %-  (wrap poke-sync):from-kiln:h
                              (need !<([desk ship desk] vase))
      %kiln-commit            %-  (wrap poke-commit):from-kiln:h
                              (need !<([term ?] vase))
      %kiln-info              %-  (wrap poke-info):from-kiln:h
                              (need !<([tape (unit toro:clay)] vase))
      %kiln-label             %-  (wrap poke-label):from-kiln:h
                              (need !<([desk @tas] vase))
      %kiln-merge             %-  (wrap poke-merge):from-kiln:h
                              (need !<([desk ship desk case ?($auto germ:clay)] vase))
      %kiln-cancel            %-  (wrap poke-cancel):from-kiln:h
                              (need !<(desk vase))
      %kiln-cancel-autocommit  %-  (wrap poke-cancel-autocommit):from-kiln:h
                              (need !<(~ vase))
      %kiln-mount             %-  (wrap poke-mount):from-kiln:h
                              (need !<([path term] vase))
      %kiln-rm                %-  (wrap poke-rm):from-kiln:h
                              (need !<(path vase))
      %kiln-schedule          %-  (wrap poke-schedule):from-kiln:h
                              (need !<([path @da @t] vase))
      %kiln-track             %-  (wrap poke-track):from-kiln:h
                              (need !<([desk ship desk] vase))
      %kiln-sync              %-  (wrap poke-sync):from-kiln:h
                              (need !<([desk ship desk] vase))
      %kiln-syncs             %-  (wrap poke-syncs):from-kiln:h
                              (need !<(~ vase))
      %kiln-start-autoload    %-  (wrap poke-start-autoload):from-kiln:h
                              (need !<(~ vase))
      %kiln-wipe-ford         %-  (wrap poke-wipe-ford):from-kiln:h
                              (need !<(@ud vase))
      %kiln-keep-ford         %-  (wrap poke-keep-ford):from-kiln:h
                              (need !<([@ud @ud] vase))
      %kiln-autoload          %-  (wrap poke-autoload):from-kiln:h
                              (need !<((unit ?) vase))
      %kiln-overload          %-  (wrap poke-overload):from-kiln:h
                              (need !<([@dr @da] vase))
      %kiln-wash-gall         %-  (wrap poke-wash-gall):from-kiln:h
                              (need !<(* vase))
      %kiln-unmount           %-  (wrap poke-unmount):from-kiln:h
                              (need !<($@(term [knot path]) vase))
      %kiln-unsync            %-  (wrap poke-unsync):from-kiln:h
                              (need !<([desk ship desk] vase))
      %kiln-permission        %-  (wrap poke-permission):from-kiln:h
                              (need !<([desk path ?] vase))
      %write-sec-atom         %-  (wrap poke-sec-atom):from-write:h
                              (need !<([host:eyre @] vase))
      %write-paste            %-  (wrap poke-paste):from-write:h
                              (need !<([?(%hoon %md %txt) @t] vase))
      %write-tree             %-  (wrap poke-tree):from-write:h
                              (need !<([path mime] vase))
      %write-wipe             %-  (wrap poke-wipe):from-write:h
                              (need !<(path vase))
    ==
  [`(list card:agent:mall)`cards `agent:mall`..handle-init]
::
++  handle-subscribe
  |=  =path
  =/  h  (help hid)
  =^  cards  lac
    ?+  path  ~|([%hood-bad-path wire] !!)
      [%drum *]  ((wrap peer):from-drum:h t.path)
    ==
  [cards ..handle-init]
::
++  handle-unsubscribe
  |=  path
  `..handle-init
::
++  handle-peek
  |=  path
  *(unit (unit cage))
::
++  handle-agent-response
  |=  [=wire =gift:agent:mall]
  =/  h  (help hid)
  =^  cards  lac
    ?+  wire  ~|([%hood-bad-wire wire] !!)
        [%helm %hi *]      %+  (wrap coup-hi):from-helm:h  t.t.wire
                           ?>(?=(%poke-ack -.gift) p.gift)
        [%kiln %fancy *]   %+  (wrap take-coup-fancy):from-kiln:h  t.t.wire
                           ?>(?=(%poke-ack -.gift) p.gift)
        [%kiln %reload *]  %+  (wrap take-coup-reload):from-kiln:h  t.t.wire
                           ?>(?=(%poke-ack -.gift) p.gift)
        [%kiln %spam *]    %+  (wrap take-coup-spam):from-kiln:h  t.t.wire
                           ?>(?=(%poke-ack -.gift) p.gift)
        [%drum %phat *]
      ?-  -.gift
          %http-response  !!
          %poke-ack            ((wrap take-coup-phat):from-drum:h t.t.wire p.gift)
          %subscription-ack    ((wrap reap-phat):from-drum:h t.t.wire p.gift)
          %subscription-close  ((wrap quit-phat):from-drum:h t.t.wire)
          %subscription-update
        %+  (wrap diff-sole-effect-phat):from-drum:h  t.t.wire
        ?>  ?=(%sole-effect p.cage.gift)
        (need !<(sole-effect q.cage.gift))
      ==
    ==
  [cards ..handle-init]
::
++  handle-arvo-response
  |=  [=wire =sign-arvo]
  =/  h  (help hid)
  =^  cards  lac
    ?+  wire  ~|([%hood-bad-wire wire] !!)
      [%helm *]   ((wrap take):from-helm:h t.wire sign-arvo)
      [%drum *]   ((wrap take):from-drum:h t.wire sign-arvo)
      [%kiln *]   ((wrap take-general):from-kiln:h t.wire sign-arvo)
      [%write *]  ((wrap take):from-write:h t.wire sign-arvo)
    ==
  [cards ..handle-init]
::
++  handle-error
  |=  [term tang]
  `..handle-init
--
