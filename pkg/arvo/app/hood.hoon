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
    hood-helm, hood-kiln, hood-drum, hood-write
::                                                      ::  ::
::::                                                    ::  ::
  ::                                                    ::  ::
|%
++  hood-module
  ::  each hood module follows this general shape
  =>  |%
      +$  part  [%module %0 pith]
      +$  pith  ~
      ++  take
        |~  [wire sign-arvo]
        *(quip card:agent:gall part)
      ++  take-agent
        |~  [wire gift:agent:gall]
        *(quip card:agent:gall part)
      ++  poke
        |~  [mark vase]
        *(quip card:agent:gall part)
      --
  |=  [bowl:gall own=part]
  |_  moz=(list card:agent:gall)
  ++  abet  [(flop moz) own]
  --
--
::                                                      ::  ::
::::                                                    ::  ::  state handling
  ::                                                    ::  ::
!:
=>  |%                                                  ::
    ++  hood-old                                        ::  unified old-state
      {?($1 $2 $3) lac/(map @tas hood-part-old)}        ::
    ++  hood-1                                          ::  unified state
      {$3 lac/(map @tas hood-part)}                     ::
    ++  hood-good                                       ::  extract specific
      =+  hed=$:hood-head
      |@  ++  $
            |:  paw=$:hood-part
            ?-  hed
              $drum  ?>(?=($drum -.paw) `part:hood-drum`paw)
              $helm  ?>(?=($helm -.paw) `part:hood-helm`paw)
              $kiln  ?>(?=($kiln -.paw) `part:hood-kiln`paw)
              $write  ?>(?=($write -.paw) `part:hood-write`paw)
            ==
      --
    ++  hood-head  _-:$:hood-part                       ::  initialize state
    ++  hood-make                                       ::
      =+  $:{our/@p hed/hood-head}                      ::
      |@  ++  $
            ?-  hed
              $drum  (make:hood-drum our)
              $helm  *part:hood-helm
              $kiln  *part:hood-kiln
              $write  *part:hood-write
            ==
      --
    ++  hood-part-old  hood-part                        ::  old state for ++prep
    ++  hood-port                                       ::  state transition
      |:  paw=$:hood-part-old  ^-  hood-part            ::
      paw                                               ::
    ::                                                  ::
    ++  hood-part                                       ::  current module state
      $%  {$drum $2 pith-2:hood-drum}                   ::
          {$helm $0 pith:hood-helm}                     ::
          {$kiln $0 pith:hood-kiln}                     ::
          {$write $0 pith:hood-write}                   ::
      ==                                                ::
    --                                                  ::
::                                                      ::  ::
::::                                                    ::  ::  app proper
  ::                                                    ::  ::
^-  agent:gall
=|  hood-1                                              ::  module states
=>  |%
    ++  help
    |=  hid/bowl:gall
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
        ^-  (quip card:agent:gall _lac)
        %-  ably
        ^-  (quip card:agent:gall hood-part)
        (handle a)
      ::  per-module interface wrappers
      ++  from-drum  (from-module %drum [..$ _se-abet]:(hood-drum))
      ++  from-helm  (from-module %helm [..$ _abet]:(hood-helm))
      ++  from-kiln  (from-module %kiln [..$ _abet]:(hood-kiln))
      ++  from-write  (from-module %write [..$ _abet]:(hood-write))
      --
    --
|_  hid/bowl:gall                                       ::  gall environment
++  on-init
  `..on-init
::
++  on-save
  !>([%3 lac])
::
++  on-load
  |=  =old-state=vase
  =/  old-state  !<(hood-old old-state-vase)
  =^  cards  lac
    =.  lac  lac.old-state
    ?-  -.old-state
      %1  ((wrap on-load):from-drum:(help hid) %1)
      %2  ((wrap on-load):from-drum:(help hid) %2)
      %3  `lac
    ==
  [cards ..on-init]
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card:agent:gall agent:gall)
  =/  h  (help hid)
  =^  cards  lac
    ?:  =(%helm (end 3 4 mark))
      ((wrap poke):from-helm:h mark vase)
    ?:  =(%drum (end 3 4 mark))
      ((wrap poke):from-drum:h mark vase)
    ?:  =(%kiln (end 3 4 mark))
      ((wrap poke):from-kiln:h mark vase)
    ?:  =(%write (end 3 5 mark))
      ((wrap poke):from-write:h mark vase)
    ::  XX should rename and move to libs
    ::
    ?+  mark  ~|([%poke-hood-bad-mark mark] !!)
      %hood-load  (poke-hood-load:h !<(hood-part vase))
      %atom       ((wrap poke-atom):from-helm:h !<(@ vase))
      %dill-belt  ((wrap poke-dill-belt):from-drum:h !<(dill-belt:dill vase))
      %dill-blit  ((wrap poke-dill-blit):from-drum:h !<(dill-blit:dill vase))
      %hood-sync  ((wrap poke-sync):from-kiln:h !<([desk ship desk] vase))
    ==
  [cards ..on-init]
::
++  on-watch
  |=  =path
  =/  h  (help hid)
  =^  cards  lac
    ?+  path  ~|([%hood-bad-path wire] !!)
      [%drum *]  ((wrap peer):from-drum:h t.path)
    ==
  [cards ..on-init]
::
++  on-leave
  |=  path
  `..on-init
::
++  on-peek
  |=  path
  *(unit (unit cage))
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  =/  h  (help hid)
  =^  cards  lac
    ?+  wire  ~|([%hood-bad-wire wire] !!)
      [%helm *]   ((wrap take-agent):from-helm:h wire sign)
      [%kiln *]   ((wrap take-agent):from-kiln:h wire sign)
      [%drum *]   ((wrap take-agent):from-drum:h wire sign)
      [%write *]  ((wrap take-agent):from-write:h wire sign)
    ==
  [cards ..on-init]
::
++  on-arvo
  |=  [=wire =sign-arvo]
  =/  h  (help hid)
  =^  cards  lac
    ?+  wire  ~|([%hood-bad-wire wire] !!)
      [%helm *]   ((wrap take):from-helm:h t.wire sign-arvo)
      [%drum *]   ((wrap take):from-drum:h t.wire sign-arvo)
      [%kiln *]   ((wrap take-general):from-kiln:h t.wire sign-arvo)
      [%write *]  ((wrap take):from-write:h t.wire sign-arvo)
    ==
  [cards ..on-init]
::
++  on-fail
  |=  [term tang]
  `..on-init
--
