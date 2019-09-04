::                                                      ::  ::
::::  /hoon/hood/app                                    ::  ::
  ::                                                    ::  ::
/?    310                                               ::  zuse version
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
      ::
      +$  move  [bone card]
      +$  card  $%  [%fake ~]
                ==
      --
  |=  [bowl:gall own=part]
  |_  moz=(list move)
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
^-  agent:mall
=|  hood-1                                              ::  module states
=>  |%
    ++  able                                            ::  find+make part
      |=  hid=bowl:mall
      =+  hed=$:hood-head
      |@  ++  $
            =+  rep=(~(get by lac) hed)
            =+  par=?^(rep u.rep `hood-part`(hood-make our.hid hed))
            ((hood-good hed) par)
      --
    ::
    ++  ably                                            ::  save part
      =+  $:{(list) hood-part}
      |@  ++  $
            [(flop +<-) (~(put by lac) +<+< +<+)]
      --
    ::                                                  ::  ::
    ::::                                                ::  ::  generic handling
      ::                                                ::  ::
    ++  prep
      |=  old/(unit hood-old)  ^-  (quip _!! _+>)
      :-  ~
      ?~  old  +>
      +>(lac (~(run by lac.u.old) hood-port))
    ::
    ::  ++  poke-hood-load                                  ::  recover lost brain
    ::    |=  dat/hood-part
    ::    ?>  =(our.hid src.hid)
    ::    ~&  loaded+-.dat
    ::    [~ %_(+> lac (~(put by lac) -.dat dat))]
    ::
    ::
    ++  from-module                                     ::  create wrapper
      |*  _[identity=%module start=..$ finish=_abet]:(hood-module)
      |=  hid=bowl:mall
      =-  [wrap=- *start]                 ::  usage (wrap handle-arm):from-foo
      |*  handle/_finish
      |=  a=_+<.handle
      =.  +>.handle  (start hid ((able hid) identity))
      (ably (handle a))
    ::  per-module interface wrappers
    ++  from-drum  (from-module %drum [..$ _se-abet]:(hood-drum))
    ++  from-helm  (from-module %helm [..$ _abet]:(hood-helm))
    ++  from-kiln  (from-module %kiln [..$ _abet]:(hood-kiln))
    ++  from-write  (from-module %write [..$ _abet]:(hood-write))
    --
|_  hid/bowl:mall                                       ::  gall environment
++  handle-init
  `..handle-init
::
++  handle-prep
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
  ^-  (quip move:agent:mall agent:mall)
  =^  moves  lac
    ?+  mark  ~|([%poke-hood-bad-mark mark] !!)
      %atom  ((wrap poke-atom):(from-helm hid) (need !<(@ vase)))
    ==
  [moves ..handle-init]
::
++  handle-peer
  |=  path
  `..handle-init
::
++  handle-pull
  |=  path
  `..handle-init
::
++  handle-peek
  |=  path
  *(unit (unit cage))
::
++  handle-mall
  |=  [wire internal-gift:mall]
  `..handle-init
::
++  handle-take
  |=  [wire vase]
  `..handle-init
::
++  handle-lame
  |=  [term tang]
  `..handle-init
::
++  handle-stay
  !>([%1 lac])
--
