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
    hood-helm, hood-kiln, hood-drum, hood-write,
    hood-womb
::                                                      ::  ::
::::                                                    ::  ::
  ::                                                    ::  ::
|%
++  hood-module
  ::  each hood module follows this general shape
  =>  |%
      +=  part  [%module %0 pith]
      +=  pith  ~
      ::
      +=  move  [bone card]
      +=  card  $%  [%fake _!!]
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
      |*  hed/hood-head                                 ::
      |=  paw/hood-part                                 ::
      ?-  hed                                           ::
        $drum  ?>(?=($drum -.paw) `part:hood-drum`paw)  ::
        $helm  ?>(?=($helm -.paw) `part:hood-helm`paw)  ::
        $kiln  ?>(?=($kiln -.paw) `part:hood-kiln`paw)  ::
        $womb  ?>(?=($womb -.paw) `part:hood-womb`paw)  ::
        $write  ?>(?=($write -.paw) `part:hood-write`paw) ::
      ==                                                ::  module name
    ++  hood-head  _-:*hood-part                        ::  initialize state
    ++  hood-make                                       ::
      |*  {our/@p hed/hood-head}                        ::
      ?-  hed                                           ::
        $drum  (make:hood-drum our)                     ::
        $helm  *part:hood-helm                          ::
        $kiln  *part:hood-kiln                          ::
        $womb  *part:hood-womb                          ::
        $write  *part:hood-write                        ::
      ==                                                ::
    ++  hood-part-old  hood-part                        ::  old state for ++prep
    ++  hood-port                                       ::  state transition
      |=  paw/hood-part-old  ^-  hood-part              ::
      paw                                               ::
    ::                                                  ::
    ++  hood-part                                       ::  current module state
      $%  {$drum $2 pith-2:hood-drum}                   ::
          {$helm $0 pith:hood-helm}                     ::
          {$kiln $0 pith:hood-kiln}                     ::
          {$womb $1 pith:hood-womb}                     ::
          {$write $0 pith:hood-write}                   ::
      ==                                                ::
    --                                                  ::
::                                                      ::  ::
::::                                                    ::  ::  app proper
  ::                                                    ::  ::
=,  gall
|_  $:  hid/bowl                                        ::  gall environment
        hood-1                                          ::  module states
    ==                                                  ::
++  able                                                ::  find+make part
  |*  hed/hood-head
  =+  rep=(~(get by lac) hed)
  =+  par=?^(rep u.rep `hood-part`(hood-make our.hid hed))
  ((hood-good hed) par)
::
++  ably                                                ::  save part
  |*  {(list) hood-part}
  [(flop +<-) %_(+> lac (~(put by lac) +<+< +<+))]
::                                                      ::  ::
::::                                                    ::  ::  generic handling
  ::                                                    ::  ::
++  prep
  |=  old/(unit hood-old)  ^-  (quip _!! _+>)
  :-  ~
  ?~  old  +>
  +>(lac (~(run by lac.u.old) hood-port))
::
++  poke-hood-load                                      ::  recover lost brain
  |=  dat/hood-part
  ?>  =(our.hid src.hid)
  ~&  loaded+-.dat
  [~ %_(+> lac (~(put by lac) -.dat dat))]
::
::
++  from-module                                         ::  create wrapper
  |*  _[%module ..$ _abet]:(hood-module)
  =>  .(+< [identity start finish]=+<)
  =-  [wrap=- *start]                 ::  usage (wrap handle-arm):from-foo
  |*  handle/_finish
  |=  _+<.handle
  =.  +>.handle  (start hid (able identity))
  (ably (handle +<))
::
::  per-module interface wrappers
++  from-drum  (from-module %drum [..$ _se-abet]:(hood-drum))
++  from-helm  (from-module %helm [..$ _abet]:(hood-helm))
++  from-kiln  (from-module %kiln [..$ _abet]:(hood-kiln))
++  from-womb  (from-module %womb [..$ _abet]:(hood-womb))
++  from-write  (from-module %write [..$ _abet]:(hood-write))
::
::                                                      ::  ::
::::                                                    ::  ::  switchboard
  ::                                                    ::  ::
++  coup-drum-phat            (wrap take-coup-phat):from-drum
++  coup-helm-hi              (wrap coup-hi):from-helm
++  coup-helm-ask             (wrap coup-ask):from-helm
++  coup-kiln-fancy           (wrap take-coup-fancy):from-kiln
++  coup-kiln-reload          (wrap take-coup-reload):from-kiln
++  coup-kiln-spam            (wrap take-coup-spam):from-kiln
++  diff-sole-effect-drum-phat  (wrap diff-sole-effect-phat):from-drum
++  init-helm                 |=({way/wire *} [~ +>])
++  mack-kiln                 (wrap mack):from-kiln
++  made-write                (wrap made):from-write
++  made-kiln                 (wrap take-made):from-kiln
++  mere-kiln                 (wrap take-mere):from-kiln
++  mere-kiln-sync            (wrap take-mere-sync):from-kiln
++  wake-kiln-overload        (wrap take-wake-overload):from-kiln
++  note-helm                 (wrap take-note):from-helm
++  onto-drum                 (wrap take-onto):from-drum
++  peer-drum                 (wrap peer):from-drum
++  peek-x-womb               peek-x:(hood-womb hid (able %womb))
++  peer-scry-x-womb          (wrap peer-scry-x):from-womb
++  poke-atom                 (wrap poke-atom):from-helm
++  poke-dill-belt            (wrap poke-dill-belt):from-drum
++  poke-drum-put             (wrap poke-put):from-drum
++  poke-drum-link            (wrap poke-link):from-drum
++  poke-drum-unlink          (wrap poke-unlink):from-drum
++  poke-drum-exit            (wrap poke-exit):from-drum
++  poke-drum-start           (wrap poke-start):from-drum
++  poke-helm-hi              (wrap poke-hi):from-helm
++  poke-helm-init            (wrap poke-init):from-helm   :: XX used?
::++  poke-helm-invite          (wrap poke-invite):from-helm
++  poke-helm-mass            (wrap poke-mass):from-helm
++  poke-helm-reload          (wrap poke-reload):from-helm
++  poke-helm-reload-desk     (wrap poke-reload-desk):from-helm
++  poke-helm-reset           (wrap poke-reset):from-helm
++  poke-helm-serve           (wrap poke-serve):from-helm
++  poke-helm-send-hi         (wrap poke-send-hi):from-helm
++  poke-helm-send-ask        (wrap poke-send-ask):from-helm
++  poke-helm-verb            (wrap poke-verb):from-helm
++  poke-helm-nuke            (wrap poke-nuke):from-helm
++  poke-helm-begin           (wrap poke-begin):from-helm
++  poke-helm-spawn           (wrap poke-spawn):from-helm
++  poke-helm-tlon-add-fora   (wrap poke-tlon-add-fora):from-helm
++  poke-helm-tlon-add-stream  (wrap poke-tlon-add-stream):from-helm
++  poke-helm-tlon-init-stream  (wrap poke-tlon-init-stream):from-helm
++  poke-hood-sync            (wrap poke-sync):from-kiln
++  poke-hood-init-sync       (wrap poke-init-sync):from-kiln
++  poke-kiln-commit          (wrap poke-commit):from-kiln
++  poke-kiln-info            (wrap poke-info):from-kiln
++  poke-kiln-label           (wrap poke-label):from-kiln
++  poke-kiln-merge           (wrap poke-merge):from-kiln
++  poke-kiln-cancel          (wrap poke-cancel):from-kiln
++  poke-kiln-mount           (wrap poke-mount):from-kiln
++  poke-kiln-rm              (wrap poke-rm):from-kiln
++  poke-kiln-schedule        (wrap poke-schedule):from-kiln
++  poke-kiln-track           (wrap poke-track):from-kiln
++  poke-kiln-sync            (wrap poke-sync):from-kiln
++  poke-kiln-syncs           (wrap poke-syncs):from-kiln
++  poke-kiln-start-autoload  (wrap poke-start-autoload):from-kiln
++  poke-kiln-wipe-ford       (wrap poke-wipe-ford):from-kiln
++  poke-kiln-autoload        (wrap poke-autoload):from-kiln
++  poke-kiln-overload        (wrap poke-overload):from-kiln
++  poke-kiln-unmount         (wrap poke-unmount):from-kiln
++  poke-kiln-unsync          (wrap poke-unsync):from-kiln
++  poke-kiln-permission      (wrap poke-permission):from-kiln
++  poke-womb-invite          (wrap poke-invite):from-womb
++  poke-womb-save            (wrap poke-save):from-womb
++  poke-womb-obey            (wrap poke-obey):from-womb
++  poke-womb-bonus           (wrap poke-bonus):from-womb
++  poke-womb-claim           (wrap poke-claim):from-womb
++  poke-womb-do-ticket       (wrap poke-do-ticket):from-womb
++  poke-womb-do-claim        (wrap poke-do-claim):from-womb
++  poke-womb-rekey           (wrap poke-rekey):from-womb
++  poke-womb-report          (wrap poke-report):from-womb
++  poke-womb-manage          (wrap poke-manage):from-womb
++  poke-womb-recycle         (wrap poke-recycle):from-womb
++  poke-womb-manage-old-key  (wrap poke-manage-old-key):from-womb
++  poke-womb-release         (wrap poke-release):from-womb
++  poke-womb-release-ships   (wrap poke-release-ships):from-womb
++  poke-womb-reinvite        (wrap poke-reinvite):from-womb
++  poke-womb-replay-log      (wrap poke-replay-log):from-womb
++  poke-write-sec-atom       (wrap poke-sec-atom):from-write
++  poke-write-paste          (wrap poke-paste):from-write
++  poke-write-comment        (wrap poke-comment):from-write
++  poke-write-fora-post      (wrap poke-fora-post):from-write
++  poke-write-plan-info      (wrap poke-plan-info):from-write
++  poke-write-plan-account   (wrap poke-plan-account):from-write
++  poke-write-tree           (wrap poke-tree):from-write
++  poke-write-wipe           (wrap poke-wipe):from-write
++  poke-will                 (wrap poke-will):from-helm
++  quit-drum-phat            (wrap quit-phat):from-drum
++  reap-drum-phat            (wrap reap-phat):from-drum
++  woot-helm                 (wrap take-woot):from-helm
++  writ-kiln-autoload        (wrap take-writ-autoload):from-kiln
++  writ-kiln-sync            (wrap take-writ-sync):from-kiln
--
