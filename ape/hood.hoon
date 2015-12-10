::                                                      ::  ::
::::  /hoon/hood/ape                                    ::  ::
  ::                                                    ::  ::
/?  314                                                 ::  zuse version
/+  sole, talk, helm, kiln, drum, write                 ::  libraries
[. helm kiln drum]
::                                                      ::  ::
::::                                                    ::  ::
  !:                                                    ::  ::
=>  |%                                                  ::  module boilerplate
    ++  hood-0                                          :: 
      ,[%0 lac=(map ,@tas hood-part)]                   ::
    ++  hood-good                                       ::
      |*  hed=hood-head                                 ::
      |=  paw=hood-part                                 ::
      ?-  hed                                           ::
        %drum  ?>(?=(%drum -.paw) `drum-part`paw)       ::
        %helm  ?>(?=(%helm -.paw) `helm-part`paw)       ::
        %kiln  ?>(?=(%kiln -.paw) `kiln-part`paw)       ::
        %write  ?>(?=(%write -.paw) `part:write`paw)    ::
      ==                                                ::
    ++  hood-head  ,_-:*hood-part                       ::
    ++  hood-make                                       ::
      |*  [our=@p hed=hood-head]                        ::
      ?-  hed                                           ::
        %drum  (drum-port our)                          ::
        %helm  *helm-part                               ::
        %kiln  *kiln-part                               ::
        %write  *part:write                             ::
      ==                                                ::
    ++  hood-part                                       ::
      $%  [%drum %0 drum-pith]                          ::
          [%helm %0 helm-pith]                          ::
          [%kiln %0 kiln-pith]                          ::
          [%write %0 pith:write]                        ::
      ==                                                ::
    --                                                  ::
::                                                      ::  ::
::::                                                    ::  ::
  ::                                                    ::  ::
|_  $:  hid=bowl                                        ::  system state
        hood-0                                          ::  server state
    ==                                                  ::
++  able                                                ::  find/make part
  |*  hed=hood-head
  =+  rep=(~(get by lac) hed)
  =+  par=?^(rep u.rep `hood-part`(hood-make our.hid hed))
  ((hood-good hed) par)
::
++  ably                                                ::  save part
  |*  [moz=(list) rep=hood-part]
  [(flop moz) %_(+> lac (~(put by lac) -.rep rep))]
::                                                      ::  ::
::::                                                    ::  ::
  ::                                                    ::  ::
++  coup-kiln-fancy  (wrap take-coup-fancy):from-kiln
++  coup-kiln-spam                                      ::
  |=  [way=wire saw=(unit tang)]
  ~?  ?=(^ saw)  [%kiln-spam-lame u.saw]
  [~ +>]
::
++  coup-kiln-reload                                    ::
  |=  [way=wire saw=(unit tang)]
  ~?  ?=(^ saw)  [%kiln-reload-lame u.saw]
  [~ +>]
::
++  coup-kiln-overload                                  ::
  |=  [way=wire saw=(unit tang)]
  ~?  ?=(^ saw)  [%kiln-overload-lame u.saw]
  [~ +>]
::
++  coup-drum-phat  (wrap take-coup-phat):from-drum
++  coup-helm-hi    (wrap coup-hi):from-helm
++  diff-sole-effect-drum-phat  (wrap diff-sole-effect-phat):from-drum
++  from-lib
  |*  _[%helm ..$ ,_abet]:(helm)
  =>  .(+< [identity start finish]=+<)
  =-  [wrap=- *start]                 ::  usage (wrap handle-arm):from-foo
  |*  handle=_finish
  |=  _+<.handle
  =.  +>.handle  (start hid (able identity))
  (ably (handle +<))
::
++  from-drum  (from-lib %drum [..$ ,_se-abet]:(drum))
++  from-helm  (from-lib %helm [..$ ,_abet]:(helm))
++  from-kiln  (from-lib %kiln [..$ ,_abet]:(kiln))
++  from-write  (from-lib %write [..$ ,_abet]:(write))
::
++  init-helm                 |=([way=wire *] [~ +>])
++  made-write                (wrap made):from-write
++  made-kiln                 (wrap take-made):from-kiln
++  mere-kiln                 (wrap take-mere):from-kiln
++  mere-kiln-sync            (wrap take-mere-sync):from-kiln
++  wake-kiln-overload        (wrap take-wake-overload):from-kiln
++  note-helm                 (wrap take-note):from-helm
++  onto-drum                 (wrap take-onto):from-drum
++  peer-drum                 (wrap peer):from-drum
++  poke-dill-belt            (wrap poke-dill-belt):from-drum
++  poke-drum-link            (wrap poke-link):from-drum
++  poke-drum-unlink          (wrap poke-unlink):from-drum
::++  poke-drum-exit            (wrap poke-exit):from-drum
++  poke-drum-start           (wrap poke-start):from-drum
++  poke-helm-hi              (wrap poke-hi):from-helm
++  poke-helm-init            (wrap poke-init):from-helm
++  poke-helm-invite          (wrap poke-invite):from-helm
++  poke-helm-mass            (wrap poke-mass):from-helm
++  poke-helm-reload          (wrap poke-reload):from-helm
++  poke-helm-reload-desk     (wrap poke-reload-desk):from-helm
++  poke-helm-reset           (wrap poke-reset):from-helm
++  poke-helm-send-hi         (wrap poke-send-hi):from-helm
++  poke-helm-send-ask        (wrap poke-send-ask):from-helm
++  poke-helm-verb            (wrap poke-verb):from-helm
++  poke-helm-begin           (wrap poke-begin):from-helm
++  poke-hood-sync            (wrap poke-sync):from-kiln
++  poke-kiln-cp              (wrap poke-cp):from-kiln
++  poke-kiln-label           (wrap poke-label):from-kiln
++  poke-kiln-merge           (wrap poke-merge):from-kiln
++  poke-kiln-cancel          (wrap poke-cancel):from-kiln
++  poke-kiln-mount           (wrap poke-mount):from-kiln
++  poke-kiln-mv              (wrap poke-mv):from-kiln
++  poke-kiln-rm              (wrap poke-rm):from-kiln
++  poke-kiln-schedule        (wrap poke-schedule):from-kiln
++  poke-kiln-sync            (wrap poke-sync):from-kiln
++  poke-kiln-start-autoload  (wrap poke-start-autoload):from-kiln
++  poke-kiln-autoload        (wrap poke-autoload):from-kiln
++  poke-kiln-overload        (wrap poke-overload):from-kiln
++  poke-kiln-unmount         (wrap poke-unmount):from-kiln
++  poke-kiln-unsync          (wrap poke-unsync):from-kiln
++  poke-write-paste          (wrap poke-paste):from-write
++  poke-write-tree           (wrap poke-tree):from-write
++  poke-write-wipe           (wrap poke-wipe):from-write
++  poke-will                 (wrap poke-will):from-helm
++  quit-drum-phat            (wrap quit-phat):from-drum
++  reap-drum-phat            (wrap reap-phat):from-drum
++  woot-helm                 (wrap take-woot):from-helm
++  writ-kiln-autoload        (wrap take-writ-autoload):from-kiln
++  writ-kiln-sync            (wrap take-writ-sync):from-kiln
--
