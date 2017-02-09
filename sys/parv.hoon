!:                                                      ::  /sys/arvo
::                                                      ::  !%reference/2
::  %arvo: arvo microkernel.
::
=<  ::  this lifecycle wrapper makes the arvo door
    ::  (multi-armed core) look like a gate (function
    ::  or single-armed core), to fit urbit's formal
    ::  lifecycle function.  a practical interpreter 
    ::  can ignore it.
    ::
    |=  {now/@da ovo/ovum}
    ^+  .
    ~>  %slog.[0 leaf+"arvo-event"]
    .(+> +:(poke now ovo))
=>  
::                                                      ::  ::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ::  (1) public molds
::                                                      ::  ::
|%
++  arms  (map chip dope)                               ::  stated identity
++  card  {p/@tas q/*}                                  ::  tagged event
++  chip                                                ::  standard identity
  $?  $giv                                              ::  given name
      $fam                                              ::  surname
      $had                                              ::  fictitious name
      $mid                                              ::  middle name
      $gen                                              ::  generational suffix
  ==                                                    ::
++  dope  (pair @tas @t)                                ::  term/unicode pair
++  duct  (list wire)                                   ::  causal history
++  ovum  (pair wire card)                              ::  input or output
++  ruby  @pG                                           ::  64-bit passcode
++  wire  path                                          ::  cause
--  =>
::                                                      ::  ::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ::  (2) state molds
::                                                      ::  ::
|% 
++  evil                                                ::  hoon-dependent
  |*  {span/_span twig/_twig vase/_vase}                ::  inject hoon version
  |%                                                    ::
  ++  hulk                                              ::  main stack move
    $%  {$unix p/@da q/@tas r/ovum}                     ::  unix input
        {$vane p/move}                                  ::  vane action
    ==                                                  ::
  ++  mall                                              ::  any arvo version
    $?  {$1 mast}                                       ::  version 1, current
    ==                                                  ::
  ++  mast                                              ::  system state
    $:  $=  gut                                         ::
        $:  run/(list hulk)                             ::  worklist
            out/(list ovum)                             ::  output
        ==                                              ::
        $=  mal                                         ::  mammal brain
        $:  sac/worm                                    ::  vane compiler cache
            van/(list (pair term vase))                 ::
        ==                                              ::
        $=  rep                                         ::  reptile brain
        $:  orb/@p                                      ::  ship
            nym/arms                                    ::  name information
            roy/(map @ud ruby)                          ::  start secrets
            fat/(map path (pair term noun))             ::  boot filesystem
    ==  ==                                              ::
  ++  mill  (each vase milo)                            ::  vase or metavase
  ++  milo  {p/* q/*}                                   ::  untyped metavase
  ++  move  (pair duct arvo)                            ::  vane move
  ++  part                                              ::  arvo vane move
    $%  {$give p/mill}                                  ::  vane "return"
        {$poke p/wire q/(pair term mill)}               ::  vane "call"
    ==                                                  ::
  ++  worm                                              ::  compiler cache
    $:  nes/(set ^)                                     ::  ++nest
        pay/(map (pair span twig) span)                 ::  ++play
        mit/(map (pair span twig) (pair span nock))     ::  ++mint
    ==                                                  ::
  --                                                    ::
++  live  (evil)                                        ::  modern molds
++  vile  (evil typo twit vise)                         ::  old molds
--  =>
|%
::                                                      ::  ::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ::  (2) engines
::                                                      ::  ::
|%
::                                                      ::  ++le
++  le                                                  ::  deep engine
  =+  mast:live
  =*  sys  -
  |%
  ++  pike
    |=  $:  ::  lay: event route
            ::  now: date
            ::  ovo: event
            ::
            lay/@tas
            now/@da
            ovo/ovum
        ==
    work(run.gut [%unix 
  ::                                                    ::  ++poke:le
  ++  poke                                              ::  event from unix
    |=  $:  ::  now: event date
            ::  ovo: event
            ::
            now/@da
            ovo/ovum
        ==
    ^+  sys
    ?+    -.ovo  ~&(le-bad-egg++.ovo !!)
        $helo
      =+  dat=((hard {@p arms (map @ud ruby)}) +.ovo)
      sys(
    ::
        $hola                                           ::  
      =+  dat=((hard {@p arms (map @ud ruby)}) +.ovo)
    ==
  --
--
::                                                      ::  ::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ::  (3) surface
::                                                      ::  ::
|_  
::                                                      ::  ++come
++  come                                                ::  upgrade from old
  |=  
  ^-  {(list ovum) _+>}
  ~&  %hoon-come
  =^  rey  +>+  (^come +<)
  [rey +>.$]
::
    ++  keep  |=(* (^keep ((hard {@da path}) +<)))      ::  4
    ++  load  |=  {@ (list ovum) pane}                  ::  86
              ^-  {(list ovum) _+>}
              ~&  %hoon-load
              =^  rey  +>+  (^load +<)
              [rey +>.$]
    ++  peek  |=(* (^peek ((hard {@da path}) +<)))      ::  87
    ++  poke  |=  *                                     ::  42
              ^-  {(list ovum) *}
              =>  .(+< ((hard {now/@da ovo/ovum}) +<))
              =^  ova  +>+  (^poke now ovo)
              |-  ^-  {(list ovum) *}
              ?~  ova
                [~ +>.^$]
              ?:  ?=($verb -.q.i.ova)
                $(ova t.ova, lac !lac)
              ?:  ?=($veer -.q.i.ova)
                $(ova t.ova, +>+.^$ (veer now q.i.ova))
              ?:  ?=($velo -.q.i.ova)
                (fall (velo now t.ova ({@ @} +.q.i.ova)) [~ +>.^$])
              ?:  ?=(?($init $veal) -.q.i.ova)
                =+  avo=$(ova t.ova, +>+.^$ (boot (@ +.q.i.ova)))
                [[i.ova -.avo] +.avo]
              ?:  ?=($mass -.q.i.ova)
                =+  avo=$(ova t.ova)
                :_  +.avo
                :_  -.avo
                %=    i.ova
                    q.q
                  :-  %userspace
                  :-  %|
                  :~  hoon+`pit
                      zuse+`mast
                      hoon-cache+`p.niz
                      q.q.i.ova
                      dot+`.
                  ==
                ==
              =+(avo=$(ova t.ova) [[i.ova -.avo] +.avo])
    ++  wish  |=(* (^wish ((hard @ta) +<)))             ::  20

--
