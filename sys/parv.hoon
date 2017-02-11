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
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ::  (2) system molds
::                                                      ::  ::
|% 
++  task                                                ::  action from unix
  $%  {$belt $~}
      {$blew $~}
      {$blip $~}
      {$born $~}
      {$flow $~}
      {$hail $~}
      {$hook $~}
      {$they $~}
      {$thud $~}
      {$tick $~}
      {$sync $~}
  ::
      {$what $~}
      {$whom $~}
  == 
++  gift
  $%  {$sy
      {$sy
  ==  
++  
--  =>
::                                                      ::  ::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ::  (2) state molds
::                                                      ::  ::
|% 
++  evil                                                ::  hoon-dependent
  |*  {span/_span twig/_twig vase/_vase}                ::  inject hoon version
  |%                                                    ::
  ++  mall                                              ::  any arvo version
    $?  {$293 mast}                                     ::  kelvin 293, current
    ==                                                  ::
  ++  mast                                              ::  system state
    $:  $=  gut                                         ::  abdomen
        $:  run/(list move)                             ::  worklist
            out/(list ovum)                             ::  output
        ==                                              ::
        $=  bug                                         ::  insect brain
        $:  noc/@ta                                     ::  process nonce
            ver/(qual @tas @ud @ud @ud)                 ::  vendor/version
        ==                                              ::
        $=  mal                                         ::  mammal brain
        $:  lac/?                                       ::  laconic, !verbose
            bud/vase                                    ::  compiled zuse
            sac/worm                                    ::  vane compiler cache
            van/(list (pair term vase))                 ::  vanes
        ==                                              ::
        $=  rep                                         ::  reptile brain
        $:  orb/@p                                      ::  ship
            nym/arms                                    ::  name information
            roy/(map @ud ruby)                          ::  start secrets
            fat/(map path (pair term noun))             ::  boot filesystem
    ==  ==                                              ::
  ++  mill  (each vase milo)                            ::  vase or metavase
  ++  milo  {p/* q/*}                                   ::  untyped metavase
  ++  move  (pair duct part)                            ::  vane move
  ++  part                                              ::  arvo vane move
    $%  {$give p/mill}                                  ::  vane "return"
        {$pass p/wire q/(pair term mill)}               ::  vane "call"
    ==                                                  ::
  ++  worm                                              ::  compiler cache
    $:  nes/(set ^)                                     ::  ++nest
        pay/(map (pair span twig) span)                 ::  ++play
        mit/(map (pair span twig) (pair span nock))     ::  ++mint
    ==                                                  ::
  --                                                    ::
++  live  (evil)                                        ::  modern molds
++  vile  (evil typo twit vise)                         ::  old molds
++  wasp                                                ::  arvo effect
  $%  {$walk ~}                                         ::  build mammal brain
      {$wear p/@tas q/@t}                               ::  load/reload vane
      {$what p/(list (pair path (pair term noun)))}     ::  put reptile files
      {$whom p/@p q/arms r/(map @ud ruby)}              ::  put reptile identity
      {$wine p/
      {$wink p/@t q/@t}                                 ::  reset york/zuse
      {$
      {$woke ~}
  ==                                                    ::
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
  ::                                                    ::  ++born:le
  ++  born                                              ::  true if booted
    ^-  ?
    ::
    ::  when arvo is "unborn," the ship 
    ::
    =(~ roy.rep)  
  ::                                                    ::  ++pike:le
  ++  pike                                              ::  make a move
    |=  $:  ::  lay: event route
            ::  now: date
            ::  ovo: event
            ::
            lay/@tas
            now/@da
            tea/wire
        ==
    ::  print event if not in laconic mode
    ::
    ~?  &(!lac.mal !=(%belt -.q.ovo))  [%unix -.q.ovo p.ovo]
    ::
    ::  add soft move to queue; 
    ::
    %=    work
        run.gut
      :_  run.gut
      ^-  move
      :-  `wire`[p.ovo ~]
      ^-  part
      :^  %pass  t.p.ovo
        lay
      :+  %&
        [%cell [%atom %tas `%soft] %noun]
      [%soft q.ovo]
    ==
  ::                                                    ::  ++poke:le
  ++  poke                                              ::  event from unix
    |=  $:  ::  now: event date
            ::  ovo: event
            ::
            now/@da
            ovo/ovum
        ==
    ^+  sys
    ~|  
    ?+    -.ovo  ~&(le-bad-egg++.ovo sys)
        belt
        blip
        born
        hail
        hook
        they
        thud
        tick
        sync
    ::
    ::  $what: install boot files in reptile brain
    ::
        $what                                           ::  
      =+  mol  $:  fat/(map path (pair term noun))
      =+  dat=((hard {@p arms (map @ud ruby)}) +.ovo)

    ::
    ::  $whom: install identity information and complete boot process
    ::
        $whom
      =*  mol  $:  orb/@p
                   nym/arms
                   roy/(map @ud ruby)
               ==
      =+  dat=((hard mol) +.ovo)
      =:  orb.rep  orb.dat
          nym.rep  nym.dat
          roy.rep  (~(uni by roy.rep) roy.rep roy.dat)
        ==

    ==
  ++  push
  ++  work

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
