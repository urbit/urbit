::                                                      ::  ::
::::  /hoon/dojo/app                                    ::  ::::
  ::                                                    ::    ::
/?  309                                                 ::  arvo kelvin
/-  sole, lens                                          ::  console structures
/+  sole                                                ::  console library
=,  sole
::                                                      ::  ::
::::                                                    ::  ::::
  ::                                                    ::    ::
=>  |%                                                  ::  external structures
    ++  house                                           ::  all state
      $:  $5
          egg/@u                                        ::  command count
          hoc/(map bone session)                        ::  conversations
      ==                                                ::
    ++  session                                         ::  per conversation
      $:  say/sole-share                                ::  command-line state
          dir/beam                                      ::  active path
          poy/(unit dojo-project)                       ::  working
          $:  ::  sur: structure imports
              ::
              sur=(list cable:ford)
              ::  lib: library imports
              ::
              lib=(list cable:ford)
          ==
          var/(map term cage)                           ::  variable state
          old/(set term)                                ::  used TLVs
          buf/tape                                      ::  multiline buffer
      ==                                                ::
    ++  monkey                                         ::  per conversation
      $:  say/sole-share                                ::  command-line state
          dir/beam                                      ::  active path
          poy/(unit dojo-project)                       ::  working
          var/(map term cage)                           ::  variable state
          old/(set term)                                ::  used TLVs
          buf/tape                                      ::  multiline buffer
      ==                                                ::
    ++  dojo-command                                    ::
      $^  (pair dojo-sink dojo-source)                  ::  route value
      {$brev p/term}                                    ::  unbind variable
    ::
    ++  dojo-sink                                       ::
      $%  {$flat p/path}                                ::  atom to unix
          {$pill p/path}                                ::  noun to unix pill
          ::  {$tree p/path}                            ::  noun to unix tree
          {$file p/beam}                                ::  save to clay
          $:  $http                                     ::  http outbound
              p/?($post $put)
              q/(unit knot)
              r/purl:eyre
          ==
          {$poke p/goal}                                ::  poke app
          {$show p/?($0 $1 $2 $3 $4 $5)}                ::  val/type/hoon/xray
          {$verb p/term}                                ::  store variable
      ==                                                ::
    ++  dojo-source                                     ::  construction node
      $:  p/@ud                                         ::  assembly index
          q/dojo-build                                  ::  general build
      ==                                                ::
    ++  dojo-build                                      ::  one arvo step
      $~  [%ex *hoon]
      $%  {$ur p/(unit knot) q/purl:eyre}               ::  http GET request
          {$ge p/dojo-model}                            ::  generator
          {$dv p/path}                                  ::  core from source
          {$ex p/hoon}                                  ::  hoon expression
          {$sa p/mark}                                  ::  example mark value
          {$as p/mark q/dojo-source}                    ::  simple transmute
          {$do p/hoon q/dojo-source}                    ::  gate apply
          {$tu p/(list dojo-source)}                    ::  tuple
      ==                                                ::
    ++  dojo-model                                      ::  data construction
      $:  p/dojo-server                                 ::  core source
          q/dojo-config                                 ::  configuration
      ==                                                ::
    ++  dojo-server                                     ::  numbered device
      $:  p/@ud                                         ::  assembly index
          q/path                                        ::  gate path
      ==                                                ::
    ++  dojo-config                                     ::  configuration
      $:  p/(list dojo-source)                          ::  by order
          q/(map term (unit dojo-source))               ::  by keyword
      ==                                                ::
    ++  dojo-project                                    ::  construction state
      $:  mad/dojo-command                              ::  operation
          num/@ud                                       ::  number of tasks
          cud/(unit dojo-source)                        ::  now solving
          pux/(unit path)                               ::  ford working
          pro/(unit vase)                               ::  prompting loop
          per/(unit sole-edit)                          ::  pending reverse
          job/(map @ud dojo-build)                      ::  problems
          rez/(map @ud cage)                            ::  results
      ==                                                ::
    ++  bead  {p/(set beam) q/cage}                     ::  computed result
    ++  goal  {p/ship q/term}                           ::  flat application
    ++  clap                                            ::  action, user
      $%  {$peer p/path}                                ::  subscribe
          {$poke p/(cask)}                              ::  apply
          {$pull ~}                                    ::  unsubscribe
      ==                                                ::
    ++  club                                            ::  action, system
      $%  {$peer p/path}                                ::  subscribe
          {$poke p/cage}                                ::  apply
          {$pull ~}                                    ::  unsubscribe
      ==                                                ::
    ++  card                                            ::  general card
      $%  {$diff $sole-effect sole-effect}              ::
          {$send wire {ship term} clap}                 ::
          $:  $hiss
              wire
              (unit knot)
              mark
              {$hiss hiss:eyre}
          ==
          [%build wire ? schematic:ford]
          [%kill wire ~]
          {$deal wire sock term club}                   ::
          {$info wire toro:clay}                        ::
      ==                                                ::
    ++  move  (pair bone card)                          ::  user-level move
    ++  sign                                            ::
      $%  ::  %made: build result; response to %build +task
          ::
          $:  %made
              ::  date: formal date of the build
              ::
              date=@da
              ::  result: result of the build; either complete build, or error
              ::
              $=  result
              $%  ::  %complete: contains the result of the completed build
                  ::
                  [%complete build-result=build-result:ford]
                  ::  %incomplete: couldn't finish build; contains error message
                  ::
                  [%incomplete =tang]
          ==  ==
          {$unto p/cuft:gall}
      ==
    --
=>
|%
::  |parser-at: parsers for dojo expressions using :dir as working directory
::
++  parser-at
  |=  [our=ship dir=beam]
  |%
  ++  default-app         %hood
  ++  hoon-parser         (vang | (en-beam:format dir))
  ++  our                 p.dir
  ::
  ++  parse-command-line  ;~(sfix parse-command (star ace) (just '\0a'))
  ::
  ++  to-command
    |=  [gol=goal mod=dojo-model]
    ^-  dojo-command
    [[%poke gol] [0 [%ge mod(q.p [q.gol q.p.mod])]]]
  ::
  ++  parse-variable
    |*  [sym=rule src=rule]
    %+  cook
      |=  {a/term b/(unit dojo-source)}
      ^-  dojo-command
      ?~(b [%brev a] [[%verb a] u.b])
    ;~(plug sym (punt src))
  ::
  ++  parse-command
    :: =<  ;~(less |-(;~(pose (jest '|*') ;~(pfix next (knee ** |.(^$))))) .)
    %+  knee  *dojo-command  |.  ~+
    ;~  pose
      ;~  pfix  bar
        %+  cook  to-command
        (stag `goal`[our default-app] parse-model)
      ==
    ::
      ;~  pfix  col
        %+  cook
          |=  {a/goal b/$^(dojo-model dojo-source)}
          ?@  -.b  [[%poke a] b]
          (to-command a b)
        ;~  plug
          parse-goal
          ;~  pose
            ;~(pfix bar parse-model)
            ;~(pfix ace parse-source)
          ==
        ==
      ==
    ::
      ;~  pfix  tis
        ;~  pose
          (parse-variable (jest %dir) ;~(pfix ace :(stag 0 %ex parse-rood)))
          (parse-variable sym ;~(pfix ace parse-source))
        ==
      ==
    ::
      ;~  pfix  net
        ;~  pose
          (parse-variable (cold %sur hep) ;~(pfix gap parse-cables))
          (parse-variable (cold %lib lus) ;~(pfix gap parse-cables))
        ==
      ==
    ::
      ;~((glue ace) parse-sink parse-source)
      (stag [%show %0] parse-source)
    ==
  ::
  ++  parse-sink
    ;~  pose
      ;~(plug (cold %file tar) parse-beam)
      ;~(plug (cold %flat vat) (most net sym))
      ;~(plug (cold %pill dot) (most net sym))
      ;~(plug (cold %http lus) (stag %post parse-iden-url))
      ;~(plug (cold %http hep) (stag %put parse-iden-url))
      (stag %show (cook $?($1 $2 $3 $4 $5) (cook lent (stun [1 5] wut))))
    ==
  ::
  ++  parse-cables
    %+  cook
      |=  cables=(list cable:ford)
      :+  0  %ex
      ^-  hoon
      ::
      :-  %clsg
      %+  turn  cables
      |=  cable=cable:ford
      ^-  hoon
      ::
      :+  %clhp
        ?~  face.cable
          [%rock %n ~]
        [%clhp [%rock %n ~] [%sand %tas u.face.cable]]
      [%sand %tas file-path.cable]
    (most ;~(plug com gaw) parse-cable)
  ::
  ++  parse-cable
    %+  cook  |=(a=cable:ford a)
    ;~  pose
      (stag ~ ;~(pfix tar sym))
      (cook |=([face=term tis=@ file=term] [`face file]) ;~(plug sym tis sym))
      (cook |=(a=term [`a a]) sym)
    ==
  ++  parse-source  (stag 0 parse-build)
  ++  parse-build
      %+  knee  *dojo-build  |.  ~+
      ;~  pose
        ;~(plug (cold %ur lus) parse-iden-url)
        ;~(plug (cold %ge lus) parse-model)
        ;~(plug (cold %as pad) sym ;~(pfix ace parse-source))
        ;~(plug (cold %do cab) parse-hoon ;~(pfix ace parse-source))
        parse-value
      ==
  ::
  ++  parse-goal
    %+  cook  |=(a/goal a)
    ;~  pose
      ;~  plug
        ;~(pfix sig fed:ag)
        ;~(pose ;~(pfix net sym) (easy default-app))
      ==
      %+  stag  our
      ;~(pose sym (easy default-app))
    ==
  ::
  ++  parse-beam
    %+  cook
      |=  a=path
      ::  hack: fixup paths that come out of the hoon parser
      ::
      ::    We currently invoke the hoon parser to read relative paths from
      ::    the command line, and this parser will produce leading ~ path
      ::    components with paths that start with a `/`.
      ::
      ::    This entire path is nuts and we shouldn't be representing paths
      ::    as arbitrary hoons.
      ::
      =?  a  &(?=(^ a) =('' i.a))
        t.a
      (fall (de-beam:format a) [`beak`[p q r]:dir (flop a)])
    =+  vez=hoon-parser
    (sear plex:vez (stag %clsg poor:vez))
  ::
  ++  parse-iden-url
    %+  cook
      |=([a=(unit knot) b=purl:eyre] [`(fall a *knot) b])
    auru:de-purl:html
    ::
  ++  parse-model   ;~(plug parse-server parse-config)
  ++  parse-server  (stag 0 (most net sym))
  ++  parse-hoon    tall:hoon-parser
  ::
  ++  parse-rood
    ::  XX should this use +hoon-parser instead to normalize the case?
    ::
    =>  (vang | (en-beam:format dir))
    ;~  pose
      rood
    ::
      ::  XX refactor ++scat
      ::
      =-  ;~(pfix cen (stag %clsg -))
      %+  sear  |=([a=@ud b=tyke] (posh ~ ~ a b))
      ;~  pose
        porc
        (cook |=(a=(list) [(lent a) ~]) (star cen))
      ==
    ==
  ++  parse-value
    ;~  pose
      (stag %sa ;~(pfix tar pad sym))
      (stag %ex parse-hoon)
      (stag %tu (ifix [lac rac] (most ace parse-source)))
    ==
  ::
  ++  parse-config
    ;~  plug
      (star ;~(pfix ace (stag 0 parse-value)))
      %+  cook
        ~(gas by *(map term (unit dojo-source)))
      %-  star
      ;~  plug
        ;~(pfix com ace tis sym)
        (punt ;~(pfix ace (stag 0 parse-value)))
      ==
    ==
  --
--
::                                                      ::
::::                                                    ::
  ::                                                    ::
=,  gall
=+  foo=*monkey
|_  $:  hid/bowl                                        ::  system state
        house                                           ::  program state
    ==                                                  ::
++  he                                                  ::  per session
  |_  {moz/(list move) session}                         ::
  ::
  ++  he-beam
    ^-  beam
    ?.  =([%ud 0] r.dir)
      dir
    dir(r [%da now.hid])
  ::
  ++  he-disc    `disc:ford`[p q]:he-beam
  ++  he-beak    `beak`[p q r]:he-beam
  ++  he-rail    `rail:ford`[[p q] s]:he-beam
  ++  he-parser  (parser-at our.hid he-beam)
  ::
  ++  dy                                                ::  project work
    |_  dojo-project                                    ::
    ++  dy-abet  +>(poy `+<)                            ::  resolve
    ++  dy-amok  +>(poy ~)                              ::  terminate
    ++  dy-ford                                         ::  send work to ford
      |=  [way=wire schematic=schematic:ford]
      ^+  +>+>
      ?>  ?=($~ pux)
      ::  pin all builds to :now.hid so they don't get cached forever
      ::
      (he-card(poy `+>+<(pux `way)) %build way live=%.n schematic)
    ::
    ++  dy-eyre                                         ::  send work to eyre
      |=  {way/wire usr/(unit knot) req/hiss:eyre}
      ^+  +>+>
      ?>  ?=(~ pux)
      (he-card(poy `+>+<(pux `way)) %hiss way usr %httr %hiss req)
    ::
    ++  dy-stop                                         ::  stop work
      ^+  +>
      =.  poy  ~
      ?~  pux  +>
      %.  [%txt "! cancel {<u.pux>}"]
      he-diff:(he-card [%kill u.pux ~])
    ::
    ++  dy-slam                                         ::  call by ford
      |=  {way/wire gat/vase sam/vase}
      ^+  +>+>
      (dy-ford way `schematic:ford`[%call [%$ %noun gat] [%$ %noun sam]])
    ::
    ++  dy-errd                                         ::  reject change, abet
      |=  {rev/(unit sole-edit) err/@u}
      ^+  +>+>
      (he-errd(poy `+>+<) rev err)
    ::
    ++  dy-diff                                         ::  send effects, abet
      |=  fec/sole-effect
      ^+  +>+>
      (he-diff(poy `+>+<) fec)
    ::
    ++  dy-rash                                         ::  send effects, amok
      |=  fec/sole-effect
      ^+  +>+>
      (he-diff(poy ~) fec)
    ::
    ++  dy-init-command                                 ::  ++dojo-command
      |=  mad/dojo-command
      ^+  [mad +>]
      ?@  -.mad  [mad +>.$]
      =.  q.mad
        ?+(-.p.mad q.mad $http [0 %as %mime q.mad])
      =^  src  +>.$  (dy-init-source q.mad)
      [mad(q src) +>.$]
    ::
    ++  dy-init-source                                  ::  ++dojo-source
      |=  src/dojo-source
      ^+  [src +>]
      =^  bul  +>  (dy-init-build q.src)
      =:  p.src  num
          q.src  bul
        ==
      [src +>.$(num +(num), job (~(put by job) -.src +.src))]
    ::
    ++  dy-init-source-unit                             ::  (unit dojo-source)
      |=  urc/(unit dojo-source)
      ^+  [urc +>]
      ?~  urc  [~ +>]
      =^  src  +>  (dy-init-source u.urc)
      [`src +>.$]
    ::
    ++  dy-init-build                                   ::  ++dojo-build
      |=  bul/dojo-build
      ^+  [bul +>]
      ?-    -.bul
        $ex  [bul +>.$]
        $dv  [bul +>.$]
        $sa  [bul +>.$]
        $as  =^(mor +>.$ (dy-init-source q.bul) [bul(q mor) +>.$])
        $do  =^(mor +>.$ (dy-init-source q.bul) [bul(q mor) +>.$])
        $ge  =^(mod +>.$ (dy-init-model p.bul) [[%ge mod] +>.$])
        $ur  [bul +>.$]
        $tu  =^  dof  +>.$
                 |-  ^+  [p.bul +>.^$]
                 ?~  p.bul  [~ +>.^$]
                 =^  dis  +>.^$  (dy-init-source i.p.bul)
                 =^  mor  +>.^$  $(p.bul t.p.bul)
                 [[dis mor] +>.^$]
             [[%tu dof] +>.$]
      ==
    ::
    ++  dy-init-model                                   ::  ++dojo-model
      |=  mol/dojo-model
      ^+  [mol +>]
      =^  one  +>.$  (dy-init-server p.mol)
      =^  two  +>.$  (dy-init-config q.mol)
      [[one two] +>.$]
    ::
    ++  dy-init-server                                  ::  ++dojo-server
      |=  srv/dojo-server
      =.  p.srv  num
      [srv +>.$(num +(num), job (~(put by job) num [%dv [%gen q.srv]]))]
    ::
    ++  dy-init-config                                  ::  prepare config
      |=  cig/dojo-config
      ^+  [cig +>]
      =^  ord  +>.$  (dy-init-ordered p.cig)
      =^  key  +>.$  (dy-init-named q.cig)
      [[ord key] +>.$]
    ::
    ++  dy-init-ordered                                 ::  (list dojo-source)
      |=  ord/(list dojo-source)
      ^+  [ord +>]
      ?~  ord  [~ +>.$]
      =^  fir  +>.$  (dy-init-source i.ord)
      =^  mor  +>.$  $(ord t.ord)
      [[fir mor] +>.$]
    ::
    ++  dy-init-named                                   ::  (map @tas dojo-src)
      |=  key/(map term (unit dojo-source))
      ^+  [key +>.$]
      ?~  key  [~ +>.$]
      =^  top  +>.$  (dy-init-source-unit q.n.key)
      =^  lef  +>.$  $(key l.key)
      =^  rit  +>.$  $(key r.key)
      [[[p.n.key top] lef rit] +>.$]
    ::
    ++  dy-init                                         ::  full initialize
      ^+  .
      =^(dam . (dy-init-command mad) +(mad dam))
    ::
    ++  dy-hand                                         ::  complete step
      |=  cag/cage
      ^+  +>+>
      ?>  ?=(^ cud)
      (dy-step(cud ~, rez (~(put by rez) p.u.cud cag)) +(p.u.cud))
    ::
    ++  dy-meal                                         ::  vase to cage
      |=  vax/vase
      ?.  &(?=(@ -.q.vax) ((sane %tas) -.q.vax))
        ~&  %dy-meal-cage
        (dy-rash %bel ~)
      (dy-hand -.q.vax (slot 3 vax))
    ::
    ++  dy-made-edit                                    ::  sole edit
      |=  cag/cage
      ^+  +>+>
      ?>  ?=(^ per)
      ?:  ?|  ?=(^ q.q.cag)
              =((lent buf.say) q.q.cag)
              !&(?=($del -.u.per) =(+(p.u.per) (lent buf.say)))
          ==
        dy-abet(per ~)
      (dy-errd(per ~) per q.q.cag)
    ::
    ++  dy-done                                         ::  dialog submit
      |=  txt/tape
      ?:  |(?=(^ per) ?=(^ pux) ?=(~ pro))
        ~&  %dy-no-prompt
        (dy-diff %bel ~)
      (dy-slam /dial u.pro !>(txt))
    ::
    ++  dy-cast
      |*  {typ/_* bun/vase}
      |=  a/vase  ^-  typ
      ~|  [p.bun p.a]
      ?>  (~(nest ut p.bun) & p.a)
      ;;(typ q.a)
    ::
    ++  dy-over                                         ::  finish construction
      ^+  +>
      ::  XX needs filter
      ::
      :: ?:  ?=({$show $3} -.mad)
      ::  (dy-rash %tan (dy-show-source q.mad) ~)       ::  XX separate command
      ?:  ?=($brev -.mad)
        =.  var  (~(del by var) p.mad)
        =<  dy-amok
        ?+  p.mad  .
          $?($eny $now $our)  !!
          $lib  .(lib ~)
          $sur  .(sur ~)
          $dir  .(dir [[our.hid %home ud+0] /])
        ==
      =+  cay=(~(got by rez) p.q.mad)
      ?-    -.p.mad
          $verb
        =.  var  (~(put by var) p.p.mad cay)
        ~|  bad-set+[p.p.mad p.q.cay]
        =<  dy-amok
        ?+  p.p.mad  .
            $eny  ~|(%entropy-is-eternal !!)
            $now  ~|(%time-is-immutable !!)
            $our  ~|(%self-is-immutable !!)
            $lib
          %_    .
              lib
            ((dy-cast (list cable:ford) !>(*(list cable:ford))) q.cay)
          ==
        ::
            $sur
          %_    .
              sur
            ((dy-cast (list cable:ford) !>(*(list cable:ford))) q.cay)
          ==
        ::
            $dir  =+  ^=  pax  ^-  path
                      =+  pax=((dy-cast path !>(*path)) q.cay)
                      ?:  ?=(~ pax)  ~[(scot %p our.hid) %home '0']
                      ?:  ?=({@ ~} pax)  ~[i.pax %home '0']
                      ?:  ?=({@ @ ~} pax)  ~[i.pax i.t.pax '0']
                      pax
                  =.  dir  (need (de-beam:format pax))
                  =-  +>(..dy (he-diff %tan - ~))
                  rose+[" " `~]^~[leaf+"=%" (smyt (en-beam:format he-beak s.dir))]
        ==
      ::
          $poke
        %-  he-card(poy ~)
        :*  %deal
            /poke
            [our.hid p.p.p.mad]
            q.p.p.mad
            %poke
            cay
        ==
      ::
          $file
        %-  he-card(poy ~)  :*
          %info
          /file
          (foal:space:userlib (en-beam:format p.p.mad) cay)
        ==
      ::
          $flat
        ?^  q.q.cay
          (dy-rash %tan [%leaf "not an atom"]~)
        (dy-rash %sav p.p.mad q.q.cay)
      ::
          $pill
        (dy-rash %sag p.p.mad q.q.cay)
      ::
          $http
        ?>  ?=($mime p.cay)
        =+  mim=;;(mime q.q.cay)
        =+  maf=(~(add ja *math:eyre) %content-type (en-mite:mimes:html p.mim))
        (dy-eyre /show q.p.mad [r.p.mad p.p.mad maf ~ q.mim])
      ::
          $show
        |^  (prnt cay note)
        ++  prnt  ?:  (gte p.p.mad 4)
                    dy-xprint
                  dy-print
        ++  note  ^-  tang
                  ?-  p.p.mad
                    %0  ~
                    %1  [[%rose [~ "  " ~] (skol p.q.cay) ~] maar]
                    ::  XX  actually print something meaningful here
                    ::
                    %2  [[%rose [~ "  " ~] *tank ~] maar]
                    %3  ~
                    %4  ~
                    %5  [[%rose [~ "  " ~] (xskol p.q.cay) ~] maar]
                  ==
        ++  maar  ?:  =(%noun p.cay)  ~
                  [[%rose [~ "    " ~] >p.cay< ~] ~]
        --
      ==
    ::
    ++  dy-show  |=(cay/cage (dy-print cay ~))
    ::
    ::  Print a value (given as a cage) and a note (given as a tang).
    ::
    ++  dy-xprint
      |=  {cay/cage tan/tang}
      %+  dy-rash  %tan
      %-  welp  :_  tan
      ?+  p.cay  [(xsell q.cay)]~
        $tang  ;;(tang q.q.cay)
        $httr
          =+  hit=;;(httr:eyre q.q.cay)
          =-  (flop (turn `wall`- |=(a/tape leaf+(dash:us a '' ~))))
          :-  "HTTP {<p.hit>}"
          %+  weld
            (turn q.hit |=({a/@t b/@t} "{(trip a)}: {(trip b)}"))
          :-  i=""
          t=(turn `wain`?~(r.hit ~ (to-wain:format q.u.r.hit)) trip)
      ==
    ::
    ::  Print a value (given as a cage) and a note (given as a tang).
    ::
    ++  dy-print
      |=  {cay/cage tan/tang}
      %+  dy-rash  %tan
      %-  welp  :_  tan
      ?+  p.cay  [(sell q.cay)]~
        $tang  ;;(tang q.q.cay)
        $httr
          =+  hit=;;(httr:eyre q.q.cay)
          =-  (flop (turn `wall`- |=(a/tape leaf+(dash:us a '' ~))))
          :-  "HTTP {<p.hit>}"
          %+  weld
            (turn q.hit |=({a/@t b/@t} "{(trip a)}: {(trip b)}"))
          :-  i=""
          t=(turn `wain`?~(r.hit ~ (to-wain:format q.u.r.hit)) trip)
      ==
    ::
    ++  dy-edit                                         ::  handle edit
      |=  cal/sole-change
      ^+  +>+>
      =^  dat  say  (~(transceive sole say) cal)
      ?:  |(?=(^ per) ?=(^ pux) ?=(~ pro))
        ~&  %dy-edit-busy
        =^  lic  say  (~(transmit sole say) dat)
        (dy-diff %mor [%det lic] [%bel ~] ~)
      (dy-slam(per `dat) /edit u.pro !>((tufa buf.say)))
    ::
    ++  dy-type                                         ::  sole action
      |=  act/sole-action
      ?-  -.act
        $det  (dy-edit +.act)
        $ret  (dy-done (tufa buf.say))
        $clr  dy-stop
      ==
    ::
    ++  dy-cage       |=(num/@ud (~(got by rez) num))   ::  known cage
    ++  dy-vase       |=(num/@ud q:(dy-cage num))       ::  known vase
    ++  dy-silk-vase  |=(vax/vase [%$ %noun vax])       ::  vase to silk
    ++  dy-silk-sources                                 ::  arglist to silk
      |=  src/(list dojo-source)
      ^-  schematic:ford
      ::
      :+  %$  %noun
      |-
      ?~  src  !>(~)
      (slop (dy-vase p.i.src) $(src t.src))
    ::
    ++  dy-silk-config                                  ::  configure
      |=  {cay/cage cig/dojo-config}
      ^-  [wire schematic:ford]
      ?.  (~(nest ut [%cell [%atom %$ ~] %noun]) | p.q.cay)
        ::
        ::  naked gate
        ::
        ?.  &(?=({* ~} p.cig) ?=(~ q.cig))
          ~|(%one-argument !!)
        :-  /noun
        :+  %call  [%$ %noun q.cay]
        [%$ %noun (dy-vase p.i.p.cig)]
      ::
      ::  normal generator
      ::
      :-  ?+  -.q.q.cay  ~|(%bad-gen ~_((sell (slot 2 q.cay)) !!))
            $say  /gent
            $ask  /dial
            $get  /scar
          ==
      =+  gat=(slot 3 q.cay)
      :+  %call  [%$ %noun gat]
      :+  [%$ %noun !>([now=now.hid eny=eny.hid bec=he-beak])]
        (dy-silk-sources p.cig)
      :+  %mute  [%$ %noun (fall (slew 27 gat) !>(~))]
      ^-  (list [wing schematic:ford])
      %+  turn  ~(tap by q.cig)
      |=  {a/term b/(unit dojo-source)}
      ^-  [wing schematic:ford]
      :-  [a ~]
      :+  %$  %noun
      ?~(b !>([~ ~]) (dy-vase p.u.b))
    ::
    ++  dy-hoon-head                                    ::  dynamic state
      ::  todo: how do i separate the toplevel 'dojo state' comment?
      ::  dojo state
      ::
      ::  our: the name of this urbit
      ::  now: the current time
      ::  eny: a piece of random entropy
      ::
      ^-  cage
      :-  %noun
      =+  sloop=|=({a/vase b/vase} ?:(=(*vase a) b ?:(=(*vase b) a (slop a b))))
      %+  sloop
        %-  ~(rep by var)
        |=  {{a/term @ b/vase} c/vase}  ^-  vase
        (sloop b(p face+[a p.b]) c)
      !>([our=our now=now eny=eny]:hid)
    ::
    ++  dy-made-dial                                    ::  dialog product
      |=  cag/cage
      ^+  +>+>
      ?.  ?=(^ q.q.cag)
        (dy-errd ~ q.q.cag)
      =+  tan=((list tank) +2.q.q.cag)
      =.  +>+>.$  (he-diff %tan tan)
      =+  vax=(sped (slot 3 q.cag))
      ?+    -.q.vax  !!
          %&
        ?~  +.q.vax
          ~&  %dy-made-dial-abort
          (dy-rash %bel ~)
        (dy-meal (slot 7 vax))
      ::
          %|
        =<  he-pone
        %-  dy-diff(pro `(slap (slot 7 vax) [%limb %q]))
        =+  pom=(sole-prompt +<.q.vax)
        [%pro pom(cad [':' ' ' cad.pom])]
      ==
    ::
    ++  dy-made-scar                                    ::  scraper product
      |=  cag/cage
      ^+  +>+>
      ?.  ?=(^ q.q.cag)
        (dy-errd ~ q.q.cag)
      =+  tan=((list tank) +2.q.q.cag)
      =.  +>+>.$  (he-diff %tan tan)
      =+  vax=(sped (slot 3 q.cag))
      ~_  (sell q.cag)
      ?+    -.q.vax  !!
          %&
        ?~  +.q.vax
          ~&  %dy-made-scar-abort
          (dy-rash %bel ~)
        (dy-meal (slot 7 vax))
      ::
          %|
        =>  .(vax (slap vax !,(*hoon ?>(?=(%| -) .))))  :: XX working sped  #72
        =+  typ={%| (unit knot) hiss:eyre *}
        =+  [* usr hiz *]=((dy-cast typ !>($:typ)) vax)
        =.  ..dy  (he-diff %tan leaf+"< {(en-purl:html p.hiz)}" ~)
        (dy-eyre(pro `(slap (slot 15 vax) limb+%r)) /scar usr hiz)
      ==
    ::
    ++  dy-sigh-scar                                    ::  scraper result
      |=  dat/cage
      ?~  pro
        ~&  %dy-no-scraper
        (dy-show dat)
      (dy-slam(pux ~) /scar u.pro q.dat)
    ::
    ++  dy-made-gent                                    ::  generator product
      |=  cag/cage
      (dy-meal q.cag)
    ::
    ++  dy-made-noun                                    ::  generator product
      |=  cag/cage
      (dy-hand %noun q.cag)
    ::
    ++  dy-make                                         ::  build step
      ^+  +>
      ?>  ?=(^ cud)
      =+  bil=q.u.cud                 ::  XX =*
      ?:  ?=($ur -.bil)
        (dy-eyre /hand p.bil [q.bil %get ~ ~])
      %-  dy-ford
      ^-  [path schematic:ford]
      ?-  -.bil
        $ge  (dy-silk-config (dy-cage p.p.p.bil) q.p.bil)
        $dv  [/hand [%core [he-disc (weld /hoon (flop p.bil))]]]
        $ex  [/hand (dy-mare p.bil)]
        $sa  [/hand [%bunt he-disc p.bil]]
        $as  [/hand [%cast he-disc p.bil [%$ (dy-cage p.q.bil)]]]
        $do  [/hand [%call (dy-mare p.bil) [%$ (dy-cage p.q.bil)]]]
        $tu  :-  /hand
             :+  %$  %noun
             |-  ^-  vase
             ?~  p.bil  !!
             =+  hed=(dy-vase p.i.p.bil)
             ?~  t.p.bil  hed
             (slop hed $(p.bil t.p.bil))
      ==
    ::
    ++  dy-hoon-mark                                    ::  XX architect
      =+  ^=  ope
          |=  gen/hoon  ^-  hoon
          ?:  ?=(?($sgld $sgbn) -.gen)
            $(gen q.gen)
          =+  ~(open ap gen)
          ?.(=(gen -) $(gen -) gen)
      |=  gen/hoon  ^-  (unit mark)
      =.  gen  (ope gen)
      ?:  ?=({$cnts {@ ~} ~} gen)
        (bind (~(get by var) i.p.gen) head)
      ~
    ::
    ++  dy-mare                                         ::  build expression
      |=  gen/hoon
      ^-  schematic:ford
      =+  too=(dy-hoon-mark gen)
      =-  ?~(too - [%cast he-disc u.too -])
      :+  %ride  gen
      :-  [%$ dy-hoon-head]
      :^  %plan  he-rail  `coin`blob+**
      `scaffold:ford`[he-rail zuse sur lib ~ ~]
    ::
    ++  dy-step                                         ::  advance project
      |=  nex/@ud
      ^+  +>+>
      ?>  ?=(~ cud)
      ?:  =(nex num)
        dy-over
      dy-make(cud `[nex (~(got by job) nex)])
    --
  ::
  ++  he-dope
    |=  txt/tape                                        ::
    ^-  (each (unit (each dojo-command tape)) hair)     ::  prefix+result
    =+  len=+((lent txt))                               ::  line length
    =.  txt  (weld buf `tape`(weld txt "\0a"))          ::
    =+  vex=((full parse-command-line:he-parser) [1 1] txt)
    ?:  =(q.p.vex len)                                  ::  matched to line end
      [%& ~]                                            ::
    ?:  =(p.p.vex +((lent (skim txt |=(a/@ =(10 a)))))) ::  parsed all lines
      [%& ~ ?~(q.vex [%| txt] [%& p.u.q.vex])]          ::  new buffer+complete
    [%| p.p.vex (dec q.p.vex)]                          ::  syntax error
  ::
  ++  he-duke                                           ::  ++he-dope variant
    |=  txt/tape
    ^-  (each (unit (each dojo-command tape)) @ud)
    =+  foy=(he-dope txt)
    ?-  -.foy
      %|  [%| q.p.foy]
      %&  [%& p.foy]
    ==
  ::
  ++  he-abet                                           ::  resolve
    [(flop moz) %_(+> hoc (~(put by hoc) ost.hid +<+))]
  ::
  ++  he-abut                                           ::  discard
    =>  he-stop
    [(flop moz) %_(+> hoc (~(del by hoc) ost.hid))]
  ::
  ++  he-card                                           ::  emit gift
    |=  cad/card
    ^+  +>
    %_(+> moz [[ost.hid cad] moz])
  ::
  ++  he-send
    |=  {way/wire him/ship dap/term cop/clap}
    ^+  +>
    (he-card %send way [him dap] cop)
  ::
  ++  he-diff                                           ::  emit update
    |=  fec/sole-effect
    ^+  +>
    (he-card %diff %sole-effect fec)
  ::
  ++  he-stop                                           ::  abort work
    ^+  .
    ?~(poy . ~(dy-stop dy u.poy))
  ::
  ++  he-peer                                           ::  subscribe to
    |=(pax/path ?>(=(~ pax) he-prom))
  ::
  ++  he-pine                                           ::  restore prompt
    ^+  .
    ?^  poy  .
    he-prom:he-pone
  ::
  ++  he-errd                                           ::  reject update
    |=  {rev/(unit sole-edit) err/@u}  ^+  +>
    =+  red=(fall rev [%nop ~])       ::  required for error location sync
    =^  lic  say  (~(transmit sole say) red)
    (he-diff %mor [%det lic] [%err err] ~)
  ::
  ++  he-pone                                           ::  clear prompt
    ^+  .
    =^  cal  say  (~(transmit sole say) [%set ~])
    (he-diff %mor [%det cal] ~)
  ::
  ++  he-prow                                           ::  where we are
    ^-  tape
    ?:  &(=(our.hid p.dir) =(%home q.dir) =([%ud 0] r.dir) =(~ s.dir))  ~
    %+  weld
      ?:  &(=(our.hid p.dir) =([%ud 0] r.dir))
        (weld "/" (trip q.dir))
      ;:  weld
        "/"  ?:(=(our.hid p.dir) "=" (scow %p p.dir))
        "/"  ?:(=(%home q.dir) "=" (trip q.dir))
        "/"  ?:(=([%ud 0] r.dir) "=" (scow r.dir))
      ==
    ?:(=(~ s.dir) "" (spud (flop s.dir)))
  ::
  ++  he-prom                                           ::  send prompt
    %-  he-diff
    :-  %pro
    [& %$ (weld he-prow ?~(buf "> " "< "))]
  ::
  ++  he-made                                           ::  result from ford
    |=  $:  way=wire
            date=@da
            $=  result
            $%  [%complete build-result=build-result:ford]
                [%incomplete =tang]
        ==  ==
    ^+  +>
    ?>  ?=(^ poy)
    =<  he-pine
    ?-    -.result
        %incomplete
      (he-diff(poy ~) %tan tang.result)
    ::
        %complete
      ?-    -.build-result.result
          ::
          %success
        ::
        %.  (result-to-cage:ford build-result.result)
        =+  dye=~(. dy u.poy(pux ~))
        ?+  way  !!
          {$hand ~}  dy-hand:dye
          {$dial ~}  dy-made-dial:dye
          {$gent ~}  dy-made-gent:dye
          {$noun ~}  dy-made-noun:dye
          {$scar ~}  dy-made-scar:dye
          {$edit ~}  dy-made-edit:dye
        ==
      ::
          %error
        (he-diff(poy ~) %tan message.build-result.result)
    ==  ==
  ::
  ++  he-sigh                                           ::  result from eyre
    |=  {way/wire hit/httr:eyre}
    ^+  +>
    ?>  ?=(^ poy)
    =<  he-pine
    %.  [%httr !>(hit)]
    =+  dye=~(. dy u.poy(pux ~))
    ?+  way  !!
      {$hand ~}  dy-hand:dye
      {$show ~}  dy-show:dye
      {$scar ~}  dy-sigh-scar:dye
    ==
  ::
  ++  he-unto                                           ::  result from behn
    |=  {way/wire cit/cuft:gall}
    ^+  +>
    ?.  ?=($coup -.cit)
      ~&  [%strange-unto cit]
      +>
    ?~  p.cit
      (he-diff %txt ">=")
    (he-diff %tan u.p.cit)
  ::
  ++  he-lens
    |=  com/command:lens
    ^+  +>
    =+  ^-  source/dojo-source
        =|  num/@
        =-  ?.  ?=($send-api -.sink.com)  ::  XX  num is incorrect
              sor
            :-  0
            :+  %as  `mark`(cat 3 api.sink.com '-poke')
            :-  1
            :+  %do
              ^-  hoon
              :+  %brtr  [%base %noun]
              :^  %clls  [%rock %tas %post]
                [%rock %$ endpoint.sink.com]
              [%cnts [%& 6]~ ~]
            sor
        ^=  sor
        |-  ^-  dojo-source
        :-  num
        ?-    -.source.com
            $data        [%ex %sand %t data.source.com]
            $dojo
          %+  rash  command.source.com
          (ifix [(punt gap) (punt gap)] parse-build:he-parser)
        ::
            $clay
          :-  %ex
          ^-  hoon
          :+  %dtkt
            [%base %noun]
          :+  %clhp
            [%rock %tas %cx]
          %+  rash  pax.source.com
          rood:(vang | /(scot %p our.hid)/home/(scot %da now.hid))
        ::
            $url         [%ur `~. url.source.com]
            $api         !!
            $get-api
          :-  %ex
          ^-  hoon
          :+  %dtkt
            [%like ~[%json] ~]
          :*  %clsg
              [%rock %tas %gx]
              [%sand %ta (scot %p our.hid)]
              [%sand %tas api.source.com]
              [%sand %ta (scot %da now.hid)]
              (turn endpoint.source.com |=(a/@t [%sand %ta a]))
          ==
        ::
            $listen-api  !!
            $as
          :*  %as  mar.source.com
              $(num +(num), source.com next.source.com)
          ==
        ::
            $hoon
          :*  %do
              %+  rash  code.source.com
              tall:(vang | /(scot %p our.hid)/home/(scot %da now.hid))
              $(num +(num), source.com next.source.com)
          ==
        ::
            $tuple
          :-  %tu
          |-  ^-  (list dojo-source)
          ?~  next.source.com
            ~
          =.  num  +(num)
          :-  ^$(source.com i.next.source.com)
          $(next.source.com t.next.source.com)
        ==
    =+  |-  ^-  sink/dojo-sink
        ?-  -.sink.com
          $stdout       [%show %0]
          $output-file  $(sink.com [%command (cat 3 '@' pax.sink.com)])
          $output-pill  $(sink.com [%command (cat 3 '.' pax.sink.com)])
          $output-clay  [%file (need (de-beam:format pax.sink.com))]
          $url          [%http %post `~. url.sink.com]
          $to-api       !!
          $send-api     [%poke our.hid api.sink.com]
          $command      (rash command.sink.com parse-sink:he-parser)
          $app          [%poke our.hid app.sink.com]
        ==
    (he-plan sink source)
  ::
  ++  he-like                                           ::  accept line
    |=  buf/(list @c)
    =(%& -:(he-dope (tufa buf)))
  ::
  ++  he-stir                                           ::  apply change
    |=  cal/sole-change
    ^+  +>
    ::  ~&  [%his-clock ler.cal]
    ::  ~&  [%our-clock ven.say]
    =^  dat  say  (~(transceive sole say) cal)
    ?.  ?&  ?=($del -.dat)
            =(+(p.dat) (lent buf.say))
        ==
      +>.$
    =+  foy=(he-dope (tufa buf.say))
    ?:  ?=(%& -.foy)  +>.$
    ::  ~&  [%bad-change dat ted.cal]
    ::  ~&  [%our-leg leg.say]
    (he-errd `dat q.p.foy)
  ::
  ++  he-plan                                           ::  execute command
    |=  mad/dojo-command
    ^+  +>
    ?>  ?=(~ poy)
    he-pine:(dy-step:~(dy-init dy %*(. *dojo-project mad mad)) 0)
  ::
  ++  he-done                                           ::  parse command
    |=  txt/tape
    ^+  +>
    ?~  txt
      =<  he-prom(buf ~)
      %-  he-diff
      :~  %mor
          [%txt "> "]
          [%nex ~]
      ==
    =+  doy=(he-duke txt)
    ?-    -.doy
        %|  (he-errd ~ p.doy)
        %&
      ?~  p.doy
        (he-errd ~ (lent txt))
      =+  old=(weld ?~(buf "> " "  ") (tufa buf.say))
      =^  cal  say  (~(transmit sole say) [%set ~])
      =.  +>.$   (he-diff %mor txt+old nex+~ det+cal ~)
      ?-  -.u.p.doy
        %&  (he-plan(buf ~) p.u.p.doy)
        %|  he-prom(buf p.u.p.doy)
      ==
    ==
  ::
  ++  he-type                                           ::  apply input
    |=  act/sole-action
    ^+  +>
    ?^  poy
      he-pine:(~(dy-type dy u.poy) act)
    ?-  -.act
      $det  (he-stir +.act)
      $ret  (he-done (tufa buf.say))
      $clr  he-pine(buf "")
    ==
  ::
  ++  he-lame                                           ::  handle error
    |=  {wut/term why/tang}
    ^+  +>
    %-  (slog (flop `tang`[>%dojo-lame wut< why]))
    ?^  poy
      he-pine:~(dy-amok dy u.poy)
    he-pine                           ::  XX give mean to original keystroke
  --
::
++  prep
  |=  old/(unit house)
  ^+  [~ ..prep]
  ?~  old  `..prep
  `..prep(+<+ u.old)
::
::  pattern:  ++  foo  |=(data he-abet:(~(he-foo he (~(got by hoc) ost)) data))
++  arm  (arm-session ~ (~(got by hoc) ost.hid))
++  arm-session
  |=  {moz/(list move) ses/session}
  =>  ~(. he moz ses)
  =-  [wrap=- +]
  =+  he-arm=he-type
  |@  ++  $
        |:  +<.he-arm
        ^-  (quip move _..he)
        he-abet:(he-arm +<)
  --
::
++  peer-sole
  ~?  !=(our.hid src.hid)  [%dojo-peer-stranger ost.hid src.hid]
  ?>  (team:title our.hid src.hid)
  =^  moz  .
    ?.  (~(has by hoc) ost.hid)  [~ .]
    ~&  [%dojo-peer-replaced ost.hid]
    ~(he-abut he ~ (~(got by hoc) ost.hid))
  =+  ses=%*(. *session -.dir [our.hid %home ud+0])
  (wrap he-peer):(arm-session moz ses)
::
++  poke-sole-action
  |=  act/sole-action  ~|  poke+act  %.  act
  (wrap he-type):arm
::
++  poke-lens-command
  |=  com/command:lens  ~|  poke-lens+com  %.  com
  (wrap he-lens):arm
::
++  poke-json
  |=  jon=json
  ^-  [(list move) _+>.$]
  ~&  jon=jon
  [~ +>.$]
::  +poke-wipe: clear all dojo sessions
::
++  poke-wipe
  |=  *
  ^-  [(list move) _+>.$]
  ~&  %dojo-wipe
  =.  hoc
    %-  ~(run by hoc)
    |=  =session
    %_  session
      sur  ~
      lib  ~
      var  ~
      old  ~
    ==
  ::
  [~ +>.$]
::
++  made       (wrap he-made):arm
++  sigh-httr  (wrap he-sigh):arm
++  sigh-tang  |=({a/wire b/tang} ~|(`term`(cat 3 'sigh-' -.a) (mean b)))
++  lame       (wrap he-lame):arm
++  unto       (wrap he-unto):arm
++  pull
  |=  {pax/path}
  ^-  (quip move _+>)
  =^  moz  +>  ~(he-abut he ~ (~(got by hoc) ost.hid))
  [moz +>.$(hoc (~(del by hoc) ost.hid))]
--
