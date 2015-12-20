::                                                      ::  ::
::::  /hoon#dojo#app                                    ::  ::::
  ::                                                    ::    ::
/?  314                                                 ::  arvo kelvin
/-  sole                                                ::  console structures
/+  sole                                                ::  console library
[. sole]
::                                                      ::  ::
::::                                                    ::  ::::
  !:                                                    ::    ::
=>  |%                                                  ::  external structures
    ++  house                                           ::  all state
      $:  $4
          egg+@u                                        ::  command count
          hoc+(map bone session)                        ::  conversations
      ==                                                ::
    ++  session                                         ::  per conversation
      $:  say+sole-share                                ::  command-line state
          dir+beam                                      ::  active path
          poy+(unit dojo-project)                       ::  working
          {lib+(list hoof) arc+(list hoof)}             ::  lib#sur
          var+(map term cage)                           ::  variable state
          old+(set term)                                ::  used TLVs
          buf+tape                                      ::  multiline buffer
      ==                                                ::
    ++  dojo-command                                    ::
      $^  (pair dojo-sink dojo-source)                  ::  route value
      {$brev p+term}                                    ::  unbind variable
    ::
    ++  dojo-sink                                       ::
      $%  {$flat p+path}                                ::  atom to unix
          {$pill p+path}                                ::  noun to unix pill
          ::  {$tree p+path}                            ::  noun to unix tree
          {$file p+beam}                                ::  save to clay
          {$http p+?($post $put) q+purl}                ::  http outbound
          {$poke p+goal}                                ::  poke app
          {$show p+?($0 $1 $2 $3)}                      ::  print val#type#twig
          {$verb p+term}                                ::  store variable
      ==                                                ::
    ++  dojo-source                                     ::  construction node
      $:  p+@ud                                         ::  assembly index
          q+dojo-build                                  ::  general build
      ==                                                ::
    ++  dojo-build                                      ::  one ford step
      $%  {$ur p+purl}                                  ::  http GET request
          {$ge p+dojo-model}                            ::  generator
          {$dv p+path}                                  ::  core from source
          {$ex p+twig}                                  ::  hoon expression
          {$as p+mark q+dojo-source}                    ::  simple transmute
          {$do p+twig q+dojo-source}                    ::  gate apply
          {$tu p+(list dojo-source)}                    ::  tuple
      ==                                                ::
    ++  dojo-model                                      ::  data construction 
      $:  p+dojo-server                                 ::  core source
          q+dojo-config                                 ::  configuration
      ==                                                ::
    ++  dojo-server                                     ::  numbered device
      $:  p+@ud                                         ::  assembly index
          q+path                                        ::  gate path
      ==                                                ::
    ++  dojo-config                                     ::  configuration
      $:  p+(list dojo-source)                          ::  by order
          q+(map term (unit dojo-source))               ::  by keyword
      ==                                                ::
    ++  dojo-project                                    ::  construction state
      $:  mad+dojo-command                              ::  operation
          num+@ud                                       ::  number of tasks
          cud+(unit dojo-source)                        ::  now solving
          pux+(unit path)                               ::  ford working
          pro+(unit vase)                               ::  prompting loop
          per+(unit sole-edit)                          ::  pending reverse
          job+(map @ud dojo-build)                      ::  problems
          rez+(map @ud cage)                            ::  results
      ==                                                ::
    ++  bead  {p+(set beam) q+cage}                     ::  computed result
    ++  goal  {p+ship q+term}                           ::  flat application
    ++  clap                                            ::  action, user
      $%  {$peer p+path}                                ::  subscribe
          {$poke p+(cask)}                              ::  apply
          {$pull $~}                                    ::  unsubscribe
      ==                                                ::
    ++  club                                            ::  action, system
      $%  {$peer p+path}                                ::  subscribe
          {$poke p+cage}                                ::  apply
          {$pull $~}                                    ::  unsubscribe
      ==                                                ::
    ++  card                                            ::  general card
      $%  {$diff $sole-effect sole-effect}              ::
          {$send wire {ship term} clap}                 ::
          {$hiss wire mark {$hiss hiss}}                ::
          {$exec wire @p (unit {beak silk})}            ::
          {$deal wire sock term club}                   ::
          {$info wire @p toro}                          ::
      ==                                                ::
    ++  move  (pair bone card)                          ::  user-level move
    ++  sign                                            ::
      $%  {$made p+@uvH q+gage}                         ::
          {$unto p+cuft}                                ::  
      ==                                                ::
    --                                                  ::
!:                                                      ::
::::                                                    ::
  ::                                                    ::
|_  $:  hid+bowl                                        ::  system state
        house                                           ::  program state
    ==                                                  ::
++  he                                                  ::  per session
  |_  {moz+(list move) session}              ::
  ++  dp                                                ::  dojo parser
    |%  
    ++  dp-default-app  %hood
    ++  dp-message                                      ::  %poke
      |=  {gol+goal mod+dojo-model}  ^-  dojo-command
      [[%poke gol] [0 [%ge mod(q.p [q.gol q.p.mod])]]]
    ::
    ++  dp-command-line  ;~(sfix dp-command (just '\0a'))
    ++  dp-variable                                     ::  %verb or %brev
      |*  {sym+rule src+rule}
      %-  cook  :_  ;~(plug sym (punt src))
      |=  {a+term b+(unit dojo-source)}  ^-  dojo-command
      ?~(b [%brev a] [[%verb a] u.b])
    ::
    ++  dp-command                                      ::  ++dojo-command
      :: =<  ;~(less |-(;~(pose (jest '|*') ;~(pfix next (knee ** |.(^$))))) .)
      %+  knee  *dojo-command  |.  ~+
      ;~  pose  
        ;~  pfix  bar
          %+  cook  dp-message
          (stag [our.hid dp-default-app] dp-model)
        ==
      ::
        ;~  pfix  col
          %+  cook
            |=  {a+goal b+$^(dojo-model dojo-source)}
            ?@  -.b  [[%poke a] b]
            (dp-message a b)
          ;~  plug
            dp-goal
            ;~  pose
              ;~(pfix bar dp-model)
              ;~(pfix ace dp-source)
            ==
          ==
        ==
      ::
        ;~  pfix  tis
          ;~  pose
            (dp-variable (jest %dir) ;~(pfix ace :(stag 0 %ex dp-rood)))
            (dp-variable sym ;~(pfix ace dp-source))
          ==
        ==
      ::
        ;~  pfix  fas 
          ;~  pose
            (dp-variable (cold %arc hep) ;~(pfix gap dp-hooves))
            (dp-variable (cold %lib lus) ;~(pfix gap dp-hooves))
          ==
        ==
      ::
        ;~((glue ace) dp-sink dp-source)
        (stag [%show %0] dp-source)
      ==
    ++  dp-sink
      ;~  pose
        ;~(plug (cold %file tar) dp-beam)
        ;~(plug (cold %flat pat) (most fas qut))
        ;~(plug (cold %pill dot) (most fas sym))
        ;~(plug (cold %http lus) (easy %post) auri:epur)
        ;~(plug (cold %http hep) (easy %put) auri:epur)
        (stag %show (cook $?($1 $2 $3) (cook lent (stun [1 3] wut))))
      ==
    ++  dp-hooves                                       ::  hoof list
      :(stag 0 %ex %clsg (cook |=(a+tusk a) (most ;~(plug com gaw) dp-hoof)))
    ::
    ++  dp-hoof                                         ::  ++ford-hoof twig
      %+  cook  |*(a+* ~!(+.a `twig`a))
      ;~  plug
        :(stag %dtzy %tas sym)
        %-  dp-twig-punt
        ;~  (glue fas) 
          ;~(pfix fas (sear dp-case-twig nuck:so))
          (stag %dtzy ;~(plug (cold %p sig) fed:ag))
        ==
      ==
    ::
    ++  dp-twig-punt                                   ::  twig of unit
      |*(a+rule ;~(pose (stag [%cbzw %null] a) (easy [%cbzw %null])))
    ::
    ++  dp-case-twig
      |=  a+coin  ^-  (unit twig)
      ?.  ?=({$~ case} a)  ~
      %+  some
        [%dtzz %tas p.p.a]
      [%dtzy p.a]
    ::
    ++  dp-source  (stag 0 dp-build)                    ::  ++dojo-source
    ++  dp-build                                        ::  ++dojo-build
      %+  knee  *dojo-build  |.  ~+
      ;~  pose
        ;~(plug (cold %ur lus) auri:epur)
        ;~(plug (cold %ge lus) dp-model)
        ;~(plug (cold %as pam) sym ;~(pfix ace dp-source))
        ;~(plug (cold %do cab) dp-twig ;~(pfix ace dp-source))
        dp-value
      ==
    :: 
    ++  dp-goal                                          ::  ++goal
      %+  cook  |=(a+goal a)
      ;~  pose
        ;~  plug
          ;~(pfix sig fed:ag)
          ;~(pose ;~(pfix fas sym) (easy dp-default-app))
        ==
        %+  stag  our.hid
        ;~(pose sym (easy dp-default-app))
      ==
    ++  dp-beam                                         ::  ++beam
      %+  cook  |=(a+path =+((tome a) ?^(- u [he-beak (flop a)])))
      =+  vez=(vang | dp-path)
      (sear plex:vez (stag %clsg poor:vez))
    ::
    ++  dp-model   ;~(plug dp-server dp-config)         ::  ++dojo-model
    ++  dp-path    (tope he-beam)                       ::  ++path
    ++  dp-server  (stag 0 (most fas sym))              ::  ++dojo-server
    ++  dp-twig    tall:(vang | dp-path)                ::  ++twig
    ++  dp-rood                                         ::  'dir' twig
      =>  (vang | (tope dir))
      ;~  pose
        rood
      ::
        =-  ;~(pfix cen (stag %clsg -))                 ::  XX refactor ++scat
        %+  sear  |=({a+@ud b+tyke} (posh ~ ~ a b))
        ;~  pose
          porc
          (cook |=(a+(list) [(lent a) ~]) (star cen))
        ==
      ==
    ++  dp-value                                        ::  ++dojo-source
      ;~  pose
        (stag %ex dp-twig)
        (stag %tu (ifix [sel ser] (most ace dp-source)))
      ==
    ::
    ++  dp-config                                       ::  ++dojo-config
      ;~  plug
        (star ;~(pfix ace (stag 0 dp-value)))
        %+  cook
          ~(gas by *(map term (unit dojo-source)))
        %-  star
        ;~  plug 
          ;~(pfix com ace tis sym)
          (punt ;~(pfix ace (stag 0 dp-value)))
        ==
      ==
    --
  ::
  ++  dy                                                ::  project work
    |_  dojo-project                                    ::
    ++  dy-abet  +>(poy `+<)                            ::  resolve
    ++  dy-amok  +>(poy ~)                              ::  terminate
    ++  dy-ford                                         ::  send work to ford
      |=  {way+wire kas+silk}
      ^+  +>+>
      ?>  ?=($~ pux)
      (he-card(poy `+>+<(pux `way)) %exec way our.hid `[he-beak kas])
    ::
    ++  dy-eyre                                         ::  send work to eyre
      |=  {way+wire req+hiss}
      ^+  +>+>
      ?>  ?=($~ pux)
      (he-card(poy `+>+<(pux `way)) %hiss way %httr %hiss req)
    ::
    ++  dy-stop                                         ::  stop work
      ^+  +>
      =.  poy  ~
      ?~  pux  +>
      %.  [%txt "! cancel {<u.pux>}"]
      he-diff:(he-card %exec u.pux our.hid ~)
    ::
    ++  dy-slam                                         ::  call by ford
      |=  {way+wire gat+vase sam+vase}
      ^+  +>+>
      (dy-ford way %call [%$ %noun gat] [%$ %noun sam])
    ::
    ++  dy-diff                                         ::  send effects, abet
      |=  fec+sole-effect
      ^+  +>+>
      (he-diff(poy `+>+<) fec)
    ::
    ++  dy-rash                                         ::  send effects, amok
      |=  fec+sole-effect
      ^+  +>+>
      (he-diff(poy ~) fec)
    ::
    ++  dy-init-command                                 ::  ++dojo-command
      |=  mad+dojo-command
      ^+  [mad +>]
      ?@  -.mad  [mad +>.$]
      =.  q.mad
        ?+(-.p.mad q.mad $http [0 %as %mime q.mad])
      =^  src  +>.$  (dy-init-source q.mad)
      [mad(q src) +>.$]
    ::
    ++  dy-init-source                                  ::  ++dojo-source
      |=  src+dojo-source
      ^+  [src +>]
      =^  bul  +>  (dy-init-build q.src)
      =:  p.src  num
          q.src  bul
        ==
      [src +>.$(num +(num), job (~(put by job) src))]
    ::
    ++  dy-init-source-unit                             ::  (unit dojo-source)
      |=  urc+(unit dojo-source)
      ^+  [urc +>]
      ?~  urc  [~ +>]
      =^  src  +>  (dy-init-source u.urc)
      [`src +>.$]
    ::
    ++  dy-init-build                                   ::  ++dojo-build
      |=  bul+dojo-build
      ^+  [bul +>]
      ?-    -.bul
        $ex  [bul +>.$]
        $dv  [bul +>.$]
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
      |=  mol+dojo-model
      ^+  [mol +>]
      =^  one  +>.$  (dy-init-server p.mol)
      =^  two  +>.$  (dy-init-config q.mol)
      [[one two] +>.$]
    ::
    ++  dy-init-server                                  ::  ++dojo-server
      |=  srv+dojo-server
      =.  p.srv  num
      [srv +>.$(num +(num), job (~(put by job) num [%dv [%gen q.srv]]))]
    ::
    ++  dy-init-config                                  ::  prepare config
      |=  cig+dojo-config
      ^+  [cig +>]
      =^  ord  +>.$  (dy-init-ordered p.cig)
      =^  key  +>.$  (dy-init-named q.cig)
      [[ord key] +>.$]
    ::
    ++  dy-init-ordered                                 ::  (list dojo-source)
      |=  ord+(list dojo-source)
      ^+  [ord +>]
      ?~  ord  [~ +>.$]
      =^  fir  +>.$  (dy-init-source i.ord)
      =^  mor  +>.$  $(ord t.ord)
      [[fir mor] +>.$]
    ::
    ++  dy-init-named                                   ::  (map @tas dojo-src)
      |=  key+(map term (unit dojo-source))
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
      |=  cag+cage
      ^+  +>+>
      ?>  ?=(^ cud)
      (dy-step(cud ~, rez (~(put by rez) p.u.cud cag)) +(p.u.cud))
    ::
    ++  dy-meal                                         ::  vase to cage
      |=  vax+vase
      ?.  &(?=(@ -.q.vax) ((sane %tas) -.q.vax))
        ~&  %dy-meal-cage
        (dy-rash %bel ~)
      (dy-hand -.q.vax (slot 3 vax))
    ::
    ++  dy-made-edit                                    ::  sole edit
      |=  cag+cage
      ^+  +>+>
      ?>  ?=(^ per)
      ?:  ?|  ?=(^ q.q.cag) 
              =((lent buf.say) q.q.cag)
              !&(?=($del -.u.per) =(+(p.u.per) (lent buf.say)))
          ==
        dy-abet(per ~)
      =^  lic  say  (~(transmit sole say) u.per)
      (dy-diff(per ~) %mor [%det lic] [%err q.q.cag] ~)
    ::
    ++  dy-done                                         ::  dialog submit
      |=  txt+tape
      ?:  |(?=(^ per) ?=(^ pux) ?=($~ pro))
        ~&  %dy-no-prompt
        (dy-diff %bel ~)
      (dy-slam /dial u.pro !>(txt))
    ::
    ++  dy-cast
      |*  {typ+_* bun+vase}
      |=  a+vase  ^-  typ
      ?>  (~(nest ut p.bun) & p.a)
      ;;(typ q.a)
    ::
    ++  dy-over                                         ::  finish construction
      ^+  +>
      ?:  ?=({$show $3} -.mad)
        (dy-rash %tan (dy-show-source q.mad) ~)        ::  XX separate command
      ?:  ?=($brev -.mad)
        =.  var  (~(del by var) p.mad)
        =<  dy-amok
        ?+  p.mad  . 
          $?($eny $now $our)  !!
          $lib  .(lib ~)
          $arc  .(arc ~)
          $dir  .(dir [[our.hid %home ud#0] /])
        ==
      =+  cay=(~(got by rez) p.q.mad)
      ?-    -.p.mad
          $verb
        =.  var  (~(put by var) p.p.mad cay)
        ~|  bad-set#[p.p.mad p.q.cay]
        =<  dy-amok
        ?+  p.p.mad  .
          $eny  ~|(%entropy-is-eternal !!)
          $now  ~|(%time-is-immutable !!)
          $our  ~|(%self-is-immutable !!)
          $lib  .(lib ((dy-cast (list hoof) !>(*(list hoof))) q.cay))
          $arc  .(arc ((dy-cast (list hoof) !>(*(list hoof))) q.cay))
          $dir  =+  ^=  pax  ^-  path
                    =+  pax=((dy-cast path !>(*path)) q.cay)
                    ?:  ?=($~ pax)  ~[(scot %p our.hid) %home '0']
                    ?:  ?=({@ $~} pax)  ~[i.pax %home '0']
                    ?:  ?=({@ @ $~} pax)  ~[i.pax i.t.pax '0']
                    pax
                =.  dir  (need (tome pax))
                =-  +>(..dy (he-diff %tan - ~))
                rose#[" " `~]^~[leaf#"=%" (smyt (tope he-beak s.dir))]
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
          our.hid
          (foal (tope p.p.mad) cay)
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
        =+  maf=(~(add ja *math) content-type#(moon p.mim))
        (dy-eyre /show [q.p.mad p.p.mad maf ~ q.mim])
      ::
          $show
        %+  dy-print  cay
        ?-  p.p.mad
          $0  ~
          $1  [%rose [~ "  " ~] (skol p.q.cay) ~]~ 
          $2  [%rose [~ "  " ~] (dy-show-type-noun p.q.cay) ~]~
        ==
      ==
    ::
    ++  dy-show  |=(cay+cage (dy-print cay ~))
    ++  dy-print
      |=  {cay+cage tan+tang}
      %+  dy-rash  %tan
      %-  welp  :_  tan
      ?+  p.cay  [(sell q.cay)]~
        $tang  ;;(tang q.q.cay)
        $httr
          =+  hit=;;(httr q.q.cay)
          =-  (flop (turn `wall`- |=(a+tape leaf#(dash:ut a ''))))
          :-  "HTTP {<p.hit>}"
          %+  weld
            (turn q.hit |=({a+@t b+@t} "{(trip a)}: {(trip b)}"))
          (turn `wain`?~(r.hit ~ (lore q.u.r.hit)) trip)
      ==
    ::
    ++  dy-show-type-noun
      |=  a+type  ^-  tank
      =-  >[-]<
      |-  ^-  $?  $%  {$atom @tas}
                      {$cell _$ _$}
                      {$cube * _$}
                      {$face @tas _$}
                      {$fork _$ _$}
                      {$hold (list {_$ twig})}
                  ==
                  wain                :: "<|core|>"
                  $?($noun $void)
              ==
      ?+  a  a
        {$?($cube $face) ^}  a(q $(a q.a))
        {$?($cell $fork) ^}  a(p $(a p.a), q $(a q.a))
        {$bull ^}  $(a q.a)
        {$core ^}  `wain`/core
        {$hold *}  a(p (turn p.a |=({b+type c+twig} [^$(a b) c])))
      ==
    ::
    ++  dy-show-source
      |=  a+dojo-source  ^-  tank
      =-  >[-]<
      =+  `{@ bil+dojo-build}`a
      |-  ^-  $^  [_$ _$]
              $?  twig
                  $%  {$ur purl}
                      {$dv path}
                      {$as mark _$}
                      {$do twig _$}
                      {$ge path (list _$) (map term (unit _$))}
                  ==
              ==
      ?-  -.bil
        $?($ur $dv)  bil
        $ex  ?.  ?=({$cltr *} p.bil)  p.bil
                 |-  ^-  twig
                 ?~  p.p.bil  !!
                 ?~  t.p.p.bil  i.p.p.bil
                 [i.p.p.bil $(p.p.bil t.p.p.bil)]
        $tu  ?~  p.bil  !!
             |-
             ?~  t.p.bil  ^$(bil q.i.p.bil)
             [^$(bil q.i.p.bil) $(p.bil t.p.bil)]
        $as  bil(q $(bil q.q.bil))
        $do  bil(q $(bil q.q.bil))
        $ge  :+  %ge  q.p.p.bil
             [(turn p.q.p.bil ..$) (~(run by q.q.p.bil) (lift ..$))]
      ==
    ::
    ++  dy-edit                                         ::  handle edit
      |=  cal+sole-change
      ^+  +>+>
      =^  dat  say  (~(transceive sole say) cal)
      ?:  |(?=(^ per) ?=(^ pux) ?=($~ pro))
        ~&  %dy-edit-busy
        =^  lic  say  (~(transmit sole say) dat)
        (dy-diff %mor [%det lic] [%bel ~] ~)
      (dy-slam(per `dat) /edit u.pro !>((tufa buf.say)))
    ::
    ++  dy-type                                         ::  sole action
      |=  act+sole-action
      ?-  -.act
        $det  (dy-edit +.act)
        $ret  (dy-done (tufa buf.say))
        $clr  dy-stop
      ==
    ::
    ++  dy-cage       |=(num+@ud (~(got by rez) num))   ::  known cage
    ++  dy-vase       |=(num+@ud q:(dy-cage num))       ::  known vase
    ++  dy-silk-vase  |=(vax+vase [%$ %noun vax])       ::  vase to silk
    ++  dy-silk-sources                                 ::  sources to silk
      |=  src+(list dojo-source)  ^-  silk
      %-  dy-silk-vase
      |-
      ?~  src  !>(~)
      (slop (dy-vase p.i.src) $(src t.src))
    ::
    ++  dy-silk-config                                  ::  configure
      |=  {cay+cage cig+dojo-config}
      ^-  {wire silk}
      :-  ?+  -.q.q.cay  ~|(%bad-gen ~_((sell (slot 2 q.cay)) !!))
            $say  /gent
            $ask  /dial
            $get  /scar
          ==
      =+  gat=(slot 3 q.cay)
      :+  %call  (dy-silk-vase gat)
      :+  (dy-silk-vase !>([now=now.hid eny=eny.hid bec=he-beak]))
        (dy-silk-sources p.cig)
      :+  %mute  (dy-silk-vase (fall (slew 27 gat) !>(~)))
      %+  turn  (~(tap by q.cig))
      |=  {a+term b+(unit dojo-source)}
      ^-  (pair wing silk)
      :-  [a ~]
      %-  dy-silk-vase
      ?~(b !>([~ ~]) (dy-vase p.u.b))
    ::
    ++  dy-twig-head                                    ::  dynamic state
      ^-  cage
      :-  %noun
      =+  sloop=|=({a+vase b+vase} ?:(=(*vase a) b ?:(=(*vase b) a (slop a b))))
      %+  sloop
        %-  ~(rep by var)
        |=  {{a+term @ b+vase} c+vase}  ^-  vase
        (sloop b(p face#[a p.b]) c)
      !>([our=our now=now eny=eny]:hid)
    ::
    ++  dy-made-dial                                    ::  dialog product
      |=  cag+cage
      ^+  +>+>
      ?.  ?=(^ q.q.cag)
        (dy-diff %err q.q.cag)
      =+  tan=((list tank) +2.q.q.cag)
      =.  +>+>.$  (he-diff %tan tan)
      =+  vax=(spec (slot 3 q.cag))
      ?+    -.q.vax  !!
          $&
        ?~  +.q.vax
          ~&  %dy-made-dial-abort
          (dy-rash %bel ~)
        (dy-meal (slot 7 vax))
      ::
          $|
        =<  he-pone
        %-  dy-diff(pro `(slap (slot 7 vax) [%cnzy %q]))
        =+  pom=(sole-prompt +<.q.vax)
        [%pro pom(cad [':' ' ' cad.pom])]
      ==
    ::
    ++  dy-made-scar                                    ::  scraper product
      |=  cag+cage
      ^+  +>+>
      ?.  ?=(^ q.q.cag)
        (dy-diff %err q.q.cag)
      =+  tan=((list tank) +2.q.q.cag)
      =.  +>+>.$  (he-diff %tan tan)
      =+  vax=(spec (slot 3 q.cag))
      ~_  (sell q.cag)
      ?+    -.q.vax  !!
          $&
        ?~  +.q.vax
          ~&  %dy-made-scar-abort
          (dy-rash %bel ~)
        (dy-meal (slot 7 vax))
      ::
          $|
        =+  hiz=;;(hiss +<.q.vax)
        =.  ..dy  (he-diff %tan leaf#"< {(earn p.hiz)}" ~)
        (dy-eyre(pro `(slap (slot 7 vax) cnzy#%q)) /scar hiz)
      ==
    ::
    ++  dy-sigh-scar                                    ::  scraper result
      |=  dat+cage
      ?~  pro
        ~&  %dy-no-scraper
        (dy-show dat)
      (dy-slam(pux ~) /scar u.pro q.dat) 
    ::
    ++  dy-made-gent                                    ::  generator product
      |=  cag+cage
      (dy-meal q.cag)
    ::
    ++  dy-make                                         ::  build step
      ^+  +>
      ?>  ?=(^ cud)
      =+  bil=q.u.cud                 ::  XX =*
      ?:  ?=($ur -.bil)
        (dy-eyre /hand [p.bil %get ~ ~])
      %-  dy-ford
      ^-  (pair path silk)
      ?-  -.bil
        $ge  (dy-silk-config (dy-cage p.p.p.bil) q.p.bil)
        $dv  [/hand [%core he-beak (flop p.bil)]]
        $ex  [/hand (dy-mare p.bil)]
        $as  [/hand [%cast p.bil [%$ (dy-cage p.q.bil)]]]
        $do  [/hand [%call (dy-mare p.bil) [%$ (dy-cage p.q.bil)]]]
        $tu  :-  /hand
             :-  %$
             :-  %noun
             |-  ^-  vase
             ?~  p.bil  !!
             =+  hed=(dy-vase p.i.p.bil)
             ?~  t.p.bil  hed
             (slop hed $(p.bil t.p.bil))
      ==
    ::
    ++  dy-twig-mark                                    ::  XX architect
      =+  ^=  ope
          |=  gen+twig  ^-  twig
          ?:  ?=(?($sggl $sggr) -.gen)
            $(gen q.gen)
          =+  ~(open ap gen)
          ?.(=(gen -) $(gen -) gen)
      |=  gen+twig  ^-  (unit mark)
      =.  gen  (ope gen)
      ?:  ?=({$cnts {@ $~} $~} gen)
        (bind (~(get by var) i.p.gen) head)
      ?.  ?=($dtkt -.gen)  ~
      =.  p.gen  (ope p.gen)
      ?@  -.p.gen  ~
      ?+    ~(feck ap p.p.gen)  ~
          {$~ $cx}
        =+  gin=(ope q.p.gen)
        |-
        ?@  -.gin  ~
        =.  q.gin  (ope q.gin)
        ?^  -.q.gin  $(gin q.gin)
        =.  p.gin  (ope p.gin)
        ?.  ?=({{?($dtzz $dtzy) @ @} {$dtzz $n $~}} gin)
          ~
        (some q.p.gin)
      ==
    ::
    ++  dy-mare                                         ::  build expression
      |=  gen+twig
      ^-  silk
      =+  too=(dy-twig-mark gen)
      =-  ?~(too - [%cast u.too -])
      :+  %ride  gen
      :-  [%$ dy-twig-head]
      [%plan he-beam / zuse arc lib ~ ~]
    ::
    ++  dy-step                                         ::  advance project
      |=  nex+@ud
      ^+  +>+>
      ?>  ?=($~ cud)
      ?:  ?=({$show $3} -.mad)
        he-easter:dy-over
      ?:  =(nex num)
        he-easter:dy-over
      dy-make(cud `[nex (~(got by job) nex)])
    --
  ::
  ++  he-dope                                           ::  sole user of ++dp
    |=  txt+tape                                        ::
    ^-  (each (unit (each dojo-command tape)) hair)     ::  prefix#result
    =+  len=+((lent txt))                               ::  line length
    =.  txt  :(weld buf txt "\0a")                      ::
    =+  vex=((full dp-command-line):dp [1 1] txt)       ::
    ?:  =(q.p.vex len)                                  ::  matched to line end
      [%& ~]                                            ::
    ?:  =(p.p.vex +((lent (skim txt |=(a+@ =(10 a)))))) ::  parsed all lines
      [%& ~ ?~(q.vex [%| txt] `p.u.q.vex)]              ::  new buffer#complete
    [%| p.p.vex (dec q.p.vex)]                          ::  syntax error
  ::
  ++  he-duke                                           ::  ++he-dope variant
    |=  txt+tape
    ^-  (each (unit (each dojo-command tape)) @ud)
    =+  foy=(he-dope txt)
    ?-  -.foy
      $|  [%| q.p.foy]
      $&  [%& p.foy]
    ==
  ::
  ++  he-easter                                         ::  hint messages
    ^+  .
    =.  egg  +(egg)
    =-  ?~(msg ..he-diff (he-diff %tan leaf#u.msg ~))
    ^-  msg+(unit tape)
    ?+  (clan our.hid)  ~
      $pawn  ?+  egg  ~
                $5  `":: To request a planet, run  |ask 'your@email.co'"
    ==       ==  
  ::
  ++  he-abet                                           ::  resolve
    [(flop moz) %_(+> hoc (~(put by hoc) ost.hid +<+))]
  ::
  ++  he-abut                                           ::  discard
    =>  he-stop
    [(flop moz) %_(+> hoc (~(del by hoc) ost.hid))]
  ::
  ++  he-beak  -:he-beam
  ++  he-beam                                           ::  logical beam
    ^-  beam
    ?.  =(ud#0 r.dir)  dir
    dir(r [%da now.hid])
  ::
  ++  he-card                                           ::  emit gift
    |=  cad+card
    ^+  +>
    %_(+> moz [[ost.hid cad] moz])
  ::
  ++  he-send
    |=  {way+wire him+ship dap+term cop+clap}
    ^+  +>
    (he-card %send way [him dap] cop)
  ::
  ++  he-diff                                           ::  emit update
    |=  fec+sole-effect
    ^+  +>
    (he-card %diff %sole-effect fec)
  ::
  ++  he-stop                                           ::  abort work
    ^+  .
    ?~(poy . ~(dy-stop dy u.poy))
  ::
  ++  he-peer                                           ::  subscribe to
    |=(pax+path ?>(=(~ pax) he-prom))
  ::
  ++  he-pine                                           ::  restore prompt
    ^+  .
    ?^  poy  .
    he-prom:he-pone
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
    ?:(=(~ s.dir) "" (spud s.dir))
  ::
  ++  he-prom                                           ::  send prompt
    %-  he-diff
    :-  %pro
    [& %$ (weld he-prow ?~(buf "> " "< "))]
  ::
  ++  he-made                                           ::  result from ford
    |=  {way+wire dep+@uvH reg+gage}
    ^+  +>
    ?>  ?=(^ poy)
    =<  he-pine
    ?-  -.reg
      $&  %.  p.reg
          =+  dye=~(. dy u.poy(pux ~))
          ?+  way  !!
            {$hand $~}  dy-hand:dye
            {$dial $~}  dy-made-dial:dye
            {$gent $~}  dy-made-gent:dye
            {$scar $~}  dy-made-scar:dye
            {$edit $~}  dy-made-edit:dye
          ==
      $|  (he-diff(poy ~) %tan p.reg)
      $tabl  !!
    ==
  ::
  ++  he-sigh                                           ::  result from eyre
    |=  {way+wire hit+httr}
    ^+  +>
    ?>  ?=(^ poy)
    =<  he-pine
    %.  [%httr !>(hit)]
    =+  dye=~(. dy u.poy(pux ~))
    ?+  way  !!
      {$hand $~}  dy-hand:dye
      {$show $~}  dy-show:dye
      {$scar $~}  dy-sigh-scar:dye
    ==
  ::
  ++  he-unto                                           ::  result from behn
    |=  {way+wire cit+cuft}
    ^+  +>
    ?.  ?=($coup -.cit)
      ~&  [%strange-unto cit]
      +>
    ?~  p.cit  
      (he-diff %txt ">=")
    (he-diff %tan u.p.cit)
  ::
  ++  he-like                                           ::  accept line
    |=  buf+(list @c)
    =(%& -:(he-dope (tufa buf)))
  ::
  ++  he-stir                                           ::  apply change
    |=  cal+sole-change
    ^+  +>
    ::  ~&  [%his-clock ler.cal]
    ::  ~&  [%our-clock ven.say]
    =^  dat  say  (~(transceive sole say) cal)
    ?.  ?&  ?=($del -.dat)
            =(+(p.dat) (lent buf.say))
        ==
      +>.$
    =+  foy=(he-dope (tufa buf.say))
    ?:  ?=($& -.foy)  +>.$
    ::  ~&  [%bad-change dat ted.cal]
    =^  lic  say  (~(transmit sole say) dat)
    ::  ~&  [%our-leg leg.say]
    (he-diff %mor [%det lic] [%err q.p.foy] ~)
  ::
  ++  he-plan                                           ::  execute command
    |=  mad+dojo-command
    ^+  +>
    ?>  ?=($~ poy)
    he-pine:(dy-step:~(dy-init dy %*(. *dojo-project mad mad)) 0)
  ::
  ++  he-done                                           ::  parse command
    |=  txt+tape
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
        $|  (he-diff [%err p.doy])
        $&
      ?~  p.doy
        (he-diff [%err (lent txt)])
      =+  old=(weld ?~(buf "> " "  ") (tufa buf.say))
      =^  cal  say  (~(transmit sole say) [%set ~])
      =.  +>.$   (he-diff %mor txt#old nex#~ det#cal ~)
      ?-  -.u.p.doy
        $&  (he-plan(buf ~) p.u.p.doy)
        $|  he-prom(buf p.u.p.doy)
      ==
    ==
  ::
  ++  he-type                                           ::  apply input
    |=  act+sole-action
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
    |=  {wut+term why+tang}
    ^+  +>
    %-  (slog (flop `tang`[>%dojo-lame wut< why]))
    ?^  poy
      he-pine:~(dy-amok dy u.poy)
    he-pine                           ::  XX give mean to original keystroke
  --
::
++  prep
  =+  session-1==+(*session _-(poy *(unit)))
  =+  session-0==+(*session-1 _[_say syd=desk * _|2.-])
  :: ,_`..prep
  =+  ^=  hoze
      $%  {$0 p+(map bone session-0)}
          {$1 p+(map bone session-1)}
          {$2 p+(map bone session-1)}
          {$3 p+(map bone session)}
      ==
  |=  old+(unit ?(house hoze))  ^+  [~ ..prep]
  ?~  old  `..prep
  ?+  -.u.old  !!
    $4  `..prep(+<+ u.old)
  ==
::
::  pattern:  ++  foo  |=(data he-abet:(~(he-foo he (~(got by hoc) ost)) data))
++  arm  (arm-session ~ (~(got by hoc) ost.hid))
++  arm-session
  |=  {moz+(list move) ses+session}
  =>  ~(. he moz ses)
  =-  [wrap=- +]
  |*  he-arm+_he-type
  |=  _+<.he-arm  
  ^-  (quip move ..he)
  he-abet:(he-arm +<)
::
++  peer-sole
  ~?  !=(src.hid our.hid)  [%dojo-peer-stranger ost.hid src.hid]
  ::  ?>  =(src.hid our.hid)
  =^  moz  .
    ?.  (~(has by hoc) ost.hid)  [~ .]
    ~&  [%dojo-peer-replaced ost.hid]
    ~(he-abut he ~ (~(got by hoc) ost.hid))
  =+  ses=%*(. *session -.dir [our.hid %home ud#0])
  (wrap he-peer):(arm-session moz ses)
::
++  poke-sole-action
  |=  act+sole-action  ~|  poke#act  %.  act
  (wrap he-type):arm
++  made       (wrap he-made):arm
++  sigh-httr  (wrap he-sigh):arm
++  sigh-tang  |=({a+wire b+tang} ~|(`term`(cat 3 'sigh-' -.a) (mean b)))
++  lame       (wrap he-lame):arm
++  unto       (wrap he-unto):arm
++  pull
  |=  {pax+path}
  ^-  (quip move +>)
  =^  moz  +>  ~(he-abut he ~ (~(got by hoc) ost.hid))
  [moz +>.$(hoc (~(del by hoc) ost.hid))]
--
