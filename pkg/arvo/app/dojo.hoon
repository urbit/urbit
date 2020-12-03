::                                                      ::  ::
::::  /hoon/dojo/app                                    ::  ::::
  ::                                                    ::    ::
/?  309                                                 ::  arvo kelvin
/-  *sole, lens                                         ::  console structures
/+  sole, pprint,                                       ::
    auto=language-server-complete,                      ::
    easy-print=language-server-easy-print               ::
::                                                      ::  ::
::::                                                    ::  ::::
  ::                                                    ::    ::
=>  |%                                                  ::  external structures
    +$  id  @tasession                                  ::  session id
    +$  house                                           ::  all state
      $:  %6
          egg=@u                                        ::  command count
          hoc=(map id session)                          ::  conversations
          acl=(set ship)                                ::  remote access whitelist
      ==                                                ::
    +$  session                                         ::  per conversation
      $:  say=sole-share                                ::  command-line state
          dir=beam                                      ::  active path
          poy=(unit dojo-project)                       ::  working
          $:  ::  sur: structure imports
              ::
              sur=(list cable:clay)
              ::  lib: library imports
              ::
              lib=(list cable:clay)
          ==
          var=(map term cage)                           ::  variable state
          old=(set term)                                ::  used TLVs
          buf=tape                                      ::  multiline buffer
      ==                                                ::
    +$  monkey                                          ::  per conversation
      $:  say=sole-share                                ::  command-line state
          dir=beam                                      ::  active path
          poy=(unit dojo-project)                       ::  working
          var=(map term cage)                           ::  variable state
          old=(set term)                                ::  used TLVs
          buf=tape                                      ::  multiline buffer
      ==                                                ::
    +$  dojo-command                                    ::
      $^  (pair dojo-sink dojo-source)                  ::  route value
      [%brev p=term]                                    ::  unbind variable
    ::
    +$  dojo-sink                                       ::
      $%  [%flat p=path]                                ::  atom to unix
          [%pill p=path]                                ::  noun to unix pill
          ::  [%tree p=path]                            ::  noun to unix tree
          [%file p=beam]                                ::  save to clay
          $:  %http                                     ::  http outbound
              p=?(%post %put)
              r=@t
          ==
          [%poke p=goal]                                ::  poke app
          [%show p=?(%0 %1 %2 %3 %4 %5)]                ::  val/type/hoon/xray
          [%verb p=term]                                ::  store variable
      ==                                                ::
    +$  dojo-source                                     ::  construction node
      $:  p=@ud                                         ::  assembly index
          q=dojo-build                                  ::  general build
      ==                                                ::
    +$  dojo-build                                      ::  one arvo step
      $~  [%ex *hoon]
      $%  [%ur p=@t]                                    ::  http GET request
          [%ge p=dojo-model]                            ::  generator
          [%te p=term q=(list dojo-source)]             ::  thread
          [%dv p=path]                                  ::  core from source
          [%ex p=hoon]                                  ::  hoon expression
          [%sa p=mark]                                  ::  example mark value
          [%as p=mark q=dojo-source]                    ::  simple transmute
          [%do p=hoon q=dojo-source]                    ::  gate apply
          [%tu p=(list dojo-source)]                    ::  tuple
      ==                                                ::
    +$  dojo-model                                      ::  data construction
      $:  p=dojo-server                                 ::  core source
          q=dojo-config                                 ::  configuration
      ==                                                ::
    +$  dojo-server                                     ::  numbered device
      $:  p=@ud                                         ::  assembly index
          q=path                                        ::  gate path
      ==                                                ::
    +$  dojo-config                                     ::  configuration
      $:  p=(list dojo-source)                          ::  by order
          q=(map term (unit dojo-source))               ::  by keyword
      ==                                                ::
    +$  dojo-project                                    ::  construction state
      $:  mad=dojo-command                              ::  operation
          num=@ud                                       ::  number of tasks
          cud=(unit dojo-source)                        ::  now solving
          pux=(unit path)                               ::  working
          pro=(unit vase)                               ::  prompting loop
          per=(unit sole-edit)                          ::  pending reverse
          job=(map @ud dojo-build)                      ::  problems
          rez=(map @ud cage)                            ::  results
      ==                                                ::
    +$  bead  [p=(set beam) q=cage]                     ::  computed result
    +$  goal  [p=ship q=term]                           ::  flat application
    --
=>
|%
::  TODO: move to zuse? copied from clay
::
++  with-face  |=([face=@tas =vase] vase(p [%face face p.vase]))
++  with-faces
  =|  res=(unit vase)
  |=  vaz=(list [face=@tas =vase])
  ^-  vase
  ?~  vaz  (need res)
  =/  faz  (with-face i.vaz)
  =.  res  `?~(res faz (slop faz u.res))
  $(vaz t.vaz)
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
      |=  [a=term b=(unit dojo-source)]
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
          |=  [a=goal b=$^(dojo-model dojo-source)]
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
      ;~  pfix  fas
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
      ;~(plug (cold %flat pat) (most fas sym))
      ;~(plug (cold %pill dot) (most fas sym))
      ;~(plug (cold %http lus) (stag %post parse-url))
      ;~(plug (cold %http hep) (stag %put parse-url))
      (stag %show (cook $?(%1 %2 %3 %4 %5) (cook lent (stun [1 5] wut))))
    ==
  ::
  ++  parse-cables
    %+  cook
      |=  cables=(list cable:clay)
      :+  0  %ex
      ^-  hoon
      ::
      :-  %clsg
      %+  turn  cables
      |=  cable=cable:clay
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
    %+  cook  |=(a=cable:clay a)
    ;~  pose
      (stag ~ ;~(pfix tar sym))
      (cook |=([face=term tis=@ file=term] [`face file]) ;~(plug sym tis sym))
      (cook |=(a=term [`a a]) sym)
    ==
  ++  parse-source  (stag 0 parse-build)
  ++  parse-build
      %+  knee  *dojo-build  |.  ~+
      ;~  pose
        ;~(plug (cold %ur lus) parse-url)
        ;~(plug (cold %ge lus) parse-model)
        ;~(plug (cold %te hep) sym (star ;~(pfix ace parse-source)))
        ;~(plug (cold %as pam) sym ;~(pfix ace parse-source))
        ;~(plug (cold %do cab) parse-hoon ;~(pfix ace parse-source))
        parse-value
      ==
  ::
  ++  parse-goal
    %+  cook  |=(a=goal a)
    ;~  pose
      ;~  plug
        ;~(pfix sig fed:ag)
        ;~(pose ;~(pfix fas sym) (easy default-app))
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
      (fall (de-beam:format a) [`beak`[p q r]:dir a])
    =+  vez=hoon-parser
    (sear plex:vez (stag %clsg poor:vez))
  ::
  ++  parse-iden-url
    %+  cook
      |=([a=(unit knot) b=purl:eyre] [`(fall a *knot) b])
    auru:de-purl:html
  ::
  ++  parse-url
    %+  cook
      |=(a=purl:eyre (crip (en-purl:html a)))
    auri:de-purl:html
  ::
  ++  parse-model   ;~(plug parse-server parse-config)
  ++  parse-server  (stag 0 (most fas sym))
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
      ;~(plug (cold %as pam) sym ;~(pfix ace parse-source))
      (stag %sa ;~(pfix tar pam sym))
      (stag %ex parse-hoon)
      (stag %tu (ifix [sel ser] (most ace parse-source)))
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
=|  house                                               ::  program state
=*  state  -
=>  |%
::
++  xskol  `$-(type tank)`type-to-tank:pprint
++  xsell  `$-(vase tank)`vase-to-tank:pprint
::
++  he                                                  ::  per session
  |_  [hid=bowl:gall =id moz=(list card:agent:gall) session]
  ::
  ++  he-beam
    ^-  beam
    ?.  =([%ud 0] r.dir)
      dir
    dir(r [%da now.hid])
  ::
  ++  he-beak    `beak`[p q r]:he-beam
  ++  he-parser  (parser-at our.hid he-beam)
  ::
  ++  dy                                                ::  project work
    |_  dojo-project                                    ::
    ++  dy-abet  +>(poy `+<)                            ::  resolve
    ++  dy-amok  +>(poy ~)                              ::  terminate
    ::  +dy-sing: make a clay read request
    ::
    ++  dy-sing
      |=  [way=wire =care:clay =path]
      ^+  +>+>
      ?>  ?=(~ pux)
      %-  he-card(poy `+>+<(pux `way))
      =/  [=ship =desk =case:clay]  he-beak
      [%pass way %arvo %c %warp ship desk ~ %sing care case path]
    ::
    ++  dy-request
      |=  [way=wire =request:http]
      ^+  +>+>
      ?>  ?=(~ pux)
      %-  he-card(poy `+>+<(pux `way))
      [%pass way %arvo %i %request request *outbound-config:iris]
    ::
    ++  dy-stop                                         ::  stop work
      ^+  +>
      =.  poy  ~
      ?~  pux  +>
      %.  [%txt "! cancel {<u.pux>}"]
      =<  he-diff
      %-  he-card
      ?:  =(/wool u.pux)
        ::  really shoud stop the thread as well
        ::
        [%pass u.pux %agent [our.hid %spider] %leave ~]
      =/  [=ship =desk =case:clay]  he-beak
      [%pass u.pux %arvo %c %warp ship desk ~]
    ::
    ++  dy-errd                                         ::  reject change, abet
      |=  [rev=(unit sole-edit) err=@u]
      ^+  +>+>
      (he-errd(poy `+>+<) rev err)
    ::
    ++  dy-diff                                         ::  send effects, abet
      |=  fec=sole-effect
      ^+  +>+>
      (he-diff(poy `+>+<) fec)
    ::
    ++  dy-rash                                         ::  send effects, amok
      |=  fec=sole-effect
      ^+  +>+>
      (he-diff(poy ~) fec)
    ::
    ++  dy-init-command                                 ::  ++dojo-command
      |=  mad=dojo-command
      ^+  [mad +>]
      ?@  -.mad  [mad +>.$]
      =.  q.mad
        ?+(-.p.mad q.mad %http [0 %as %mime q.mad])
      =^  src  +>.$  (dy-init-source q.mad)
      [mad(q src) +>.$]
    ::
    ++  dy-init-source                                  ::  ++dojo-source
      |=  src=dojo-source
      ^+  [src +>]
      =^  bul  +>  (dy-init-build q.src)
      =:  p.src  num
          q.src  bul
        ==
      [src +>.$(num +(num), job (~(put by job) -.src +.src))]
    ::
    ++  dy-init-source-unit                             ::  (unit dojo-source)
      |=  urc=(unit dojo-source)
      ^+  [urc +>]
      ?~  urc  [~ +>]
      =^  src  +>  (dy-init-source u.urc)
      [`src +>.$]
    ::
    ++  dy-init-build                                   ::  ++dojo-build
      |=  bul=dojo-build
      ^+  [bul +>]
      ?-    -.bul
        %ex  [bul +>.$]
        %dv  [bul +>.$]
        %sa  [bul +>.$]
        %as  =^(mor +>.$ (dy-init-source q.bul) [bul(q mor) +>.$])
        %do  =^(mor +>.$ (dy-init-source q.bul) [bul(q mor) +>.$])
        %ge  =^(mod +>.$ (dy-init-model p.bul) [[%ge mod] +>.$])
        %te  =^(mod +>.$ (dy-init-ordered q.bul) [bul(q mod) +>.$])
        %ur  [bul +>.$]
        %tu  =^(dof +>.$ (dy-init-ordered p.bul) [[%tu dof] +>.$])
      ==
    ::
    ++  dy-init-model                                   ::  ++dojo-model
      |=  mol=dojo-model
      ^+  [mol +>]
      =^  one  +>.$  (dy-init-server p.mol)
      =^  two  +>.$  (dy-init-config q.mol)
      [[one two] +>.$]
    ::
    ++  dy-init-server                                  ::  ++dojo-server
      |=  srv=dojo-server
      =.  p.srv  num
      [srv +>.$(num +(num), job (~(put by job) num [%dv [%gen q.srv]]))]
    ::
    ++  dy-init-config                                  ::  prepare config
      |=  cig=dojo-config
      ^+  [cig +>]
      =^  ord  +>.$  (dy-init-ordered p.cig)
      =^  key  +>.$  (dy-init-named q.cig)
      [[ord key] +>.$]
    ::
    ++  dy-init-ordered                                 ::  (list dojo-source)
      |=  ord=(list dojo-source)
      ^+  [ord +>]
      ?~  ord  [~ +>.$]
      =^  fir  +>.$  (dy-init-source i.ord)
      =^  mor  +>.$  $(ord t.ord)
      [[fir mor] +>.$]
    ::
    ++  dy-init-named                                   ::  (map @tas dojo-src)
      |=  key=(map term (unit dojo-source))
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
      |=  cag=cage
      ^+  +>+>
      ?>  ?=(^ cud)
      (dy-step(cud ~, rez (~(put by rez) p.u.cud cag)) +(p.u.cud))
    ::
    ++  dy-meal                                         ::  vase to cage
      |=  vax=vase
      ?.  &(?=(@ -.q.vax) ((sane %tas) -.q.vax))
        ~&  %dy-meal-cage
        (dy-rash %bel ~)
      (dy-hand -.q.vax (slot 3 vax))
    ::
    ++  dy-made-edit                                    ::  sole edit
      |=  cag=cage
      ^+  +>+>
      ?>  ?=(^ per)
      ?:  ?|  ?=(^ q.q.cag)
              =((lent buf.say) q.q.cag)
              !&(?=(%del -.u.per) =(+(p.u.per) (lent buf.say)))
          ==
        dy-abet(per ~)
      (dy-errd(per ~) per q.q.cag)
    ::
    ++  dy-done                                         ::  dialog submit
      |=  txt=tape
      ?:  |(?=(^ per) ?=(^ pux) ?=(~ pro))
        ~&  %dy-no-prompt
        (dy-diff %bel ~)
      =/  res  (mule |.((slam u.pro !>(txt))))
      ?:  ?=(%| -.res)
        %-  (slog >%dy-done< p.res)
        (dy-rash %bel ~)  ::  TODO: or +dy-abet(per ~)  ?
      (dy-made-dial %noun p.res)
    ::
    ++  dy-cast
      |*  [typ=_* bun=vase]
      |=  a=vase  ^-  typ
      ~|  [p.bun p.a]
      ?>  (~(nest ut p.bun) & p.a)
      ;;(typ q.a)
    ::
    ++  dy-over                                         ::  finish construction
      ^+  +>
      ::  XX needs filter
      ::
      ?:  ?=([%show %3] -.mad)
        (dy-rash %tan (dy-show-source q.mad) ~)
      ?:  ?=(%brev -.mad)
        =.  var  (~(del by var) p.mad)
        =<  dy-amok
        ?+  p.mad  .
          $?(%eny %now %our)  !!
          %lib  .(lib ~)
          %sur  .(sur ~)
          %dir  .(dir [[our.hid %home ud+0] /])
        ==
      =+  cay=(~(got by rez) p.q.mad)
      ?-    -.p.mad
          %verb
        =.  var  (~(put by var) p.p.mad cay)
        ~|  bad-set+[p.p.mad p.q.cay]
        =<  dy-amok
        ?+  p.p.mad  .
            %eny  ~|(%entropy-is-eternal !!)
            %now  ~|(%time-is-immutable !!)
            %our  ~|(%self-is-immutable !!)
            %lib
          %_    .
              lib
            ((dy-cast (list cable:clay) !>(*(list cable:clay))) q.cay)
          ==
        ::
            %sur
          %_    .
              sur
            ((dy-cast (list cable:clay) !>(*(list cable:clay))) q.cay)
          ==
        ::
            %dir  =+  ^=  pax  ^-  path
                      =+  pax=((dy-cast path !>(*path)) q.cay)
                      ?:  ?=(~ pax)  ~[(scot %p our.hid) %home '0']
                      ?:  ?=([@ ~] pax)  ~[i.pax %home '0']
                      ?:  ?=([@ @ ~] pax)  ~[i.pax i.t.pax '0']
                      pax
                  =.  dir  (need (de-beam:format pax))
                  =-  +>(..dy (he-diff %tan - ~))
                  rose+[" " `~]^~[leaf+"=%" (smyt (en-beam:format he-beak s.dir))]
        ==
      ::
          %poke
        %-  he-card(poy ~)
        :*  %pass
            /poke
            %agent
            p.p.mad
            %poke
            cay
        ==
      ::
          %file
        %-  he-card(poy ~)
        :*  %pass  /file  %arvo  %c
            %info  (foal:space:userlib (en-beam:format p.p.mad) cay)
        ==
      ::
          %flat
        ?^  q.q.cay
          (dy-rash %tan [%leaf "not an atom"]~)
        (dy-rash %sav p.p.mad q.q.cay)
      ::
          %pill
        (dy-rash %sag p.p.mad q.q.cay)
      ::
          %http
        ?>  ?=(%mime p.cay)
        =+  mim=;;(mime q.q.cay)
        %+  dy-request  /show
        :*  ?:(=(%put p.p.mad) %'PUT' %'POST')
            r.p.mad
            ~[['content-type' (en-mite:mimes:html p.mim)]]
            `q.mim
        ==
      ::
          %show
        |^  (prnt cay note)
        ++  prnt  ?:  (gte p.p.mad 4)
                    dy-xprint
                  dy-print
        ++  note  ^-  tang
                  ?-  p.p.mad
                    %0  ~
                    %1  [[%rose [~ "  " ~] (skol p.q.cay) ~] maar]
                    %2  [[%rose [~ "  " ~] (dy-show-type-noun p.q.cay) ~] maar]
                    ::%3  handled above
                    %4  ~
                    %5  [[%rose [~ "  " ~] (xskol p.q.cay) ~] maar]
                  ==
        ++  maar  ?:  =(%noun p.cay)  ~
                  [[%rose [~ "    " ~] >p.cay< ~] ~]
        --
      ==
    ::
    ++  dy-show  |=(cay=cage (dy-print cay ~))
    ::
    ::  Print a value (given as a cage) and a note (given as a tang).
    ::
    ++  dy-xprint
      |=  [cay=cage tan=tang]
      %+  dy-rash  %tan
      %-  welp  :_  tan
      ?+  p.cay  [(xsell q.cay)]~
        %tang  ;;(tang q.q.cay)
        %httr
          =+  hit=;;(httr:eyre q.q.cay)
          =-  (flop (turn `wall`- |=(a=tape leaf+(dash:us a '' ~))))
          :-  "HTTP {<p.hit>}"
          %+  weld
            (turn q.hit |=([a=@t b=@t] "{(trip a)}: {(trip b)}"))
          :-  i=""
          t=(turn `wain`?~(r.hit ~ (to-wain:format q.u.r.hit)) trip)
      ==
    ::
    ::  Print a value (given as a cage) and a note (given as a tang).
    ::
    ++  dy-print
      |=  [cay=cage tan=tang]
      %+  dy-rash  %tan
      %-  welp  :_  tan
      ?+  p.cay  [(sell q.cay)]~
        %tang  ;;(tang q.q.cay)
        %httr
          =+  hit=;;(httr:eyre q.q.cay)
          =-  (flop (turn `wall`- |=(a=tape leaf+(dash:us a '' ~))))
          :-  "HTTP {<p.hit>}"
          %+  weld
            (turn q.hit |=([a=@t b=@t] "{(trip a)}: {(trip b)}"))
          :-  i=""
          t=(turn `wain`?~(r.hit ~ (to-wain:format q.u.r.hit)) trip)
      ==
  ++  dy-show-type-noun
    |=  a=type  ^-  tank
    =-  >[-]<
    |-  ^-  $?  $%  [%atom @tas (unit @)]
                    [%cell _$ _$]
                    [%face $@(term tune) _$]
                    [%fork (set _$)]
                    [%hold _$ hoon]
                ==
                wain                :: "<|core|>"
                $?(%noun %void)
            ==
    ?+  a  a
      [%face ^]  a(q $(a q.a))
      [%cell ^]  a(p $(a p.a), q $(a q.a))
      [%fork *]  a(p (silt (turn ~(tap in p.a) |=(b=type ^$(a b)))))
      [%hint *]  !!
      [%core ^]  `wain`/core
      [%hold *]  a(p $(a p.a))
    ==
  ::
  ::  XX needs filter
  ::
  ++  dy-shown
    =/  jank-bucwut  :: FIXME just $? fishes when defined for some reason
      |*  [a=mold b=mold]
      |=(c=_`*`*a ?:(& (a c) (b c)))
    ::
    ::$?  hoon
    ;:    jank-bucwut
        hoon
        $^  [dy-shown dy-shown]
        $%  [%ur cord]
            [%sa mark]
            [%as mark dy-shown]
            [%do hoon dy-shown]
            [%te term (list dy-shown)]
            [%ge path (list dy-shown) (map term (unit dy-shown))]
            [%dv path]
        ==
    ==
  ::
  ++  dy-show-source
    |=  a=dojo-source  ^-  tank
    =-  >[-]<
    =+  `[@ bil=dojo-build]`a
    |-  ^-  dy-shown
    ?-  -.bil
      $?(%ur %dv %sa)  bil
      %ex  ?.  ?=([%cltr *] p.bil)  p.bil
               |-  ^-  hoon
               ?~  p.p.bil  !!
               ?~  t.p.p.bil  i.p.p.bil
               [i.p.p.bil $(p.p.bil t.p.p.bil)]
      %tu  ?~  p.bil  !!
           |-
           ?~  t.p.bil  ^$(bil q.i.p.bil)
           [^$(bil q.i.p.bil) $(p.bil t.p.bil)]
      %as  bil(q $(bil q.q.bil))
      %do  bil(q $(bil q.q.bil))
      %te  bil(q (turn q.bil ..$))
      %ge  :+  %ge  q.p.p.bil
           [(turn p.q.p.bil ..$) (~(run by q.q.p.bil) (lift ..$))]
    ==
    ::
    ++  dy-edit                                         ::  handle edit
      |=  cal=sole-change
      ^+  +>+>
      =^  dat  say  (~(transceive sole say) cal)
      ?:  |(?=(^ per) ?=(^ pux) ?=(~ pro))
        ~&  %dy-edit-busy
        =^  lic  say  (~(transmit sole say) dat)
        (dy-diff %mor [%det lic] [%bel ~] ~)
      =>  .(per `dat)
      =/  res  (mule |.((slam u.pro !>((tufa buf.say)))))
      ?:  ?=(%| -.res)
        %-  (slog >%dy-edit< p.res)
        (dy-rash %bel ~)  ::  TODO: or +dy-abet(per ~)  ?
      (dy-made-edit %noun p.res)
    ::
    ++  dy-type                                         ::  sole action
      |=  act=sole-action
      ?-  -.dat.act
        %det  (dy-edit +.dat.act)
        %ret  (dy-done (tufa buf.say))
        %clr  dy-stop
        %tab  +>+>
      ==
    ::
    ++  dy-cage       |=(num=@ud (~(got by rez) num))   ::  known cage
    ++  dy-vase       |=(num=@ud q:(dy-cage num))       ::  known vase
    ++  dy-sore
      |=  src=(list dojo-source)
      ^-  vase
      ?~  src
        !>(~)
      (slop (dy-vase p.i.src) $(src t.src))
    ::
    ++  dy-run-generator
      !.
      |=  [cay=cage cig=dojo-config]
      ^+  +>+>
      ?.  (~(nest ut [%cell [%atom %$ ~] %noun]) | p.q.cay)
        ::  naked generator; takes one argument
        ::
        ?.  &(?=([* ~] p.cig) ?=(~ q.cig))
          ~|(%one-argument !!)
        =/  res  (mule |.((slam q.cay (dy-vase p.i.p.cig))))
        ?:  ?=(%| -.res)
          ::  TODO: or +dy-rash  ?
          (he-diff(poy ~) %tan leaf+"dojo: naked generator failure" p.res)
        (dy-hand %noun p.res)
      ::  normal generator
      ::
      ::    A normal generator takes as arguments:
      ::    - event args: date, entropy, beak (network location)
      ::    - positional arguments, as a list
      ::    - optional keyword arguments, as name-value pairs
      ::
      ::    The generator is a pair of a result mark and a gate.
      ::    TODO: test %ask generators
      ::
      =/  wat  (mule |.(!<(?(%ask %say) (slot 2 q.cay))))
      ?:  ?=(%| -.wat)
        (he-diff(poy ~) %tan leaf+"dojo: generator neither %ask nor %say" p.wat)
      =-  =/  res  (mule -)
          ?:  ?=(%| -.res)
            (he-diff(poy ~) %tan leaf+"dojo: generator failure" p.res)
          ?-  p.wat
            %ask  (dy-made-dial %noun p.res)
            %say  (dy-made-gent %noun p.res)
          ==
      ::  gat: generator gate
      ::  som: default gat sample
      ::  ven: event arguments
      ::  poz: positional arguments
      ::  kev: key-value named arguments
      ::  kuv: default keyword arguments
      ::  sam: fully populated sample
      ::  rog: default gat sample
      ::
      |.  ^-  vase
      =/  gat=vase  (slot 3 q.cay)
      =/  som=vase  (slot 6 gat)
      =/  ven=vase  !>([now=now.hid eny=eny.hid bec=he-beak])
      =/  poz=vase  (dy-sore p.cig)
      =/  kev=vase
        =/  kuv=(unit vase)  (slew 7 som)
        ?:  =(~ q.cig)
          (fall kuv !>(~))
        =/  soz=(list [var=term vax=vase])
          %~  tap  by
          %-  ~(run by q.cig)
          |=(val=(unit dojo-source) ?~(val !>([~ ~]) (dy-vase p.u.val)))
        ~|  keyword-arg-failure+~(key by q.cig)
        %+  slap
          (with-faces kuv+(need kuv) rep+(with-faces soz) ~)
        :+  %cncb  [%kuv]~
        %+  turn  soz
        |=  [var=term *]
        ^-  [wing hoon]
        [[var]~ [%wing var %rep ~]]
      ::
      =/  sam=vase  :(slop ven poz kev)
      ?.  (~(nest ut p.som) | p.sam)
        ~>  %slog.1^leaf+"dojo: nest-need"
        ~>  %slog.0^(skol p.som)
        ~>  %slog.1^leaf+"dojo: nest-have"
        ~>  %slog.0^(skol p.sam)
        !!
      (slam gat sam)
    ::
    ++  dy-made-dial                                    ::  dialog product
      |=  cag=cage
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
    ++  dy-made-gent                                    ::  generator product
      |=  cag=cage
      (dy-meal q.cag)
    ::
    ++  dy-made-noun                                    ::  generator product
      |=  cag=cage
      (dy-hand %noun q.cag)
    ::
    ++  dy-wool-poke
      |=  [fil=term src=(list dojo-source)]
      ^+  +>+>
      ?>  ?=(~ pux)
      =/  tid  (scot %ta (cat 3 'dojo_' (scot %uv (sham eny.hid))))
      =.  poy  `+>+<.$(pux `/wool)
      =.  +>+>.$
        %-  he-card
        [%pass /wool %agent [our.hid %spider] %watch /thread-result/[tid]]
      %-  he-card
      =/  =cage  ::  also sub
        [%spider-start !>([~ `tid fil (dy-sore src)])]
      [%pass /wool %agent [our.hid %spider] %poke cage]
    ::
    ++  dy-make                                         ::  build step
      ^+  +>
      ?>  ?=(^ cud)
      =/  bil  q.u.cud
      ?-    -.bil
          %ur  (dy-request /hand `request:http`[%'GET' p.bil ~ ~])
          %te  (dy-wool-poke p.bil q.bil)
          %ex  (dy-mere p.bil)
          %dv  (dy-sing hand+p.bil %a (snoc p.bil %hoon))
          %ge  (dy-run-generator (dy-cage p.p.p.bil) q.p.bil)
          %sa
        =+  .^(=dais:clay cb+(en-beam:format he-beak /[p.bil]))
        (dy-hand p.bil bunt:dais)
      ::
          %as
        =/  cag=cage  (dy-cage p.q.bil)
        =+  .^(=tube:clay cc+(en-beam:format he-beak /[p.cag]/[p.bil]))
        (dy-hand p.bil (tube q.cag))
      ::
          %do
        =/  gat  (dy-eval p.bil)
        ?:  ?=(%| -.gat)
          (he-diff(poy ~) %tan leaf+"dojo: %do create gate failed" p.gat)
        =/  res  (mule |.((slam q.p.gat (dy-vase p.q.bil))))
        ?:  ?=(%| -.res)
          (he-diff(poy ~) %tan leaf+"dojo: %do execute failed" p.res)
        (dy-hand %noun p.res)
      ::
          %tu
        %+  dy-hand  %noun
        |-  ^-  vase
        ?~  p.bil  !!
        =/  hed  (dy-vase p.i.p.bil)
        ?~  t.p.bil  hed
        (slop hed $(p.bil t.p.bil))
      ==
    ::  +dy-hoon-var: if input is a dojo variable lookup, perform it
    ::
    ::    If the expression is a bare reference to a Dojo variable,
    ::    produce that variable's value; otherwise, produce ~.
    ::
    ++  dy-hoon-var
      =+  ^=  ope
          |=  gen=hoon  ^-  hoon
          ?:  ?=(?(%sggl %sggr) -.gen)
            $(gen q.gen)
          =+  ~(open ap gen)
          ?.(=(gen -) $(gen -) gen)
      |=  gen=hoon  ^-  (unit cage)
      =.  gen  (ope gen)
      ?:  ?=([%cnts [@ ~] ~] gen)
        (~(get by var) i.p.gen)
      ~
    ::  +dy-mere: execute hoon and complete construction step
    ::
    ++  dy-mere
      |=  =hoon
      =/  res  (dy-eval hoon)
      ?:  ?=(%| -.res)
        (he-diff(poy ~) %tan leaf+"dojo: hoon expression failed" p.res)
      (dy-hand p.res)
    ::  +dy-eval: run hoon source against the dojo subject
    ::
    ::    TODO: use /lib and /sur imports to construct subject
    ::
    ++  dy-eval
      |=  =hoon
      ^-  (each cage tang)
      ?^  val=(dy-hoon-var hoon)
        &+u.val
      !.
      %-  mule  |.
      :-  %noun
      =/  vaz=(list [term vase])
        (turn ~(tap by var) |=([lal=term cag=cage] [lal q.cag]))
      =/  sut  (slop !>([our=our now=now eny=eny]:hid) !>(..zuse))
      =?  sut  ?=(^ vaz)  (slop (with-faces vaz) sut)
      (slap sut hoon)
    ::
    ++  dy-step                                         ::  advance project
      |=  nex=@ud
      ^+  +>+>
      ?>  ?=(~ cud)
      ?:  =(nex num)
        dy-over
      ?:  =([%show %3] -.mad)                           :: just show source
        dy-over
      dy-make(cud `[nex (~(got by job) nex)])
    --
  ::
  ++  he-dope
    |=  txt=tape                                        ::
    ^-  (each (unit (each dojo-command tape)) hair)     ::  prefix+result
    =+  len=+((lent txt))                               ::  line length
    =.  txt  (weld buf `tape`(weld txt "\0a"))          ::
    =+  vex=((full parse-command-line:he-parser) [1 1] txt)
    ?:  =(q.p.vex len)                                  ::  matched to line end
      [%& ~]                                            ::
    ?:  =(p.p.vex +((lent (skim txt |=(a=@ =(10 a)))))) ::  parsed all lines
      [%& ~ ?~(q.vex [%| txt] [%& p.u.q.vex])]          ::  new buffer+complete
    [%| p.p.vex (dec q.p.vex)]                          ::  syntax error
  ::
  ++  he-duke                                           ::  ++he-dope variant
    |=  txt=tape
    ^-  (each (unit (each dojo-command tape)) @ud)
    =+  foy=(he-dope txt)
    ?-  -.foy
      %|  [%| q.p.foy]
      %&  [%& p.foy]
    ==
  ::
  ++  he-abet                                           ::  resolve
    [(flop moz) %_(state hoc (~(put by hoc) id +<+>+))]
  ::
  ++  he-card                                           ::  emit gift
    |=  =card:agent:gall
    ^+  +>
    =?  card  ?=(%pass -.card)
      card(p [id p.card])
    %_(+> moz [card moz])
  ::
  ++  he-diff                                           ::  emit update
    |=  fec=sole-effect
    ^+  +>
    (he-card %give %fact ~[/sole/[id]] %sole-effect !>(fec))
  ::
  ++  he-stop                                           ::  abort work
    ^+  .
    ?~(poy . ~(dy-stop dy u.poy))
  ::
  ++  he-peer                                           ::  subscribe to
    |=  pax=path
    ?>(=(~ pax) he-prom)
  ::
  ++  he-pine                                           ::  restore prompt
    ^+  .
    ?^  poy  .
    he-prom:he-pone
  ::
  ++  he-errd                                           ::  reject update
    |=  [rev=(unit sole-edit) err=@u]  ^+  +>
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
    ?:(=(~ s.dir) "" (spud s.dir))
  ::
  ++  he-prom                                           ::  send prompt
    %-  he-diff
    :-  %pro
    [& %$ (weld he-prow ?~(buf "> " "< "))]
  ::
  ++  he-writ
    |=  [way=wire =riot:clay]
    ^+  +>
    ?>  ?=(^ poy)
    =<  he-pine
    ?+    way  !!
        [%hand *]
      ?~  riot
        (he-diff(poy ~) %tan >%generator-build-fail< >(snoc t.way %hoon)< ~)
      (~(dy-hand dy u.poy(pux ~)) noun+!<(vase q.r.u.riot))
    ==
  ::
  ++  he-unto                                           ::  result from agent
    |=  [way=wire cit=sign:agent:gall]
    ^+  +>
    ?.  ?=(%poke-ack -.cit)
      ~&  [%strange-unto cit]
      +>
    ?~  p.cit
      (he-diff %txt ">=")
    (he-diff %tan leaf+"dojo: app poke failed" u.p.cit)
  ::
  ++  he-wool
    |=  [way=wire =sign:agent:gall]
    ^+  +>
    ?-    -.sign
        %poke-ack
      ?~  p.sign
        +>.$
      =.  +>.$  (he-diff(poy ~) %tan leaf+"dojo: thread poke failed" u.p.sign)
      (he-card %pass /wool %agent [our.hid %spider] %leave ~)
    ::
        %watch-ack
      ?~  p.sign
        +>.$
      (he-diff(poy ~) %tan leaf+"dojo: thread watch failed" u.p.sign)
    ::
        %fact
      ?+    p.cage.sign  ~|([%dojo-thread-bad-mark-result p.cage.sign] !!)
          %thread-fail
        =+  !<([=term =tang] q.cage.sign)
        %+  he-diff(poy ~)  %tan
        (flop `^tang`[leaf+"thread failed: {<term>}" tang])
      ::
          %thread-done
        ?>  ?=(^ poy)
        (~(dy-hand dy u.poy(pux ~)) %noun q.cage.sign)
      ==
    ::
        %kick  +>.$
    ==
  ::  +he-http-response: result from http-client
  ::
  ++  he-http-response
    |=  [way=wire response=client-response:iris]
    ^+  +>
    ?>  ?=(^ poy)
    =<  he-pine
    ?.  ?=(%finished -.response)
      ~&  %dojo-received-http-progress
      +>
    ::
    ~!  response
    %.  [%httr !>((to-httr:iris response-header.response full-file.response))]
    =+  dye=~(. dy u.poy(pux ~))
    ?+  way  !!
      [%hand ~]  dy-hand:dye
      [%show ~]  dy-show:dye
    ==
  ::
  ++  he-lens
    |=  com=command:lens
    ^+  +>
    =/  source=dojo-source
        =|  num=@
        =-  ?.  ?=(%send-api -.sink.com)  ::  XX  num is incorrect
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
            %data        [%ex %sand %t data.source.com]
            %dojo
          %+  rash  command.source.com
          (ifix [(punt gap) (punt gap)] parse-build:he-parser)
        ::
            %clay
          :-  %ex
          ^-  hoon
          :+  %dtkt
            [%base %noun]
          :+  %clhp
            [%rock %tas %cx]
          %+  rash  pax.source.com
          rood:(vang | /(scot %p our.hid)/home/(scot %da now.hid))
        ::
            %url         [%ur (crip (en-purl:html url.source.com))]
            %api         !!
            %get-api
          :-  %ex
          ^-  hoon
          :+  %dtkt
            [%like ~[%json] ~]
          :*  %clsg
              [%rock %tas %gx]
              [%sand %ta (scot %p our.hid)]
              [%sand %tas api.source.com]
              [%sand %ta (scot %da now.hid)]
              (turn endpoint.source.com |=(a=@t [%sand %ta a]))
          ==
        ::
            %listen-api  !!
            %export      !!
            %import      !!
            %export-all  !!
            %import-all  !!
            %as
          :*  %as  mar.source.com
              $(num +(num), source.com next.source.com)
          ==
        ::
            %hoon
          :*  %do
              %+  rash  code.source.com
              tall:(vang | /(scot %p our.hid)/home/(scot %da now.hid))
              $(num +(num), source.com next.source.com)
          ==
        ::
            %tuple
          :-  %tu
          |-  ^-  (list dojo-source)
          ?~  next.source.com
            ~
          =.  num  +(num)
          :-  ^$(source.com i.next.source.com)
          $(next.source.com t.next.source.com)
        ==
    =+  |-  ^-  sink=dojo-sink
        ?-  -.sink.com
          %stdout       [%show %0]
          %output-file  $(sink.com [%command (cat 3 '@' pax.sink.com)])
          %output-pill  $(sink.com [%command (cat 3 '.' pax.sink.com)])
          %output-clay  [%file (need (de-beam:format pax.sink.com))]
          %url          [%http %post (crip (en-purl:html url.sink.com))]
          %to-api       !!
          %send-api     [%poke our.hid api.sink.com]
          %command      (rash command.sink.com parse-sink:he-parser)
          %app          [%poke our.hid app.sink.com]
        ==
    (he-plan sink source)
  ::
  ++  he-like                                           ::  accept line
    |=  buf=(list @c)
    =(%& -:(he-dope (tufa buf)))
  ::
  ++  he-stir                                           ::  apply change
    |=  cal=sole-change
    ^+  +>
    ::  ~&  [%his-clock ler.cal]
    ::  ~&  [%our-clock ven.say]
    =^  dat  say  (~(transceive sole say) cal)
    ?.  ?&  ?=(%del -.dat)
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
    |=  mad=dojo-command
    ^+  +>
    ?>  ?=(~ poy)
    he-pine:(dy-step:~(dy-init dy %*(. *dojo-project mad mad)) 0)
  ::
  ++  he-done                                           ::  parse command
    |=  txt=tape
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
  ++  he-tab
    |=  pos=@ud
    ^+  +>
    =*  res  +>
    =/  [back-pos=@ud fore-pos=@ud txt=tape]
        (insert-magic:auto (add (lent buf) pos) :(weld buf (tufa buf.say)))
    =/  id-len  (sub fore-pos back-pos)
    =/  fore-pos-diff  (sub fore-pos pos)
    =+  vex=((full parse-command-line:he-parser) [1 1] txt)
    ?.  ?=([* ~ [* @ %ex *] *] vex)
      (he-tab-not-hoon pos :(weld buf (tufa buf.say) "\0a"))
    =/  typ  p:(slop q:he-hoon-head !>(..dawn))
    =/  tl  (tab-list-hoon:auto typ p.q.q.p.u.q.vex)
    =/  advance  (advance-hoon:auto typ p.q.q.p.u.q.vex)
    =?  res  ?=(^ advance)
      =/  to-send
        (trip (rsh 3 (sub pos back-pos) u.advance))
      =|  fxs=(list sole-effect)
      =.  .
        |-  ^+  +.$
        ?.  (gth fore-pos-diff 0)
          +.$
        =^  lic  say  (~(transmit sole say) %del pos)
        %=  $
          fxs            [det+lic fxs]
          fore-pos-diff  (dec fore-pos-diff)
        ==
      ::  =.  pos  (add pos fore-pos-diff)
      |-  ^+  res
      ?~  to-send
        (he-diff %mor (flop fxs))
      =^  lic  say  (~(transmit sole say) %ins pos `@c`i.to-send)
      $(to-send t.to-send, fxs [`sole-effect`det+lic fxs], pos +(pos))
    ::  If couldn't search (eg cursor not in appropriate position), do
    ::  nothing.
    ::
    ?:  ?=(~ tl)
      res
    ::  If no options, ring the bell
    ::
    ?:  =([~ ~] tl)
      (he-diff %bel ~)
    ::  If only one option, don't print unless the option is already
    ::  typed in.
    ::
    ?:  &(?=([* ~] u.tl) !=((met 3 (need advance)) id-len))
      res
    ::  Else, print results
    ::
    =/  lots  (gth (lent u.tl) 10)
    %+  he-diff  %tab
    %+  turn  u.tl
    |=  [=term =type]
    ~|  term
    :-  term
    ?:  lots
      *tank
    ::  +perk is broken because *perk crashes.
    ::
    ?:  =(%perk term)
      *tank
    ~(duck easy-print type)
  ::
  ::  Full tab complete for all Dojo sinks and sources is a madmans job.
  ::  Instead, we try to parse limited but common forms we know we can
  ::  autocomplete correctly
  ++  he-tab-not-hoon
    |=  [pos=@ud txt=tape]
    ^+  +>
    =*  res  +>
    |^
    =/  naked-poke=(unit term)
      %+  rust  txt
      (full (ifix [col (just `@`10)] ;~(pose sym (easy %$))))
    ?^  naked-poke
      (complete-naked-poke u.naked-poke)
    =/  variable=(unit term)
      %+  rust  txt
      (full (ifix [tis (just `@`10)] ;~(pose sym (easy %$))))
    ?^  variable
      (complete-variable u.variable)
    =/  gen-poke-to-app=(unit [term term])
      %+  rust  txt
      ;~  sfix
        ;~  (glue bar)
          ;~(pose ;~(pfix col sym) (easy %$))
          ;~(pose sym (easy %$))
        ==
        (just `@`10)
      ==
    ?^  gen-poke-to-app
      (complete-gen-poke-to-app u.gen-poke-to-app)
    =/  naked-gen=(unit term)
      %+  rust  txt
      (full (ifix [lus (just `@`10)] ;~(pose sym (easy %$))))
    ?~  naked-gen
      res
    (complete-naked-gen u.naked-gen)
    ::
    ++  complete-naked-poke
      |=  app=term
      =/  pax=path
        /(scot %p our.hid)/[q.byk.hid]/(scot %da now.hid)/app
      %+  complete  (cat 3 ':' app)
      %+  murn  ~(tap by dir:.^(arch %cy pax))
      |=  [=term ~]
      ^-  (unit [^term tank])
      ?.  =(app (end 3 (met 3 app) term))
        ~
      ?~  =<(fil .^(arch %cy (weld pax ~[term %hoon])))
        ~
      `[(cat 3 ':' term) *tank]
    ::
    ++  complete-variable
      |=  variable=term
      %+  complete  variable
      %+  murn  ~(tap by var)
      |=  [name=term =cage]
      ^-  (unit [term tank])
      ?.  =(variable (end 3 (met 3 variable) name))
        ~
      `[name (sell q.cage)]
    ::
    ++  complete-gen-poke-to-app
      |=  [app=term gen=term]
      =.  app
        ?:(?=(%$ app) %hood app)
      %+  complete
        ?:  =(%hood app)
          (cat 3 '|' gen)
        :((cury cat 3) ':' app '|' gen)
      =/  pfix=path
        /(scot %p our.hid)/[q.byk.hid]/(scot %da now.hid)/gen/[app]
      ::
      %^  tab-generators:auto  pfix  `app
      %+  murn
        ~(tap by dir:.^(arch %cy pfix))
      |=  [=term ~]
      ?.  =(gen (end 3 (met 3 gen) term))
        ~
      ?~  =<(fil .^(arch %cy (weld pfix ~[term %hoon])))
        ~
      (some term)
    ::
    ++  complete-naked-gen
      |=  gen=term
      %+  complete  (cat 3 '+' gen)
      =/  pax=path
        /(scot %p our.hid)/[q.byk.hid]/(scot %da now.hid)/gen
      %^  tab-generators:auto  pax  ~
      %+  murn
        ~(tap by dir:.^(arch %cy pax))
      |=  [=term ~]
      ?.  =(gen (end 3 (met 3 gen) term))
        ~
      ?~  =<(fil .^(arch %cy (weld pax ~[term %hoon])))
        ~
      (some term)
    ::
    ++  complete
      |=  [completing=term options=(list [term tank])]
      ?~  options
        res
      =/  advance
        (longest-match:auto options)
      =.  pos
        (dec (lent txt))  :: lock cursor at end
      =/  back-pos
        (sub pos (met 3 completing))
      =/  to-send
        (trip (rsh 3 (sub pos back-pos) advance))
      =|  fxs=(list sole-effect)
      ::
      :: Cursor is guaranteed to be at end so we don't worry about the
      :: backwards case
      ::
      =.  res
        |-  ^+  res
        ?~  to-send
          (he-diff %mor (flop fxs))
        =^  lic  say  (~(transmit sole say) %ins pos `@c`i.to-send)
        $(to-send t.to-send, fxs [`sole-effect`det+lic fxs], pos +(pos))
      ::  If no options, ring the bell
      ::
      ?:  =(~ options)
        (he-diff %bel ~)
      ::  If only one option, don't print unless the option is already
      ::  typed in.
      ::
      ?:  &(?=([* ~] options) !=((met 3 advance) (met 3 completing)))
        res
      ::  Else, print results
      ::
      %+  he-diff  %tab
      options
    --
  ::
  ++  he-type                                           ::  apply input
    |=  act=sole-action
    ^+  +>
    ?^  poy
      he-pine:(~(dy-type dy u.poy) act)
    ?-  -.dat.act
      %det  (he-stir +.dat.act)
      %ret  (he-done (tufa buf.say))
      %clr  he-pine(buf "")
      %tab  (he-tab +.dat.act)
    ==
  ::
  ++  he-lame                                           ::  handle error
    |=  [wut=term why=tang]
    ^+  +>
    %-  (slog (flop `tang`[>%dojo-lame wut< why]))
    ?^  poy
      he-pine:~(dy-amok dy u.poy)
    he-pine                           ::  XX give mean to original keystroke
  ::
  ++  he-hoon-head                                      ::  dynamic state
    ::  todo: how do i separate the toplevel 'dojo state' comment?
    ::  dojo state
    ::
    ::  our: the name of this urbit
    ::  now: the current time
    ::  eny: a piece of random entropy
    ::
    ^-  cage
    :-  %noun
    =+  sloop=|=([a=vase b=vase] ?:(=(*vase a) b ?:(=(*vase b) a (slop a b))))
    %+  sloop
      %-  ~(rep by var)
      |=  [[a=term @ b=vase] c=vase]  ^-  vase
      (sloop b(p face+[a p.b]) c)
    !>([our=our now=now eny=eny]:hid)
  --
--
^-  agent:gall
|_  hid=bowl:gall
++  on-init
  `..on-init
::
++  on-save
  !>(state)
::
++  on-load
  |=  old=vase
  ?:  ?=(%6 +<.old)
    `..on-init(state !<(house old))
  =/  old-5  !<([%5 egg=@u hoc=(map id session)] old)
  =/  =house  [%6 egg.old-5 hoc.old-5 *(set ship)]
  `..on-init(state house)
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card:agent:gall _..on-init)
  =^  moves  state
    ^-  (quip card:agent:gall house)
    ?+  mark  ~|([%dojo-poke-bad-mark mark] !!)
    ::
        %sole-action
      =/  act  !<(sole-action vase)
      he-abet:(~(he-type he hid id.act ~ (~(got by hoc) id.act)) act)
    ::
        %lens-command
      =+  !<([=id =command:lens] vase)
      he-abet:(~(he-lens he hid id ~ (~(got by hoc) id)) command)
    ::
        %allow-remote-login
      =/  who  !<(@p vase)
      `state(acl (~(put in acl) who))
    ::
        %revoke-remote-login
      =/  who  !<(@p vase)
      :_  state(acl (~(del in acl) who))
      [%give %kick ~ `who]~
    ::
        %list-remote-logins
      ~&  acl
      `state
    ::
        %wipe
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
      [~ state]
    ==
  ::
  [moves ..on-init]
::
++  on-watch
  |=  =path
  ^-  (quip card:agent:gall _..on-init)
  ?>  ?|  (team:title our.hid src.hid)
          (~(has in acl) src.hid)
      ==
  ?>  ?=([%sole @ ~] path)
  =/  id  i.t.path
  =?  hoc  (~(has by hoc) id)
    ~&  [%dojo-peer-replaced id]
    (~(del by hoc) id)
  =/  =session  %*(. *session -.dir [our.hid %home ud+0])
  =^  moves  state
    he-abet:~(he-prom he hid id ~ session)
  [moves ..on-init]
::
++  on-leave
  |=  =path
  ?>  ?=([%sole *] path)
  =.  hoc  (~(del by hoc) t.path)
  [~ ..on-init]
::
++  on-peek
  |=  path
  *(unit (unit cage))
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ?>  ?=([@ @ *] wire)
  =/  =session  (~(got by hoc) i.wire)
  =/  he-full  ~(. he hid i.wire ~ session)
  =^  moves  state
    =<  he-abet
    ^+  he
    ?+  i.t.wire  ~|([%dojo-bad-on-agent wire -.sign] !!)
      %poke  (he-unto:he-full t.wire sign)
      %wool  (he-wool:he-full t.wire sign)
    ==
  [moves ..on-init]
::
++  on-arvo
  |=  [=wire =sign-arvo]
  ?>  ?=([@ *] wire)
  =/  =session  (~(got by hoc) i.wire)
  =/  he-full  ~(. he hid i.wire ~ session)
  =^  moves  state
    =<  he-abet
    ?+    +<.sign-arvo  ~|([%dojo-bad-take +<.sign-arvo] !!)
        %writ           (he-writ:he-full t.wire +>.sign-arvo)
        %http-response  (he-http-response:he-full t.wire +>.sign-arvo)
    ==
  [moves ..on-init]
::  if dojo fails unexpectedly, kill whatever each session is working on
::
++  on-fail
  |=  [=term =tang]
  =/  sessions=(list (pair id session))  ~(tap by hoc)
  |-  ^-  (quip card:agent:gall _..on-init)
  ?~  sessions
    [~ ..on-init]
  =^  cards-1  state
    he-abet:(~(he-lame he hid p.i.sessions ~ q.i.sessions) term tang)
  =^  cards-2  ..on-init
    $(sessions t.sessions)
  [(weld cards-1 cards-2) ..on-init]
--
