::                                                      ::  ::
::::  /hoon/dojo/app                               ::  ::::
  ::                                                    ::    ::
/?  314                                                 ::  arvo kelvin
/-  sole                                               ::  console structures
/+  sole                                                ::  console library
[. sole]
::                                                      ::  ::
::::                                                    ::  ::::
  !:                                                    ::    ::
=>  |%                                                  ::  external structures
    ++  house                                           ::  all state
      $:  %1
          hoc=(map bone session)                        ::  conversations
      ==                                                ::
    ++  session                                         ::  per conversation
      $:  say=sole-share                                ::  command-line state
          dir=beam                                      ::  active path
          poy=(unit dojo-project)                       ::  working
          [lib=(list hoof) arc=(list hoof)]             ::  lib/sur
          var=(map term cage)                           ::  variable state
          old=(set term)                                ::  used TLVs
          buf=tape                                      ::  multiline buffer
      ==                                                ::
    ++  dojo-command                                    ::
      $%  [%flat p=path q=dojo-source]                  ::  noun to unix atom
          [%pill p=path q=dojo-source]                  ::  noun to unix pill
          ::  [%tree p=path q=dojo-source]              ::  noun to unix tree
          [%file p=beam q=dojo-source]                  ::  save to clay
          [%poke p=goal q=dojo-source]                  ::  make and poke
          [%show p=dojo-source]                         ::  print
          [%verb p=term q=(unit dojo-source)]           ::  store variable
      ==                                                ::
    ++  dojo-source                                     ::  construction node
      $:  p=@ud                                         ::  assembly index
          q=dojo-build                                  ::  general build
      ==                                                ::
    ++  dojo-build                                      ::  one ford step
      $%  [%ec p=mark q=twig]                           ::  caged expression
          [%ex p=twig]                                  ::  hoon expression
          [%di p=dojo-model]                            ::  dialog
          [%dv p=path]                                  ::  gate from source
          [%fi p=dojo-filter q=dojo-source]             ::  filter
          [%ge p=dojo-model]                            ::  generator
          [%sc p=dojo-model]                            ::  script
          [%tu p=(list dojo-source)]                    ::  tuple
          [%ur p=purl]                                  ::  http GET request
      ==                                                ::
    ++  dojo-filter                                     ::  pipeline filter
      $|  mark                                          ::  simple transmute
      twig                                              ::  function gate
                                                        ::
    ++  dojo-model                                      ::  data construction 
      $:  p=dojo-server                                 ::  core source
          q=dojo-config                                 ::  configuration
      ==                                                ::
    ++  dojo-server                                     ::  numbered device
      $:  p=@ud                                         ::  assembly index
          q=path                                        ::  gate path
      ==                                                ::
    ++  dojo-config                                     ::  configuration
      $:  p=(list dojo-source)                          ::  by order
          q=(map term (unit dojo-source))               ::  by keyword
      ==                                                ::
    ++  dojo-project                                    ::  construction state
      $:  mad=dojo-command                              ::  operation
          num=@ud                                       ::  number of tasks
          cud=(unit dojo-source)                        ::  now solving
          pux=(unit path)                               ::  ford working
          pro=(unit vase)                               ::  prompting loop
          per=(unit sole-edit)                          ::  pending reverse
          job=(map ,@ud dojo-build)                     ::  problems
          rez=(map ,@ud cage)                           ::  results
      ==                                                ::
    ++  bead  ,[p=(set beam) q=cage]                    ::  computed result
    ++  goal  ,[p=ship q=term]                          ::  flat application
    ++  clap                                            ::  action, user
      $%  [%peer p=path]                                ::  subscribe
          [%poke p=term q=*]                            ::  apply
          [%pull ~]                                     ::  unsubscribe
      ==                                                ::
    ++  club                                            ::  action, system
      $%  [%peer p=path]                                ::  subscribe
          [%poke p=cage]                                ::  apply
          [%pull ~]                                     ::  unsubscribe
      ==                                                ::
    ++  card                                            ::  general card
      $%  [%diff %sole-effect sole-effect]              ::
          [%send wire [ship term] clap]                 ::
          [%hiss wire mark [%purl purl]]                ::
          [%exec wire @p (unit ,[beak silk])]           ::
          [%deal wire sock term club]                   ::
          [%info wire @p toro]                          ::
      ==                                                ::
    ++  move  (pair bone card)                          ::  user-level move
    ++  sign                                            ::
      $%  [%made p=@uvH q=gage]                         ::
          [%unto p=cuft]                                ::  
      ==                                                ::
    --                                                  ::
!:                                                      ::
::::                                                    ::
  ::                                                    ::
|_  $:  hid=bowl                                        ::  system state
        house                                           ::  program state
    ==                                                  ::
++  he                                                  ::  per session
  |_  [[ost=bone moz=(list move)] session]              ::
  ++  dp                                                ::  dojo parser
    |%  
    ++  dp-command-line  ;~(sfix dp-command (just '\0a'))
    ++  dp-command                                      ::  ++dojo-command
      %+  knee  *dojo-command  |.  ~+
      ;~  pose
        ;~  pfix  bar
          %+  cook
            |=  [a=path b=dojo-config]
            ^-  dojo-command
            [%poke [our.hid %hood] [0 %ge [0 [%cat %hood a]] b]]
          ;~(plug (most fas sym) dp-config)
        ==
      ::
        ;~  plug  (cold %poke col)
          %+  cook
            |=  [a=goal b=(each dojo-source (trel term path dojo-config))]
            ^-  (pair goal dojo-source)
            :-  a
            ?-  -.b
              %&  p.b
              %|  ?+  p.p.b  !!
                    %di  [0 %di [0 [%dog q.a q.p.b]] r.p.b]
                    %ge  [0 %ge [0 [%cat q.a q.p.b]] r.p.b]
                    %sc  [0 %sc [0 [%pig q.a q.p.b]] r.p.b]
                  ==
            == 
          ;~  plug
            dp-goal
            %+  pick  ;~(pfix ace dp-source)
            ;~  plug
              ;~  pose
                (cold %di wut)
                (cold %ge lus)
                (cold %sc pam)
              ==
              (most fas sym)
              dp-config
            ==
          ==
        ==
      ::
        ;~  plug  (cold %verb tis)
          ;~(plug sym (punt ;~(pfix ace dp-source)))
        ==
      ::
        ;~  pfix  fas
          %+  cook
            |=(a=(list twig) `dojo-command`[%verb %dir ~ [0 %ex %clsg a]])
          dp-poor
        ==
      ::
        ;~  plug  (cold %file tar)
          ;~((glue ace) dp-beam dp-source)
        ==
      ::
        ;~  plug  (cold %flat pat)
          ;~((glue ace) (most fas sym) dp-source)
        ==
      ::
        ;~  plug  (cold %pill dot)
          ;~((glue ace) (most fas sym) dp-source)
        ==
      ::
        (stag %show dp-source)
      ==
    ++  dp-source  (stag 0 dp-build)                    ::  ++dojo-source
    ++  dp-build                                        ::  ++dojo-build
      %+  knee  *dojo-build  |.  ~+
      ;~  pose
        ;~(pfix lus ;~(pose (stag %ur auri:epur) (stag %ge dp-model-cat)))
        ;~(plug (cold %di wut) dp-model-dog)
        ;~(plug (cold %fi cab) ;~((glue ace) dp-filter dp-source))
        dp-value
      ==
    :: 
    ++  dp-filter  ;~(pose ;~(sfix sym cab) dp-twig)     ::  ++dojo-filter
    ++  dp-goal                                          ::  ++goal
      %+  cook  |=(a=goal a)
      ;~  pose
        ;~  plug
          ;~(pfix sig fed:ag) 
          ;~(pfix fas sym)
        ==
        (cook |=(a=term `goal`[our.hid a]) sym)
        (easy [our.hid %hood])
      ==
    ++  dp-beam                                         ::  ++beam
      %+  sear  tome
      =+  vez=(vang & dp-path)
      (sear plex:vez (stag %clsg poor:vez))
    ::
    ++  dp-model-cat  ;~(plug dp-server-cat dp-config)  ::  ++dojo-model
    ++  dp-model-dog  ;~(plug dp-server-dog dp-config)  ::  ++dojo-model
    ++  dp-model-pig  ;~(plug dp-server-pig dp-config)  ::  ++dojo-model
    ++  dp-path       (tope he-beam)                    ::  ++path
    ++  dp-server-cat   (stag 0 (stag %cat dp-device))  ::  ++dojo-server
    ++  dp-server-dog   (stag 0 (stag %dog dp-device))  ::  ++dojo-server
    ++  dp-server-pig   (stag 0 (stag %pig dp-device))  ::  ++dojo-server
    ++  dp-twig         tall:(vang & dp-path)           ::  ++twig
    ++  dp-poor         poor:(vang & (tope dir))        ::  (list ++twig)
    ++  dp-device       (most fas sym)                  ::  ++dojo-device
    ++  dp-value                                        ::  ++dojo-source
      ;~  pose
        (stag %tu (ifix [kel ker] (most ace dp-source)))
        (stag %ex dp-twig)
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
      |=  [way=wire kas=silk]
      ^+  +>+>
      ?>  ?=(~ pux)
      (he-card(poy `+>+<(pux `way)) %exec way our.hid `[he-beak kas])
    ::
    ++  dy-eyre                                         ::  send work to eyre
      |=  [way=wire req=[%purl purl]]
      ^+  +>+>
      ?>  ?=(~ pux)
      (he-card(poy `+>+<(pux `way)) %hiss way %httr req)
    ::
    ++  dy-stop                                         ::  stop work
      ^+  +>
      ?~  pux  +>
      (he-card(poy ~) %exec u.pux our.hid ~)
    ::
    ++  dy-slam                                         ::  call by ford
      |=  [way=wire gat=vase sam=vase]
      ^+  +>+>
      (dy-ford way %call [%$ %noun gat] [%$ %noun sam])
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
      ?-  -.mad
        %file  =^(src +>.$ (dy-init-source q.mad) [[%file p.mad src] +>.$])
        %flat  =^(src +>.$ (dy-init-source q.mad) [[%flat p.mad src] +>.$])
        %pill  =^(src +>.$ (dy-init-source q.mad) [[%pill p.mad src] +>.$])
        %poke  =^(src +>.$ (dy-init-source q.mad) [[%poke p.mad src] +>.$])
        %show  =^(src +>.$ (dy-init-source p.mad) [[%show src] +>.$])
        %verb  =^(src +>.$ (dy-init-source-unit q.mad) [[%verb p.mad src] +>.$])
      ==
    ::
    ++  dy-init-source-unit                             ::  (unit dojo-source)
      |=  urc=(unit dojo-source)
      ^+  [urc +>]
      ?~  urc  [~ +>]
      =^  src  +>  (dy-init-source u.urc)
      [`src +>.$]
    ::
    ++  dy-init-source                                  ::  ++dojo-source
      |=  src=dojo-source
      ^+  [src +>]
      =^  bul  +>  (dy-init-build q.src)
      =:  p.src  num
          q.src  bul
        ==
      [src +>.$(num +(num), job (~(put by job) src))]
    ::
    ++  dy-init-build                                   ::  ++dojo-build
      |=  bul=dojo-build
      ^+  [bul +>]
      ?-    -.bul
        %ec  [bul +>.$]
        %ex  [bul +>.$]
        %di  =^(mod +>.$ (dy-init-model p.bul) [[%di mod] +>.$])
        %dv  [bul +>.$]
        %fi  =^  mor  +>.$  (dy-init-source q.bul)
             [bul(q mor) +>.$]
        %ge  =^(mod +>.$ (dy-init-model p.bul) [[%ge mod] +>.$])
        %sc  !!
        %ur  [bul +>.$]
        %tu  =^  dof  +>.$
                 |-  ^+  [p.bul +>.^$]
                 ?~  p.bul  [~ +>.^$]
                 =^  dis  +>.^$  (dy-init-source i.p.bul)
                 =^  mor  +>.^$  $(p.bul t.p.bul)
                 [[dis mor] +>.^$]
             [[%tu dof] +>.$]
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
      [srv +>.$(num +(num), job (~(put by job) num [%dv q.srv]))]
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
      =^  lic  say  (~(transmit sole say) u.per)
      (dy-diff(per ~) %mor [%det lic] [%err q.q.cag] ~)
    ::
    ++  dy-done                                         ::  dialog submit
      |=  txt=tape
      ?.  ?=(^ pro)
        ~&  %dy-no-prompt
        (dy-diff %bel ~)
      (dy-slam /dial u.pro !>(txt))
    ::
    ++  dy-cast
      |*  [typ=_,* bun=vase]
      |=  a=vase  ^-  typ
      ?>  (~(nest ut p.bun) & p.a)
      ;;(typ q.a)
    ::
    ++  dy-over                                         ::  finish construction
      ^+  +>
      ?-    -.mad
          %poke       
        %-  he-card(poy ~)
        :*  %deal
            /poke
            [our.hid p.p.mad]
            q.p.mad
            %poke
            (~(got by rez) p.q.mad)
        ==
      ::
          %file
        %-  he-card(poy ~)  :*
          %info
          /file
          our.hid
          (foal (tope p.mad) (~(got by rez) p.q.mad))
        ==
      ::
          %flat
        =+  out=q.q:(~(got by rez) p.q.mad)
        ?^  out 
          (dy-rash %tan [%leaf "not an atom"]~)
        (dy-rash %sav p.mad out)
      ::
          %pill
        (dy-rash %sag p.mad q.q:(~(got by rez) p.q.mad))
      ::
          %verb
        ?~  q.mad 
          =.  var  (~(del by var) p.mad)
          =<  dy-amok
          ?+  p.mad  . 
            ?(%eny %now %our)  !!
            %lib  .(lib ~)
            %arc  .(arc ~)
            %dir  .(dir [[our.hid %home ud/0] /])
          ==
        =+  cay=(~(got by rez) p.u.q.mad)
        =.  var  (~(put by var) p.mad cay)
        ~|  bad-set/[p.mad p.q.cay]
        =<  dy-amok
        ?+  p.mad  .
          %eny  ~|(%entropy-is-eternal !!)
          %now  ~|(%time-is-immutable !!)
          %our  ~|(%self-is-immutable !!)
          %lib  .(lib ((dy-cast (list hoof) !>(*(list hoof))) q.cay))
          %arc  .(arc ((dy-cast (list hoof) !>(*(list hoof))) q.cay))
          %dir  =.  dir  (need (tome ((dy-cast path !>(*path)) q.cay)))
                =-  +(..dy (he-diff %tan - ~))
                rose/[" " `~]^~[leaf/"=%" (smyt (tope he-beak s.dir))]
        ==
      ::
          %show
        =+  cay=(~(got by rez) p.p.mad)
        %+  dy-rash  %tan
        ?+  p.cay  [(sell q.cay)]~
          %tang  ;;(tang q.q.cay)
          %httr
            =+  hit=;;(httr q.q.cay)
            =-  (flop (turn `wall`- |=(a=tape leaf/(dash:ut a ''))))
            :-  "HTTP {<p.hit>}"
            %+  weld
              (turn q.hit |=([a=@t b=@t] "{(trip a)}: {(trip b)}"))
            (turn `wain`?~(r.hit ~ (lore q.u.r.hit)) trip)
        ==
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
      (dy-slam(per `dat) /edit u.pro !>((tufa buf.say)))
    ::
    ++  dy-type                                         ::  sole action
      |=  act=sole-action
      ?-  -.act
        %det  (dy-edit +.act)
        %ret  (dy-done (tufa buf.say))
      ==
    ::
    ++  dy-cage       |=(num=@ud (~(got by rez) num))   ::  known cage
    ++  dy-vase       |=(num=@ud q:(dy-cage num))       ::  known vase
    ++  dy-silk-vase  |=(vax=vase [%$ %noun vax])  ::  vase to silk
    ++  dy-silk-config                                  ::  configure
      |=  [cag=cage cig=dojo-config]
      ^-  silk
      :+  %ride  [%cnzy %$]
      :+  %mute  [%$ cag]
      ^-  (list (pair wing silk))
      :*  :-  [[~ 12] ~]
          (dy-silk-vase !>([now=now.hid eny=eny.hid bec=he-beak]))
      ::
          :-  [[~ 26] ~]
          %-  dy-silk-vase
          |-  ^-  vase
          ?~  p.cig  !>(~)
          (slop (dy-vase p.i.p.cig) $(p.cig t.p.cig))
      ::
          %+  turn  (~(tap by q.cig))
          |=  [a=term b=(unit dojo-source)]
          ^-  (pair wing silk)
          :-  [a [~ 27] ~]
          %-  dy-silk-vase
          ?~(b !>([~ ~]) (dy-vase p.u.b))
      ==
    ::
    ++  dy-silk-init-modo                               ::  init and config
      |=  [cag=cage cig=dojo-config]
      ^-  silk
      (dy-silk-config cag cig)
    ::
    ++  dy-twig-head                                    ::  dynamic state
      ^-  cage
      :-  %noun
      =+  sloop=|=([a=vase b=vase] ?:(=(*vase a) b ?:(=(*vase b) a (slop a b))))
      %+  sloop
        %-  ~(rep by var)
        |=  [[a=term @ b=vase] c=vase]  ^-  vase
        (sloop b(p face/[a p.b]) c)
      !>(`[our=@p now=@da eny=@uvI]`[our now eny]:hid)
    ::
    ++  dy-made-dial                                    ::  dialog product
      |=  cag=cage
      ^+  +>+>
      ?.  ?=(^ q.q.cag)
        (dy-diff %err q.q.cag)
      =+  tan=((list tank) +2.q.q.cag)
      =.  +>+>.$  (he-diff %tan tan)
      =+  vax=(spec (slot 3 q.cag))
      ?+    -.q.vax  !!
          %& 
        ?~  +.q.vax
          ~&  %dy-made-dial-abort
          (dy-rash %bel ~)
        (dy-meal (slot 7 vax))
      ::
          %|
        =<  he-pone
        %-  dy-diff(pro `(slap (slot 7 vax) [%cnzy %q]))
        =+  pom=(sole-prompt +<.q.vax)
        [%pro pom(cad [':' ' ' cad.pom])]
      ==
    ::
    ++  dy-made-gent                                    ::  generator product
      |=  cag=cage
      (dy-meal q.cag)
    ::
    ++  dy-make                                         ::  build step
      ^+  +>
      ?>  ?=(^ cud)
      =+  bil=q.u.cud                 ::  XX =*
      ?:  ?=(%ur -.bil)
        (dy-eyre /hand %purl p.bil)
      %-  dy-ford
      ^-  (pair path silk)
      ?+  -.bil  !!
        %di  [/dial (dy-silk-init-modo (dy-cage p.p.p.bil) q.p.bil)]
        %ge  [/gent (dy-silk-init-modo (dy-cage p.p.p.bil) q.p.bil)]
        %dv  [/hand [%core he-beak (flop p.bil)]]
        %ec  [/hand [%cast p.bil (dy-mare q.bil)]]
        %ex  [/hand (dy-mare p.bil)]
        %fi  =+  dat=[%$ (dy-cage p.q.bil)]
             [/hand ?@(p.bil [%cast p.bil dat] [%call (dy-mare p.bil) dat])]
        %tu  :-  /hand
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
          |=  gen=twig  ^-  twig
          ?:  ?=(?(%sggl %sggr) -.gen)
            $(gen q.gen)
          =+  ~(open ap gen)
          ?.(=(gen -) $(gen -) gen)
      |=  gen=twig  ^-  (unit mark)
      =.  gen  (ope gen)
      ?:  ?=([%cnts [@ ~] ~] gen)
        (bind (~(get by var) i.p.gen) head)
      ?.  ?=(%dtkt -.gen)  ~
      =.  p.gen  (ope p.gen)
      ?@  -.p.gen  ~
      ?+    ~(feck ap p.p.gen)  ~
          [~ %cx]
        =+  gin=(ope q.p.gen)
        |-
        ?@  -.gin  ~
        =.  q.gin  (ope q.gin)
        ?^  -.q.gin  $(gin q.gin)
        =.  p.gin  (ope p.gin)
        ?.  ?=([[?(%dtzz %dtzy) @ @] [%dtzz %n ~]] gin)
          ~
        (some q.p.gin)
      ==
    ::
    ++  dy-mare                                         ::  build expression
      |=  gen=twig
      ^-  silk
      :+  %cast  (fall (dy-twig-mark gen) %noun)
      :+  %ride  gen
      :-  [%$ dy-twig-head]
      [%plan he-beam / zuse arc lib ~ ~]
    ::
    ++  dy-step                                         ::  advance project
      |=  nex=@ud
      ^+  +>+>
      ?>  ?=(~ cud)
      ?:  =(nex num)
        dy-over
      dy-make(cud `[nex (~(got by job) nex)])
    --
  ::
  ++  he-dope                                           ::  sole user of ++dp
    |=  txt=tape                                        ::
    ^-  (each (unit (each dojo-command tape)) hair)     ::  prefix/result
    =+  len=+((lent txt))                               ::  line length
    =.  txt  :(weld buf txt "\0a")                      ::
    =+  vex=((full dp-command-line):dp [1 1] txt)       ::
    ?:  =(q.p.vex len)                                  ::  matched until line end
      [%& ~]                                            ::
    ?:  =(p.p.vex +((lent (skim txt |=(a=@ =(10 a)))))) ::  parsed all lines
      [%& ~ ?~(q.vex [%| txt] `p.u.q.vex)]              ::  new buffer/complete
    [%| p.p.vex (dec q.p.vex)]                          ::  syntax error
  ::
  ++  he-duke                                           ::  ++he-dope variant
    |=  txt=tape
    ^-  (each (unit (each dojo-command tape)) ,@ud)
    =+  foy=(he-dope txt)
    ?-  -.foy
      %|  [%| q.p.foy]
      %&  [%& p.foy]
    ==
  ::
  ++  he-abet                                           ::  resolve
    [(flop moz) %_(+> hoc (~(put by hoc) ost +<+))]
  ::
  ++  he-beak  -:he-beam
  ++  he-beam                                           ::  logical beam
    ^-  beam
    ?.  =(ud/0 r.dir)  dir
    dir(r [%da now.hid])
  ::
  ++  he-card                                           ::  emit gift
    |=  cad=card
    ^+  +>
    %_(+> moz [[ost cad] moz])
  ::
  ++  he-send
    |=  [way=wire him=ship dap=term cop=clap]
    ^+  +>
    (he-card %send way [him dap] cop)
  ::
  ++  he-diff                                           ::  emit update
    |=  fec=sole-effect
    ^+  +>
    (he-card %diff %sole-effect fec)
  ::
  ++  he-stop                                           ::  abort work
    ^+  .
    ?~(poy . ~(dy-stop dy u.poy))
  ::
  ++  he-peer                                           ::  subscribe to
    |=(pax=path ?>(=(~ pax) he-prom))
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
  ++  he-prom                                           ::  send prompt
    %-  he-diff
    :-  %pro
    [& %$ ?~(buf "> " "< ")]
  ::
  ++  he-made                                           ::  result from ford
    |=  [way=wire dep=@uvH reg=gage]
    ^+  +>
    ?>  ?=(^ poy)
    =<  he-pine
    ?-  -.reg
      %&  %.  p.reg
          =+  dye=~(. dy u.poy(pux ~))
          ?+  way  !!
            [%hand ~]  dy-hand:dye
            [%dial ~]  dy-made-dial:dye
            [%gent ~]  dy-made-gent:dye
            [%edit ~]  dy-made-edit:dye
          ==
      %|  (he-diff(poy ~) %tan p.reg)
      %tabl  !!
    ==
  ::
  ++  he-sigh                                           ::  result from ford
    |=  [way=wire hit=httr]
    ^+  +>
    ?>  ?=(^ poy)
    ?>  ?=([%hand ~] way)             ::  XX options?
    he-pine:(~(dy-hand dy u.poy(pux ~)) %httr !>(hit))
  ::
  ++  he-unto                                           ::  result from behn
    |=  [way=wire cit=cuft]
    ^+  +>
    ?.  ?=(%coup -.cit)
      ~&  [%strange-unto cit]
      +>
    ?~  p.cit  
      (he-diff %txt ">=")
    (he-diff %tan u.p.cit)
  ::
  ++  he-like                                           ::  accept line
    |=  buf=(list ,@c)
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
    =^  lic  say  (~(transmit sole say) dat)
    ::  ~&  [%our-leg leg.say]
    (he-diff %mor [%det lic] [%err q.p.foy] ~)
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
        %|  (he-diff [%err p.doy])
        %&
      ?~  p.doy
        (he-diff [%err (lent txt)])
      =+  old=(weld ?~(buf "> " "  ") (tufa buf.say))
      =^  cal  say  (~(transmit sole say) [%set ~])
      =.  +>.$   (he-diff %mor txt/old nex/~ det/cal ~)
      ?-  -.u.p.doy
        %&  (he-plan(buf ~) p.u.p.doy)
        %|  he-prom(buf p.u.p.doy)
      ==
    ==
  ::
  ++  he-type                                           ::  apply input
    |=  act=sole-action
    ^+  +>
    ?^  poy
      he-pine:(~(dy-type dy u.poy) act)
    ?-  -.act
      %det  (he-stir +.act)
      %ret  (he-done (tufa buf.say))
    ==
  ::
  ++  he-lame                                           ::  handle error
    |=  [wut=term why=tang]
    ^+  +>
    %-  (slog (flop `tang`[>%dojo-lame wut< why]))
    ?^  poy
      he-pine:~(dy-amok dy u.poy)
    he-pine                           ::  XX give mean to original keystroke
  --
::
++  prep
  =+  sosh==+(*session -(poy *(unit)))
  :: ,_`..prep
  |=  old=(unit ?(house [%0 (map bone ,_sosh)]))  ^+  [~ ..prep]
  ?~  old  `..prep
  ?:  ?=(%1 -.u.old)  `..prep(+<+ u.old)
  %=  $
      u.old
    [%1 (~(run by +.u.old) |=(sos=_sosh sos(poy ~)))]
  ==
::
::  pattern:  ++  foo  |=(data he-abet:(~(he-foo he (~(got by hoc) ost)) data))
++  arm  (arm-session (~(got by hoc) ost.hid))
++  arm-session
  |=  ses=session
  =>  ~(. he [ost.hid ~] ses)
  =-  [wrap=- +]
  |*  he-arm=_he-type
  |=  _+<.he-arm  ^-  (quip move ..he)
  he-abet:(he-arm +<)
::
++  peer-sole
  ~?  !=(src.hid our.hid)  [%dojo-peer-stranger ost.hid src.hid]
  ::  ?>  =(src.hid our.hid)
  ~?  (~(has by hoc) ost.hid)  [%dojo-peer-replaced ost.hid]
  =+  ses=%*(. *session -.dir [our.hid %home ud/0])
  (wrap he-peer):(arm-session ses)
::
++  poke-sole-action
  |=  act=sole-action  ~|  poke/act  %.  act
  (wrap he-type):arm
::
++  made       (wrap he-made):arm
++  sigh-httr  (wrap he-sigh):arm
++  sigh-tang  |=([a=wire b=tang] ~|(`term`(cat 3 'sigh-' -.a) (mean b)))
++  lame       (wrap he-lame):arm
++  unto       (wrap he-unto):arm
++  pull
  |=  [pax=path]
  ^-  (quip move +>)
  =^  moz  +>
    he-abet:~(he-stop he [[ost.hid ~] (~(got by hoc) ost.hid)])
  [moz +>.$(hoc (~(del by hoc) ost.hid))]
--
