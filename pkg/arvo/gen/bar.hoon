:: > =h -bar
:: > `@ux`(mug h)
:: 0x41fd.c951
::
::  build /mar/hoon/hoon using arvo from pill
::  produces a vase:h136 (unmigrated old types)
::
/+  pill
:-  %say
|=  [[now=@da eny=@uvJ bec=beak] ~ ~]
=+  .^(pil=pill:pill %cx (en-beam [p.bec %base da+now] /brass/pill))
|^
:-  %noun
=?  pil  ?=(%cash -.pil)
  ^-  $>(%pill pill:pill)
  [%pill +<.pil]
?>  ?=(%pill -.pil)
::
=.  kernel-ova.pil                            ::  filter to /sys
  %+  turn  kernel-ova.pil
  |=  =unix-event:pill
  ^-  unix-event:pill
  ?.  ?=([%what *] q.unix-event)  unix-event
  =/  files=(list (pair path (cask)))
    %+  skim  p.q.unix-event
    |=  [=path *]
    ?=([%sys *] path)
  unix-event(q [%what files])
::
=/  =wynn
  :~  zuse+zuse
      lull+lull
      arvo+arvo
      hoon+hoon-version
      nock+4
  ==
::
~&  >  %booting-arvo
=/  res=toon
  %-  mock
  :_  ~
  :_  [%2 [%0 3] %0 2]
  ;:  weld
    boot-ova.pil
    ^-  (list)
    :~  [*@da //arvo %wack *@uvJ]
        [*@da //arvo %whom *@p]
        [*@da //arvo %wyrd [~.nonce /] wynn]
    ==
    `(list)`(turn kernel-ova.pil (lead *@da))
    `(list)`[*@da [/d/term/1 %boot & %fake *@p]]^~
  ==
::
?.  ?=(%0 -.res)
  ?-  -.res
    %1  ~&  [%blocked p.res]  !!
    %2  ~&  %fail  (mean p.res)
  ==
=/  arv=*  +7.p.res
=/  wish  (wisher arv)
::
::  extract old arvo's compiler tools via wish
::  these operate on h136 types natively
::
~&  >  %extracting-tools
=/  old-slap=*  (wish 'slap')
=/  old-slam=*  (wish 'slam')
=/  old-slub=*  (wish 'slub')
=/  old-slym=*  (wish 'slym')
::
::  get zuse as vase:h136 (NOT migrated to h135)
::
=/  old-zuse=*  (wish '!>(..zuse)')
~&  >  [%old-zuse-mug `@ux`(mug old-zuse)]
::
::  parse ford-cord with our parser to get hoon AST
::
~&  >  %parsing-ford
=/  builder  (slum wish '!>(|=(fc=@t =+(x=(ride -:!>(..zuse) fc) [p.x .*(..zuse q.x)])))')
=/  ford-vase  (slum +3.builder ford-cord)
~&  >  [%mugs `@ux`(mug builder) `@ux`(mug ford-vase)]
::
~&  >  [%ford-vase-mug `@ux`(mug ford-vase)]
::
::  navigate to ++fusion > ++ford
::
=/  fuz=*    (slum old-slub [ford-vase [%limb %fusion]])
=/  fod=*    (slum old-slub [fuz [%limb %ford]])
::
::  construct files map with /mar/hoon/hoon source
::
=/  src=@t  .^(@t %cx (en-beam [p.bec q.bec da+now] /mar/hoon/hoon))
~&  >  [%src-mug `@ux`(mug src)]
=/  fils=(map path (each page lobe:clay))
  %-  ~(gas by *(map path (each page lobe:clay)))
  :~  [/mar/hoon/hoon [%& %hoon src]]
  ==
::
::  call ford gate with args [files file-store verb]
::  using old slym (untyped sample replacement)
::
~&  >  %building
=/  cor=*  (slum old-slym [fod [fils ~ 0]])
::
::  get build-file arm and call it with path
::  wish for the path vase in old arvo to get h136 type metadata
::
=/  bf=*  (slum old-slub [cor [%limb %build-file]])
=/  pax=*  (wish '!>(/mar/hoon/hoon)')
=/  result=*  (slum old-slym [bf /mar/hoon/hoon])
~&  >  [%result-mug `@ux`(mug result)]
result
::
++  wisher
  |=  arv=*
  ?>  ?=(^ arv)
  =/  ton=toon  (mock [arv +10.arv] ~)
  =/  wish-gate=*  ?>(?=(%0 -.ton) p.ton)
  |=  txt=@t
  =/  res  (mule |.((slum wish-gate txt)))
  ?>  ?=(%& -.res)
  p.res
::
++  ford-cord
  '''
  =>  ..zuse
  !:
  ~!  .
  =~
    =/  bud
      ^~
      =/  zuse  !>(..zuse)
      ~&  >  [%virt-mug `@ux`(mug zuse)]
      :*  zuse=zuse
          nave=(slap zuse !,(*hoon nave:clay))
          cork=(slap zuse !,(*hoon cork))
          same=(slap zuse !,(*hoon same))
          mime=(slap zuse !,(*hoon mime))
          cass=(slap zuse !,(*hoon cass:clay))
      ==
    ::  virtualization gates without access to namespace
    ::
    =/  mule  ~(mule vi |)
    =/  mole  ~(mole vi |)
    =/  road
      |*  =(trap *)
      ^+  $:trap
      =/  res  (mule trap)
      ?-  -.res
        %&  p.res
        %|  (mean p.res)
      ==
    ::
    =,  clay
    ~%  %clay-utilities  ..part  ~
    |%
    ++  has-arm
      |=  [arm=@tas =mark core=vase]
      ^-  ?
      ?.  (slob arm p.core)  |
      ?~  rib=(mole |.((slub core [%wing ~[arm]])))  |
      (slob mark p.u.rib)
    ::
    ++  parse-pile
      ~/  %parse-pile
      |=  [pax=path txt=@t]
      ^-  pile
      =/  [=hair res=(unit [=pile =nail])]
        %-  road  |.
        =>  [pile-rule=pile-rule pax=pax txt=txt trip=trip]
        ~>  %memo./clay/pile
        ((pile-rule pax) [1 1] (trip txt))
      ?^  res  pile.u.res
      (report-error pax txt hair)
    ::
    ++  report-error
      |=  [pax=path txt=@t =hair]
      %-  mean
      =/  lyn  p.hair
      =/  col  q.hair
      ^-  (list tank)
      :~  leaf+"syntax error at [{<lyn>} {<col>}] in {<pax>}"
        ::
          =/  =wain  (to-wain:format txt)
          ?:  (gth lyn (lent wain))
            '<<end of file>>'
          (snag (dec lyn) wain)
        ::
          leaf+(runt [(dec col) '-'] "^")
      ==
    ::
    ++  parsing-rules
      |%
      ++  pant
        |*  fel=^rule
        ;~(pose fel (easy ~))
      ::
      ++  mast
        |*  [bus=^rule fel=^rule]
        ;~(sfix (more bus fel) bus)
      ::
      ++  rune
        |*  [bus=^rule fel=^rule]
        %-  pant
        %+  mast  gap
        ;~(pfix fas bus gap fel)
      ::
      ++  taut-rule
        %+  cook  |=(taut +<)
        ;~  pose
          (stag ~ ;~(pfix tar sym))
          ;~(plug (stag ~ sym) ;~(pfix tis sym))
          (cook |=(a=term [`a a]) sym)
        ==
      --
    ::
    ++  pile-rule
      =>  [..lull parsing-rules]
      =,  clay
      |=  pax=path
      %-  full
      %+  ifix
        :_  gay
        ::  parse optional /? and ignore
        ::
        ;~(plug gay (punt ;~(plug fas wut gap dem gap)))
      ;~  plug
        %+  cook  (bake zing (list (list taut)))
        %+  rune  hep
        (most ;~(plug com gaw) taut-rule)
      ::
        %+  cook  (bake zing (list (list taut)))
        %+  rune  lus
        (most ;~(plug com gaw) taut-rule)
      ::
        %+  rune  tis
        ;~(plug sym ;~(pfix gap stap))
      ::
        %+  rune  sig
        ;~((glue gap) sym wyde:vast stap)
      ::
        %+  rune  cen
        ;~(plug sym ;~(pfix gap ;~(pfix cen sym)))
      ::
        %+  rune  buc
        ;~  (glue gap)
          sym
          ;~(pfix cen sym)
          ;~(pfix cen sym)
        ==
      ::
        %+  rune  tar
        ;~  (glue gap)
          sym
          ;~(pfix cen sym)
          ;~(pfix stap)
        ==
      ::
        %+  stag  %tssg
        (most gap tall:(vang & pax))
      ==
    --
    ~%  %clay  +  ~
    |%
    ++  fusion
      ~%  %fusion  ..fusion  ~
      |%
      ++  with-face  |=([face=@tas =vase] vase(p [%face face p.vase]))
      ++  with-faces
        =|  res=(unit vase)
        |=  vaz=(list [face=@tas =vase])
        ^-  vase
        ?~  vaz  (need res)
        =/  faz  (with-face i.vaz)
        =.  res  `?~(res faz (slop faz u.res))
        $(vaz t.vaz)
      ::
      ++  ford
        !.
        =>  |%
            +$  args
              $+  args
              $:  files=(map path (each page lobe))
                  file-store=(map lobe page)
                  verb=@
              ==
            ::
            +$  bush
              $%  [%file =cage]
                  [%hoon text=@t deps=(list (pair (unit term) bush)) =path]
                  [%arch =spec files=(map @ta bush) =path]
                  [%mark grad=(unit (trel bush bush bush)) cor=vase =mark]
                  $:  %tube
                      $=  p
                      $@  ?(%same %mime)  ::  identity / (mime -> hoon)
                      [a=[=mark bush=(unit bush)] b=[=mark bush=(unit bush)]]
              ==  ==
            ::
            +$  bush-node
              $%  [%hoon =path]
                  [%file =mark =path]  :: leaf
                  [%mark =mark]
                  [%tube =mars]
                  [%arch =spec =path]
              ==
            --
        =>  |%
            ++  bush-to-vase
              =/  only-prelude=?  |
              =|  sut=vase
              |=  =bush
              ^-  vase
              =*  b2v-buc  $
              ?-    -.bush
                  %file
                q.cage.bush
              ::
                  %hoon
                =.  sut  zuse.bud
                =;  tus=vase
                  ?:  only-prelude  tus
                  ~>  %memo./clay/ford
                  :: %-  (trace 1 |.("make: hoon: {(spud path.bush)}"))
                  (slub tus hoon:(parse-pile path.bush text.bush))
                =.  only-prelude  |
                ~>  %memo./clay/ford
                |-  ^-  vase
                ?~  deps.bush  sut
                =/  dep=vase  b2v-buc(bush q.i.deps.bush, only-prelude |)
                =?  p.dep  ?=(^ p.i.deps.bush)  [%face u.p.i.deps.bush p.dep]
                $(deps.bush t.deps.bush, sut (slop dep sut))
              ::
                  %arch
                ~>  %memo./clay/ford
                :: %-  (trace 1 |.("make: arch: {(spud path.bush)}"))
                =/  [type-val=type type-map=type]
                  =>  [sut=sut spec=spec.bush ..ut]
                  ~>  %memo./clay/ford
                  :-  (~(play ut p.sut) [%kttr spec])
                  %-  ~(play ut p.sut)
                  [%kttr %make [%wing ~[%map]] ~[[%base %atom %ta] spec]]
                ::
                =.  sut  *vase
                ~>  %memo./clay/ford
                =/  res=(map @ta vase)
                  (~(run by files.bush) bush-to-vase)
                ::
                :-  type-map
                |-
                ?~  res  ~
                ?.  (~(nest ut type-val) | p.q.n.res)
                  ~|  [%nest-fail path.bush p.n.res]
                  !!
                :-  [p.n.res q.q.n.res]
                [$(res l.res) $(res r.res)]
              ::
                  %mark
                =.  sut  *vase
                ~>  %memo./clay/ford
                :: %-  (trace 1 |.("make: mark: %{(trip mark.bush)}"))
                =*  cor  cor.bush
                ?~  grad.bush
                  %+  slub  (slop (with-face cor+cor) zuse.bud)
                  !,  *hoon
                  =/  typ  _+<.cor
                  =/  dif  _*diff:grad:cor
                  ^-  (nave:clay typ dif)
                  |%
                  ++  diff  |=([old=typ new=typ] (diff:~(grad cor old) new))
                  ++  form  form:grad:cor
                  ++  join
                    |=  [a=dif b=dif]
                    ^-  (unit (unit dif))
                    ?:  =(a b)
                      ~
                    `(join:grad:cor a b)
                  ++  mash
                    |=  [a=[=ship =desk =dif] b=[=ship =desk =dif]]
                    ^-  (unit dif)
                    ?:  =(dif.a dif.b)
                      ~
                    `(mash:grad:cor a b)
                  ++  pact  |=([v=typ d=dif] (pact:~(grad cor v) d))
                  ++  vale  noun:grab:cor
                  --
                =/  deg=vase  (bush-to-vase p.u.grad.bush)
                =/  tub=vase  (bush-to-vase q.u.grad.bush)
                =/  but=vase  (bush-to-vase r.u.grad.bush)
                %+  slub
                  (with-faces deg+deg tub+tub but+but cor+cor nave+nave.bud ~)
                !,  *hoon
                =/  typ  _+<.cor
                =/  dif  _*diff:deg
                ^-  (nave typ dif)
                |%
                ++  diff
                  |=  [old=typ new=typ]
                  ^-  dif
                  (diff:deg (tub old) (tub new))
                ++  form  form:deg
                ++  join  join:deg
                ++  mash  mash:deg
                ++  pact
                  |=  [v=typ d=dif]
                  ^-  typ
                  (but (pact:deg (tub v) d))
                ++  vale  noun:grab:cor
                --
              ::
                  %tube
                =.  sut  *vase
                ~>  %memo./clay/ford
                ?@  p.bush
                  ?-    p.bush
                      %same
                    :: %-  (trace 4 |.("make: tube: identity shortcircuit"))
                    same.bud
                  ::
                      %mime
                    :: %-  (trace 4 |.("make: tube: hoon -> mime"))
                    =>(..zuse !>(|=(m=mime q.q.m)))
                  ==
                =/  a  a.p.bush
                =/  b  b.p.bush
                :: %-  (trace 1 |.("make: tube: %{(trip mark.a)} -> %{(trip mark.b)}"))
                =/  old=(unit vase)  (bind bush.a bush-to-vase)
                ?:  &(?=(^ old) (has-arm %grow mark.b u.old))
                  :: %-  (trace 4 |.("+grow:{(trip mark.a)}"))
                  %+  slub  (with-faces cor+u.old ~)
                  :+  %brcl  !,(*hoon v=+<.cor)
                  :+  %sggr
                    [%spin %cltr [%sand %t (crip "grow-{<mark.a>}->{<mark.b>}")] ~]
                  :+  %tsgl  limb/mark.b
                  !,(*hoon ~(grow cor v))
                =/  new=(unit vase)  (bind bush.b bush-to-vase)
                ?:  &(?=(^ new) (has-arm %grab mark.a u.new))
                  :: %-  (trace 4 |.("+grab:{(trip mark.b)}"))
                  =;  v=vase
                    ?^  q.v  v
                    ~_('clay: @ product of +grab not supported' !!)
                  %+  slub  u.new
                  :+  %sggr
                    [%spin %cltr [%sand %t (crip "grab-{<mark.a>}->{<mark.b>}")] ~]
                  tsgl/[limb/mark.a limb/%grab]
                ?:  ?=(%noun mark.b)
                  :: %-  (trace 4 |.("default"))
                  same.bud
                ~|(no-cast-between+[mark.a mark.b] !!)  ::  XX +jump arm
              ::
              ==
            --
        ~%  %ford-gate  ..ford  ~
        |=  args
        ~%  %ford-core  ..$  ~
        |%
        ::  Chapter for constructing $bush (dependency graph of a file) given its
        ::  desk-wide identifier
        ::
        +|  %bush-construction
        ::
        ++  parse-header
          |=  [pax=path txt=@t]
          ^-  (list (pair (unit term) bush-node))
          ~>  %memo./clay/ford
          =*  out  (list (pair (unit term) bush-node))
          =/  [=hair res=(unit [=out =nail])]
            (header-rule [1 1] (trip txt))
          ?^  res  out.u.res
          (report-error pax txt hair)
        ::
        ++  header-rule
          |^
          =,  parsing-rules
          %+  cook  pile-header-to-bush
          %+  ifix
            :_  gay
            ::  parse optional /? and ignore
            ::
            ;~(plug gay (punt ;~(plug fas wut gap dem gap)))
          ;~  plug
            %+  cook  (bake zing (list (list taut)))
            %+  rune  hep
            (most ;~(plug com gaw) taut-rule)
          ::
            %+  cook  (bake zing (list (list taut)))
            %+  rune  lus
            (most ;~(plug com gaw) taut-rule)
          ::
            %+  rune  tis
            ;~(plug sym ;~(pfix gap stap))
          ::
            %+  rune  sig
            ;~((glue gap) sym wyde:vast stap)
          ::
            %+  rune  cen
            ;~(plug sym ;~(pfix gap ;~(pfix cen sym)))
          ::
            %+  rune  buc
            ;~  (glue gap)
              sym
              ;~(pfix cen sym)
              ;~(pfix cen sym)
            ==
          ::
            %+  rune  tar
            ;~  (glue gap)
              sym
              ;~(pfix cen sym)
              ;~(pfix stap)
            ==
          ==
          ::
          ++  pile-header-to-bush
            |=  $:  sur=(list taut)
                    lib=(list taut)
                    raw=(list [face=term =path])
                    raz=(list [face=term =spec =path])
                    maz=(list [face=term =mark])
                    caz=(list [face=term =mars])
                    bar=(list [face=term =mark =path])
                ==
            ^-  (list (pair (unit term) bush-node))
            %-  zing
            ^-  (list (list (pair (unit term) bush-node)))
            :~
              (turn sur (taut-to-bush-node %sur))
              (turn lib (taut-to-bush-node %lib))
              (turn raw |=([face=term =path] [`face hoon+(snoc path %hoon)]))
              (turn raz |=([face=term =spec =path] [`face arch+[spec path]]))
              (turn maz |=([face=term =mark] [`face mark+mark]))
              (turn caz |=([face=term =mars] [`face tube+mars]))
              (turn bar |=([face=term =mark =path] [`face file+[mark path]]))
            ==
          ::
          ++  taut-to-bush-node
            |=  prefix=term
            |=  =taut
            ^-  (pair (unit term) bush-node)
            :-  face.taut
            [%hoon (fit-path prefix pax.taut)]
          --
        ::
        ++  build-bush
          |=  nod=bush-node
          ^-  bush
          ::  the cycle set below catches dependency cycles in the bush,
          ::  but it can't catch cycles which are reentrant through read-file
          ::  or cast-path. for those cases, we use loop detection
          ::  as implemented in the runtime.
          ::  example:
          ::  1. copy a mark (e.g. noun.hoon) as foo.hoon
          ::  2. commit a %foo page directly to clay: *%/foo/foo &foo 42
          ::  3. add a /* import to the mark definition: /*  foo  %foo  /foo/foo
          ::  4. try scrying for that file: .^(* %cx %/foo/foo)
          ::
          ~>  %loop.'clay: loop detected'
          ~>  %memo./clay/ford
          %-  %+  trace  1  |.
              ?-  -.nod
                %hoon  "bush: hoon: {(spud path.nod)}"
                %file  "bush: file: mar=%{(trip mark.nod)} {(spud path.nod)}"
                %mark  "bush: mark: %{(trip mark.nod)}"
                %tube  "bush: tube: %{(trip a.mars.nod)} -> %{(trip b.mars.nod)}"
                %arch  "bush: arch: {(spud path.nod)}"
              ==
          =|  cycle=(set bush-node)
          |-  ^-  bush
          =*  bush-loop  $
          ?:  (~(has in cycle) nod)  ~|  [cycle+nod cycle]  !!
          =.  cycle  (~(put in cycle) nod)
          ?-    -.nod
              %file
            =/  file=cage  (cast-path path.nod mark.nod)
            [%file file]
          ::
              %hoon
            =/  file=cage  (read-file path.nod)
            ?>  =(%hoon p.file)
            =+  !<(src=@t q.file)
            =/  deps=(list (pair (unit term) bush-node))
              (parse-header path.nod src)
            ::
            :^  %hoon  src
              %+  turn  deps
              |=  [u=(unit term) don=bush-node]
              [u bush-loop(nod don)]
            path.nod
          ::
              %mark
            =/  cor=vase  (build-fit %mar mark.nod)
            =/  gad=vase  (slap cor limb/%grad)
            ?^  q.gad  [%mark ~ cor mark.nod]
            =/  deg  bush-loop(nod mark+q.gad)
            =/  tub  bush-loop(nod tube+[mark.nod q.gad])
            =/  but  bush-loop(nod tube+[q.gad mark.nod])
            [%mark `[deg tub but] cor mark.nod]
          ::
              %tube
            ?:  =(a.mars.nod b.mars.nod)  tube+%same
            ?:  =([%mime %hoon] [a.mars.nod b.mars.nod])  tube+%mime
            :+  %tube
              =/  pax=(unit path)  (try-fit-path %mar a.mars.nod)
              [a.mars.nod ?~(pax ~ `bush-loop(nod hoon+u.pax))]
            =/  pax=(unit path)  (try-fit-path %mar b.mars.nod)
            [b.mars.nod ?~(pax ~ `bush-loop(nod hoon+u.pax))]
          ::
              %arch
            =/  fiz=(list @ta)
              =/  len  (lent path.nod)
              %+  murn  ~(tap by files)
              |=  [pax=path *]
              ^-  (unit @ta)
              ?.  =(path.nod (scag len pax))
                ~
              =/  pat  (slag len pax)
              ?:  ?=([@ %hoon ~] pat)
                `i.pat
              ~
            ::
            =|  rez=(map @ta bush)
            |-
            ?~  fiz
              [%arch spec.nod rez path.nod]
            =*  nom=@ta   i.fiz
            =/  pax=path  (weld path.nod nom %hoon ~)
            =/  res=bush  bush-loop(nod hoon+pax)
            $(fiz t.fiz, rez (~(put by rez) nom res))
          ==
        ::
        +|  %external-interface
        ::
        ::  +read-file: retrieve marked, validated file contents at path
        ::
        ++  read-file
          ~/  %read-file
          |=  =path
          ^-  cage
          ~>  %memo./clay/ford
          ~|  %error-validating^path
          %-  (trace 1 |.("read file {(spud path)}"))
          =/  file
            ~|  %file-not-found^path
            (~(got by files) path)
          =/  page
            ?:  ?=(%& -.file)
              p.file
            ~|  %tombstoned-file^path^p.file
            (~(got by file-store) p.file)
          (validate-page path page)
        ::
        ::  +build-nave: build a statically typed mark core
        ::
        ++  build-nave
          ~/  %build-nave
          |=  mak=mark
          ^-  vase
          ~>  %memo./clay/ford
          ~|  %error-building-mark^mak
          (bush-to-vase (build-bush %mark mak))
        ::  +build-dais: build a dynamically typed mark definition
        ::
        ++  build-dais
          ~/  %build-dais
          |=  mak=mark
          ^-  dais
          ~>  %memo./clay/ford
          ~|  %error-building-dais^mak
          =/  nav=vase  (build-nave mak)
          %-  (trace 1 |.("make dais {<mak>}"))
          ^-  dais
          =>  [nav=nav ..zuse]
          ~>  %memo./clay/ford
          |_  sam=vase
          ++  diff
            |=  new=vase
            (slam (slub nav limb/%diff) (slop sam new))
          ++  form  !<(mark (slub nav limb/%form))
          ++  join
            |=  [a=vase b=vase]
            ^-  (unit (unit vase))
            =/  res=vase  (slam (slub nav limb/%join) (slop a b))
            ?~  q.res    ~
            ?~  +.q.res  [~ ~]
            ``(slub res !,(*hoon ?>(?=([~ ~ *] .) u.u)))
          ++  mash
            |=  [a=[=ship =desk diff=vase] b=[=ship =desk diff=vase]]
            ^-  (unit vase)
            =/  res=vase
              %+  slam  (slub nav limb/%mash)
              %+  slop
                :(slop [[%atom %p ~] ship.a] [[%atom %tas ~] desk.a] diff.a)
              :(slop [[%atom %p ~] ship.b] [[%atom %tas ~] desk.b] diff.b)
            ?~  q.res
              ~
            `(slub res !,(*hoon ?>((^ .) u)))
          ++  pact
            |=  diff=vase
            (slam (slub nav limb/%pact) (slop sam diff))
          ++  vale
            |:  noun=q:(slub nav !,(*hoon *vale))
            (slam (slub nav limb/%vale) noun/noun)
          --
        ::  +build-cast: produce gate to convert mark .a to, statically typed
        ::
        ++  build-cast
          ~/  %build-cast
          |=  [a=mark b=mark]
          ^-  vase
          ~>  %memo./clay/ford
          ~|  error-building-cast+[a b]
          (bush-to-vase (build-bush %tube a b))
        ::  +build-tube: produce a $tube mark conversion gate from .a to .b
        ::
        ++  build-tube
          |=  [a=mark b=mark]
          ^-  tube
          ~>  %spin.[%build-tube]  ~>  %spin.[a]  ~>  %spin.[b]
          ~>  %memo./clay/ford
          ~|  error-building-tube+[a b]
          =/  gat=vase  (build-cast a b)
          %-  (trace 1 |.("make tube {<a>} -> {<b>}"))
          =>  [gat=gat ..zuse]
          ~>  %memo./clay/ford
          |=(v=vase (slam gat v))
        ::
        ++  validate-page
          |=  [=path =page]
          ^-  cage
          ~|  validate-page-fail+path^from+p.page
          =/  mak=mark  (head (flop path))
          ?:  =(mak p.page)
            (page-to-cage page)
          =/  [mark vax=vase]  (page-to-cage page)
          =/  =tube  (build-tube p.page mak)
          [mak (tube vax)]
        ::
        ++  page-to-cage
          |=  =page
          ^-  cage
          ?:  =(%hoon p.page)
            [%hoon [%atom %t ~] ;;(@ q.page)]
          ?:  =(%mime p.page)
            [%mime =>([;;(mime q.page) ..zuse] !>(-))]
          =/  =dais  (build-dais p.page)
          :-  p.page
          =>  [dais=dais dat=q.page]
          ~>  %memo./clay/ford
          (vale:dais dat)
        ::
        ++  cast-path
          |=  [=path mak=mark]
          ^-  cage
          ~>  %memo./clay/ford
          =/  mok  (head (flop path))
          ~|  error-casting-path+[path mok mak]
          =/  cag=cage  (read-file path)
          ?:  =(mok mak)
            cag
          =/  =tube  (build-tube mok mak)
          ~|  error-running-cast+[path mok mak]
          [mak (tube q.cag)]
        ::
        ++  prelude
          |=  =path
          ^-  vase
          %*($ bush-to-vase +< (build-bush %hoon path), only-prelude &)
        ::
        ++  build-file
          |=  =path
          ^-  vase
          ~>  %memo./clay/ford
          (bush-to-vase (build-bush %hoon path))
        ::  +build-fit: build file at path, maybe converting '-'s to '/'s in path
        ::
        ++  build-fit
          |=  [pre=@tas pax=@tas]
          ^-  vase
          (build-file (fit-path pre pax))
        ::
        +|  %helpers
        ::
        ::  +fit-path: find path, maybe converting '-'s to '/'s
        ::
        ::    Try '-' before '/', applied left-to-right through the path,
        ::    e.g. 'a-foo/bar' takes precedence over 'a/foo-bar'.
        ::
        ++  fit-path
          |=  [pre=@tas pax=@tas]
          ^-  path
          ~_  leaf/"clay: no files match /{(trip pre)}/{(trip pax)}/hoon"
          (need (try-fit-path pre pax))
        ::
        ::
        ++  try-fit-path
          |=  [pre=@tas pax=@tas]
          ^-  (unit path)
          =/  paz  (segments pax)
          |-  ^-  (unit path)
          ?~  paz  ~
          =/  pux=path  pre^(snoc i.paz %hoon)
          ?:  (~(has by files) pux)
            `pux
          $(paz t.paz)
        ::
        ++  trace
          |=  [pri=@ print=(trap tape)]
          (^trace verb pri print)
        --
      ::
      ++  trace
        |=  [verb=@ pri=@ print=(trap tape)]
        ?:  (lth verb pri)
          same
        (slog leaf+"ford: {(print)}" ~)
      --
    --
  ==
  '''
--
