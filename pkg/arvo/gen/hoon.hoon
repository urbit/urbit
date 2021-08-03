|=  *
~%  %hone  ..part  ~
|%
++  ut
  ~%    %ut
      +>+
    ==
      %ar     ar
      %fan    fan
      %rib    rib
      %vet    vet
      %blow   blow
      %burp   burp
      %busk   busk
      %buss   buss
      %crop   crop
      %duck   duck
      %dune   dune
      %dunk   dunk
      %epla   epla
      %emin   emin
      %emul   emul
      %feel   feel
      %felt   felt
      %fine   fine
      %fire   fire
      %fish   fish
      %fond   fond
      %fund   fund
      %funk   funk
      %fuse   fuse
      %gain   gain
      %lose   lose
      %mile   mile
      %mine   mine
      %mint   mint
      %moot   moot
      %mull   mull
      %nest   nest
      %peel   peel
      %play   play
      %peek   peek
      %repo   repo
      %rest   rest
      %sink   sink
      %tack   tack
      %toss   toss
      %wrap   wrap
    ==
  =+  :*  fan=*(set [type hoon])
          rib=*(set [type type hoon])
          vet=`?`&
      ==
  =+  sut=`type`%noun
  |%
  ++  clip
    |=  ref=type
    ?>  ?|(!vet (nest(sut ref) & sut))
    ref
  ::
  ::  +ar: texture engine
  ::
  ++  ar  !:
    ~%    %ar
        +>
      ==
        %fish  fish
        %gain  gain
        %lose  lose
      ==
    |_  [ref=type =skin]
    ::
    ::  =fish: make a $nock that tests a .ref at .axis for .skin
    ::
    ++  fish
      |=  =axis
      ^-  nock
      ?@  skin  [%1 &]
      ?-    -.skin
      ::
          %base
        ?-  base.skin
          %cell      $(skin [%cell [%base %noun] [%base %noun]])
          %flag      ?:  (~(nest ut bool) | ref)
                       [%1 &]
                     %+  flan
                       $(skin [%base %atom %$])
                     %+  flor
                       [%5 [%0 axis] [%1 &]]
                     [%5 [%0 axis] [%1 |]]
          %noun      [%1 &]
          %null      $(skin [%leaf %n ~])
          %void      [%1 |]
          [%atom *]  ?:  (~(nest ut [%atom %$ ~]) | ref)
                       [%1 &]
                     ?:  (~(nest ut [%cell %noun %noun]) | ref)
                       [%1 |]
                     (flip [%3 %0 axis])
        ==
      ::
          %cell
        ?:  (~(nest ut [%atom %$ ~]) | ref)  [%1 |]
        %+  flan
          ?:  (~(nest ut [%cell %noun %noun]) | ref)
            [%1 &]
          [%3 %0 axis]
        %+  flan
          $(ref (peek(sut ref) %free 2), skin skin.skin)
        $(ref (peek(sut ref) %free 3), skin ^skin.skin)
      ::
          %leaf
        ?:  (~(nest ut [%atom %$ `atom.skin]) | ref)
          [%1 &]
        [%5 [%1 atom.skin] [%0 axis]]
      ::
          %dbug  $(skin skin.skin)
          %help  $(skin skin.skin)
          %name  $(skin skin.skin)
          %over  $(skin skin.skin)
          %spec  $(skin skin.skin)
          %wash  [%1 1]
      ==
    ::
    ::  -gain: make a $type by restricting .ref to .skin
    ::
    ++  gain
      |-  ^-  type
      ?@  skin  [%face skin ref]
      ?-    -.skin
      ::
          %base
        ?-    base.skin
            %cell      $(skin [%cell [%base %noun] [%base %noun]])
            %flag      (fork $(skin [%leaf %f &]) $(skin [%leaf %f |]) ~)
            %null      $(skin [%leaf %n ~])
            %void      %void
            %noun      ?:((~(nest ut %void) | ref) %void ref)
            [%atom *]
          =|  gil=(set type)
          |-  ^-  type
          ?-    ref
            %void      %void
            %noun      [%atom p.base.skin ~]
            [%atom *]  ?.  (fitz p.base.skin p.ref)
                          ~>(%mean.'atom-mismatch' !!)
                       :+  %atom
                         (max p.base.skin p.ref)
                       q.ref
            [%cell *]  %void
            [%core *]  %void
            [%face *]  (face p.ref $(ref q.ref))
            [%fork *]  (fork (turn ~(tap in p.ref) |=(=type ^$(ref type))))
            [%hint *]  (hint p.ref $(ref q.ref))
            [%hold *]  ?:  (~(has in gil) ref)  %void
                       $(gil (~(put in gil) ref), ref repo(sut ref))
          ==
        ==
      ::
          %cell
        =|  gil=(set type)
        |-  ^-  type
        ?-    ref
            %void      %void
            %noun      [%cell %noun %noun]
            [%atom *]  %void
            [%cell *]  =+  ^$(skin skin.skin, ref p.ref)
                       ?:  =(%void -)  %void
                       (cell - ^$(skin ^skin.skin, ref q.ref))
            [%core *]  =+  ^$(skin skin.skin, ref p.ref)
                       ?:  =(%void -)  %void
                       ?.  =(%noun ^skin.skin)
                         (cell - ^$(skin ^skin.skin, ref %noun))
                       [%core - q.ref]
            [%face *]  (face p.ref $(ref q.ref))
            [%fork *]  (fork (turn ~(tap in p.ref) |=(=type ^$(ref type))))
            [%hint *]  (hint p.ref $(ref q.ref))
            [%hold *]  ?:  (~(has in gil) ref)  %void
                       $(gil (~(put in gil) ref), ref repo(sut ref))
        ==
      ::
          %leaf
        =|  gil=(set type)
        |-  ^-  type
        ?-  ref
          %void      %void
          %noun      [%atom aura.skin `atom.skin]
          [%atom *]  ?:  &(?=(^ q.ref) !=(atom.skin u.q.ref))
                       %void
                     ?.  (fitz aura.skin p.ref)
                        ~>(%mean.'atom-mismatch' !!)
                     :+  %atom
                       (max aura.skin p.ref)
                     `atom.skin
          [%cell *]  %void
          [%core *]  %void
          [%face *]  (face p.ref $(ref q.ref))
          [%fork *]  (fork (turn ~(tap in p.ref) |=(=type ^$(ref type))))
          [%hint *]  (hint p.ref $(ref q.ref))
          [%hold *]  ?:  (~(has in gil) ref)  %void
                     $(gil (~(put in gil) ref), ref repo(sut ref))
        ==
      ::
          %dbug  $(skin skin.skin)
          %help  (hint [sut %help help.skin] $(skin skin.skin))
          %name  (face term.skin $(skin skin.skin))
          %over  $(skin skin.skin, sut (~(play ut sut) %wing wing.skin))
          %spec  =/  yon  $(skin skin.skin)
                 =/  hit  (~(play ut sut) ~(example ax spec.skin))
                 ?>  (~(nest ut hit) & yon)
                 hit
          %wash  =-  $(ref (~(play ut ref) -))
                 :-  %wing
                 |-  ^-  wing
                 ?:  =(0 depth.skin)  ~
                 [[%| 0 ~] $(depth.skin (dec depth.skin))]
      ==
    ::
    ::  -lose: make a $type by restricting .ref to exclude .skin
    ::
    ++  lose
      |-  ^-  type
      ?@  skin  [%face skin ref]
      ?-    -.skin
      ::
          %base
        ?-    base.skin
            %cell      $(skin [%cell [%base %noun] [%base %noun]])
            %flag      $(skin [%base %atom %f])
            %null      $(skin [%leaf %n ~])
            %void      ref
            %noun      %void
            [%atom *]
          =|  gil=(set type)
          |-  ^-  type
          ?-    ref
            %void      %void
            %noun      [%cell %noun %noun]
            [%atom *]  %void
            [%cell *]  ref
            [%core *]  ref
            [%face *]  (face p.ref $(ref q.ref))
            [%fork *]  (fork (turn ~(tap in p.ref) |=(=type ^$(ref type))))
            [%hint *]  (hint p.ref $(ref q.ref))
            [%hold *]  ?:  (~(has in gil) ref)  %void
                       $(gil (~(put in gil) ref), ref repo(sut ref))
          ==
        ==
      ::
          %cell
        =|  gil=(set type)
        |-  ^-  type
        ?-    ref
            %void      %void
            %noun      [%atom %$ ~]
            [%atom *]  ref
            [%cell *]  =+  ^$(skin skin.skin, ref p.ref)
                       ?:  =(%void -)  %void
                       (cell - ^$(skin ^skin.skin, ref q.ref))
            [%core *]  =+  ^$(skin skin.skin, ref p.ref)
                       ?:  =(%void -)  %void
                       ?.  =(%noun ^skin.skin)
                         (cell - ^$(skin ^skin.skin, ref %noun))
                       [%core - q.ref]
            [%face *]  (face p.ref $(ref q.ref))
            [%fork *]  (fork (turn ~(tap in p.ref) |=(=type ^$(ref type))))
            [%hint *]  (hint p.ref $(ref q.ref))
            [%hold *]  ?:  (~(has in gil) ref)  %void
                       $(gil (~(put in gil) ref), ref repo(sut ref))
        ==
      ::
          %leaf
        =|  gil=(set type)
        |-  ^-  type
        ?-  ref
          %void      %void
          %noun      %noun
          [%atom *]  ?:  =(q.ref `atom.skin)
                       %void
                     ref
          [%cell *]  ref
          [%core *]  ref
          [%face *]  (face p.ref $(ref q.ref))
          [%fork *]  (fork (turn ~(tap in p.ref) |=(=type ^$(ref type))))
          [%hint *]  (hint p.ref $(ref q.ref))
          [%hold *]  ?:  (~(has in gil) ref)  %void
                     $(gil (~(put in gil) ref), ref repo(sut ref))
        ==
      ::
          %dbug  $(skin skin.skin)
          %help  $(skin skin.skin)
          %name  $(skin skin.skin)
          %over  $(skin skin.skin)
          %spec  $(skin skin.skin)
          %wash  ref
      ==
    --
  ::
  ++  blow
    |=  [gol=type gen=hoon]
    ^-  [type nock]
    =+  pro=(mint gol gen)
    =+  jon=(apex:musk bran q.pro)
    ?:  |(?=(~ jon) ?=(%wait -.u.jon))
      [p.pro q.pro]
    [p.pro %1 p.u.jon]
  ::
  ++  bran
    ~+
    =+  gil=*(set type)
    |-  ~+  ^-  seminoun:musk
    ?-    sut
      %noun      [full/[~ ~ ~] ~]
      %void      [full/[~ ~ ~] ~]
      [%atom *]  ?~(q.sut [full/[~ ~ ~] ~] [full/~ u.q.sut])
      [%cell *]  (combine:musk $(sut p.sut) $(sut q.sut))
      [%core *]  %+  combine:musk
                   p.r.q.sut
                 $(sut p.sut)
      [%face *]  $(sut repo)
      [%fork *]  [full/[~ ~ ~] ~]
      [%hint *]  $(sut repo)
      [%hold *]  ?:  (~(has in gil) sut)
                   [full/[~ ~ ~] ~]
                 $(sut repo, gil (~(put in gil) sut))
    ==
  ::
  ++  burp
    ::  expel undigested seminouns
    ::
    ^-  type
    ~+
    ~=  sut
    ?+  sut      sut
      [%cell *]  [%cell burp(sut p.sut) burp(sut q.sut)]
      [%core *]  :+  %core
                   burp(sut p.sut)
                 :*  p.q.sut
                     burp(sut q.q.sut)
                     :_  q.r.q.sut
                     ?:  ?=([[%full ~] *] p.r.q.sut)
                       p.r.q.sut
                     [[%full ~ ~ ~] ~]
                  ==
      [%face *]  [%face p.sut burp(sut q.sut)]
      [%fork *]  [%fork (~(run in p.sut) |=(type burp(sut +<)))]
      [%hint *]  (hint p.sut burp(sut q.sut))
      [%hold *]  [%hold burp(sut p.sut) q.sut]
    ==
  ::
  ++  busk
    ~/  %busk
    |=  gen=hoon
    ^-  type
    [%face [~ [gen ~]] sut]
  ::
  ++  buss
    ~/  %buss
    |=  [cog=term gen=hoon]
    ^-  type
    [%face [[[cog ~ gen] ~ ~] ~] sut]
  ::
  ++  crop
    ~/  %crop
    |=  ref=type
    =+  bix=*(set [type type])
    =<  dext
    |%
    ++  dext
      ^-  type
      ~_  leaf+"crop"
      ::  ~_  (dunk 'dext: sut')
      ::  ~_  (dunk(sut ref) 'dext: ref')
      ?:  |(=(sut ref) =(%noun ref))
        %void
      ?:  =(%void ref)
        sut
      ?-    sut
          [%atom *]
        ?+  ref      sint
          [%atom *]  ?^  q.sut
                       ?^(q.ref ?:(=(q.ref q.sut) %void sut) %void)
                     ?^(q.ref sut %void)
          [%cell *]  sut
        ==
      ::
          [%cell *]
        ?+  ref      sint
          [%atom *]  sut
          [%cell *]  ?.  (nest(sut p.ref) | p.sut)  sut
                     (cell p.sut dext(sut q.sut, ref q.ref))
        ==
      ::
          [%core *]  ?:(?=(?([%atom *] [%cell *]) ref) sut sint)
          [%face *]  (face p.sut dext(sut q.sut))
          [%fork *]  (fork (turn ~(tap in p.sut) |=(type dext(sut +<))))
          [%hint *]  (hint p.sut dext(sut q.sut))
          [%hold *]  ?<  (~(has in bix) [sut ref])
                     dext(sut repo, bix (~(put in bix) [sut ref]))
          %noun      dext(sut repo)
          %void      %void
      ==
    ::
    ++  sint
      ^-  type
      ?+    ref    !!
        [%core *]  sut
        [%face *]  dext(ref repo(sut ref))
        [%fork *]  =+  yed=~(tap in p.ref)
                   |-  ^-  type
                   ?~  yed  sut
                   $(yed t.yed, sut dext(ref i.yed))
        [%hint *]  dext(ref repo(sut ref))
        [%hold *]  dext(ref repo(sut ref))
      ==
    --
  ::
  ++  cool
    |=  [pol=? hyp=wing ref=type]
    ^-  type
    =+  fid=(find %both hyp)
    ?-  -.fid
      %|  sut
      %&  =<  q
          %+  take  p.p.fid
          |=(a=type ?:(pol (fuse(sut a) ref) (crop(sut a) ref)))
    ==
  ::
  ++  duck  ^-(tank ~(duck us sut))
  ++  dune  |.(duck)
  ++  dunk
    |=  paz=term  ^-  tank
    :+  %palm
      [['.' ~] ['-' ~] ~ ~]
    [[%leaf (mesc (trip paz))] duck ~]
  ::
  ++  elbo
    |=  [lop=palo rig=(list (pair wing hoon))]
    ^-  type
    ?:  ?=(%& -.q.lop)
      |-  ^-  type
      ?~  rig
        p.q.lop
      =+  zil=(play q.i.rig)
      =+  dar=(tack(sut p.q.lop) p.i.rig zil)
      %=  $
        rig      t.rig
        p.q.lop  q.dar
      ==
    =+  hag=~(tap in q.q.lop)
    %-  fire
    |-  ^+  hag
    ?~  rig
      hag
    =+  zil=(play q.i.rig)
    =+  dix=(toss p.i.rig zil hag)
    %=  $
      rig  t.rig
      hag  q.dix
    ==
  ::
  ++  ergo
    |=  [lop=palo rig=(list (pair wing hoon))]
    ^-  (pair type nock)
    =+  axe=(tend p.lop)
    =|  hej=(list (pair axis nock))
    ?:  ?=(%& -.q.lop)
      =-  [p.- (hike axe q.-)]
      |-  ^-  (pair type (list (pair axis nock)))
      ?~  rig
        [p.q.lop hej]
      =+  zil=(mint %noun q.i.rig)
      =+  dar=(tack(sut p.q.lop) p.i.rig p.zil)
      %=  $
        rig      t.rig
        p.q.lop  q.dar
        hej      [[p.dar q.zil] hej]
      ==
    =+  hag=~(tap in q.q.lop)
    =-  [(fire p.-) [%9 p.q.lop (hike axe q.-)]]
    |-  ^-  (pair (list (pair type foot)) (list (pair axis nock)))
    ?~  rig
      [hag hej]
    =+  zil=(mint %noun q.i.rig)
    =+  dix=(toss p.i.rig p.zil hag)
    %=  $
      rig  t.rig
      hag  q.dix
      hej  [[p.dix q.zil] hej]
    ==
  ::
  ++  endo
    |=  [lop=(pair palo palo) dox=type rig=(list (pair wing hoon))]
    ^-  (pair type type)
    ?:  ?=(%& -.q.p.lop)
      ?>  ?=(%& -.q.q.lop)
      |-  ^-  (pair type type)
      ?~  rig
        [p.q.p.lop p.q.q.lop]
      =+  zil=(mull %noun dox q.i.rig)
      =+  ^=  dar
          :-  p=(tack(sut p.q.p.lop) p.i.rig p.zil)
              q=(tack(sut p.q.q.lop) p.i.rig q.zil)
      ?>  =(p.p.dar p.q.dar)
      %=  $
        rig        t.rig
        p.q.p.lop  q.p.dar
        p.q.q.lop  q.q.dar
      ==
    ?>  ?=(%| -.q.q.lop)
    ?>  =(p.q.p.lop p.q.q.lop)
    =+  hag=[p=~(tap in q.q.p.lop) q=~(tap in q.q.q.lop)]
    =-  [(fire p.-) (fire(vet |) q.-)]
    |-  ^-  (pair (list (pair type foot)) (list (pair type foot)))
    ?~  rig
      hag
    =+  zil=(mull %noun dox q.i.rig)
    =+  ^=  dix
        :-  p=(toss p.i.rig p.zil p.hag)
            q=(toss p.i.rig q.zil q.hag)
    ?>  =(p.p.dix p.q.dix)
    %=  $
      rig  t.rig
      hag  [q.p.dix q.q.dix]
    ==
  ::
  ++  bock  (pair axis *)
  ::
  ++  ad
    |%
    ++  arc
      |%
      ++  deft                                          ::  generic
        |%
        ++  bath  *                                     ::  leg match type
        ++  claw  *                                     ::  arm match type
        ++  form  |*([* *] p=+<-)                       ::  attach build state
        ++  skin  |*(p=* p)                             ::  reveal build state
        ++  meat  |*(p=* p)                             ::  remove build state
        --
      ++  make                                          ::  for mint and bent
        |%
        ++  bath  type                                  ::  leg match type
        ++  claw  onyx                                  ::  arm
        ++  form  |*([* *] [p=+<- q=+<+])               ::
        ++  skin  |*([p=* q=*] q)                       ::  unwrap baggage
        ++  meat  |*([p=* q=*] p)                       ::  unwrap filling
        --
      --
    ++  def
      =+  deft:arc
      |@  ++  $
      =>  +<
      |%
      ++  pord  |*(* (form +< *nock))                   ::  wrap mint formula
      ++  rosh  |*(* (form +< *(list pock)))            ::  wrap mint changes
      ++  fleg  _(pord $:bath)                          ::  legmatch + code
      ++  fram  _(pord $:claw)                          ::  armmatch +
      ++  foat  _(rosh $:bath)                          ::  leg with changes
      ++  fult  _(rosh $:claw)                          ::  arm with changes
      --  --
    ++  deb
      =+  deft:arc
      |@  ++  $
      =>  +<
      |%
      ++  pord  |*(* (form +< **))                      ::  wrap bent formula
      ++  rosh  |*(* (form +< *(list bock)))            ::  wrap bent changes
      ++  fleg  _(pord $:bath)                          ::  legmatch + code
      ++  fram  _(pord $:claw)                          ::  armmatch +
      ++  foat  _(rosh $:bath)                          ::  leg with changes
      ++  fult  _(rosh $:claw)                          ::  arm with changes
      --  --
    ::
    ::
    ++  lib
      |%
      ++  deft
        =>  (def deft:arc)
        |%
        ++  halp  ^|(|:([** $:hoon] $:fleg))
        ++  vant
          |%  ++  trep  ^|(|:($:,[bath wing bath] $:,[axis bath]))
              ++  tasp  ^|(|:($:,[[axis bath] fleg foat] $:foat))
              ++  tyle  ^|(|:($:foat $:foat))
          --
        ++  vunt
          |%  ++  trep  ^|(|:($:,[claw wing bath] $:,[axis claw]))
              ++  tasp  ^|(|:($:,[[axis claw] fleg fult] $:fult))
              ++  tyle  ^|(|:($:fult $:foat))
        --  --
      ::
      ++  make
        =>  (def make:arc)
        |%
        ++  halp  |~  [* a=hoon]
                  ^-  fleg
                  (mint %noun a)
        ++  vant
          |%  ++  trep  |:  $:,[a=type b=wing c=type]
                        ^-  [axis type]
                        (tack(sut a) b c)
              ++  tasp  |:  $:,[a=(pair axis type) b=fleg c=foat]
                        ^-  foat
                        [q.a [[p.a (skin b)] (skin c)]]
              ++  tyle  |:($:foat +<)
          --
        ++  vunt
          |%  ++  trep  |:  $:,[a=claw b=wing c=bath]
                        ^-  (pair axis claw)
                        (toss b c a)
              ++  tasp  |:  $:,[a=(pair axis claw) b=fleg c=fult]
                        ^-  fult
                        [q.a [[p.a (skin b)] (skin c)]]
              ++  tyle  |:  $:fult
                        ^-  foat
                        [(fire +<-) +<+]
        --  --
      ::
      ++  bend
        =>  (deb make:arc)
        |%
        ++  halp  |~  [jut=* a=hoon]
                  ^-  fleg
                  (bent jut %noun a)
        ++  vant
          |%  ++  trep  |:  $:,[a=type b=wing c=type]
                        ^-  [axis type]
                        (tack(sut a) b c)
              ++  tasp  |:  $:,[a=(pair axis type) b=fleg c=foat]
                        ^-  foat
                        [q.a [[p.a (skin b)] (skin c)]]
              ++  tyle  |:($:foat +<)
          --
        ++  vunt
          |%  ++  trep  |:  $:,[a=claw b=wing c=bath]
                        ^-  (pair axis claw)
                        (toss b c a)
              ++  tasp  |:  $:,[a=(pair axis claw) b=fleg c=fult]
                        ^-  fult
                        [q.a [[p.a (skin b)] (skin c)]]
              ++  tyle  |:  $:fult
                        ^-  foat
                        [(fire +<-) +<+]
      --  --  --
    ::
    ++  bin
      =+  deft:lib
      |@  ++  $
      =>  +<
      |%
      ++  rame
        =>  vant  |%
            ++  clom  bath
            ++  chog  fleg
            ++  ceut  foat
        --
      ++  gelp
        =>  vunt  |%
            ++  clom  claw
            ++  chog  fram
            ++  ceut  fult
        --
      ++  ecbo  (ecco rame)
      ++  eclo  (ecco gelp)
      ++  ecco
        =+  rame
        |@  ++  $
        =>  +<
        |:  $:,[jut=* rum=clom rig=(list (pair wing hoon))]
        ^-  foat
        %-  tyle
        |-  ^-  ceut
        ?~  rig  (rosh rum)
        =+  mor=$(rig t.rig)
        =+  zil=(halp `*`jut `hoon`q.i.rig)
        =+  dar=(trep (meat mor) p.i.rig (meat zil))
        (tasp dar zil mor)
      --  --  --  --
  ::
  ++  oc
    =+  inc=(bin:ad)
    |@  ++  $
    =>  inc
    |%
    ++  echo
      |:  $:,[jut=* rum=bath rig=(list (pair wing hoon))]
      (ecbo jut rum rig)
    ::
    ++  ecmo
      |:  $:,[jut=* hag=claw rig=(list (pair wing hoon))]
      (eclo jut hag rig)
    --  --
  ::
  ++  atco
    |=  [jut=* lop=palo rig=(list (pair wing hoon))]
    ^-  vase
    ::  ~&  %atco
    =+  cin=(oc (bin:ad bend:lib:ad))
    =.  rig  (flop rig)         ::  XX this unbreaks, void order in devulc
    =+  axe=(tend p.lop)
    ?:  ?=(%& -.q.lop)
      =-  [p.- (bike jut axe q.-)]
      (echo:cin jut p.q.lop rig)
    =/  mec  (ecmo:cin jut ~(tap in q.q.lop) rig)
    ::  ~&  %bikeling
    =/  bic  (bike jut axe q.mec)
    ::  ~&  %bikeled
    ?:  =(%bent .*(bic [%0 (peg p.q.lop 2)]))
      ::  ~&  %hard
      =+  ;;([hud=poly sat=type gen=hoon] .*(bic [%0 (peg p.q.lop 3)]))
      ::  ~&  %easy
      ::  XX set vet to | if hud is wet
      (bent(sut sat) bic %noun gen)
    ::  ~&  %not-so-hard
    ::  ~&  pqlop=p.q.lop
    ::  ~&  %maybe-so-hard
    ::  ~&  easy=had
    [p.mec .*(bic [%9 p.q.lop %1 bic])]
  ::
  ++  etco
    |=  [lop=palo rig=(list (pair wing hoon))]
    ^-  (pair type nock)
    =+  cin=(oc (bin:ad make:lib:ad))
    =.  rig  (flop rig)         ::  XX this unbreaks, void order in devulc
    =+  axe=(tend p.lop)
    ?:  ?=(%& -.q.lop)
      =-  [p.- (hike axe q.-)]
      (echo:cin ~ p.q.lop rig)
    =-  [p.- [%9 p.q.lop (hike axe q.-)]]
    (ecmo:cin ~ ~(tap in q.q.lop) rig)
  ::
  ++  et
    |_  [hyp=wing rig=(list (pair wing hoon))]
    ::
    ++  play
      ^-  type
      =+  lug=(find %read hyp)
      ?:  ?=(%| -.lug)  ~>(%mean.'hoon' ?>(?=(~ rig) p.p.lug))
      (elbo p.lug rig)
    ::
    ++  bent
      |=  [jut=* gol=type]
      ^-  vase
      =+  lug=(find %read hyp)
      ?:  ?=(%| -.lug)  ~>(%mean.'hoon' ?>(?=(~ rig) p.lug))
      =-  ?>(?|(!vet (nest(sut gol) & p.-)) -)
      (atco jut p.lug rig)
    ::
    ++  mint
      |=  gol=type
      ^-  (pair type nock)
      =+  lug=(find %read hyp)
      ?:  ?=(%| -.lug)  ~>(%mean.'hoon' ?>(?=(~ rig) p.lug))
      =-  ?>(?|(!vet (nest(sut gol) & p.-)) -)
      (etco p.lug rig)
    ::
    ++  mull
      |=  [gol=type dox=type]
      ^-  [type type]
      =+  lug=[p=(find %read hyp) q=(find(sut dox) %read hyp)]
      ?:  ?=(%| -.p.lug)
        ?>   &(?=(%| -.q.lug) ?=(~ rig))
        [p.p.p.lug p.p.q.lug]
      ?>  ?=(%& -.q.lug)
      =-  ?>(?|(!vet (nest(sut gol) & p.-)) -)
      (endo [p.p.lug p.q.lug] dox rig)
    --
  ::
  ++  epla
    ~/  %epla
    |=  [hyp=wing rig=(list (pair wing hoon))]
    ^-  type
    ~(play et hyp rig)
  ::
  ++  emin
    ~/  %emin
    |=  [gol=type hyp=wing rig=(list (pair wing hoon))]
    ^-  (pair type nock)
    (~(mint et hyp rig) gol)
  ::
  ++  emul
    ~/  %emul
    |=  [gol=type dox=type hyp=wing rig=(list (pair wing hoon))]
    ^-  (pair type type)
    (~(mull et hyp rig) gol dox)
  ::
  ++  felt  !!
  ::                                                    ::
  ++  feel                                              ::  detect existence
    |=  rot=(list wing)
    ^-  ?
    =.  rot  (flop rot)
    |-  ^-  ?
    ?~  rot  &
    =/  yep  (fond %free i.rot)
    ?~  yep  |
    ?-    -.yep
      %&  %=  $
            rot  t.rot
            sut  p:(fine %& p.yep)
          ==
      %|  ?-  -.p.yep
            %&  |
            %|  %=  $
                  rot  t.rot
                  sut  p:(fine %| p.p.yep)
                ==
    ==    ==
  ::
  ++  fond
    ~/  %fond
    |=  [way=vial hyp=wing]
    =>  |%
        ++  pony                                        ::  raw match
                  $@  ~                                 ::  void
                  %+  each                              ::  natural/abnormal
                    palo                                ::  arm or leg
                  %+  each                              ::  abnormal
                    @ud                                 ::  unmatched
                  (pair type nock)                      ::  synthetic
        --
    ^-  pony
    ?~  hyp
      [%& ~ %& sut]
    =+  mor=$(hyp t.hyp)
    ?-    -.mor
        %|
      ?-    -.p.mor
          %&  mor
          %|
        =+  fex=(mint(sut p.p.p.mor) %noun [%wing i.hyp ~])
        [%| %| p.fex (comb q.p.p.mor q.fex)]
      ==
    ::
        %&
      =.  sut
        =*  lap  q.p.mor
        ?-  -.lap
          %&  p.lap
          %|  (fork (turn ~(tap in q.lap) head))
        ==
      =>  :_  +
          :*  axe=`axis`1
              lon=p.p.mor
              heg=?^(i.hyp i.hyp [%| p=0 q=(some i.hyp)])
          ==
      ?:  ?=(%& -.heg)
        [%& [`p.heg lon] %& (peek way p.heg)]
      =|  gil=(set type)
      =<  $
      |%  ++  here  ?:  =(0 p.heg)
                      [%& [~ `axe lon] %& sut]
                    [%| %& (dec p.heg)]
          ++  lose  [%| %& p.heg]
          ++  stop  ?~(q.heg here lose)
          ++  twin  |=  [hax=pony yor=pony]
                    ^-  pony
                    ~_  leaf+"find-fork"
                    ?:  =(hax yor)  hax
                    ?~  hax  yor
                    ?~  yor  hax
                    ?:  ?=(%| -.hax)
                      ?>  ?&  ?=(%| -.yor)
                              ?=(%| -.p.hax)
                              ?=(%| -.p.yor)
                              =(q.p.p.hax q.p.p.yor)
                          ==
                      :+  %|
                        %|
                      [(fork p.p.p.hax p.p.p.yor ~) q.p.p.hax]
                    ?>  ?=(%& -.yor)
                    ?>  =(p.p.hax p.p.yor)
                    ?:  &(?=(%& -.q.p.hax) ?=(%& -.q.p.yor))
                      :+  %&  p.p.hax
                      [%& (fork p.q.p.hax p.q.p.yor ~)]
                    ?>  &(?=(%| -.q.p.hax) ?=(%| -.q.p.yor))
                    ?>  =(p.q.p.hax p.q.p.yor)
                    =+  wal=(~(uni in q.q.p.hax) q.q.p.yor)
                    :+  %&  p.p.hax
                    [%| p.q.p.hax wal]
          ++  $
            ^-  pony
            ?-    sut
                %void       ~
                %noun       stop
                [%atom *]   stop
                [%cell *]
              ?~  q.heg  here
              =+  taf=$(axe (peg axe 2), sut p.sut)
              ?~  taf  ~
              ?:  |(?=(%& -.taf) ?=(%| -.p.taf))
                taf
              $(axe (peg axe 3), p.heg p.p.taf, sut q.sut)
            ::
                [%core *]
              ?~  q.heg  here
              =^  zem  p.heg
                  =+  zem=(loot u.q.heg q.r.q.sut)
                  ?~  zem  [~ p.heg]
                  ?:(=(0 p.heg) [zem 0] [~ (dec p.heg)])
              ?^  zem
                :+  %&
                  [`axe lon]
                =/  zut  ^-  foot
                         ?-  q.p.q.sut
                           %wet  [%wet q.u.zem]
                           %dry  [%dry q.u.zem]
                         ==
                [%| (peg 2 p.u.zem) [[sut zut] ~ ~]]
              =+  pec=(peel way r.p.q.sut)
              ?.  sam.pec  lose
              ?:  con.pec  $(sut p.sut, axe (peg axe 3))
              $(sut (peek(sut p.sut) way 2), axe (peg axe 6))
            ::
                [%hint *]
              $(sut repo)
            ::
                [%face *]
              ?:  ?=(~ q.heg)  here(sut q.sut)
              =*  zot  p.sut
              ?@  zot
                ?:(=(u.q.heg zot) here(sut q.sut) lose)
              =<  main
              |%
              ++  main
                ^-  pony
                =+  tyr=(~(get by p.zot) u.q.heg)
                ?~  tyr
                  next
                ?~  u.tyr
                  $(sut q.sut, lon [~ lon], p.heg +(p.heg))
                ?.  =(0 p.heg)
                  next(p.heg (dec p.heg))
                =+  tor=(fund way u.u.tyr)
                ?-  -.tor
                  %&  [%& (weld p.p.tor `vein`[~ `axe lon]) q.p.tor]
                  %|  [%| %| p.p.tor (comb [%0 axe] q.p.tor)]
                ==
              ++  next
                |-  ^-  pony
                ?~  q.zot
                  ^$(sut q.sut, lon [~ lon])
                =+  tiv=(mint(sut q.sut) %noun i.q.zot)
                =+  fid=^$(sut p.tiv, lon ~, axe 1, gil ~)
                ?~  fid  ~
                ?:  ?=([%| %& *] fid)
                  $(q.zot t.q.zot, p.heg p.p.fid)
                =/  vat=(pair type nock)
                    ?-    -.fid
                      %&  (fine %& p.fid)
                      %|  (fine %| p.p.fid)
                    ==
                [%| %| p.vat (comb (comb [%0 axe] q.tiv) q.vat)]
              --
            ::
                [%fork *]
              =+  wiz=(turn ~(tap in p.sut) |=(a=type ^$(sut a)))
              ?~  wiz  ~
              |-  ^-  pony
              ?~  t.wiz  i.wiz
              (twin i.wiz $(wiz t.wiz))
            ::
                [%hold *]
              ?:  (~(has in gil) sut)
                ~
              $(gil (~(put in gil) sut), sut repo)
            ==
      --
    ==
  ::
  ++  find
    ~/  %find
    |=  [way=vial hyp=wing]
    ^-  port
    ~_  (show [%c %find] %l hyp)
    =-  ?@  -  !!
        ?-    -<
          %&  [%& p.-]
          %|  ?-  -.p.-
                %|  [%| p.p.-]
                %&  !!
        ==    ==
    (fond way hyp)
  ::
  ++  fund
    ~/  %fund
    |=  [way=vial gen=hoon]
    ^-  port
    =+  hup=~(reek ap gen)
    ?~  hup
      [%| (mint %noun gen)]
    (find way u.hup)
  ::
  ++  fine
    ~/  %fine
    |=  tor=port
    ^-  (pair type nock)
    ?-  -.tor
      %|  p.tor
      %&  =+  axe=(tend p.p.tor)
          ?-  -.q.p.tor
            %&  [`type`p.q.p.tor %0 axe]
            %|  [(fire ~(tap in q.q.p.tor)) [%9 p.q.p.tor %0 axe]]
    ==    ==
  ::
  ++  fire
    |=  hag=(list [p=type q=foot])
    ^-  type
    ?:  ?=([[* [%wet ~ %1]] ~] hag)
      p.i.hag
    %-  fork
    %+  turn
      hag.$
    |=  [p=type q=foot]
    ?.  ?=([%core *] p)
      ~_  (dunk %fire-type)
      ~_  leaf+"expected-fork-to-be-core"
      ~_  (dunk(sut p) %fork-type)
      ~>(%mean.'fire-core' !!)
    :-  %hold
    =+  dox=[%core q.q.p q.p(r.p %gold)]
    ?:  ?=(%dry -.q)
      ::  ~_  (dunk(sut [%cell q.q.p p.p]) %fire-dry)
      ?>  ?|(!vet (nest(sut q.q.p) & p.p))
      [dox p.q]
    ?>  ?=(%wet -.q)
    ::  ~_  (dunk(sut [%cell q.q.p p.p]) %fire-wet)
    =.  p.p  (redo(sut p.p) q.q.p)
    ?>  ?|  !vet
            (~(has in rib) [sut dox p.q])
            !=(** (mull(sut p, rib (~(put in rib) sut dox p.q)) %noun dox p.q))
        ==
    [p p.q]
  ::
  ++  fish
    ~/  %fish
    |=  axe=axis
    =+  vot=*(set type)
    |-  ^-  nock
    ?-  sut
        %void       [%1 1]
        %noun       [%1 0]
        [%atom *]   ?~  q.sut
                      (flip [%3 %0 axe])
                    [%5 [%1 u.q.sut] [%0 axe]]
        [%cell *]
      %+  flan
        [%3 %0 axe]
      (flan $(sut p.sut, axe (peg axe 2)) $(sut q.sut, axe (peg axe 3)))
    ::
        [%core *]   ~>(%mean.'fish-core' !!)
        [%face *]   $(sut q.sut)
        [%fork *]   =+  yed=~(tap in p.sut)
                    |-  ^-  nock
                    ?~(yed [%1 1] (flor ^$(sut i.yed) $(yed t.yed)))
        [%hint *]   $(sut q.sut)
        [%hold *]
      ?:  (~(has in vot) sut)
        ~>(%mean.'fish-loop' !!)
      =>  %=(. vot (~(put in vot) sut))
      $(sut repo)
    ==
  ::
  ++  fuse
    ~/  %fuse
    |=  ref=type
    =+  bix=*(set [type type])
    |-  ^-  type
    ?:  ?|(=(sut ref) =(%noun ref))
      sut
    ?-    sut
        [%atom *]
      ?-    ref
          [%atom *]   =+  foc=?:((fitz p.ref p.sut) p.sut p.ref)
                      ?^  q.sut
                        ?^  q.ref
                          ?:  =(q.sut q.ref)
                            [%atom foc q.sut]
                          %void
                        [%atom foc q.sut]
                      [%atom foc q.ref]
          [%cell *]   %void
          *           $(sut ref, ref sut)
      ==
        [%cell *]
      ?-  ref
        [%cell *]   (cell $(sut p.sut, ref p.ref) $(sut q.sut, ref q.ref))
        *           $(sut ref, ref sut)
      ==
    ::
        [%core *]  $(sut repo)
        [%face *]  (face p.sut $(sut q.sut))
        [%fork *]  (fork (turn ~(tap in p.sut) |=(type ^$(sut +<))))
        [%hint *]  (hint p.sut $(sut q.sut))
        [%hold *]
      ?:  (~(has in bix) [sut ref])
        ~>(%mean.'fuse-loop' !!)
      $(sut repo, bix (~(put in bix) [sut ref]))
    ::
        %noun       ref
        %void       %void
    ==
  ::
  ++  gain
    ~/  %gain
    |=  gen=hoon  ^-  type
    (chip & gen)
  ::
  ++  hamp
    ::  generate formula from foot
    ::
    |=  [hud=poly gol=type gen=hoon]
    ^-  ^
    ~+
    =+  %hemp-141
    [%bent hud sut gen]
  ::
  ++  hemp
    ::  generate formula from foot
    ::
    |=  [hud=poly gol=type gen=hoon]
    ^-  nock
    ~+
    =+  %hemp-141
    ?-  hud
      %dry  q:(mint gol gen)
      %wet  q:(mint(vet |) gol gen)
    ==
  ::
  ++  laze
    ::  produce lazy core generator for static execution
    ::
    |=  [nym=(unit term) hud=poly dom=(map term tome)]
    ~+
    ^-  seminoun
    =+  %hemp-141
    ::  tal: map from battery axis to foot
    ::
    =;  tal=(map @ud hoon)
      ::  produce lazy battery
      ::
      :_  ~
      :+  %lazy  1
      |=  axe=@ud
      ^-  (unit noun)
      %+  bind  (~(get by tal) axe)
      |=  gen=hoon
      %.  [hud %noun gen]
      hemp(sut (core sut [nym hud %gold] sut [[%lazy 1 ..^$] ~] dom))
    ::
    %-  ~(gas by *(map @ud hoon))
    =|  yeb=(list (pair @ud hoon))
    =+  axe=1
    |^  ?-  dom
          ~        yeb
          [* ~ ~]  (chapter q.q.n.dom)
          [* * ~]  %=  $
                     dom  l.dom
                     axe  (peg axe 3)
                     yeb  (chapter(axe (peg axe 2)) q.q.n.dom)
                   ==
          [* ~ *]  %=  $
                     dom  r.dom
                     axe  (peg axe 3)
                     yeb  (chapter(axe (peg axe 2)) q.q.n.dom)
                   ==
          [* * *]  %=  $
                     dom  r.dom
                     axe  (peg axe 7)
                     yeb  %=  $
                            dom  l.dom
                            axe  (peg axe 6)
                            yeb  (chapter(axe (peg axe 2)) q.q.n.dom)
        ==         ==     ==
    ++  chapter
      |=  dab=(map term hoon)
      ^+  yeb
      ?-  dab
        ~        yeb
        [* ~ ~]  [[axe q.n.dab] yeb]
        [* * ~]  %=  $
                   dab  l.dab
                   axe  (peg axe 3)
                   yeb  [[(peg axe 2) q.n.dab] yeb]
                 ==
        [* ~ *]  %=  $
                   dab  r.dab
                   axe  (peg axe 3)
                   yeb  [[(peg axe 2) q.n.dab] yeb]
                 ==
        [* * *]  %=  $
                   dab  r.dab
                   axe  (peg axe 7)
                   yeb  %=  $
                          dab  l.dab
                          axe  (peg axe 6)
                          yeb  [[(peg axe 2) q.n.dab] yeb]
      ==         ==     ==
    --
  ::
  ++  lose
    ~/  %lose
    |=  gen=hoon  ^-  type
    (chip | gen)
  ::
  ++  chip
    ~/  %chip
    |=  [how=? gen=hoon]  ^-  type
    ?:  ?=([%wtts *] gen)
      (cool how q.gen (play ~(example ax p.gen)))
    ?:  ?=([%wthx *] gen)
      =+  (play %wing q.gen)
      ~>  %slog.[0 [%leaf "chipping"]]
      ?:  how
        =-  ~>  %slog.[0 (dunk(sut +<) 'chip: gain: ref')]
            ~>  %slog.[0 (dunk(sut -) 'chip: gain: gain')]
            -
        ~(gain ar - p.gen)
      ~(lose ar - p.gen)
    ?:  ?&(how ?=([%wtpm *] gen))
      |-(?~(p.gen sut $(p.gen t.p.gen, sut ^$(gen i.p.gen))))
    ?:  ?&(!how ?=([%wtbr *] gen))
      |-(?~(p.gen sut $(p.gen t.p.gen, sut ^$(gen i.p.gen))))
    =+  neg=~(open ap gen)
    ?:(=(neg gen) sut $(gen neg))
  ::
  ++  bake
    |=  [dox=type hud=poly dab=(map term hoon)]
    ^-  *
    ?:  ?=(~ dab)
      ~
    =+  ^=  dov
        ::  this seems wrong but it's actually right
        ::
        ?-  hud
          %dry  (mull %noun dox q.n.dab)
          %wet  ~
        ==
    ?-  dab
      [* ~ ~]  dov
      [* ~ *]  [dov $(dab r.dab)]
      [* * ~]  [dov $(dab l.dab)]
      [* * *]  [dov $(dab l.dab) $(dab r.dab)]
    ==
  ::
  ++  balk
    |=  [dox=type hud=poly dom=(map term tome)]
    ^-  *
    ?:  ?=(~ dom)
      ~
    =+  dov=(bake dox hud q.q.n.dom)
    ?-    dom
      [* ~ ~]   dov
      [* ~ *]   [dov $(dom r.dom)]
      [* * ~]   [dov $(dom l.dom)]
      [* * *]   [dov $(dom l.dom) $(dom r.dom)]
    ==
  ::
  ++  mile
    ::  mull all chapters and feet in a core
    ::
    |=  [dox=type mel=vair nym=(unit term) hud=poly dom=(map term tome)]
    ^-  (pair type type)
    =+  yet=(core sut [nym hud %gold] sut (laze nym hud dom) dom)
    =+  hum=(core dox [nym hud %gold] dox (laze nym hud dom) dom)
    =+  (balk(sut yet) hum hud dom)
    [yet hum]
  ::
  ++  mane
    ::  mint all chapters and feet in a core
    ::
    |=  [gol=type mel=vair nym=(unit term) hud=poly dom=(map term tome)]
    ^-  vase
    |^
    =/  log  (chapters-check (core-check gol))
    =/  dog  (get-tomes log)
    =-  :_  dez
        (core sut [nym hud mel] sut [[%full ~] %1 dez] dom)
    ^=  dez
    =.  sut  (core sut [nym hud %gold] sut (laze nym hud dom) dom)
    |-  ^-  ?(~ ^)
    ?:  ?=(~ dom)
      ~
    =/  dov=?(~ ^)
      =/  dab=(map term hoon)  q.q.n.dom
      =/  dag  (arms-check dab (get-arms dog p.n.dom))
      |-  ^-  ?(~ ^)
      ?:  ?=(~ dab)
        ~
      =/  gog  (get-arm-type log dag p.n.dab)
      =+  vad=(hamp hud gog q.n.dab)
      ?-    dab
        [* ~ ~]   vad
        [* ~ *]   [vad $(dab r.dab)]
        [* * ~]   [vad $(dab l.dab)]
        [* * *]   [vad $(dab l.dab) $(dab r.dab)]
      ==
    ?-    dom
      [* ~ ~]   dov
      [* ~ *]   [dov $(dom r.dom)]
      [* * ~]   [dov $(dom l.dom)]
      [* * *]   [dov $(dom l.dom) $(dom r.dom)]
    ==
    ::
    ::  all the below arms are used for gol checking and should have no
    ::  effect other than giving more specific errors
    ::
    ::  all the possible types we could be expecting.
    ::
    +$  gol-type
      $~  %noun
      $@  %noun
      $%  [%cell p=type q=type]
          [%core p=type q=coil]
          [%fork p=(set gol-type)]
      ==
    ::  check that we're looking for a core
    ::
    ++  core-check
      |=  log=type
      |-  ^-  gol-type
      ?+    log  $(log repo(sut log))
          %noun      (nice log &)
          %void      (nice %noun |)
          [%atom *]  (nice %noun |)
          [%cell *]  (nice log (nest(sut p.log) & %noun))
          [%core *]  (nice log(r.p.q %gold) &)
          [%fork *]
        =/  tys  ~(tap in p.log)
        :-  %fork
        |-  ^-  (set gol-type)
        ?~  tys
          ~
        =/  a  ^$(log i.tys)
        =/  b  $(tys t.tys)
        (~(put in b) a)
      ==
    ::  check we have the expected number of chapters
    ::
    ++  chapters-check
      |=  log=gol-type
      |-  ^-  gol-type
      ?-    log
          %noun      (nice log &)
          [%cell *]  (nice log &)
          [%core *]  ~_  leaf+"core-number-of-chapters"
                     (nice log =(~(wyt by dom) ~(wyt by q.r.q.log)))
          [%fork *]
        =/  tys  ~(tap in p.log)
        |-  ^-  gol-type
        ?~  tys
          log
        =/  a  ^$(log i.tys)
        =/  b  $(tys t.tys)
        log
      ==
    ::  get map of tomes if exists
    ::
    ++  get-tomes
      |=  log=gol-type
      ^-  (unit (map term tome))
      ?-    log
          %noun      ~
          [%cell *]  ~
          [%fork *]  ~  ::  maybe could be more aggressive
          [%core *]  `q.r.q.log
      ==
    ::  get arms in tome
    ::
    ++  get-arms
      |=  [dog=(unit (map term tome)) nam=term]
      ^-  (unit (map term hoon))
      %+  bind  dog
      |=  a=(map term tome)
      ~_  leaf+"unexpcted-chapter.{(trip nam)}"
      q:(~(got by a) nam)
    ::  check we have the expected number of arms
    ::
    ++  arms-check
      |=  [dab=(map term hoon) dag=(unit (map term hoon))]
      ?~  dag
        dag
      =/  a
        =/  exp  ~(wyt by u.dag)
        =/  hav  ~(wyt by dab)
        ~_  =/  expt  (scow %ud exp)
            =/  havt  (scow %ud hav)
            leaf+"core-number-of-arms.exp={expt}.hav={havt}"
        ~_  =/  missing  ~(tap in (~(dif in ~(key by u.dag)) ~(key by dab)))
            leaf+"missing.{<missing>}"
        ~_  =/  extra  ~(tap in (~(dif in ~(key by dab)) ~(key by u.dag)))
            leaf+"extra.{<extra>}"
        ~_  =/  have  ~(tap in ~(key by dab))
            leaf+"have.{<have>}"
        (nice dag =(exp hav))
      a
    ::  get expected type of this arm
    ::
    ++  get-arm-type
      |=  [log=gol-type dag=(unit (map term hoon)) nam=term]
      ^-  type
      %-  fall  :_  %noun
      %+  bind  dag
      |=  a=(map term hoon)
      =/  gen=hoon
        ~_  leaf+"unexpected-arm.{(trip nam)}"
        (~(got by a) nam)
      (play(sut log) gen)
    ::
    ++  nice
      |*  [typ=* gud=?]
      ?:  gud
        typ
      ~_  leaf+"core-nice"
      !!
    --
  ++  mine
    ::  mint all chapters and feet in a core
    ::
    |=  [gol=type mel=vair nym=(unit term) hud=poly dom=(map term tome)]
    ^-  (pair type nock)
    |^
    =/  log  (chapters-check (core-check gol))
    =/  dog  (get-tomes log)
    =-  :_  [%1 dez]
        (core sut [nym hud mel] sut [[%full ~] dez] dom)
    ^=  dez
    =.  sut  (core sut [nym hud %gold] sut (laze nym hud dom) dom)
    |-  ^-  ?(~ ^)
    ?:  ?=(~ dom)
      ~
    =/  dov=?(~ ^)
      =/  dab=(map term hoon)  q.q.n.dom
      =/  dag  (arms-check dab (get-arms dog p.n.dom))
      |-  ^-  ?(~ ^)
      ?:  ?=(~ dab)
        ~
      =/  gog  (get-arm-type log dag p.n.dab)
      =+  vad=(hemp hud gog q.n.dab)
      ?-    dab
        [* ~ ~]   vad
        [* ~ *]   [vad $(dab r.dab)]
        [* * ~]   [vad $(dab l.dab)]
        [* * *]   [vad $(dab l.dab) $(dab r.dab)]
      ==
    ?-    dom
      [* ~ ~]   dov
      [* ~ *]   [dov $(dom r.dom)]
      [* * ~]   [dov $(dom l.dom)]
      [* * *]   [dov $(dom l.dom) $(dom r.dom)]
    ==
    ::
    ::  all the below arms are used for gol checking and should have no
    ::  effect other than giving more specific errors
    ::
    ::  all the possible types we could be expecting.
    ::
    +$  gol-type
      $~  %noun
      $@  %noun
      $%  [%cell p=type q=type]
          [%core p=type q=coil]
          [%fork p=(set gol-type)]
      ==
    ::  check that we're looking for a core
    ::
    ++  core-check
      |=  log=type
      |-  ^-  gol-type
      ?+    log  $(log repo(sut log))
          %noun      (nice log &)
          %void      (nice %noun |)
          [%atom *]  (nice %noun |)
          [%cell *]  (nice log (nest(sut p.log) & %noun))
          [%core *]  (nice log(r.p.q %gold) &)
          [%fork *]
        =/  tys  ~(tap in p.log)
        :-  %fork
        |-  ^-  (set gol-type)
        ?~  tys
          ~
        =/  a  ^$(log i.tys)
        =/  b  $(tys t.tys)
        (~(put in b) a)
      ==
    ::  check we have the expected number of chapters
    ::
    ++  chapters-check
      |=  log=gol-type
      |-  ^-  gol-type
      ?-    log
          %noun      (nice log &)
          [%cell *]  (nice log &)
          [%core *]  ~_  leaf+"core-number-of-chapters"
                     (nice log =(~(wyt by dom) ~(wyt by q.r.q.log)))
          [%fork *]
        =/  tys  ~(tap in p.log)
        |-  ^-  gol-type
        ?~  tys
          log
        =/  a  ^$(log i.tys)
        =/  b  $(tys t.tys)
        log
      ==
    ::  get map of tomes if exists
    ::
    ++  get-tomes
      |=  log=gol-type
      ^-  (unit (map term tome))
      ?-    log
          %noun      ~
          [%cell *]  ~
          [%fork *]  ~  ::  maybe could be more aggressive
          [%core *]  `q.r.q.log
      ==
    ::  get arms in tome
    ::
    ++  get-arms
      |=  [dog=(unit (map term tome)) nam=term]
      ^-  (unit (map term hoon))
      %+  bind  dog
      |=  a=(map term tome)
      ~_  leaf+"unexpcted-chapter.{(trip nam)}"
      q:(~(got by a) nam)
    ::  check we have the expected number of arms
    ::
    ++  arms-check
      |=  [dab=(map term hoon) dag=(unit (map term hoon))]
      ?~  dag
        dag
      =/  a
        =/  exp  ~(wyt by u.dag)
        =/  hav  ~(wyt by dab)
        ~_  =/  expt  (scow %ud exp)
            =/  havt  (scow %ud hav)
            leaf+"core-number-of-arms.exp={expt}.hav={havt}"
        ~_  =/  missing  ~(tap in (~(dif in ~(key by u.dag)) ~(key by dab)))
            leaf+"missing.{<missing>}"
        ~_  =/  extra  ~(tap in (~(dif in ~(key by dab)) ~(key by u.dag)))
            leaf+"extra.{<extra>}"
        ~_  =/  have  ~(tap in ~(key by dab))
            leaf+"have.{<have>}"
        (nice dag =(exp hav))
      a
    ::  get expected type of this arm
    ::
    ++  get-arm-type
      |=  [log=gol-type dag=(unit (map term hoon)) nam=term]
      ^-  type
      %-  fall  :_  %noun
      %+  bind  dag
      |=  a=(map term hoon)
      =/  gen=hoon
        ~_  leaf+"unexpected-arm.{(trip nam)}"
        (~(got by a) nam)
      (play(sut log) gen)
    ::
    ++  nice
      |*  [typ=* gud=?]
      ?:  gud
        typ
      ~_  leaf+"core-nice"
      !!
    --
  ::
  ++  bent
    ~/  %bent
    |=  [jut=noun gol=type gen=hoon]
    |^  ^-  vase
    %-  (slog [%palm [" : " ~ ~ ~] (sell sut jut) ?@(-.gen -.gen %cons) ~] ~)
    ?:  ?&(=(%void sut) !?=([%dbug *] gen))
      ?.  |(!vet ?=([%lost *] gen) ?=([%zpzp *] gen))
        ~>(%mean.'mint-vain' !!)
      ~>  %mean.'bent-vain'
      [%void !!]
    ?-    gen
    ::
        [^ *]
      =+  hed=$(gen p.gen, gol %noun)
      =+  tal=$(gen q.gen, gol %noun)
      [(nice [%cell p.hed p.tal]) [q.hed q.tal]]
    ::
        [%brcn *]  (grow %gold p.gen %dry [%$ 1] q.gen)
        [%brpt *]  (grow %gold p.gen %wet [%$ 1] q.gen)
        [%cnts *]  (~(bent et p.gen q.gen) jut gol)
        [%dtkt *]  pile
        [%dtls *]  [(nice [%atom %$ ~]) +((@ q:$(gen p.gen, gol [%atom %$ ~])))]
        [%sand *]  [(nice (play gen)) q.gen]
        [%rock *]  [(nice (play gen)) q.gen]
        [%dttr *]
      [(nice %noun) .*(q:$(gen p.gen, gol %noun) q:$(gen q.gen, gol %noun))]
    ::
        [%dtts *]
      =+  [one two]=[$(gen p.gen, gol %noun) $(gen q.gen, gol %noun)]
      [(nice bool) =(q.one q.two)]
    ::
        [%dtwt *]  [(nice bool) .?(q:$(gen p.gen, gol %noun))]
        [%hand *]  pile
        [%ktbr *]  =+(vat=$(gen p.gen) [(nice (wrap(sut p.vat) %iron)) q.vat])
        [%ktls *]
      =+(hif=(nice (play p.gen)) [hif q:$(gen q.gen, gol hif)])
    ::
        [%ktpm *]  =+(vat=$(gen p.gen) [(nice (wrap(sut p.vat) %zinc)) q.vat])
        [%ktsg *]  $(gen p.gen)
        [%tune *]  [(face p.gen sut) jut]
        [%ktwt *]  =+(vat=$(gen p.gen) [(nice (wrap(sut p.vat) %lead)) q.vat])
    ::
        [%note *]
      =+  hum=$(gen q.gen)
      [(hint [sut p.gen] p.hum) q.hum]
    ::
        [%sgzp *]  ~_(duck(sut (play p.gen)) $(gen q.gen))
        [%sggr *]  ::  !!  hmm how to jet
      ?:  ?=([%fast *] p.gen)
        pile
      $(gen q.gen)
    ::
        [%tsgr *]
      =+  fid=$(gen p.gen, gol %noun)
      $(sut p.fid, jut q.fid, gen q.gen)
    ::
        [%tscm *]
      $(gen q.gen, sut (busk p.gen))
    ::
        [%wtcl *]
      =+  nor=$(gen p.gen, gol bool)
      =+  fex=(gain p.gen)
      =+  wux=(lose p.gen)
      =+  hiq=(play(sut fex) q.gen)
      =+  ran=(play(sut wux) r.gen)
      :-  (fork hiq ran ~)
      ?:  =(0 q.nor)
        q:$(sut fex, gen q.gen)
      q:$(sut wux, gen r.gen)
    ::
        [%wthx *]  pile
        [%fits *]  pile
    ::
        [%dbug *]
      ~_  (show %o p.gen)
      $(gen q.gen)
    ::
        [%zpcm *]  [(nice (play p.gen)) q.gen]          ::  XX validate!
        [%lost *]  pile
        [%zpmc *]
      =+  vos=$(gol %noun, gen q.gen)
      =+  ref=p:$(gol %noun, gen p.gen)
      [(nice (cell ref p.vos)) burp(sut p.vos) q.vos]
    ::
        [%zpgl *]
      :-  (nice (play [%kttr p.gen]))
      =<  q
      %_    $
          gol  %noun
          gen
        :^    %wtcl
            :+  %cncl  [%limb %levi]
            :~  [%tsgr [%zpgr [%kttr p.gen]] [%$ 2]]
                [%tsgr q.gen [%$ 2]]
            ==
          [%tsgr q.gen [%$ 3]]
        [%zpzp ~]
      ==
    ::
        [%zpts *]  pile
        [%zppt *]  ?:((feel p.gen) $(gen q.gen) $(gen r.gen))
    ::
        [%zpzp ~]  ~>(%mean.'bent-zap' !!)
        *
      =+  doz=~(open ap gen)
      ?:  =(doz gen)
        ~_  (show [%c 'hoon'] [%q gen])
        ~>(%mean.'mint-open' !!)
      $(gen doz)
    ==
    ::
    ++  pile
      ^-  vase
      =/  gun  (mint gol gen)
      [p.gun .*(jut q.gun)]
    ::
    ++  nice
      |=  typ=type
      ~_  leaf+"mint-nice"
      ?>  ?|(!vet (nest(sut gol) & typ))
      typ
    ::
    ++  grow
      |=  [mel=vair nym=(unit term) hud=poly ruf=hoon dom=(map term tome)]
      ^-  vase
      =+  dan=^$(gen ruf, gol %noun)
      =+  pul=(mane gol mel nym hud dom)
      [(nice p.pul) q.pul q.dan]
    --
  ::
  ++  mint
    ~/  %mint
    |=  [gol=type gen=hoon]
    ^-  [p=type q=nock]
    ::~&  %pure-mint
    |^  ^-  [p=type q=nock]
    ?:  ?&(=(%void sut) !?=([%dbug *] gen))
      ?.  |(!vet ?=([%lost *] gen) ?=([%zpzp *] gen))
        ~>(%mean.'mint-vain' !!)
      [%void %0 0]
    ?-    gen
    ::
        [^ *]
      =+  hed=$(gen p.gen, gol %noun)
      =+  tal=$(gen q.gen, gol %noun)
      [(nice (cell p.hed p.tal)) (cons q.hed q.tal)]
    ::
        [%brcn *]  (grow %gold p.gen %dry [%$ 1] q.gen)
        [%brpt *]  (grow %gold p.gen %wet [%$ 1] q.gen)
    ::
        [%cnts *]  (~(mint et p.gen q.gen) gol)
    ::
        [%dtkt *]
      =+  nef=$(gen [%kttr p.gen])
      [p.nef [%12 [%1 hoon-version p.nef] q:$(gen q.gen, gol %noun)]]
    ::
        [%dtls *]  [(nice [%atom %$ ~]) [%4 q:$(gen p.gen, gol [%atom %$ ~])]]
        [%sand *]  [(nice (play gen)) [%1 q.gen]]
        [%rock *]  [(nice (play gen)) [%1 q.gen]]
    ::
        [%dttr *]
      [(nice %noun) [%2 q:$(gen p.gen, gol %noun) q:$(gen q.gen, gol %noun)]]
    ::
        [%dtts *]
      =+  [one two]=[$(gen p.gen, gol %noun) $(gen q.gen, gol %noun)]
      [(nice bool) [%5 q:$(gen p.gen, gol %noun) q:$(gen q.gen, gol %noun)]]
    ::
        [%dtwt *]  [(nice bool) [%3 q:$(gen p.gen, gol %noun)]]
        [%hand *]  [p.gen q.gen]
        [%ktbr *]  =+(vat=$(gen p.gen) [(nice (wrap(sut p.vat) %iron)) q.vat])
    ::
        [%ktls *]
      =+(hif=(nice (play p.gen)) [hif q:$(gen q.gen, gol hif)])
    ::
        [%ktpm *]  =+(vat=$(gen p.gen) [(nice (wrap(sut p.vat) %zinc)) q.vat])
        [%ktsg *]  (blow gol p.gen)
        [%tune *]  [(face p.gen sut) [%0 %1]]
        [%ktwt *]  =+(vat=$(gen p.gen) [(nice (wrap(sut p.vat) %lead)) q.vat])
    ::
        [%note *]
      =+  hum=$(gen q.gen)
      [(hint [sut p.gen] p.hum) q.hum]
    ::
        [%sgzp *]  ~_(duck(sut (play p.gen)) $(gen q.gen))
        [%sggr *]
      =+  hum=$(gen q.gen)
      :: ?:  &(huz !?=(%|(@ [?(%sgcn %sgls) ^]) p.gen))
      ::  hum
      :-  p.hum
      :+  %11
        ?-    p.gen
            @   p.gen
            ^   [p.p.gen q:$(gen q.p.gen, gol %noun)]
        ==
      q.hum
    ::
        [%tsgr *]
      =+  fid=$(gen p.gen, gol %noun)
      =+  dov=$(sut p.fid, gen q.gen)
      [p.dov (comb q.fid q.dov)]
    ::
        [%tscm *]
      $(gen q.gen, sut (busk p.gen))
    ::
        [%wtcl *]
      =+  nor=$(gen p.gen, gol bool)
      =+  fex=(gain p.gen)
      =+  wux=(lose p.gen)
      =+  ^=  duy
          ?:  =(%void fex)
            ?:(=(%void wux) [%0 0] [%1 1])
          ?:(=(%void wux) [%1 0] q.nor)
      =+  hiq=$(sut fex, gen q.gen)
      =+  ran=$(sut wux, gen r.gen)
      [(fork p.hiq p.ran ~) (cond duy q.hiq q.ran)]
    ::
        [%wthx *]
      :-  (nice bool)
      =+  fid=(find %read [[%& 1] q.gen])
      ~>  %mean.'mint-fragment'
      ?>  &(?=(%& -.fid) ?=(%& -.q.p.fid))
      (~(fish ar `type`p.q.p.fid `skin`p.gen) (tend p.p.fid))
    ::
        [%fits *]
      :-  (nice bool)
      =+  ref=(play p.gen)
      =+  fid=(find %read q.gen)
      ~|  [%test q.gen]
      |-  ^-  nock
      ?-  -.fid
        %&  ?-  -.q.p.fid
              %&  (fish(sut ref) (tend p.p.fid))
              %|  $(fid [%| (fine fid)])
            ==
        %|  [%7 q.p.fid (fish(sut ref) 1)]
      ==
    ::
        [%dbug *]
      ~_  (show %o p.gen)
      =+  hum=$(gen q.gen)
      [p.hum [%11 [%spot %1 p.gen] q.hum]]
    ::
        [%zpcm *]   [(nice (play p.gen)) [%1 q.gen]]    ::  XX validate!
        [%lost *]
      ?:  vet
        ~_  (dunk(sut (play p.gen)) 'lost')
        ~>(%mean.'mint-lost' !!)
      [%void [%0 0]]
    ::
        [%zpmc *]
      =+  vos=$(gol %noun, gen q.gen)
      =+  ref=p:$(gol %noun, gen p.gen)
      [(nice (cell ref p.vos)) (cons [%1 burp(sut p.vos)] q.vos)]
    ::
        [%zpgl *]
      =/  typ  (nice (play [%kttr p.gen]))
      =/  val
        =<  q
        %_    $
            gol  %noun
            gen
          :^    %wtcl
              :+  %cncl  [%limb %levi]
              :~  [%tsgr [%zpgr [%kttr p.gen]] [%$ 2]]
                  [%tsgr q.gen [%$ 2]]
              ==
            [%tsgr q.gen [%$ 3]]
          [%zpzp ~]
        ==
      [typ val]
    ::
        [%zpts *]   [(nice %noun) [%1 q:$(vet |, gen p.gen)]]
        [%zppt *]   ?:((feel p.gen) $(gen q.gen) $(gen r.gen))
    ::
        [%zpzp ~]  [%void [%0 0]]
        *
      =+  doz=~(open ap gen)
      ?:  =(doz gen)
        ~_  (show [%c 'hoon'] [%q gen])
        ~>(%mean.'mint-open' !!)
      $(gen doz)
    ==
    ::
    ++  nice
      |=  typ=type
      ~_  leaf+"mint-nice"
      ?>  ?|(!vet (nest(sut gol) & typ))
      typ
    ::
    ++  grow
      |=  [mel=vair nym=(unit term) hud=poly ruf=hoon dom=(map term tome)]
      ^-  [p=type q=nock]
      =+  dan=^$(gen ruf, gol %noun)
      =+  pul=(mine gol mel nym hud dom)
      [(nice p.pul) (cons q.pul q.dan)]
    --
  ::
  ++  moot
    =+  gil=*(set type)
    |-  ^-  ?
    ?-  sut
      [%atom *]  |
      [%cell *]  |($(sut p.sut) $(sut q.sut))
      [%core *]  $(sut p.sut)
      [%face *]  $(sut q.sut)
      [%fork *]  (levy ~(tap in p.sut) |=(type ^$(sut +<)))
      [%hint *]  $(sut q.sut)
      [%hold *]  |((~(has in gil) sut) $(gil (~(put in gil) sut), sut repo))
      %noun      |
      %void      &
    ==
  ::
  ++  mull
    ~/  %mull
    |=  [gol=type dox=type gen=hoon]
    |^  ^-  [p=type q=type]
    ?:  =(%void sut)
      ~>(%mean.'mull-none' !!)
    ?-    gen
    ::
        [^ *]
      =+  hed=$(gen p.gen, gol %noun)
      =+  tal=$(gen q.gen, gol %noun)
      [(nice (cell p.hed p.tal)) (cell q.hed q.tal)]
    ::
        [%brcn *]  (grow %gold p.gen %dry [%$ 1] q.gen)
        [%brpt *]  (grow %gold p.gen %wet [%$ 1] q.gen)
        [%cnts *]  (~(mull et p.gen q.gen) gol dox)
        [%dtkt *]  =+($(gen q.gen, gol %noun) $(gen [%kttr p.gen]))
        [%dtls *]  =+($(gen p.gen, gol [%atom %$ ~]) (beth [%atom %$ ~]))
        [%sand *]  (beth (play gen))
        [%rock *]  (beth (play gen))
    ::
        [%dttr *]
      =+([$(gen p.gen, gol %noun) $(gen q.gen, gol %noun)] (beth %noun))
    ::
        [%dtts *]
      =+([$(gen p.gen, gol %noun) $(gen q.gen, gol %noun)] (beth bool))
    ::
        [%dtwt *]  =+($(gen p.gen, gol %noun) (beth bool)) ::  XX  =|
        [%hand *]  [p.gen p.gen]
        [%ktbr *]
      =+(vat=$(gen p.gen) [(wrap(sut p.vat) %iron) (wrap(sut q.vat) %iron)])
    ::
        [%ktls *]
      =+  hif=[p=(nice (play p.gen)) q=(play(sut dox) p.gen)]
      =+($(gen q.gen, gol p.hif) hif)
    ::
        [%ktpm *]
      =+(vat=$(gen p.gen) [(wrap(sut p.vat) %zinc) (wrap(sut q.vat) %zinc)])
    ::
        [%tune *]
      [(face p.gen sut) (face p.gen dox)]
    ::
        [%ktwt *]
      =+(vat=$(gen p.gen) [(wrap(sut p.vat) %lead) (wrap(sut q.vat) %lead)])
    ::
        [%note *]
      =+  vat=$(gen q.gen)
      [(hint [sut p.gen] p.vat) (hint [dox p.gen] q.vat)]
    ::
        [%ktsg *]  $(gen p.gen)
        [%sgzp *]  ~_(duck(sut (play p.gen)) $(gen q.gen))
        [%sggr *]  $(gen q.gen)
        [%tsgr *]
      =+  lem=$(gen p.gen, gol %noun)
      $(gen q.gen, sut p.lem, dox q.lem)
    ::
        [%tscm *]
      =/  boc  (busk p.gen)
      =/  nuf  (busk(sut dox) p.gen)
      $(gen q.gen, sut boc, dox nuf)
    ::
        [%wtcl *]
      =+  nor=$(gen p.gen, gol bool)
      =+  ^=  hiq  ^-  [p=type q=type]
          =+  fex=[p=(gain p.gen) q=(gain(sut dox) p.gen)]
          ?:  =(%void p.fex)
            :-  %void
            ?:  =(%void q.fex)
              %void
            ~>(%mean.'if-z' (play(sut q.fex) q.gen))
          ?:  =(%void q.fex)
            ~>(%mean.'mull-bonk-b' !!)
          $(sut p.fex, dox q.fex, gen q.gen)
      =+  ^=  ran  ^-  [p=type q=type]
          =+  wux=[p=(lose p.gen) q=(lose(sut dox) p.gen)]
          ?:  =(%void p.wux)
            :-  %void
            ?:  =(%void q.wux)
              %void
            ~>(%mean.'if-a' (play(sut q.wux) r.gen))
          ?:  =(%void q.wux)
            ~>(%mean.'mull-bonk-c' !!)
          $(sut p.wux, dox q.wux, gen r.gen)
      [(nice (fork p.hiq p.ran ~)) (fork q.hiq q.ran ~)]
    ::
        [%fits *]
      =+  waz=[p=(play p.gen) q=(play(sut dox) p.gen)]
      =+  ^=  syx  :-  p=(cove q:(mint %noun [%wing q.gen]))
                   q=(cove q:(mint(sut dox) %noun [%wing q.gen]))
      =+  pov=[p=(fish(sut p.waz) p.syx) q=(fish(sut q.waz) q.syx)]
      ?.  &(=(p.syx q.syx) =(p.pov q.pov))
        ~>(%mean.'mull-bonk-a' !!)
      (beth bool)
    ::
        [%wthx *]
      ~>  %mean.'mull-bonk-x'
      =+  :-  =+  (find %read [[%& 1] q.gen])
              ?>  &(?=(%& -.-) ?=(%& -.q.p.-))
              new=[type=p.q.p.- axis=(tend p.p.-)]
          =+  (find(sut dox) %read [%& 1] q.gen)
          ?>  &(?=(%& -.-) ?=(%& -.q.p.-))
          old=[type=p.q.p.- axis=(tend p.p.-)]
      ?>  =(axis.old axis.new)
      ?>  (nest(sut type.old) & type.new)
      (beth bool)
    ::
        [%dbug *]  ~_((show %o p.gen) $(gen q.gen))
        [%zpcm *]  [(nice (play p.gen)) (play(sut dox) p.gen)]
        [%lost *]
      ?:  vet
        ::  ~_  (dunk(sut (play p.gen)) 'also')
        ~>(%mean.'mull-skip' !!)
      (beth %void)
    ::
        [%zpts *]  (beth %noun)
    ::
        [%zpmc *]
      =+  vos=$(gol %noun, gen q.gen)       ::  XX validate!
      [(nice (cell (play p.gen) p.vos)) (cell (play(sut dox) p.gen) q.vos)]
    ::
        [%zpgl *]
      ::  XX is this right?
      (beth (play [%kttr p.gen]))
    ::
        [%zppt *]
      =+  [(feel p.gen) (feel(sut dox) p.gen)]
      ?.  =(-< ->)
        ~>(%mean.'mull-bonk-f' !!)
      ?:  -<
        $(gen q.gen)
      $(gen r.gen)
    ::
        [%zpzp *]  (beth %void)
        *
      =+  doz=~(open ap gen)
      ?:  =(doz gen)
        ~_  (show [%c 'hoon'] [%q gen])
        ~>(%mean.'mull-open' !!)
      $(gen doz)
    ==
    ::
    ++  beth
      |=  typ=type
      [(nice typ) typ]
    ::
    ++  nice
      |=  typ=type
      ::  ~_  (dunk(sut gol) 'need')
      ::  ~_  (dunk(sut typ) 'have')
      ~_  leaf+"mull-nice"
      ?>  ?|(!vet (nest(sut gol) & typ))
      typ
    ::
    ++  grow
      |=  [mel=vair nym=(unit term) hud=poly ruf=hoon dom=(map term tome)]
      ::  make al
      ~_  leaf+"mull-grow"
      ^-  [p=type q=type]
      =+  dan=^$(gen ruf, gol %noun)
      =+  yaz=(mile(sut p.dan) q.dan mel nym hud dom)
      [(nice p.yaz) q.yaz]
    --
  ++  meet  |=(ref=type &((nest | ref) (nest(sut ref) | sut)))
  ::                                                    ::
  ++  miss                                              ::  nonintersection
    |=  $:  ::  ref: symmetric type
            ::
            ref=type
        ==
    ::  intersection of sut and ref is empty
    ::
    ^-  ?
    =|  gil=(set (set type))
    =<  dext
    |%
    ++  dext
      ^-  ?
      ::
      ?:  =(ref sut)
        (nest(sut %void) | sut)
      ?-  sut
        %void      &
        %noun      (nest(sut %void) | ref)
        [%atom *]  sint
        [%cell *]  sint
        [%core *]  sint(sut [%cell %noun %noun])
        [%fork *]  %+  levy  ~(tap in p.sut)
                   |=(type dext(sut +<))
        [%face *]  dext(sut q.sut)
        [%hint *]  dext(sut q.sut)
        [%hold *]  =+  (~(gas in *(set type)) `(list type)`[sut ref ~])
                   ?:  (~(has in gil) -)
                      &
                   %=  dext
                     sut  repo
                     gil  (~(put in gil) -)
      ==           ==
    ++  sint
      ?+  ref      dext(sut ref, ref sut)
        [%atom *]  ?.  ?=([%atom *] sut)  &
                   ?&  ?=(^ q.ref)
                       ?=(^ q.sut)
                       !=(q.ref q.sut)
                   ==
        [%cell *]  ?.  ?=([%cell *] sut)  &
                   ?|  dext(sut p.sut, ref p.ref)
                       dext(sut q.sut, ref q.ref)
      ==           ==
    --
  ++  mite  |=(ref=type |((nest | ref) (nest(sut ref) & sut)))
  ++  nest
    ~/  %nest
    |=  [tel=? ref=type]
    =|  $:  seg=(set type)                              ::  degenerate sut
            reg=(set type)                              ::  degenerate ref
            gil=(set [p=type q=type])                   ::  assume nest
        ==
    =<  dext
    ~%  %nest-in  ..$  ~
    |%
    ++  deem
      |=  [mel=vair ram=vair]
      ^-  ?
      ?.  |(=(mel ram) =(%lead mel) =(%gold ram))  |
      ?-  mel
        %lead  &
        %gold  meet
        %iron  dext(sut (peek(sut ref) %rite 2), ref (peek %rite 2))
        %zinc  dext(sut (peek %read 2), ref (peek(sut ref) %read 2))
      ==
    ::
    ++  deep
      |=  $:  dom=(map term tome)
              vim=(map term tome)
          ==
      ^-  ?
      ?:  ?=(~ dom)  =(vim ~)
      ?:  ?=(~ vim)  |
      ?&  =(p.n.dom p.n.vim)
          $(dom l.dom, vim l.vim)
          $(dom r.dom, vim r.vim)
      ::
          =+  [dab hem]=[q.q.n.dom q.q.n.vim]
          |-  ^-  ?
          ?:  ?=(~ dab)  =(hem ~)
          ?:  ?=(~ hem)  |
          ?&  =(p.n.dab p.n.hem)
              $(dab l.dab, hem l.hem)
              $(dab r.dab, hem r.hem)
              %=  dext
                sut  (play q.n.dab)
                ref  (play(sut ref) q.n.hem)
      ==  ==  ==
    ::
    ++  dext
      =<  $
      ~%  %nest-dext  +  ~
      |.
      ^-  ?
      =-  ?:  -  &
          ?.  tel  |
          ~_  (dunk %need)
          ~_  (dunk(sut ref) %have)
          ~>  %mean.'nest-fail'
          !!
      ?:  =(sut ref)  &
      ?-  sut
        %void      sint
        %noun      &
        [%atom *]  ?.  ?=([%atom *] ref)  sint
                   ?&  (fitz p.sut p.ref)
                       |(?=(~ q.sut) =(q.sut q.ref))
                   ==
        [%cell *]  ?.  ?=([%cell *] ref)  sint
                   ?&  dext(sut p.sut, ref p.ref, seg ~, reg ~)
                       dext(sut q.sut, ref q.ref, seg ~, reg ~)
                   ==
        [%core *]  ?.  ?=([%core *] ref)  sint
                   ?:  =(q.sut q.ref)  dext(sut p.sut, ref p.ref)
                   ?&  =(q.p.q.sut q.p.q.ref)  ::  same wet/dry
                       meet(sut q.q.sut, ref p.sut)
                       dext(sut q.q.ref, ref p.ref)
                       (deem(sut q.q.sut, ref q.q.ref) r.p.q.sut r.p.q.ref)
                       ?:  =(%wet q.p.q.sut)  =(q.r.q.sut q.r.q.ref)
                       ?|  (~(has in gil) [sut ref])
                           %.  [q.r.q.sut q.r.q.ref]
                           %=  deep
                             gil  (~(put in gil) [sut ref])
                             sut  sut(p q.q.sut, r.p.q %gold)
                             ref  ref(p q.q.ref, r.p.q %gold)
                       ==  ==
                   ==
        [%face *]  dext(sut q.sut)
        [%fork *]  ?.  ?=(?([%atom *] %noun [%cell *] [%core *]) ref)  sint
                   (lien ~(tap in p.sut) |=(type dext(tel |, sut +<)))
        [%hint *]  dext(sut q.sut)
        [%hold *]  ?:  (~(has in seg) sut)  |
                   ?:  (~(has in gil) [sut ref])  &
                   %=  dext
                     sut  repo
                     seg  (~(put in seg) sut)
                     gil  (~(put in gil) [sut ref])
      ==           ==
    ::
    ++  meet  &(dext dext(sut ref, ref sut))
    ++  sint
      ^-  ?
      ?-  ref
        %noun       |
        %void       &
        [%atom *]   |
        [%cell *]   |
        [%core *]   dext(ref repo(sut ref))
        [%face *]   dext(ref q.ref)
        [%fork *]   (levy ~(tap in p.ref) |=(type dext(ref +<)))
        [%hint *]   dext(ref q.ref)
        [%hold *]   ?:  (~(has in reg) ref)  &
                    ?:  (~(has in gil) [sut ref])  &
                    %=  dext
                      ref  repo(sut ref)
                      reg  (~(put in reg) ref)
                      gil  (~(put in gil) [sut ref])
      ==            ==
    --
  ::
  ++  peek
    ~/  %peek
    |=  [way=?(%read %rite %both %free) axe=axis]
    ^-  type
    ?:  =(1 axe)
      sut
    =+  [now=(cap axe) lat=(mas axe)]
    =+  gil=*(set type)
    |-  ^-  type
    ?-    sut
        [%atom *]   %void
        [%cell *]   ?:(=(2 now) ^$(sut p.sut, axe lat) ^$(sut q.sut, axe lat))
        [%core *]
      ?.  =(3 now)  %noun
      =+  pec=(peel way r.p.q.sut)
      =/  tow
        ?:  =(1 lat)  1
        (cap lat)
      %=    ^$
          axe  lat
          sut
        ?:  ?|  =([& &] pec)
                &(sam.pec =(tow 2))
                &(con.pec =(tow 3))
            ==
          p.sut
        ~_  leaf+"payload-block"
        ?.  =(way %read)  !!
        %+  cell
          ?.(sam.pec %noun ^$(sut p.sut, axe 2))
        ?.(con.pec %noun ^$(sut p.sut, axe 3))
      ==
    ::
        [%fork *]   (fork (turn ~(tap in p.sut) |=(type ^$(sut +<))))
        [%hold *]
      ?:  (~(has in gil) sut)
        %void
      $(gil (~(put in gil) sut), sut repo)
    ::
        %void       %void
        %noun       %noun
        *           $(sut repo)
    ==
  ::
  ++  peel
    |=  [way=vial met=?(%gold %iron %lead %zinc)]
    ^-  [sam=? con=?]
    ?:  ?=(%gold met)  [& &]
    ?-  way
      %both  [| |]
      %free  [& &]
      %read  [?=(%zinc met) |]
      %rite  [?=(%iron met) |]
    ==
  ::
  ++  play
    ~/  %play
    =>  .(vet |)
    |=  gen=hoon
    ^-  type
    ?-  gen
      [^ *]      (cell $(gen p.gen) $(gen q.gen))
      [%brcn *]  (core sut [p.gen %dry %gold] sut *seminoun q.gen)
      [%brpt *]  (core sut [p.gen %wet %gold] sut *seminoun q.gen)
      [%cnts *]  ~(play et p.gen q.gen)
      [%dtkt *]  $(gen [%kttr p.gen])
      [%dtls *]  [%atom %$ ~]
      [%rock *]  |-  ^-  type
                 ?@  q.gen  [%atom p.gen `q.gen]
                 [%cell $(q.gen -.q.gen) $(q.gen +.q.gen)]
      [%sand *]  ?@  q.gen
                   ?:  =(%n p.gen)  ?>(=(0 q.gen) [%atom p.gen `q.gen])
                   ?:  =(%f p.gen)  ?>((lte q.gen 1) bool)
                   [%atom p.gen ~]
                 $(-.gen %rock)
      [%tune *]  (face p.gen sut)
      [%dttr *]  %noun
      [%dtts *]  bool
      [%dtwt *]  bool
      [%hand *]  p.gen
      [%ktbr *]  (wrap(sut $(gen p.gen)) %iron)
      [%ktls *]  $(gen p.gen)
      [%ktpm *]  (wrap(sut $(gen p.gen)) %zinc)
      [%ktsg *]  $(gen p.gen)
      [%ktwt *]  (wrap(sut $(gen p.gen)) %lead)
      [%note *]  (hint [sut p.gen] $(gen q.gen))
      [%sgzp *]  ~_(duck(sut ^$(gen p.gen)) $(gen q.gen))
      [%sggr *]  $(gen q.gen)
      [%tsgr *]  $(gen q.gen, sut $(gen p.gen))
      [%tscm *]  $(gen q.gen, sut (busk p.gen))
      [%wtcl *]  =+  [fex=(gain p.gen) wux=(lose p.gen)]
                 %-  fork  :~
                   ?:(=(%void fex) %void $(sut fex, gen q.gen))
                   ?:(=(%void wux) %void $(sut wux, gen r.gen))
                 ==
      [%fits *]  bool
      [%wthx *]  bool
      [%dbug *]  ~_((show %o p.gen) $(gen q.gen))
      [%zpcm *]  $(gen p.gen)
      [%lost *]  %void
      [%zpmc *]  (cell $(gen p.gen) $(gen q.gen))
      [%zpgl *]  (play [%kttr p.gen])
      [%zpts *]  %noun
      [%zppt *]  ?:((feel p.gen) $(gen q.gen) $(gen r.gen))
      [%zpzp *]  %void
      *          =+  doz=~(open ap gen)
                 ?:  =(doz gen)
                   ~_  (show [%c 'hoon'] [%q gen])
                   ~>  %mean.'play-open'
                   !!
                 $(gen doz)
    ==
  ::                                                    ::
  ++  redo                                              ::  refurbish faces
    ~/  %redo
    |=  $:  ::  ref: raw payload
            ::
            ref=type
        ==
    ::  :type: subject refurbished to reference namespace
    ::
    ^-  type
    ::  hos: subject tool stack
    ::  wec: reference tool stack set
    ::  gil: repetition set
    ::
    =|  hos=(list tool)
    =/  wec=(set (list tool))  [~ ~ ~]
    =|  gil=(set (pair type type))
    =<  ::  errors imply subject/reference mismatch
        ::
        ~|  %redo-match
        ::  reduce by subject
        ::
        dext
    |%
    ::                                                  ::
    ++  dear                                            ::  resolve tool stack
      ::  :(unit (list tool)): unified tool stack
      ::
      ^-  (unit (list tool))
      ::  empty implies void
      ::
      ?~  wec  `~
      ::  any reference faces must be clear
      ::
      ?.  ?=([* ~ ~] wec)
        ~&  [%dear-many wec]
        ~
      :-  ~
      ::  har: single reference tool stack
      ::
      =/  har  n.wec
      ::  len: lengths of [sut ref] face stacks
      ::
      =/  len  [p q]=[(lent hos) (lent har)]
      ::  lip: length of sut-ref face stack overlap
      ::
      ::      AB
      ::       BC
      ::
      ::    +lip is (lent B), where +hay is forward AB
      ::    and +liv is forward BC (stack BA and CB).
      ::
      ::    overlap is a weird corner case.  +lip is
      ::    almost always 0.  brute force is fine.
      ::
      =/  lip
        =|  lup=(unit @ud)
        =|  lip=@ud
        |-  ^-  @ud
        ?:  |((gth lip p.len) (gth lip q.len))
          (fall lup 0)
        ::  lep: overlap candidate: suffix of subject face stack
        ::
        =/  lep  (slag (sub p.len lip) hos)
        ::  lap: overlap candidate: prefix of reference face stack
        ::
        =/  lap  (scag lip har)
        ::  save any match and continue
        ::
        $(lip +(lip), lup ?.(=(lep lap) lup `lip))
      ::  ~&  [har+har hos+hos len+len lip+lip]
      ::  produce combined face stack (forward ABC, stack CBA)
      ::
      (weld hos (slag lip har))
    ::                                                  ::
    ++  dext                                            ::  subject traverse
      ::  :type: refurbished subject
      ::
      ^-  type
      ::  check for trivial cases
      ::
      ?:  ?|  =(sut ref)
              ?=(?(%noun %void [?(%atom %core) *]) ref)
          ==
        done
      ::  ~_  (dunk 'redo: dext: sut')
      ::  ~_  (dunk(sut ref) 'redo: dext: ref')
      ?-    sut
          ?(%noun %void [?(%atom %core) *])
        ::  reduce reference and reassemble leaf
        ::
        done:(sint &)
      ::
          [%cell *]
        ::  reduce reference to match subject
        ::
        =>  (sint &)
        ?>  ?=([%cell *] sut)
        ::  leaf with possible recursive descent
        ::
        %=    done
            sut
          ::  clear face stacks for descent
          ::
          =:  hos  ~
              wec  [~ ~ ~]
            ==
          ::  descend into cell
          ::
          :+  %cell
            dext(sut p.sut, ref (peek(sut ref) %free 2))
          dext(sut q.sut, ref (peek(sut ref) %free 3))
        ==
      ::
          [%face *]
        ::  push face on subject stack, and descend
        ::
        dext(hos [p.sut hos], sut q.sut)
      ::
          [%hint *]
        ::  work through hint
        ::
        (hint p.sut dext(sut q.sut))
      ::
          [%fork *]
        ::  reconstruct each case in fork
        ::
        (fork (turn ~(tap in p.sut) |=(type dext(sut +<))))
      ::
          [%hold *]
        ::  reduce to hard
        ::
        =>  (sint |)
        ?>  ?=([%hold *] sut)
        ?:  (~(has in fan) [p.sut q.sut])
          ::  repo loop; redo depends on its own product
          ::
          done:(sint &)
        ?:  (~(has in gil) [sut ref])
          ::  type recursion, stop renaming
          ::
          done:(sint |)
        ::  restore unchanged holds
        ::
        =+  repo
        =-  ?:(=(- +<) sut -)
        dext(sut -, gil (~(put in gil) sut ref))
      ==
    ::                                                  ::
    ++  done                                            ::  complete assembly
      ^-  type
      ::  :type: subject refurbished
      ::
      ::  lov: combined face stack
      ::
      =/  lov
          =/  lov  dear
          ?~  lov
            ::  ~_  (dunk 'redo: dear: sut')
            ::  ~_  (dunk(sut ref) 'redo: dear: ref')
            ~&  [%wec wec]
            !!
          (need lov)
      ::  recompose faces
      ::
      |-  ^-  type
      ?~  lov  sut
      $(lov t.lov, sut (face i.lov sut))
    ::
    ++  sint                                            ::  reduce by reference
      |=  $:  ::  hod: expand holds
              ::
              hod=?
          ==
      ::  ::.: reference with face/fork/hold reduced
      ::
      ^+  .
      ::  =-  ~>  %slog.[0 (dunk 'sint: sut')]
      ::      ~>  %slog.[0 (dunk(sut ref) 'sint: ref')]
      ::      ~>  %slog.[0 (dunk(sut =>(- ref)) 'sint: pro')]
      ::      -
      ?+    ref  .
          [%hint *]  $(ref q.ref)
          [%face *]
        ::  extend all stacks in set
        ::
        %=  $
          ref  q.ref
          wec  (~(run in wec) |=((list tool) [p.ref +<]))
        ==
      ::
          [%fork *]
        ::  reconstruct all relevant cases
        ::
        =-  ::  ~>  %slog.[0 (dunk 'fork: sut')]
            ::  ~>  %slog.[0 (dunk(sut ref) 'fork: ref')]
            ::  ~>  %slog.[0 (dunk(sut (fork ->)) 'fork: pro')]
            +(wec -<, ref (fork ->))
        =/  moy  ~(tap in p.ref)
        |-  ^-  (pair (set (list tool)) (list type))
        ?~  moy  [~ ~]
        ::  head recurse
        ::
        =/  mor  $(moy t.moy)
        ::  prune reference cases outside subject
        ::
        ?:  (miss i.moy)  mor
        ::  unify all cases
        ::
        =/  dis  ^$(ref i.moy)
        [(~(uni in p.mor) wec.dis) [ref.dis q.mor]]
      ::
          [%hold *]
        ?.  hod  .
        $(ref repo(sut ref))
      ==
    --
  ::
  ++  repo
    ^-  type
    ?-  sut
      [%core *]   [%cell %noun p.sut]
      [%face *]   q.sut
      [%hint *]   q.sut
      [%hold *]   (rest [[p.sut q.sut] ~])
      %noun       (fork [%atom %$ ~] [%cell %noun %noun] ~)
      *           ~>(%mean.'repo-fltt' !!)
    ==
  ::
  ++  rest
    ~/  %rest
    |=  leg=(list [p=type q=hoon])
    ^-  type
    ?:  (lien leg |=([p=type q=hoon] (~(has in fan) [p q])))
      ~>(%mean.'rest-loop' !!)
    =>  .(fan (~(gas in fan) leg))
    %-  fork
    %~  tap  in
    %-  ~(gas in *(set type))
    (turn leg |=([p=type q=hoon] (play(sut p) q)))
  ::
  ++  sink
    ~/  %sink
    |^  ^-  cord
    ?-  sut
      %void      'void'
      %noun      'noun'
      [%atom *]  (rap 3 'atom ' p.sut ' ' ?~(q.sut '~' u.q.sut) ~)
      [%cell *]  (rap 3 'cell ' (mup p.sut) ' ' (mup q.sut) ~)
      [%face *]  (rap 3 'face ' ?@(p.sut p.sut (mup p.sut)) ' ' (mup q.sut) ~)
      [%fork *]  (rap 3 'fork ' (mup p.sut) ~)
      [%hint *]  (rap 3 'hint ' (mup p.sut) ' ' (mup q.sut) ~)
      [%hold *]  (rap 3 'hold ' (mup p.sut) ' ' (mup q.sut) ~)
    ::
        [%core *]
      %+  rap  3
      :~  'core '
          (mup p.sut)
          ' '
          ?~(p.p.q.sut '~' u.p.p.q.sut)
          ' '
          q.p.q.sut
          ' '
          r.p.q.sut
          ' '
          (mup q.q.sut)
          ' '
          (mup p.r.q.sut)
      ==
    ==
    ::
    ++  mup  |=(* (scot %p (mug +<)))
    --
  ::
  ++  take
    |=  [vit=vein duz=$-(type type)]
    ^-  (pair axis type)
    :-  (tend vit)
    =.  vit  (flop vit)
    |-  ^-  type
    ?~  vit  (duz sut)
    ?~  i.vit
      |-  ^-  type
      ?+  sut      ^$(vit t.vit)
        [%face *]  (face p.sut ^$(vit t.vit, sut q.sut))
        [%hint *]  (hint p.sut ^$(sut q.sut))
        [%fork *]  (fork (turn ~(tap in p.sut) |=(type ^$(sut +<))))
        [%hold *]  $(sut repo)
      ==
    =+  vil=*(set type)
    |-  ^-  type
    ?:  =(1 u.i.vit)
      ^$(vit t.vit)
    =+  [now lat]=(cap u.i.vit)^(mas u.i.vit)
    ?-  sut
      %noun      $(sut [%cell %noun %noun])
      %void      %void
      [%atom *]  %void
      [%cell *]  ?:  =(2 now)
                   (cell $(sut p.sut, u.i.vit lat) q.sut)
                  (cell p.sut $(sut q.sut, u.i.vit lat))
      [%core *]  ?:  =(2 now)
                   $(sut repo)
                 (core $(sut p.sut, u.i.vit lat) q.sut)
      [%face *]  (face p.sut $(sut q.sut))
      [%fork *]  (fork (turn ~(tap in p.sut) |=(type ^$(sut +<))))
      [%hint *]  (hint p.sut $(sut q.sut))
      [%hold *]  ?:  (~(has in vil) sut)
                   %void
                 $(sut repo, vil (~(put in vil) sut))
    ==
  ::
  ++  tack
    |=  [hyp=wing mur=type]
    ~_  (show [%c %tack] %l hyp)
    =+  fid=(find %rite hyp)
    ?>  ?=(%& -.fid)
    (take p.p.fid |=(type mur))
  ::
  ++  tend
    |=  vit=vein
    ^-  axis
    ?~(vit 1 (peg $(vit t.vit) ?~(i.vit 1 u.i.vit)))
  ::
  ++  toss
    ~/  %toss
    |=  [hyp=wing mur=type men=(list [p=type q=foot])]
    ^-  [p=axis q=(list [p=type q=foot])]
    =-  [(need p.wib) q.wib]
    ^=  wib
    |-  ^-  [p=(unit axis) q=(list [p=type q=foot])]
    ?~  men
      [*(unit axis) ~]
    =+  geq=(tack(sut p.i.men) hyp mur)
    =+  mox=$(men t.men)
    [(mate p.mox `_p.mox`[~ p.geq]) [[q.geq q.i.men] q.mox]]
  ::
  ++  wrap
    ~/  %wrap
    |=  yoz=?(%lead %iron %zinc)
    ~_  leaf+"wrap"
    ^-  type
    ?+  sut  sut
      [%cell *]  (cell $(sut p.sut) $(sut q.sut))
      [%core *]  ?>(|(=(%gold r.p.q.sut) =(%lead yoz)) sut(r.p.q yoz))
      [%face *]  (face p.sut $(sut q.sut))
      [%fork *]  (fork (turn ~(tap in p.sut) |=(type ^$(sut +<))))
      [%hint *]  (hint p.sut $(sut q.sut))
      [%hold *]  $(sut repo)
    ==
  --
::
++  bike
  ~/  %bike
  |=  [jut=* a=axis pac=(list (pair axis *))]
  |^  =/  rel=(map axis *)  (roll pac insert)
      =/  ord=(list axis)   (sort ~(tap in ~(key by rel)) gth)
      |-  ^-  *
      ?~  ord
        .*(jut [%0 a])
      =/  b=axis  i.ord
      =/  c=*  (~(got by rel) b)
      =/  d=*  $(ord t.ord)
      .*(jut [%10 [b %1 c] %1 d])
  ::
  ++  contains
    |=  [container=axis contained=axis]
    ^-  ?
    =/  big=@    (met 0 container)
    =/  small=@  (met 0 contained)
    ?:  (lte small big)  |
    =/  dif=@  (sub small big)
    =(container (rsh [0 dif] contained))
  ::
  ++  parent
    |=  a=axis
    `axis`(rsh 0 a)
  ::
  ++  sibling
    |=  a=axis
    ^-  axis
    ?~  (mod a 2)
      +(a)
    (dec a)
  ::
  ++  insert
    |=  [e=[axe=axis fol=*] n=(map axis *)]
    ^-  (map axis *)
    ?:  =/  a=axis  axe.e
        |-  ^-  ?
        ?:  =(1 a)  |
        ?:  (~(has by n) a)
          &
        $(a (parent a))
      ::  parent already in
      n
    =.  n
      ::  remove children
      %+  roll  ~(tap by n)
      |=  [[axe=axis fol=*] m=_n]
      ?.  (contains axe.e axe)  m
      (~(del by m) axe)
    =/  sib  (sibling axe.e)
    =/  un   (~(get by n) sib)
    ?~  un   (~(put by n) axe.e fol.e)
    ::  replace sibling with parent
    %=  $
      n  (~(del by n) sib)
      e  :-  (parent sib)
         ?:  (gth sib axe.e)
           [fol.e u.un]
         [u.un fol.e]
    ==
  --
::
++  slip
  |=  [vax=vase gen=hoon]  ^-  vase
  (~(bent ut p.vax) q.vax %noun gen)
--
