section 3bE, tree sync
======================

### `++invert-miso`

    ++  invert-miso                                         ::  invert miso
      |=  mis=miso
      ?-  -.mis
        %del  [%ins p.mis]
        %ins  [%del p.mis]
        %mut  [%mut (limp p.mis)]
      ==
    ::

XX document

### `++cosh`

    ++  cosh                                                ::  locally rehash
      |=  ank=ankh                                          ::  NB v/unix.c
      ank(p rehash:(zu ank))
    ::

XX document

### `++cost`

    ++  cost                                                ::  new external patch
      |=  [bus=ankh ank=ankh]                               ::  NB v/unix.c
      ^-  soba
      :-  [p.ank p.bus] 
      %-  flop
      myz:(change-tree:(zu ank) %c bus)
    ::

XX document

### `++loth`

    ++  loth
      |=  pat=(map path ,*)
      ^-  (set path)
      %+  roll  (~(tap by pat) ~)
      |=  [[p=path *] q=(set path)]
      %.  p  %~  put  in  q
    ::

XX document

### `++luth`

    ++  luth
      |=  [p=(map path ,*) q=(map path ,*)]                 ::  merge keysets
      ^-  (set path)
      (~(uni in (loth p)) (loth q))
    ::

XX document

### `++blob-to-lobe`

    ++  blob-to-lobe                                        ::  p.blob
      |=  p=blob
      ^-  lobe
      ?-   -.p
         %delta  p.p
         %direct  p.p
         %indirect  p.p
      ==
    ::

XX document

### `++ze`

    ++  ze
      |_  [lim=@da dome rang]

XX document

### `++aeon-to-tako`

      ++  aeon-to-tako  ~(got by hit)

XX document

### `++make-yaki`

      ++  make-yaki                                         ::  make yaki
        |=  [p=(list tako) q=(map path lobe) t=@da]
        ^-  yaki
        =+  ^=  has
            %^  cat  7  (sham [%yaki (roll p add) q t])
            (sham [%tako (roll p add) q t])
        [p q has t]
      ::

XX document

### `++tako-to-yaki`

      ++  tako-to-yaki  ~(got by hut)                       ::  grab yaki

XX document

### `++lobe-to-blob`

      ++  lobe-to-blob  ~(got by lat)                       ::  grab blob

XX document

### `++lobe-to-noun`

      ++  lobe-to-noun                                      ::  grab blob
        |=  p=lobe                                          ::  ^-  *
        %-  blob-to-noun  
        (lobe-to-blob p)
      ::

XX document

### `++make-direct`

      ++  make-direct                                       ::  make blob
        |=  [p=* q=umph]
        ^-  blob
        [%direct (mug p) p q]
      ::

XX document

### `++make-delta`

      ++  make-delta                                        ::  make blob delta
        |=  [p=lobe q=udon]
        ^-  blob
        =+  t=[%delta 0 p q]
        =+  z=(blob-to-noun t)
        =+  ^=  has
            %^  cat  7  (sham [%blob z])
            (sham [%lobe z])
        [%delta has p q]
      ::

XX document

### `++blob-to-umph`

      ++  blob-to-umph                                      ::  blob umph [prep]
        |=  p=blob                                          ::  used in merge
        ^-  umph
        ?-   -.p
           %delta  p.r.p
           %direct  r.p
           %indirect  p.r.p
        ==
      ::

XX document

### `++blob-to-noun`

      ++  blob-to-noun                                      ::  grab blob
        |=  p=blob
        ?-   -.p
           %delta  (lump r.p (lobe-to-noun q.p))
           %direct  q.p
           %indirect  q.p
        ==
      ::
      ::
      ::

XX document

### `++diff-yakis`

      ++  diff-yakis                                        ::  fundamental diff
        |=  [p=yaki q=yaki]
        ^-  (map path miso)
        %+  roll  (~(tap in (luth q.p q.q)) ~)
        |=  [pat=path yeb=(map path miso)]
        =+  leb=(~(get by q.p) pat)
        =+  lob=(~(get by q.q) pat)
        ?~  leb  (~(put by yeb) pat [%ins (lobe-to-noun (need lob))])
        ?~  lob  (~(put by yeb) pat [%del (lobe-to-noun (need leb))])
        ?:  =(u.leb u.lob)  yeb
        =+  veq=(lobe-to-blob u.leb)
        =+  voq=(lobe-to-blob u.lob)
        %+  ~(put by yeb)  pat
        :-  %mut  
        ?:  &(?=(%delta -.voq) =(u.leb q.voq))              ::  avoid diff
          r.voq
        =+  zeq=(blob-to-noun veq)
        =+  zoq=(blob-to-noun voq)
        ((diff (blob-to-umph (lobe-to-blob u.leb))) zeq zoq)
      ::

XX document

### `++lobes-at-path`

      ++  lobes-at-path                                     ::    lobes-at-path:ze
        |=  [oan=aeon pax=path]                             ::  data at path
        ^-  (map path lobe)
        ?:  =(0 oan)  ~
        %-  mo
        %+  skim
          %.  ~
          %~  tap  by
          =<  q
          %-  tako-to-yaki
          %-  aeon-to-tako
          oan
        |=  [p=path q=lobe]
        ?|  ?=(~ pax)
            ?&  !?=(~ p)
                =(-.pax -.p)
                $(p +.p, pax +.pax)
        ==  ==
      ::

XX document

### `++case-to-aeon`

      ++  case-to-aeon                                      ::    case-to-aeon:ze
        |=  lok=case                                        ::  act count through
        ^-  (unit aeon)
        ?-    -.lok
            %da
          ?:  (gth p.lok lim)  ~
          |-  ^-  (unit aeon)
          ?:  =(0 let)  [~ 0]                               ::  avoid underflow
          ?:  %+  gte  p.lok 
              =<  t
              %-  tako-to-yaki
              %-  aeon-to-tako
              let
            [~ let]
          $(let (dec let))
        ::
            %tas  (~(get by lab) p.lok)
            %ud   ?:((gth p.lok let) ~ [~ p.lok])
        ==
      ::

XX document

### `++as-arch`

      ++  as-arch                                           ::    as-arch:ze
        ^-  arch                                            ::  arch report
        :+  p.ank
          ?~(q.ank ~ [~ p.u.q.ank])
        |-  ^-  (map ,@ta ,~)
        ?~  r.ank  ~
        [[p.n.r.ank ~] $(r.ank l.r.ank) $(r.ank r.r.ank)]
      ::

XX document

### `++reachable-takos`

      ++  reachable-takos                                   ::  reachable
        |=  p=tako                                          ::  XX slow
        ^-  (set tako)
        =+  y=(tako-to-yaki p)
        =+  t=(~(put in _(set tako)) p)
        %+  roll  p.y
        |=  [q=tako s=_t]
        ?:  (~(has in s) q)                                 ::  already done
          s                                                 ::  hence skip
        (~(uni in s) ^$(p q))                               ::  otherwise traverse
      ::

XX document

### `++new-lobes`

      ++  new-lobes                                         ::  object hash set
        |=  [b=(set lobe) a=(set tako)]                     ::  that aren't in b
        ^-  (set lobe)
        %+  roll  (~(tap in a) ~)
        |=  [tak=tako bar=(set lobe)]
        ^-  (set lobe)
        =+  yak=(tako-to-yaki tak)
        %+  roll  (~(tap by q.yak) ~)
        |=  [[path lob=lobe] far=_bar]
        ^-  (set lobe)
        ?~  (~(has in b) lob)                               ::  don't need
          far
        =+  gar=(lobe-to-blob lob)
        ?-  -.gar
          %direct  (~(put in far) lob)
          %delta  (~(put in $(lob q.gar)) lob)
          %indirect  (~(put in $(lob s.gar)) lob)
        ==
      ::

XX document

### `++new-lobes-takos`

      ++  new-lobes-takos                                   ::  garg & repack
        |=  [b=(set lobe) a=(set tako)]
        ^-  [(set tako) (set lobe)]
        [a (new-lobes b a)]
      ::

XX document

### `++reachable-between-takos`

      ++  reachable-between-takos
        |=  [a=(unit tako) b=tako]                          ::  pack a through b
        ^-  [(set tako) (set lobe)]
        =+  ^=  sar 
            ?~  a  ~
            (reachable-takos r:(tako-to-yaki u.a))
        =+  yak=`yaki`(tako-to-yaki b)
        %+  new-lobes-takos  (new-lobes ~ sar)              ::  get lobes
        |-  ^-  (set tako)                                  ::  walk onto sar
        ?:  (~(has in sar) r.yak)
          ~
        =+  ber=`(set tako)`(~(put in `(set tako)`~) `tako`r.yak)
        %-  ~(uni in ber)
        ^-  (set tako)
        %+  roll  p.yak
        |=  [yek=tako bar=(set tako)]
        ^-  (set tako)
        ?:  (~(has in bar) yek)                             ::  save some time
          bar
        %-  ~(uni in bar)
        ^$(yak (tako-to-yaki yek))
      ::

XX document

### `++takos-to-yakis`

      ++  takos-to-yakis                                    ::  trivial
        |=  a=(set tako)
        ^-  (set yaki)
        (sa (turn (~(tap by a)) tako-to-yaki))
      ::

XX document

### `++lobes-to-blobs`

      ++  lobes-to-blobs                                    ::  trivial
        |=  a=(set lobe)
        ^-  (set blob)
        (sa (turn (~(tap by a)) lobe-to-blob))
      ::

XX document

### `++make-nako`

      ++  make-nako                                         ::  gack a through b
        |=  [a=aeon b=aeon]
        ^-  [(map aeon tako) aeon (set yaki) (set blob)]
        :_  :-  b
            =-  [(takos-to-yakis -<) (lobes-to-blobs ->)]
            %+  reachable-between-takos
              (~(get by hit) a)                             ::  if a not found, a=0
            (aeon-to-tako b)
        ^-  (map aeon tako)
        %-  mo  %+  skim  (~(tap by hit) ~)
        |=  [p=aeon *]
        &((gth p a) (lte p b))
      ::
      ::::::::::::::::::::::::::::::::::::::::::::::::::::::::

XX document

### `++query`

      ++  query                                             ::    query:ze
        |=  ren=?(%u %v %x %y %z)                           ::  endpoint query
        ^-  (unit ,*)
        ?-  ren
          %u  [~ `rang`+<+>.query]
          %v  [~ `dome`+<+<.query]
          %x  ?~(q.ank ~ [~ q.u.q.ank])
          %y  [~ as-arch]
          %z  [~ ank]
        ==
      ::

XX document

### `++rewind`

      ++  rewind                                            ::    rewind:ze
        |=  oan=aeon                                        ::  rewind to aeon
        ^+  +>
        ?:  =(let oan)  +>
        ?:  (gth oan let)  !!                               ::  don't have version
        +>(ank (checkout-ankh q:(tako-to-yaki (aeon-to-tako oan))), let oan)
      ::
      ::::

XX document

### `++update-lat`

      ++  update-lat                                        ::   update-lat:ze
        |=  [lag=(map path blob) sta=(map lobe blob)]       ::  fix lat
        ^-  [(map lobe blob) (map path lobe)]
        %+  roll  (~(tap by lag) ~)
        |=  [[pat=path bar=blob] [lut=_sta gar=(map path lobe)]]
        ?~  (~(has by lut) p.bar)
          [lut (~(put by gar) pat p.bar)]
        :-  (~(put by lut) p.bar bar)
        (~(put by gar) pat p.bar)
      ::

XX document

### `++apply-changes`

      ++  apply-changes                                     ::   apply-changes:ze
        |=  lar=(list ,[p=path q=miso])                     ::  store changes
        ^-  (map path blob)
        =+  ^=  hat                                         ::  current state
            ?:  =(let 0)                                    ::  initial commit
              ~                                             ::  has nothing
            =<  q
            %-  tako-to-yaki
            %-  aeon-to-tako
            let
        %-  |=  bar=(map path blob)                         ::  find unchanged
            =+  sar=(sa (turn lar |=([p=path *] p)))        ::  changed paths
            %+  roll  (~(tap by hat) ~)
            |=  [[pat=path gar=lobe] bat=_bar]
            ?:  (~(has in sar) pat)                         ::  has update
              bat
            (~(put by bat) pat (lobe-to-blob gar))  ::  use original
        %+  roll  lar
        |=  [[pat=path mys=miso] bar=(map path blob)]
        ^+  bar
        ?-    -.mys
            %ins                                            ::  insert if not exist
          ?:  (~(has by bar) pat)  !!                       ::
          ?:  (~(has by hat) pat)  !!                       ::
          (~(put by bar) pat (make-direct p.mys %c))        ::  TODO content type?
            %del                                            ::  delete if exists
          ?.  |((~(has by hat) pat) (~(has by bar) pat))  !!
          (~(del by bar) pat)
            %mut                                            ::  mutate, must exist
          =+  ber=(~(get by bar) pat)
          ?~  ber
            =+  har=(~(get by hat) pat)
            ?~  har  !!
            %+  ~(put by bar)  pat
            (make-delta u.har p.mys)
          %+  ~(put by bar)  pat
          (make-delta p.u.ber p.mys)
        ==

XX document

### `++checkout-ankh`

      ++  checkout-ankh                                     ::    checkout-ankh:ze
        |=  hat=(map path lobe)                             ::  checkout commit
        ^-  ankh
        %-  cosh
        %+  roll  (~(tap by hat) ~)
        |=  [[pat=path bar=lobe] ank=ankh]
        ^-  ankh
        %-  cosh
        ?~  pat
          =+  zar=(lobe-to-noun bar)
          ank(q [~ (sham zar) zar])
        =+  nak=(~(get by r.ank) i.pat)
        %=  ank
          r  %+  ~(put by r.ank)  i.pat 
             $(pat t.pat, ank (fall nak _ankh))
        ==
      ::

XX document

### `++forge-yaki`

      ++  forge-yaki                                        ::    forge-yaki:ze
        |=  [wen=@da par=(unit tako) lem=soba]              ::  forge yaki
        =+  ^=  per
            ?~  par  ~
            ~[u.par]
        =+  gar=(update-lat (apply-changes q.lem) lat)
        :-  %^  make-yaki  per  +.gar  wen                  ::  from existing diff
        -.gar                                               ::  fix lat
      ::

XX document

### `++forge-nori`

      ++  forge-nori                                        ::    forge-nori:ze
        |=  yak=yaki                                        ::  forge nori (ugly op)
        ^-  nori                                            ::  basically zerg w/ nori
        ?~  p.yak  !!                                       ::  no parent -> can't diff
        :+  %&  *cart                                       ::  diff w/ 1st parent
        (~(tap by (diff-yakis (tako-to-yaki i.p.yak) yak)) ~)
      ::
      ::  graph algorithms (bottleneck)
      ::

XX document

### `++reduce-merge-points`

      ++  reduce-merge-points                             ::  reduce merge points
        |=  unk=(set yaki)                                ::  maybe need jet
        =|  gud=(set yaki)
        =+  ^=  zar
            ^-  (map tako (set tako))
            %+  roll  (~(tap in unk) ~)
            |=  [yak=yaki qar=(map tako (set tako))]
            (~(put by qar) r.yak (reachable-takos r.yak))
        |-  
        ^-  (set yaki)
        ?~  unk  gud
        =+  tek=`yaki`n.unk
        =+  bun=(~(del in `(set yaki)`unk) tek)
        ?:  %+  roll  (~(tap by (~(uni in gud) bun)) ~)   ::  only good + unknown
            |=  [tak=yaki god=?]
            ^-  ?
            ?.  god  god
            (~(has in (~(got by zar) r.tak)) tek)
          $(gud (~(put in gud) tek), unk bun)
        $(unk bun)
      ::

XX document

### `++future-find-merge-points`

      ++  future-find-merge-points                        ::  merge points fast
        |=  [p=yaki q=yaki]                               ::  (future zeal)
        ^-  (set yaki)                                    ::  zear still uses zule
        %-  reduce-merge-points                           ::  this is test-only
        =+  s=(~(put in _(set tako)) r.p)                 ::  not actually used
        =+  t=(~(put in _(set tako)) t.p)                 ::  but might be active
        =|  u=(set yaki)                                  ::  eventually
        |-  ^-  (set yaki)
        =+  v=(~(int in s) t)                             ::  found common
        =+  ^=  qez                                       ::  drop common
            ^-  [s=(set tako) t=(set tako)]
            %+  roll  (~(tap in v) ~)
            |=  [tak=tako bar=_s zar=_t]
            [(~(del in bar) tak) (~(del in zar) tak)]
        ?:  &(=(~ s.qez) =(~ s.qez))
          (~(uni in u) (takos-to-yakis v))
        %=  $
          u  (~(uni in u) (takos-to-yakis v))
          s  (add-parents s.qez)
          t  (add-parents t.qez)
        ==
      ::

XX document

### `++add-parents`

      ++  add-parents                                     ::  expand set
        |=  qez=(set tako)
        ^-  (set tako)
        %+  roll  (~(tap in qez) ~)
        |=  [tak=tako zar=(set tako)]
        %-  ~(uni in (~(put in zar) tak))
        (sa p:(tako-to-yaki tak))
      ::

XX document

### `++find-merge-points`

      ++  find-merge-points                               ::  merge points
        |=  [p=yaki q=yaki]                               ::  maybe need jet
        ^-  (set yaki)
        %-  reduce-merge-points
        =+  r=(reachable-takos r.p)
        |-  ^-  (set yaki)
        ?:  (~(has in r) q)  (~(put in _(set yaki)) q)    ::  done 
        %+  roll  p.q
        |=  [t=tako s=(set yaki)]
        ?:  (~(has in r) t)
          (~(put in s) (tako-to-yaki t))                 ::  found
        (~(uni in s) ^$(q (tako-to-yaki t)))             ::  traverse
      ::
      ::  merge logic
      ::

XX document

### `++clean`

      ++  clean                                          ::  clean
        |=  wig=(urge)
        ^-  (urge)
        ?~  wig  ~
        ?~  t.wig  wig
        ?:  ?=(%& -.i.wig)
          ?:  ?=(%& -.i.t.wig)
            $(wig [[%& (add p.i.wig p.i.t.wig)] t.t.wig])
          [i.wig $(wig t.wig)]
        ?:  ?=(%| -.i.t.wig)
          $(wig [[%| (welp p.i.wig p.i.t.wig) (welp q.i.wig q.i.t.wig)] t.t.wig])
        [i.wig $(wig t.wig)]
      ::

XX document

### `++match-conflict`

      ++  match-conflict                                ::  match conflict
        |=  [us=[ship desk] th=[ship desk] p=(urge) q=(urge) r=(list)]
        ^-  [p=[p=(list) q=(list)] q=[p=(urge) q=(urge) r=(list)]]
        =+  cas=(hard (list ,@t))
        =+  cat=(hard (urge ,@t))
        =+  mar=(match-merge (cat p) (cat q) (cas r))
        :-  :-  s.q.mar 
            (annotate us th p.p.mar q.p.mar s.q.mar)    ::  annotation
        :-  p.q.mar
        :-  q.q.mar
        r.q.mar
      ::

XX document

### `++annotate`

      ++  annotate                                      ::  annotate conflict
        |=  [us=[ship desk] th=[ship desk] p=(list ,@t) q=(list ,@t) r=(list ,@t)]
        ^-  (list ,@t)
        %-  zing
        ^-  (list (list ,@t))
        %-  flop
        ^-  (list (list ,@t))
        :-  :_  ~
            %^  cat  3  '<<<<<<<<<<<<' 
            %^  cat  3  ' '
            %^  cat  3  `@t`(scot %p -.us)
            %^  cat  3  '/'
            +.us
        :-  p
        :-  ~['------------']
        :-  r
        :-  ~['++++++++++++']
        :-  q
        :-  :_  ~
            %^  cat  3  '>>>>>>>>>>>>' 
            %^  cat  3  ' '
            %^  cat  3  `@t`(scot %p -.th)
            %^  cat  3  '/'
            +.th
        ~
      ::

XX document

### `++match-merge`

      ++  match-merge                                   ::  match merge
        |=  [p=(urge ,@t) q=(urge ,@t) r=(list ,@t)]    ::  resolve conflict
        =|  s=[p=(list ,@t) q=(list ,@t)]               ::  p chunk
        =|  t=[p=(list ,@t) q=(list ,@t)]               ::  q chunk
        |-  ^-  $:  p=[p=(list ,@t) q=(list ,@t)]
                    $=  q
                    $:  p=(urge ,@t)
                        q=(urge ,@t)
                        r=(list ,@t)
                        s=(list ,@t)
                ==  ==
        ?~  p  [[q.s q.t] p q r p.s]                    ::  can't be conflict
        ?~  q  [[q.s q.t] p q r p.s]                    ::  can't be conflict
        ?-  -.i.p
          %&  ?>  ?=(%| -.i.q)                          ::  is possibly conflict
              ?:  (gte p.i.p (lent p.i.q))              ::  trivial resolve
                :::-  (weld p.s p.i.q)                    ::  extend to q
                :-  :-  (welp (flop (scag (lent p.i.q) r)) q.s)
                    (welp q.i.q q.t)
                :-  ?:  =(p.i.p (lent p.i.q))  t.p
                    [[%& (sub p.i.p (lent p.i.q))] t.p]
                :-  t.q
                :-  (flop (slag (lent p.i.q) r))
                (welp (flop (scag (lent p.i.q) r)) p.s)
              =+  tex=(flop (scag p.i.p r))
              ?~  t.p                                   ::  extend to end
                %=  $
                  ::s  [(welp p.s tex) (welp q.s tex)]
                  p  ~[[%| [tex tex]]]
                  ::r  (slag p.i.p r)
                ==
              ?>  ?=(%| -.i.t.p)                        ::  fake skip
              %=  $
                ::s  [(welp p.s tex) (welp q.s tex)]
                p  [[%| [(welp p.i.t.p tex) (welp q.i.t.p tex)]] t.t.p]
                ::r  (slag p.i.p r)
              ==
          %|  ?-  -.i.q
                 %&  =+  mar=$(p q, q p, s t, t s)      ::  swap recursion
                     [[q.p.mar p.p.mar] q.q.mar p.q.mar r.q.mar s.q.mar]
                 %|  ?:  =((lent p.i.p) (lent p.i.q))   ::  perfect conflict
                       ?>  =(p.i.p p.i.q)               ::  sane conflict
                       :-  :-  (welp q.i.p q.s)
                           (welp q.i.q q.t)
                       :-  t.p 
                       :-  t.q 
                       :-  (scag (lent p.i.p) r)
                       (welp (flop (scag (lent p.i.p) r)) p.s)
                     ?.  (lth (lent p.i.p) (lent p.i.q))
                       =+  mar=$(p q, q p, s t, t s)    ::  swap recursion
                       [[q.p.mar p.p.mar] q.q.mar p.q.mar r.q.mar s.q.mar]
                     ?>  .=  p.i.p                      ::  sane conflict
                         (slag (sub (lent p.i.q) (lent p.i.p)) p.i.q)
                     %=  $                              ::  extend p
                       p  t.p
                       p.s  (welp p.i.p p.s)
                       q.s  (welp q.i.p q.s)
                       p.t  (welp p.i.p p.s)            ::  subset of q
                       q.t  (welp q.i.q q.s)            ::  just consume all out
                       q  [[%| (scag (sub (lent p.i.q) (lent p.i.p)) p.i.q) ~] t.q]
                       r  (slag (lent p.i.p) r)
                     ==
                 ==
          ==

XX document

### `++qeal`

      ++  qeal                                          ::  merge p,q
        |*  [us=[ship desk] th=[ship desk] pat=path p=miso q=miso r=(list) con=?]
        ^-  miso                                        ::  in case of conflict
        ~|  %qeal-fail
        ?>  ?=(%mut -.p)
        ?>  ?=(%mut -.q)
        ?>  ?=(%c -.q.p.p)
        ?>  ?=(%c -.q.p.q)
        =+  s=(clean p.q.p.p)
        =+  t=(clean p.q.p.q)
        :-  %mut
        :-  %c  ::  todo is this p.p.p?
        :-  %c
        |-  ^-  (urge)
        ::?~  s  ?:  (qual t)  t
        ::       ~|  %qail-conflict  !!
        ::?~  t  ?:  (qual s)  s
        ::       ~|  %qail-conflict  !!
        ?~  s  t
        ?~  t  s
        ?-    -.i.s
            %&
          ?-    -.i.t
              %&
            ?:  =(p.i.s p.i.t)
              [i.s $(s t.s, t t.t, r (slag p.i.s r))]
            ?:  (gth p.i.s p.i.t)
              [i.t $(t t.t, p.i.s (sub p.i.s p.i.t), r (slag p.i.t r))]
            [i.s $(s t.s, p.i.t (sub p.i.t p.i.s), r (slag p.i.s r))]
              %|
            ?:  =(p.i.s (lent p.i.t))
              [i.t $(s t.s, t t.t, r (slag p.i.s r))]
            ?:  (gth p.i.s (lent p.i.t))
              :-  i.t 
              $(t t.t, p.i.s (sub p.i.s (lent p.i.t)), r (slag (lent p.i.t) r))
            ?.  con  ~|  %quil-conflict  !!           ::  conflict
            ~&  [%quil-conflict-soft pat]
            =+  mar=(match-conflict us th s t r)
            [[%| p.mar] $(s p.q.mar, t q.q.mar, r r.q.mar)]
          ==
            %|
          ?-    -.i.t
              %|
            ?.  con  ~|  %quil-conflict  !!
            ~&  [%quil-conflict-soft pat]
            =+  mar=(match-conflict us th s t r)
            [[%| p.mar] $(s p.q.mar, t q.q.mar, r r.q.mar)]
              %&
            ?:  =(p.i.t (lent p.i.s))
              [i.s $(s t.s, t t.t, r (slag p.i.t r))]
            ?:  (gth p.i.t (lent p.i.s))
              :-  i.s
              $(s t.s, p.i.t (sub p.i.t (lent p.i.s)), r (slag (lent p.i.s) r))
            ?.  con  ~|  %quil-conflict  !!
            ~&  [%quil-conflict-soft pat]
            =+  mar=(match-conflict us th s t r)
            [[%| p.mar] $(s p.q.mar, t q.q.mar, r r.q.mar)]
          ==
        ==

XX document

### `++quil`

      ++  quil                                          ::  merge p,q
        |=  $:  us=[ship desk]
                th=[ship desk]
                pat=path
                p=(unit miso)
                q=(unit miso)
                r=(unit (list))
                con=?
            ==
        ^-  (unit miso)
        ?~  p  q                                        ::  trivial
        ?~  q  p                                        ::  trivial
        ?-  -.u.p
          %ins  ?>  ?=(%ins -.u.q)
                ?.  con  !!
                %-  some
                :-  %ins
                %-  role
                %-  annotate
                :-  us 
                :-  th
                :-  (lore ((hard ,@) p.u.p)) 
                :-  (lore ((hard ,@) p.u.q))
                ~
          %del  p
          %mut  ?>  ?=(%mut -.u.q)
                %-  some
                %^  qeal  us  th
                :^  pat  u.p  u.q                       ::  merge p,q
                :-  %-  need  r
                con
        ==
      ::

XX document

### `++meld`

      ++  meld                                          ::  merge p,q from r
        |=  [p=yaki q=yaki r=yaki con=? us=[ship desk] th=[ship desk]]
        ^-  (map path blob)
        =+  s=(diff-yakis r p)
        =+  t=(diff-yakis r q)
        =+  lut=(luth s t)
        %-  |=  res=(map path blob)                        ::  add old
            ^-  (map path blob)
            %-  ~(uni by res)
            %-  mo
            %+  turn  
              %+  skip  (~(tap by q.r) ~)                  ::  loop through old
              |=  [pat=path bar=lobe]  ^-  ?
              (~(has in lut) pat)                          ::  skip updated
            |=  [pat=path bar=lobe]  ^-  [path blob]
            [pat (lobe-to-blob bar)]                       ::  lookup objects
        %+  roll  (~(tap in (luth s t)) ~)
        |=  [pat=path res=(map path blob)]
        =+  ^=  v
            %-  need
            %^  quil  us  th
            :-  pat
            :+  (~(get by s) pat)
              (~(get by t) pat)
            :_  con
            %-  %-  lift  lore
            %-  %-  lift  %-  hard  ,@                     ::  for %c
            %-  %-  lift  lobe-to-noun
            %-  ~(get by q.r)
            pat
        ?-    -.v
            %del  res                                      ::  no longer exists
            %ins                                           ::  new file
          %+  ~(put by res)  pat 
          %+  make-direct  p.v  %c                         ::  TODO content type?
            %mut                                           ::  patch from r
          %+  ~(put by res)  pat
          %-  make-direct
          :_  %c
          %+  lump  p.v
          %-  lobe-to-noun
          %-  ~(got by q.r)  pat
        ==
      ::
      ::  merge types
      ::

XX document

### `++mate`

      ++  mate                                          ::  merge p,q
        |=  con=?                                       ::  %mate, %meld
        |=  [p=yaki q=yaki us=[ship desk] th=[ship desk]]
        ^-  (map path blob)
        =+  r=(~(tap in (find-merge-points p q)) ~)
        ?~  r
          ~|(%mate-no-ancestor !!)
        ?:  =(1 (lent r))
          (meld p q i.r con us th)
        ~|(%mate-criss-cross !!)
      ::

XX document

### `++keep`

      ++  keep                                          ::  %this
        |=  [p=yaki q=yaki [ship desk] [ship desk]]
        ^-  (map path blob)
        %+  roll  (~(tap by q.p) ~)
        |=  [[pat=path lob=lobe] zar=(map path blob)]
        ^-  (map path blob)
        (~(put by zar) pat (lobe-to-blob lob))
      ::

XX document

### `++drop`

      ++  drop                                          ::  %that
        |=  [p=yaki q=yaki r=[ship desk] s=[ship desk]]
        ^-  (map path blob)
        (keep q p r s)
      ::

XX document

### `++forge`

      ++  forge                                         ::  %forge
        |=  [p=yaki q=yaki s=[ship desk] t=[ship desk]]
        ^-  (map path blob)
        =+  r=(~(tap in (find-merge-points p q)) ~)
        ?~  r
          ~|(%forge-no-ancestor !!)
        %-  |=  [r=yaki lut=(map lobe blob) hat=(map tako yaki)]
            =.  lat  lut
            =.  hut  hat
            (meld p q r & s t)                          ::  fake merge
        %+  roll  t.r                                   ::  fake ancestor
        |=  [par=yaki [for=_i.r lut=_lat hat=_hut]]
        =.  lat  lut
        =+  ^=  far
            ^-  (map path lobe)
            %-  ~(tur by (forge par for s t))
            |=  [k=path v=blob]  (blob-to-lobe v)
        =+  u=(make-yaki [r.par r.for ~] far `@da`0)    ::  fake yaki
        :-  u
        :_  (~(put by hat) r.u u)
        =<  -
        %-  update-lat
        :_  ~
        %-  ~(tur by q.u)
        |=  [path k=lobe]
        (lobe-to-blob k)
      ::
      ::  actual merge
      ::

XX document

### `++merge`

      ++  merge
        |=  [us=[ship desk] th=[ship desk]]
        |=  [p=yaki q=yaki r=@da s=$+([yaki yaki [ship desk] [ship desk]] (map path blob))]
        ^-  [yaki (map path blob)]
        =+  u=(s p q us th)
        =+  ^=  t
            ^-  (map path lobe)
            %+  roll  (~(tap by u) ~)
            |=  [[pat=path bar=blob] yeb=(map path lobe)]
            (~(put by yeb) pat (blob-to-lobe bar))
        :_  u
        (make-yaki [r.p r.q ~] t r)
      ::

XX document

### `++strategy`

      ++  strategy                                          ::  merge strategy
        |=  gem=?(%meld %mate %that %this)
        ?-  gem
          %meld  (mate %.y)
          %mate  (mate %.n)
          %this  keep
          %that  drop
        ==
      ::

XX document

### `++construct-merge`

      ++  construct-merge                                   ::    construct-merge:ze
        |=  [gem=germ who=ship des=desk sab=saba now=@da]   ::  construct merge
        ^-  (unit (unit mizu))                              ::::::
        =+  for=s.sab                                       ::  foreign dome
        =+  mer=(merge [who des] [p.sab q.sab])
        ?-  gem
            %init                                           ::  force fine
              ?.  =(let 0)                                  ::  hell no
                !!
              =+  hot=(~(put by _(map aeon tako)) 1 (~(got by hit.for) let.for))
              [~ [~ [1 hot hut lat]]]                       ::  trivial
            %fine
              =+  der=(~(got by hit.for) let.for)
              =+  owr=(~(got by hit) let)
              ?:  =(der owr)
                [~ ~]
              ?:  (~(has in (reachable-takos owr)) der)
                [~ ~]
              ?.  (~(has in (reachable-takos der)) owr)
                ~                                          ::  not a fast forward
              ~&  [%merge-fine p.sab q.sab]
              [~ [~ [+(let) (~(put by hit) +(let) der) hut lat]]]
            ?(%mate %that %this %meld)
              =+  foreign-head=(tako-to-yaki (~(got by hit.for) let.for))
              =+  our-head=(tako-to-yaki (~(got by hit) let))
              ?:  =(r.foreign-head r.our-head)
                [~ ~]                                      ::  up to date
              ?:  (~(has in (reachable-takos r.our-head)) r.foreign-head)
                [~ ~]                                      ::  up to date
              ?:  ?&  |(=(gem %mate) =(gem %meld))
                      (~(has in (reachable-takos r.foreign-head)) r.our-head)
                  ==
                $(gem %fine)                               ::  use fast forward
              =+  gar=(mer our-head foreign-head now (strategy gem))
              =+  yak=-.gar
              =+  hek=+.gar
              =.  lat  -:(update-lat hek ~)                ::  add new blobs
              =.  hut  (~(put by _(map tako yaki)) r.yak yak)
              =.  let  +(let)
              =.  hit  (~(put by _(map aeon tako)) let r.yak)
              [~ [~ [let hit hut lat]]]
        ==
      ::

XX document

### `++read`

      ++  read                                              ::    read:ze
        |=  mun=mood                                        ::  read at point
        ^-  (unit)
        ?:  ?=(%v p.mun)
          [~ `dome`+<+<.read]
        ?:  &(?=(%w p.mun) !?=(%ud -.q.mun))
          ?^(r.mun ~ [~ let])
        ?:  ?=(%w p.mun)
          =+  ^=  yak
              %-  tako-to-yaki
              %-  aeon-to-tako
              let
          ?^(r.mun ~ [~ [t.yak (forge-nori yak)]])
          ::?>  ?=(^ hit)  ?^(r.mun ~ [~ i.hit])     ::  what do?? need [@da nori]
        (query(ank ank:(descend-path:(zu ank) r.mun)) p.mun)
      ::

XX document

### `++read-at-aeon`

      ++  read-at-aeon                                      ::    read-at-aeon:ze
        |=  [oan=aeon mun=mood]                             ::  seek and read
        ^-  (unit)
        ?:  &(?=(%w p.mun) !?=(%ud -.q.mun))                ::  NB only for speed
          ?^(r.mun ~ [~ oan])
        (read:(rewind oan) mun)
      ::

XX document

### `++equiv`

      ++  equiv                                             ::  test paths
        |=  [p=(map path lobe) q=(map path lobe)]
        ^-  ?
        %-  |=  qat=?
            ?.  qat  %.n
            %+  roll  (~(tap by q) ~)
            |=  [[pat=path lob=lobe] eq=?]
            ^-  ?
            ?.  eq  %.n
            (~(has by p) pat)
        %+  roll  (~(tap by p) ~)
        |=  [[pat=path lob=lobe] eq=?]
        ^-  ?
        ?.  eq  %.n
        =+  zat=(~(get by q) pat)
        ?~  zat  %.n
        =((lobe-to-noun u.zat) (lobe-to-noun lob))
      ::

XX document

### `++edit`

      ++  edit                                              ::    edit:ze
        |=  [wen=@da lem=nori]                              ::  edit
        ^+  +>
        ?-  -.lem
          &  =+  ^=  yet 
                 %+  forge-yaki  wen
                 ?:  =(let 0)                               ::  initial import
                   [~ q.lem]
                 [(some r:(tako-to-yaki (aeon-to-tako let))) q.lem]
             =+  yak=-.yet
             =.  lat  +.yet                                 ::  merge objects
             ?.  ?|  =(0 let)
                     !=((lent p.yak) 1)
                     !(equiv q.yak q:(tako-to-yaki (aeon-to-tako let)))
                 ==
               +>.$                                         ::  silently ignore
             =:  let  +(let)
                 hit  (~(put by hit) +(let) r.yak)
                 hut  (~(put by hut) r.yak yak)
             ==
             +>.$(ank (checkout-ankh q.yak))
          |  +>.$(lab ?<((~(has by lab) p.lem) (~(put by lab) p.lem let)))
        ==
      --
    ::

XX document

### `++zu`

    ++  zu                                                  ::  filesystem
      |=  ank=ankh                                          ::  filesystem state
      =|  myz=(list ,[p=path q=miso])                       ::  changes in reverse
      =|  ram=path                                          ::  reverse path into
      |%

XX document

### `++rehash`

      ++  rehash                                            ::  local rehash
        ^-  cash
        %+  mix  ?~(q.ank 0 p.u.q.ank)
        =+  axe=1
        |-  ^-  cash
        ?~  r.ank  _@
        ;:  mix
          (shaf %dash (mix axe (shaf %dush (mix p.n.r.ank p.q.n.r.ank))))
          $(r.ank l.r.ank, axe (peg axe 2))
          $(r.ank r.r.ank, axe (peg axe 3))
        ==
      ::

XX document

### `++update-hash`

      ++  update-hash  %_(. p.ank rehash)                   ::  rehash and save

XX document

### `++ascend`

      ++  ascend                                            ::  ascend
        |=  [lol=@ta kan=ankh]
        ^+  +>
        ?>  &(?=(^ ram) =(lol i.ram))
        %=    +>
            ram  t.ram
            ank
          ?:  =([0 ~ ~] ank)
            ?.  (~(has by r.kan) lol)  kan
            kan(r (~(del by r.kan) lol))
          kan(r (~(put by r.kan) lol ank))
        ==
      ::

XX document

### `++push-change`

      ++  push-change                                       ::  add change
        |=  mis=miso
        ^+  +>
        +>(myz [[(flop ram) mis] myz])
      ::

XX document

### `++descend`

      ++  descend                                           ::  descend
        |=  lol=@ta
        ^+  +>
        =+  you=(~(get by r.ank) lol)
        +>.$(ram [lol ram], ank ?~(you [*cash ~ ~] u.you))
      ::

XX document

### `++descend-path`

      ++  descend-path                                      ::  descend recursively
        |=  way=path
        ^+  +>
        ?~(way +> $(way t.way, +> (descend i.way)))
      ::

XX document

### `++overwrite`

      ++  overwrite                                         ::  write over
        |=  [pum=umph val=(unit ,[p=cash q=*])]
        ^+  +>
        ?~  q.ank
          ?~  val  +>
          (push-change %ins q.u.val)
        ?~  val
          (push-change %del q.u.q.ank)
        ?:  =(q.u.val q.u.q.ank)  +>
        (push-change %mut ((diff pum) q.u.q.ank q.u.val))
      ::

XX document

### `++change-tree`

      ++  change-tree                                       ::  modify tree
        |=  [pum=umph bus=ankh]
        ^+  +>
        =.  +>  (overwrite pum q.bus)
        =+  [yeg=(~(tap by r.ank) ~) gey=(~(tap by r.bus) ~)]
        =.  +>.$
          |-  ^+  +>.^$
          ?~  yeg  +>.^$
          ?:  (~(has by r.bus) p.i.yeg)  $(yeg t.yeg)
          $(yeg t.yeg, myz myz:rm-r(ank q.i.yeg, ram [p.i.yeg ram]))
        |-  ^+  +>.^$
        ?~  gey  +>.^$
        $(gey t.gey, myz myz:^$(bus q.i.gey, +> (descend p.i.gey)))
      ::

XX document

### `++rm-r`

      ++  rm-r                                              ::  rm -r
        |-  ^+  +
        =.  +  ?~(q.ank + (push-change %del q.u.q.ank))
        =+  dyr=(~(tap by r.ank) ~)
        |-  ^+  +.^$
        ?~  dyr  +.^$
        =.  +.^$  rm-r:(descend p.i.dyr)
        $(dyr t.dyr)
      ::

XX document

### `++drum`

      ++  drum                                              ::  apply effect
        |=  [pax=path mis=miso]                             ::  XX unused (++dune)
        ^+  +>
        ?^  pax
          update-hash:(ascend:$(pax t.pax, +> (descend i.pax)) i.pax ank)
        ~|  %clay-fail
        ?-    -.mis
            %del
          ?>  &(?=(^ q.ank) =(q.u.q.ank p.mis))
          +>.$(p.ank (mix p.u.q.ank p.ank), q.ank ~)
        ::
            %ins
          ?>  ?=(~ q.ank)
          =+  sam=(sham p.mis)
          +>.$(p.ank (mix sam p.ank), q.ank [~ sam p.mis])
        ::
            %mut
          ?>  ?=(^ q.ank)
          =+  nex=(lump p.mis q.u.q.ank)
          =+  sam=(sham nex)
          +>.$(p.ank :(mix sam p.u.q.ank p.ank), q.ank [~ sam nex])
        ==
      ::

XX document

### `++dune`

      ++  dune                                              ::  apply
        |-  ^+  +                                           ::  XX unused (++durn)
        ?~  myz  +
        =>  .(+ (drum p.i.myz q.i.myz))
        $(myz ?>(?=(^ myz) t.myz))
      ::

XX document

### `++durn`

      ++  durn                                              ::  apply forward
        |=  nyp=soba                                        ::  XX unused
        ^+  +>
        ?:  =([0 0] p.nyp)
          dune(myz q.nyp)
        =>  ?:  =(p.ank p.p.nyp)  .
            ~&  [%durn-in-wrong p.ank p.p.nyp]
            .
        =.  +>  dune(myz q.nyp)
        =>  ?:  =(p.ank q.p.nyp)  .
            ~&  [%durn-out-wrong p.ank q.p.nyp]
            .
        +>
      --

XX document
