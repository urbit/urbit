!:
=>  %=    .
        +
      =>  +
      =>
        |%
        ++  down
          $&  [p=down q=down]
          $%  [%$ p=tape]
              [%code p=tape]
              [%inco p=tape]
              [%head p=haxe q=down]
              [%link p=tape q=tape r=(unit tape)]
              [%lord p=(list down)]
              [%lund p=(list down)]
              [%parg p=down]
              [%quot p=down]
              [%rong p=down]
              [%emph p=down]
              [%hrul ~]
              [%html p=tape]
          ==
        ++  haxe
          |=  a=*
          ?@  a
            ?:(&(!=(0 a) (lth a 6)) a 6)
          6
        --
      |%
      ++  cott
        ;~  pose
          (shim 0 8)
          (shim 11 37)
          (shim 39 59)
          (just `@`61)
          (shim 63 95)
          (shim 97 255)
        ==
      ++  copt
        ;~  pose
          (shim 0 9)
          (shim 11 37)
          (shim 39 39)
          (shim 43 59)
          (just `@`61)
          (shim 63 90)
          (shim 94 94)
          (shim 97 255)
        ==
      ++  urlc  ;~(pose (shim 0 9) (shim 11 31) (shim 33 39) (shim 42 255))
      ++  uctt  ;~(pose (shim 0 9) (shim 11 91) (shim 94 255))
      ++  uctc  ;~(pose (shim 0 9) (shim 11 33) (shim 35 39) (shim 42 255))
      ++  htmc  ;~(pose (shim 0 9) (shim 11 255))
      ++  escp  (mask "\\`*_\{}[]()#+-.!")
      ++  escd  ;~(pfix bas escp)
      ++  whit  (mask (tape 9 32 ~))
      ++  dent  ;~(pose (jest '    ') (just `@`9))
      ++  blan  (cold ~ ;~(plug (star whit) (just `@`10)))
      ++  mcat  (bend |=([a=tape b=tape] (some (weld a b))))
      ++  wcat  |*(a=_rule (cook |=([a=tape b=tape] (weld a b)) a))
      ++  codt
        %+  knee  *tape  |.  ~+
        ;~  pose
          (wcat ;~(plug (plus cott) codt))
          (cook |=(a=tape (weld "&lt;" a)) ;~(pfix gal ;~(pose codt (easy ~))))
          (cook |=(a=tape (weld "&gt;" a)) ;~(pfix gar ;~(pose codt (easy ~))))
          (cook |=(a=tape (weld "&amp;" a)) ;~(pfix pam ;~(pose codt (easy ~))))
          (plus cott)
        ==
      ++  inlt
        %+  knee  *tape  |.  ~+
        ;~  pose
          ;~(mcat (plus copt) inlt)
          ;~(mcat (plus escd) inlt)
          ;~  mcat
            %-  wcat
              ;~  plug
                ;~(plug gal (cook trip inle))
                ;~(sfix (wcat ;~(plug (star copt) (easy ">"))) gar)
              ==
            inlt
          ==
          ;~  mcat
            %-  wcat
              ;~  plug
                ;~(plug gal fas (cook trip inle))
                ;~(sfix (wcat ;~(plug (star copt) (easy ">"))) gar)
              ==
            inlt
          ==
          ;~  mcat
            (wcat ;~(plug ;~(plug pam hax ;~(sfix (plus nud) sem)) (easy ";")))
            inlt
          ==
          ;~  mcat
            (wcat ;~(plug ;~(plug pam ;~(sfix (plus alf) sem)) (easy ";")))
            inlt
          ==
          (cook |=(a=tape (weld "&lt;" a)) ;~(pfix gal ;~(pose inlt (easy ~))))
          (cook |=(a=tape (weld "&gt;" a)) ;~(pfix gar ;~(pose inlt (easy ~))))
          (cook |=(a=tape (weld "&amp;" a)) ;~(pfix pam ;~(pose inlt (easy ~))))
        ==
      ++  kite
        |=  bud=(list ,@t)
        |=  tub=nail
        |-  ^-  (like ,@t)
        ?@  bud
          (fail tub)
        =+  foo=((jest i.bud) tub)
        ?~  q.foo
          $(bud t.bud)
        foo
      ++  bloc
        %+  knee  *@t  |.  ~+
        %-  kite
          :~  '<address>'  '<article>'  '<aside>'  '<audio>'  '<blockquote>'
              '<canvas>'  '<dd>'  '<div>'  '<dl>'  '<fieldset>'  '<figcaption>'
              '<figure>'  '<footer>'  '<form>'  '<h1>'  '<h2>'  '<h3>'  '<h4>'
              '<h5>'  '<h6>'  '<header>'  '<hgroup>'  '<hr>'  '<noscript>'
              '<ol>'  '<output>'  '<p>'  '<pre>'  '<section>'  '<table>'
              '<tfoot>'  '<ul>'  '<video>'  '<style>'
          ==
      ++  inle
        %+  knee  *@t  |.  ~+
        %-  kite
          :~  'b'  'big'  'i'  'small'  'tt'  'abbr'  'acronym'
              'cite'  'code'  'dfn'  'em'  'kbd'  'strong'  'samp'
              'var'  'a'  'bdo'  'br'  'img'  'map'  'object'  'q'
              'script'  'span'  'sub'  'sup'  'button'  'input'
              'label'  'select'  'textarea'  'link'
          ==
      ++  htmb
        %+  knee  *tape  |.  ~+
        %+  cook  |=(a=(list tape) (reel a |=([p=tape q=tape] (weld p q))))
        (plus (cook |=(a=tape (weld a "\0a")) ;~(sfix (plus htmc) (just '\0a'))))
      ++  blok
        %+  knee  *tape  |.  ~+
        %+  cook  |=(a=[tape tape] (weld a))
        ;~(plug (cook trip bloc) ;~(plug (just '\0a') htmb))
      ++  intt
        %+  knee  *down  |.  ~+
        ;~  pose
          (ifix [(jest '**') (jest '**')] (stag %rong intt))
          (ifix [(jest '__') (jest '__')] (stag %rong intt))
          (ifix [tar tar] (stag %emph intt))
          (ifix [cab cab] (stag %emph intt))
          (ifix [tec tec] (stag %inco codt))
          (stag %$ inlt)
          link
          (stag %$ ;~(plug tar (easy ~)))
          (stag %$ ;~(plug cab (easy ~)))
          (stag %$ ;~(plug pel (easy ~)))
          (stag %$ ;~(plug per (easy ~)))
          (stag %$ ;~(plug sel (easy ~)))
          (stag %$ ;~(plug ser (easy ~)))
        ==
      ++  inli
        |=  tub=nail
        ^-  (like down)
        ?~  q.tub
          (fail tub)
        ?:  |(=(i.q.tub 10) =(i.q.tub '>'))
          (fail tub)
        =+  ^=  foo
          ;~  pose
            dent
            ;~(plug (mask "+*-") (plus whit))
            ;~(plug dim:ag dot (plus whit))
          ==
        =+  bar=(foo tub)
        ?~  q.bar
          %-
          %+  cook  |=(a=(list down) (reel a |=([p=down q=down] [p q])))
          ;~(sfix (plus intt) (just `@`10))
          tub
        (fail tub)
      ++  parg
        |=  [a=@ b=@]
        %+  knee  *down  |.  ~+
        ;~  plug
          inli
          %+  cook  |=(a=(list down) (reel a |=([p=down q=down] [p q])))
            (plus ;~(plug (easy [%$ "\0a"]) (colk a b inli)))
        ==
      ++  link
        %+  knee  *down  |.  ~+
        %+  stag  %link
        ;~  plug
          (ifix [sel ser] (plus ;~(pose uctt escd)))
          ;~(pfix pel (star ;~(pose urlc escd)))
          ;~  pose
            %+  cook  |=(a=tape (some a))
              (ifix [ace per] (ifix [doq doq] (plus ;~(pose uctc escd))))
            (cold ~ per)
          ==
        ==
      ++  barg
        |=  a=@
        %+  knee  *haxe  |.  ~+
        ;~  pfix  (stun [a a] (jest '> '))
        ;~  pose
          (cold 1 ;~(plug (plus tis) (star whit) (just `@`10)))
          (cold 2 ;~(plug (plus hep) (star whit) (just `@`10)))
        ==
        ==
      ++  neck
        %+  knee  *[haxe down]  |.  ~+
        ;~  pose
          ;~(pfix (jest '######') (stag 6 ;~(pfix (star whit) inli)))
          ;~(pfix (jest '#####') (stag 5 ;~(pfix (star whit) inli)))
          ;~(pfix (jest '####') (stag 4 ;~(pfix (star whit) inli)))
          ;~(pfix (jest '###') (stag 3 ;~(pfix (star whit) inli)))
          ;~(pfix (jest '##') (stag 2 ;~(pfix (star whit) inli)))
          ;~(pfix (jest '#') (stag 1 ;~(pfix (star whit) inli)))
        ==
      ++  mark
        |=  p=tape
        (scan p park)
      ++  hrul
        %+  knee  *down  |.  ~+
        %+  sear
          |=(a=(list tape) ?:((gte (lent (zing a)) 3) (some [%hrul ~]) ~))
          ;~  sfix
            (more (star whit) ;~(pose (plus cab) (plus tar) (plus hep)))
            (just `@`10)
          ==
      ++  colk
        |*  [a=@ b=@ fel=_rule]
        ;~(pfix (stun [a a] (jest '> ')) (stun [b b] dent) fel)
      ++  code
        |=  [a=@ b=@]
        %+  knee  *tape  |.  ~+
        %+  cook  weld
        ;~  plug
          (ifix [dent (just '\0a')] codt)
          %+  cook  |=(a=(list tape) (reel a |=([p=tape q=tape] (weld p q))))
          %-  star
          ;~(plug (easy '\0a') (colk a b (ifix [dent (just '\0a')] codt)))
        ==
      ++  lelm
        |=  [a=@ b=@]
        %+  knee  *down  |.  ~+
        ;~  pose
          inli
          hrul
          ;~(pfix (just `@`10) (lmel a b))
        ==
      ++  lmel
        |=  [a=@ b=@]
        %+  knee  *down  |.  ~+
        ;~  pose
          ;~((bend) (stag %lund (plus (lulm a b))) (lmel a b))
          ;~((bend) (stag %lord (plus (lolm a b))) (lmel a b))
          ;~((bend) (stag %code (colk a b (code a b))) (lmel a b))
          ;~((bend) (stag %head (colk a b neck)) (lmel a b))
          ;~((bend) (stag %parg (colk a b (parg a b))) (lmel a b))
          (colk a b inli)
          (colk a b hrul)
          ;~(pfix (plus (colk a b blan)) (lmel a b))
        ==
      ++  lulm
        |=  [a=@ b=@]
        %+  knee  *down  |.  ~+
        (colk a b ;~(pfix (mask "+*-") (plus whit) (lelm a +(b))))
      ++  lolm
        |=  [a=@ b=@]
        %+  knee  *down  |.  ~+
        (colk a b ;~(pfix dim:ag dot (plus whit) (lelm a +(b))))
      ++  bark
        |=  a=@
        %+  knee  *down  |.  ~+
        ;~  pose
          (stag %html blok)
          (stag %lund (plus (lulm a 0)))
          (stag %lord (plus (lolm a 0)))
          (stag %code (colk a 0 (code a 0)))
          (stag %head (colk a 0 neck))
          %+  cook  |=([a=down b=haxe] [%head b a])
            (colk a 0 ;~(plug inli (barg a)))
          (colk a 0 hrul)
          (stag %parg (colk a 0 (parg a 0)))
          (colk a 0 inli)
          (cold [%$ ""] (colk a 0 (plus blan)))
        ==
      ++  dark
        |=  a=@
        %+  knee  *down  |.  ~+
        |=  tub=nail
        ^-  (like down)
        =+  vex=((cook lent (star (jest '> '))) tub)
        ?~  q.vex
          vex
        ?:  (lth p.u.q.vex a)
          (fail tub)
        ?:  (gth p.u.q.vex a)
          (;~((bend) (stag %quot (dark +(a))) (dark a)) tub)
        (;~((bend) (bark a) (dark a)) tub)
      ++  park
        %+  knee  *down  |.  ~+
        %+  cook  |=(a=(list down) (reel a |=([p=down q=down] [p q])))
          ;~(sfix (plus (dark 0)) (star blan))
      ++  appd
        |=  [p=@ q=@]
        ^-  @
        (cat 3 p q)
      ++  wtag
        |=  [a=@ b=@]
        ^-  @
        :(appd '<' a '>' b '</' a '>')
      ++  sett
        |=  [a=@ b=tape]
        ^-  @
        :(appd a '="' (rap 3 b) '"')
      ++  hark
        |=  a=down
        ^-  @
        ?-  a
          [%$ *]  (rap 3 p.a)
          [%code *]  (wtag 'pre' (wtag 'code' (rap 3 p.a)))
          [%inco *]  (wtag 'code' (rap 3 p.a))
          [%head *]  (wtag (cat 3 'h' (add '0' p.a)) (hark q.a))
          [%link *]
            ?~  r.a
              :(appd '<a ' (sett 'href' q.a) '>' (rap 3 p.a) '</a>')
              :(appd '<a ' (sett 'href' q.a) ' ' (sett 'title' u.r.a) '>' (rap 3 p.a) '</a>')
          [%lord *]  (wtag 'ol' (reel (turn p.a |=(a=down (wtag 'li' (hark a)))) appd))
          [%lund *]  (wtag 'ul' (reel (turn p.a |=(a=down (wtag 'li' (hark a)))) appd))
          [%parg *]  (wtag 'p' (hark p.a))
          [%quot *]  (wtag 'blockquote' (hark p.a))
          [%rong *]  (wtag 'strong' (hark p.a))
          [%emph *]  (wtag 'em' (hark p.a))
          [%hrul *]  '<hr>'
          [%html *]  (rap 3 p.a)
          ^  (cat 3 (hark p.a) (hark q.a))
        ==
      --
    ==
|=  txt=@
=+  tpt=(trip txt)
=+  mdp=(mark tpt)
:(appd '<html><body>' (hark mdp) '</body></html>')
