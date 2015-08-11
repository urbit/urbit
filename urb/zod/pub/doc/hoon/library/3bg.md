section 3bG, URL handling
=========================

### `++deft`

Import URL path

    ++  deft                                                ::  import url path
      |=  rax=(list ,@t)
      |-  ^-  pork
      ?~  rax
        [~ ~]
      ?~  t.rax
        =+  den=(trip i.rax)
        =+  ^=  vex
          %-  %-  full
              ;~(plug sym ;~(pose (stag ~ ;~(pfix dot sym)) (easy ~)))
          [[1 1] (trip i.rax)]
        ?~  q.vex
          [~ [i.rax ~]]
        [+.p.u.q.vex [-.p.u.q.vex ~]]
      =+  pok=$(rax t.rax)
      :-  p.pok
      [i.rax q.pok]
    ::

Parse the extension the from last element of url, which is delimited
either by a `.` or a `/`.

`rax` is a [`++list`]() of [`@t`]().

    ~zod/try=> (deft /foo/bar/'baz.txt')
    [p=[~ ~.txt] q=<|foo bar baz|>]
    ~zod/try=> (deft /foo/bar/baz)
    [p=~ q=<|foo bar baz|>]

### `++fain`

Restructure path

    ++  fain                                                ::  path restructure
      |=  [hom=path raw=path]
      =+  bem=(need (tome raw))
      =+  [mer=(flop s.bem) moh=(flop hom)]
      |-  ^-  (pair beam path)
      ?~  moh
        [bem(s hom) (flop mer)]
      ?>  &(?=(^ mer) =(i.mer i.moh))
      $(mer t.mer, moh t.moh)
    ::

Splits a concrete
[`++spur`]() out of a full `++path`, producing a location [`++beam`]() and a remainder [`++path`]().

`hom` is a [`++path`]()

    ~zod/try=> (fain / %)
    [p=[[p=~zod q=%try r=[%da p=~2014.11.1..00.07.17..c835]] s=/] q=/]
    ~zod/try=> (fain /lok %)
    ! exit
    ~zod/try=> (fain / %/mer/lok/tem)
    [ p=[[p=~zod q=%try r=[%da p=~2014.11.1..00.08.03..bfdf]] s=/] 
      q=/tem/lok/mer
    ]
    ~zod/try=> (fain /mer %/mer/lok/tem)
    [p=[[p=~zod q=%try r=[%da p=~2014.11.1..00.08.15..4da0]] s=/mer] q=/tem/lok]
    ~zod/try=> (fain /lok/mer %/mer/lok/tem)
    [p=[[p=~zod q=%try r=[%da p=~2014.11.1..00.08.24..4d9e]] s=/lok/mer] q=/tem]
    ~zod/try=> (fain /lok/mer %/mer)
    ! exit
    ~zod/try=> (fain /hook/hymn/tor %/tor/hymn/hook/'._req_1234__')
    [ p=[[p=~zod q=%try r=[%da p=~2014.11.1..00.09.25..c321]] s=/hook/hymn/tor]
      q=/._req_1234__
    ]

### `++fuel`

Parse fcgi

    ++  fuel                                                ::  parse fcgi
      |=  [bem=beam but=path]
      ^-  epic
      ?>  ?=([%web @ *] but)
      =+  dyb=(slay i.t.but)
      ?>  ?&  ?=([~ %many *] dyb)
              ?=([* * *] p.u.dyb)
              ::  ?=([%$ %tas *] i.p.u.dyb)
              ?=([%many *] i.p.u.dyb)
              ?=([%blob *] i.t.p.u.dyb)
          ==
      =+  ced=((hard cred) p.i.t.p.u.dyb)
      ::  =+  nep=q.p.i.p.u.dyb
      =+  ^=  nyp  ^-  path
          %+  turn  p.i.p.u.dyb
          |=  a=coin  ^-  @ta
          ?>  ?=([%$ %ta @] a)
          ?>(((sane %ta) q.p.a) q.p.a)
      =+  ^=  gut  ^-  (list ,@t)
          %+  turn  t.t.p.u.dyb
          |=  a=coin  ^-  @t
          ?>  ?=([%$ %t @] a)
          ?>(((sane %t) q.p.a) q.p.a)
      =+  ^=  quy
          |-  ^-  (list ,[p=@t q=@t])
          ?~  gut  ~
          ?>  ?=(^ t.gut)
          [[i.gut i.t.gut] $(gut t.t.gut)]
      :*  (~(gas by *(map cord cord)) quy)
          ced
          bem
          t.t.but
          nyp
      ==
    ::

Retrieieves the %eyre FCGI, producing a [`++epic`](). Used primarily in
[`/hook`]() files. See the [`%eyre`]() doc for more detail.

`bem` is a [`++beam`]().

`but` is a [`++path`]().

    ~zod/main=> (fuel [[p=~zod q=%try r=[%ud p=2]] s=/psal] /web/'._.~-~~~~.gen~-~-_~~05vg0001v09f0n30fbh7dn6ab2jakmmspdq04nef5h70qbd5lh6atr4c5j2qrbldpp62q1df1in0sr1ding0c3qgt7kclj74qb65lm6atrkc5k2qpr5e1mmispdchin4p3fegmiqrjpdlo62p1dchsn4p39comn8pbcehgmsbbef5p7crrifr3o035dhgfrk2b5__')
    [ qix={}
        ced
      [ hut=[p=%.y q=[~ 8.445] r=[%.n p=.0.0.0.0]]
        aut={[p=%$ q={'~rovryn-natlet-fidryd-dapmyn--todred-simpeg-hatwel-firfet'}]}
        orx='laspex-harnum-fadweb-mipbyn'
        acl=[~ 'en-US,en;q=0.8']
        cip=[%.y p=.127.0.0.1]
        cum={}
      ]
      bem=[[p=~zod q=%try r=[%ud p=2]] s=/psal]
      but=/
      nyp=/gen
    ]

### `++sifo`

64-bit encode

    ++  sifo                                                ::  64-bit encode
      |=  tig=@
      ^-  tape
      =+  poc=(~(dif fo 3) 0 (met 3 tig))
      =+  pad=(lsh 3 poc (swap 3 tig))
      =+  ^=  cha
      'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
      =+  ^=  sif
          |-  ^-  tape
          ?~  pad
            ~
          =+  d=(end 0 6 pad)
          [(cut 3 [d 1] cha) $(pad (rsh 0 6 pad))]
      (weld (flop (slag poc sif)) (trip (fil 3 poc '=')))
    ::

Encodes an atom to MIME base64, producing a [`++tape`]().

    ~zod/main=> (sifo 'foobar')
    "Zm9vYmFy"
    ~zod/main=> (sifo 1)
    "Q=="
    ~zod/main=> (sifo (shax %hi))
    "j0NDRmSPa5bfid2pAcUXaxCm2Dlh3TwayItZstwyeqQ="

### `++urle`

Encode URL

    ++  urle                                                ::  URL encode
      |=  tep=tape
      ^-  tape
      %-  zing
      %+  turn  tep
      |=  tap=char
      =+  xen=|=(tig=@ ?:((gte tig 10) (add tig 55) (add tig '0')))
      ?:  ?|  &((gte tap 'a') (lte tap 'z'))
              &((gte tap 'A') (lte tap 'Z'))
              &((gte tap '0') (lte tap '9'))
              =('.' tap)
              =('-' tap)
              =('~' tap)
              =('_' tap)
          ==
        [tap ~]
      ['%' (xen (rsh 0 4 tap)) (xen (end 0 4 tap)) ~]
    ::

The inverse of [`++urld`](). Accepts a tape `tep` and replaces all
characters other than alphanumerics and `.`, `-`, `~`, and `_`, with URL
escape sequences.

    ~zod/main=> (urle "hello")
    "hello"
    ~zod/main=> (urle "hello dear")
    "hello%20dear"
    ~zod/main=> (urle "hello-my?=me  !")
    "hello-my%3F%3Dme%20%20%21"

### `++urld`

Decode URL

    ++  urld                                                ::  URL decode
      |=  tep=tape
      ^-  (unit tape)
      ?~  tep  [~ ~]
      ?:  =('%' i.tep)
        ?.  ?=([@ @ *] t.tep)  ~
        =+  nag=(mix i.t.tep (lsh 3 1 i.t.t.tep))
        =+  val=(rush nag hex:ag)
        ?~  val  ~
        =+  nex=$(tep t.t.t.tep)
        ?~(nex ~ [~ [`@`u.val u.nex]])
      =+  nex=$(tep t.tep)
      ?~(nex ~ [~ i.tep u.nex])
    ::

The inverse of [`++urle`](). Parses a URL escaped tape to the
[`++unit`]() of an unescaped `++tape`.

`tep` is a [`++tape`]().

    ~zod/main=> (urld "hello")
    [~ "hello"]
    ~zod/main=> (urld "hello%20dear")
    [~ "hello dear"]
    ~zod/main=> (urld "hello-my%3F%3Dme%20%20%21")
    [~ "hello-my?=me  !"]
    ~zod/main=> (urld "hello-my%3F%3Dme%20%2%21")
    ~

### `++earl`

Localize purl

    ++  earl                                                ::  localize purl
      |=  [who=@p pul=purl]
      ^-  purl
      pul(q.q [(rsh 3 1 (scot %p who)) q.q.pul])
    ::

Prepends a ship name to the spur of a [`++purl`]().

`who` is a [`@p`](), a ship name.

`pul` is a [`++purl`]().

    ~zod/main=> (need (epur 'http://123.1.1.1/me.ham'))
    [p=[p=%.n q=~ r=[%.n p=.123.1.1.1]] q=[p=[~ ~.ham] q=<|me|>] r=~]
    ~zod/main=> (earl ~zod (need (epur 'http://123.1.1.1/me.ham')))
    [p=[p=%.n q=~ r=[%.n p=.123.1.1.1]] q=[p=[~ ~.ham] q=<|zod me|>] r=~]
    ~zod/main=> (earl ~pittyp (need (epur 'http://123.1.1.1/me.ham')))
    [p=[p=%.n q=~ r=[%.n p=.123.1.1.1]] q=[p=[~ ~.ham] q=<|pittyp me|>] r=~]
    ~zod/main=> (earn (earl ~pittyp (need (epur 'http://123.1.1.1/me.ham'))))
    "http://123.1.1.1/pittyp/me"

### `++earn`

Purl to tape

    ++  earn                                                ::  purl to tape
      |^  |=  pul=purl
          ^-  tape
          :(weld (head p.pul) "/" (body q.pul) (tail r.pul))
      ::

Parses a `++purl` `pul` to a [`++tape`]().

`pul` is a [`++purl`]().

    ~zod/main=> (earn [| ~ [%| .127.0.0.1]] [~ ~] ~)
    "http://127.0.0.1/"
    ~zod/main=> (earn [| ~ `/com/google/www] [~ ~] ~)
    "http://www.google.com/"
    ~zod/main=> (earn [& ~ `/com/google/www] [~ ~] ~)
    "https://www.google.com/"
    ~zod/main=> (earn [& `200 `/com/google/www] [~ ~] ~)
    "https://www.google.com:200/"
    ~zod/main=> (earn [& `200 `/com/google/www] [~ /search] ~)
    "https://www.google.com:200/search"
    ~zod/main=> (earn [& ~ `/com/google/www] [`%html /search] ~)
    "https://www.google.com/search"
    ~zod/main=> (earn [& ~ `/com/google/www] [~ /search] [%q 'urbit'] ~)
    "https://www.google.com/search?q=urbit"
    ~zod/main=> (earn [& ~ `/com/google/www] [~ /search] [%q 'urbit escaping?'] ~)
    "https://www.google.com/search?q=urbit%20escaping%3F"

### `++body`

Render URL path

      ++  body
        |=  pok=pork  ^-  tape
        ?~  q.pok  ~
        |-
        =+  seg=(trip i.q.pok)
        ?~  t.q.pok
          ?~(p.pok seg (welp seg '.' (trip u.p.pok)))
        (welp seg '/' $(q.pok t.q.pok))
      ::
      

Renders URL path `pok` as a [`++tape`]().

    ~zod/main=> (body:earn ~ /foo/mol/lok)
    "foo/mol/lok"
    ~zod/main=> (body:earn `%htm /foo/mol/lok)
    "foo/mol/lok.htm"
    ~zod/main=> (body:earn `%htm /)
    ""

### `++head`

Render URL beginning

      ++  head
        |=  har=hart
        ^-  tape
        ;:  weld
          ?:(&(p.har !=([& /localhost] r.har)) "https://" "http://")
        ::
          ?-  -.r.har
            |  (trip (rsh 3 1 (scot %if p.r.har)))
            &  =+  rit=(flop p.r.har)
               |-  ^-  tape
               ?~(rit ~ (weld (trip i.rit) ?~(t.rit "" `tape`['.' $(rit t.rit)])))
          ==
        ::
          ?~(q.har ~ `tape`[':' (trip (rsh 3 2 (scot %ui u.q.har)))])
        ==
      ::

Renders a `++hart`, usually the beginning of a URL, as the [`++tape`]()
of a traditional URL.

    ~zod/main=> (head:earn | ~ %| .127.0.0.1)
    "http://127.0.0.1"
    ~zod/main=> (head:earn & ~ %| .127.0.0.1)
    "https://127.0.0.1"
    ~zod/main=> (head:earn & [~ 8.080] %| .127.0.0.1)
    "https://127.0.0.1:8080"
    ~zod/main=> (head:earn & [~ 8.080] %& /com/google/www)
    "https://www.google.com:8080"

### `++tail`

Render query string

      ++  tail
        |=  kay=quay
        ^-  tape
        ?:  =(~ kay)  ~
        :-  '?'
        |-  ^-  tape
        ?~  kay  ~
        ;:  weld
          (urle (trip p.i.kay))
          "="
          (urle (trip q.i.kay))
          ?~(t.kay ~ `tape`['&' $(kay t.kay)])
        ==
      --
    ::

Renders a `quay`, a query string in hoon, to the [`++tape`]() of a
traditional query string.

`kay` is a [`++quay`]().

    ~zod/main=> (tail:earn ~)
    ""
    ~zod/main=> (tail:earn [%ask 'bid'] ~)
    "?ask=bid"
    ~zod/main=> (tail:earn [%ask 'bid'] [%make 'well'] ~)
    "?ask=bid&make=well"

### `++epur`

Top-level URL parser

    ++  epur                                                ::  url/header parser
      =<  |=(a=cord (rush a auri))
      |%

Toplevel URL parser.

`a` is a [`++cord`](/doc/hoon/library/1#++cord).

    ~zod/main=> (epur 'http://127.0.0.1/')
    [~ [p=[p=%.n q=~ r=[%.n p=.127.0.0.1]] q=[p=~ q=<||>] r=~]]
    ~zod/main=> (epur 'http://www.google.com/')
    [~ [p=[p=%.n q=~ r=[%.y p=<|com google www|>]] q=[p=~ q=<||>] r=~]]
    ~zod/main=> (epur 'https://www.google.com/')
    [~ [p=[p=%.y q=~ r=[%.y p=<|com google www|>]] q=[p=~ q=<||>] r=~]]
    ~zod/main=> (epur 'https//www.google.com/')
    ~
    ~zod/main=> (epur 'https://www.google.com:200/')
    [~ [p=[p=%.y q=[~ 200] r=[%.y p=<|com google www|>]] q=[p=~ q=<||>] r=~]]
    ~zod/main=> (epur 'https://www.google.com:200/search')
    [ ~
      [p=[p=%.y q=[~ 200] r=[%.y p=<|com google www|>]] q=[p=~ q=<|search|>] r=~]
    ]
    ~zod/main=> (epur 'https://www.google.com/search')
    [~ [p=[p=%.y q=~ r=[%.y p=<|com google www|>]] q=[p=~ q=<|search|>] r=~]]
    ~zod/main=> (epur 'https://www.google.com/search?q=urbit')
    [ ~ 
      [ p=[p=%.y q=~ r=[%.y p=<|com google www|>]]
        q=[p=~ q=<|search|>]
        r=~[[p='q' q='urbit']]
      ]
    ]
    ~zod/main=> (epur 'https://www.google.com/search?q=urb it')
    ~
    ~zod/main=> (epur 'https://www.google.com/search?q=urb%20it')
    [ ~
      [ p=[p=%.y q=~ r=[%.y p=<|com google www|>]] 
        q=[p=~ q=<|search|>] 
        r=~[[p='q' q='urb it']]
      ]
    ]
    ~zod/main=> (epur 'https://www.google.com/search?q=urbit%20escaping%3F')
    [ ~ 
      [ p=[p=%.y q=~ r=[%.y p=<|com google www|>]] 
        q=[p=~ q=<|search|>]
        r=~[[p='q' q='urbit escaping?']]
      ]
    ]

### `++apat`

      ++  apat                                              ::  2396 abs_path
        %+  cook  deft
        (ifix [fas ;~(pose fas (easy ~))] (more fas smeg))

URL path as ++pork

    ~zod/try=> (scan "/foo/mol/lok" apat:epur)
    [p=~ q=<|foo mol lok|>]
    ~zod/try=> (scan "/foo/mol/lok.htm" apat:epur)
    [p=[~ ~.htm] q=<|foo mol lok|>]

### `++auri`

      ++  auri
        %+  cook
          |=  a=purl
          ?.(=([& /localhost] r.p.a) a a(p.p &))
        ;~  plug
          ;~  plug
            %+  sear
              |=  a=@t
              ^-  (unit ,?)
              ?+(a ~ %http [~ %|], %https [~ %&])
            ;~(sfix scem ;~(plug col fas fas))
            thor
          ==
          ;~(plug ;~(pose apat (easy *pork)) yque)
        ==

URL parsing rule

    ~zod/main=> (auri:epur [1 1] "http://127.0.0.1/")
    [ p=[p=1 q=18] 
        q
      [ ~
          u
        [ p=[p=[p=%.n q=~ r=[%.n p=.127.0.0.1]] q=[p=~ q=<||>] r=~]
          q=[p=[p=1 q=18] q=""]
        ]
      ]
    ]
    ~zod/main=> (auri:epur [1 1] "http://www.google.com/")
    [ p=[p=1 q=23]
        q
      [ ~
         u
        [ p=[p=[p=%.n q=~ r=[%.y p=<|com google www|>]] q=[p=~ q=<||>] r=~] 
          q=[p=[p=1 q=23] q=""]
        ]
      ]
    ]
    ~zod/main=> (auri:epur [1 1] "https://www.google.com/")
    [ p=[p=1 q=24]
        q
      [ ~
         u
        [ p=[p=[p=%.y q=~ r=[%.y p=<|com google www|>]] q=[p=~ q=<||>] r=~] 
          q=[p=[p=1 q=24] q=""]
        ]
      ]
    ]
    ~zod/main=> (auri:epur [1 1] "https//www.google.com/")
    [ p=[p=1 q=6] q=~]
    ~zod/main=> (auri:epur [1 1] "https://www.google.com:200/")
    [ p=[p=1 q=28]
      q=[~ u=[p=[p=[p=%.y q=[~ 200] r=[%.y p=<|com google www|>]] q=[p=~ q=<||>] r=~] q=[p=[p=1 q=28] q=""]]]
    ]
    ~zod/main=> (auri:epur [1 1] "https://www.google.com:200/search")
    [ p=[p=1 q=34]
      q=[~ u=[p=[p=[p=%.y q=[~ 200] r=[%.y p=<|com google www|>]] q=[p=~ q=<|search|>] r=~] q=[p=[p=1 q=34] q=""]]]
    ]
    ~zod/main=> (auri:epur [1 1] "https://www.google.com/search")
    [ p=[p=1 q=30]
      q=[~ u=[p=[p=[p=%.y q=~ r=[%.y p=<|com google www|>]] q=[p=~ q=<|search|>] r=~] q=[p=[p=1 q=30] q=""]]]
    ]
    ~zod/main=> (auri:epur [1 1] "https://www.google.com/search?q=urbit")
    [ p=[p=1 q=38]
        q
      [ ~
          u
        [ p=[p=[p=%.y q=~ r=[%.y p=<|com google www|>]] q=[p=~ q=<|search|>] r=~[[p='q' q='urbit']]]
          q=[p=[p=1 q=38] q=""]
        ]
      ]
    ]
    ~zod/main=> (auri:epur [1 1] "https://www.google.com/search?q=urb it")
    [ p=[p=1 q=36]
        q
      [ ~
          u
        [ p=[p=[p=%.y q=~ r=[%.y p=<|com google www|>]] q=[p=~ q=<|search|>] r=~[[p='q' q='urb']]]
          q=[p=[p=1 q=36] q=" it"]
        ]
      ]
    ]
    ~zod/main=> (auri:epur [1 1] "https://www.google.com/search?q=urb%20it")
    [ p=[p=1 q=41]
        q
      [ ~
          u
        [ p=[p=[p=%.y q=~ r=[%.y p=<|com google www|>]] q=[p=~ q=<|search|>] r=~[[p='q' q='urb it']]]
          q=[p=[p=1 q=41] q=""]
        ]
      ]
    ]
    ~zod/main=> (auri:epur [1 1] "https://www.google.com/search?q=urbit%20escaping%3F")
    [ p=[p=1 q=52]
        q
      [ ~
          u
        [ p=[p=[p=%.y q=~ r=[%.y p=<|com google www|>]] q=[p=~ q=<|search|>] r=~[[p='q' q='urbit escaping?']]]
          q=[p=[p=1 q=52] q=""]
        ]
      ]
    ]

### `++cock`

      ++  cock                                              ::  cookie
        (most ;~(plug sem ace) ;~(plug toke ;~(pfix tis tosk)))

HTTP cookies, results in associative list of cord to cord.

    ~zod/try=> (scan "sam=lop" cock:epur)
    [['sam' 'lop'] ~]
    ~zod/try=> (scan "sam=lop; res=\"salo don -keg!mo\"" cock:epur)
    [['sam' 'lop'] ~[['res' 'salo don -keg!mo']]]
    ~zod/try=> (scan "sam=lop; res=\"salo don -keg!mo\";  so" cock:epur)
    ! {1 34}
    ! exit

### `++dlab`

      ++  dlab                                              ::  2396 domainlabel
        %+  sear
          |=  a=@ta
          ?.(=('-' (rsh 3 (dec (met 3 a)) a)) [~ u=a] ~)
        %+  cook  cass
        ;~(plug aln (star alp))
      ::

Domain label: alphanumeric, with `-` allowed in middle.

    ~zod/try=> (scan "google" dlab:epur)
    ~.google
    ~zod/try=> (scan "lera2" dlab:epur)
    ~.lera2
    ~zod/try=> (scan "gor-tem" dlab:epur)
    ~.gor-tem
    ~zod/try=> (scan "gortem-" dlab:epur)
    ! {1 8}
    ! exit

### `++fque`

      ++  fque  (cook crip (plus pquo))                     ::  normal query field

One or more query string characters

    ~zod/try=> (scan "%20" fque:epur)
    ' '
    ~zod/try=> (scan "sam" fque:epur)
    'sam'
    ~zod/try=> (scan "les+tor" fque:epur)
    'les tor'
    ~zod/try=> (scan "sore-%22mek%22" fque:epur)
    'sore-"mek"'
    ~zod/try=> (scan "" fque:epur)
    ! {1 1}
    ! exit

### `++fquu`

      ++  fquu  (cook crip (star pquo))                     ::  optional field

Zero or more query string characters

    ~zod/try=> (scan "%20" fquu:epur)
    ' '
    ~zod/try=> (scan "sam" fquu:epur)
    'sam'
    ~zod/try=> (scan "les+tor" fquu:epur)
    'les tor'
    ~zod/try=> (scan "sore-%22mek%22" fquu:epur)
    'sore-"mek"'
    ~zod/try=> (scan "" fquu:epur)
    ''

### `++pcar`

      ++  pcar  ;~(pose pure pesc psub col pat)             ::  2396 path char

Single URL path character: literal, `%` escape, subpath delimiter, `:`
or `@`

    ~zod/try=> (scan "a" pcar:epur)
    ~~a
    ~zod/try=> (scan "ab" pcar:epur)
    ! {1 2}
    ! exit
    ~zod/try=> (scan "-" pcar:epur)
    ~~-
    ~zod/try=> (scan "." pcar:epur)
    ~~~.
    ~zod/try=> (scan "%20" pcar:epur)
    ~~.
    ~zod/try=> (scan "!" pcar:epur)
    ~~~21.

### `++pcok`

      ++  pcok  ;~(less bas sem com doq prn)                ::  cookie char

Cookie character

    ~zod/try=> (scan "a" pcok:epur)
    ~~a
    ~zod/try=> (scan "ab" pcok:epur)
    ! {1 2}
    ! exit
    ~zod/try=> (scan "!" pcok:epur)
    ~~~21.
    ~zod/try=> (scan ";" pcok:epur)
    ! {1 2}
    ! exit

### `++pesc`

      ++  pesc  ;~(pfix cen mes)                            ::  2396 escaped

URL `%` escape, by two hex characters.

    ~zod/try=> `@t`(scan "%22" pesc:epur)
    '"'
    ~zod/try=> `@t`(scan "%20" pesc:epur)
    ' '

### `++pold`

      ++  pold  (cold ' ' (just '+'))                       ::  old space code

Old URL `' '` escape

    ~zod/try=> `@t`(scan "+" pold:epur)
    ' '
    ~zod/try=> `@t`(scan " " pold:epur)
    ! {1 1}
    ! exit

### `++pque`

      ++  pque  ;~(pose pcar fas wut)                       ::  3986 query char

Irregular query string character.

    ~zod/try=> `@t`(scan "a" pque:epur)
    'a'
    ~zod/try=> `@t`(scan "?" pque:epur)
    '?'
    ~zod/try=> `@t`(scan "%20" pque:epur)
    ' '
    ~zod/try=> `@t`(scan "+" pque:epur)
    '+'

### `++pquo`

      ++  pquo  ;~(pose pure pesc pold)                     ::  normal query char

Character in query string key/value

    ~zod/try=> (scan "a" pquo:epur)
    'a'
    ~zod/try=> (scan "ab" pquo:epur)
    ! {1 2}
    ! exit
    ~zod/try=> (scan "%22" pquo:epur)
    '"'
    ~zod/try=> (scan "%20" pquo:epur)
    ' '
    ~zod/try=> (scan "+" pquo:epur)
    ' '

### `++pure`

      ++  pure  ;~(pose aln hep dot cab sig)                ::  2396 unreserved

URL-safe character

    ~zod/try=> (scan "a" pure:epur)
    ~~a
    ~zod/try=> (scan "%20" pure:epur)
    ! {1 1}
    ! exit
    ~zod/try=> (scan "." pure:epur)
    ~~~.
    ~zod/try=> (scan "-" pure:epur)
    ~~-

### `++psub`

      ++  psub  ;~  pose                                    ::  3986 sub-delims
                  zap  buc  pam  soq  pel  per
                  tar  lus  com  sem  tis
                ==

URL path subdelimeter

    ~zod/try=> `@t`(scan "+" psub:epur)
    '+'
    ~zod/try=> `@t`(scan "(" psub:epur)
    '('
    ~zod/try=> `@t`(scan "$" psub:epur)
    '$'
    ~zod/try=> `@t`(scan "a" psub:epur)
    ! {1 1}
    ! exit

### `++ptok`

      ++  ptok  ;~  pose                                    ::  2616 token
                  aln  zap  hax  buc  cen  pam  soq  tar  lus
                  hep  dot  ket  cab  tec  bar  sig
                ==

Character valid in HTTP token

    ~zod/try=> `tape`(murn =+(a=' ' |-(`tape`?:(=(0x7f a) ~ [a $(a +(a))]))) (curr rush ptok):epur)
    "!#$%&'*+-.0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ^_`abcdefghijklmnopqrstuvwxyz|~"
    ~zod/try=> `tape`(skim =+(a=' ' |-(`tape`?:(=(0x7f a) ~ [a $(a +(a))]))) |=(a=char ?=(~ (rush a ptok:epur))))
    " "(),/:;<=>?@[\]{}"

### `++scem`

      ++  scem                                              ::  2396 scheme
        %+  cook  cass
        ;~(plug alf (star ;~(pose aln lus hep dot)))
      ::

URI scheme: alphabetic character, followed by any number of
alphanumeric, `+` `-` or `.`

    ~zod/try=> `@t`(scan "http" scem:epur)
    'http'
    ~zod/try=> `@t`(scan "https" scem:epur)
    'https'
    ~zod/try=> `@t`(scan "chrome-extension" scem:epur)
    'chrome-extension'

### `++smeg`

      ++  smeg  (cook crip (plus pcar))                     ::  2396 segment

URL path segment

    ~zod/try=> (scan "foo" smeg:epur)
    'foo'
    ~zod/try=> (scan "bar%20baz-bam" smeg:epur)
    'bar baz-bam'

### `++tock`

      ++  tock  (cook crip (plus pcok))                     ::  6265 cookie-value

HTTP cookie value

    ~zod/try=> (rush 'sam' tock:epur)
    [~ 'sam']
    ~zod/try=> (rush 'las!tore' tock:epur)
    [~ 'las!tore']
    ~zod/try=> (rush '"sop""les"tor' tock:epur)
    ~
    ~zod/try=> (rush '"zemug"' tock:epur)
    ~

### `++tosk`

      ++  tosk  ;~(pose tock (ifix [doq doq] tock))         ::  6265 cookie-value

Possibly quoted HTTP cookie value

    ~zod/try=> (rush 'sam' tosk:epur)
    [~ 'sam']
    ~zod/try=> (rush 'las!tore' tosk:epur)
    [~ 'las!tore']
    ~zod/try=> (rush '"sop""les"tor' tosk:epur)
    ~
    ~zod/try=> (rush '"zemug"' tosk:epur)
    [~ 'zemug']

### `++toke`

      ++  toke  (cook crip (plus ptok))                     ::  2616 token

HTTP cookie name

    ~zod/try=> (rush 'sam' toke:epur)
    [~ 'sam']
    ~zod/try=> (rush 'las!tore' toke:epur)
    [~ 'las!tore']
    ~zod/try=> (rush 'sop""les"tor' toke:epur)
    ~
    ~zod/try=> (rush '"zemug"' toke:epur)
    ~

### `++thor`

      ++  thor                                              ::  2396 host/port
        %+  cook  |*(a=[* *] [+.a -.a])
        ;~  plug
          thos
          ;~(pose (stag ~ ;~(pfix col dim:ag)) (easy ~))
        ==

Parse ++host and unit `@ui` port.

    ~zod/try=> (scan "localhost" thor:epur)
    [~ [%.y i='localhost' t=~]]
    ~zod/try=> (scan "localhost:8080" thor:epur)
    [[~ q=8.080] [%.y i='localhost' t=~]]
    ~zod/try=> (scan "192.168.0.1:8080" thor:epur)
    [[~ q=8.080] [%.n q=3.232.235.521]]
    ~zod/try=> (scan "www.google.com" thor:epur)
    [~ [%.y i='com' t=~['google' 'www']]]

### `++thos`

      ++  thos                                              ::  2396 host, no local
        ;~  plug
          ;~  pose
            %+  stag  %&
            %+  sear                                        ::  LL parser weak here
              |=  a=(list ,@t)
              =+  b=(flop a)
              ?>  ?=(^ b)
              =+  c=(end 3 1 i.b)
              ?.(&((gte c 'a') (lte c 'z')) ~ [~ u=b])
            (most dot dlab)
          ::
            %+  stag  %|
            =+  tod=(ape:ag ted:ab)
            %+  bass  256
            ;~(plug tod (stun [3 3] ;~(pfix dot tod)))
          ==
        ==

URI host: dot-separated segments, or IP address.

    ~zod/try=> (scan "localhost" thos:epur)
    [%.y i='localhost' t=~]
    ~zod/try=> (scan "192.168.0.1" thos:epur)
    [%.n q=3.232.235.521]
    ~zod/try=> (scan "192.168.0.1:80" thos:epur)
    ! {1 12}
    ! exit
    ~zod/try=> (scan "www.google.com" thos:epur)
    [%.y i='com' t=~['google' 'www']]

### `++yque`

      ++  yque                                              ::  query ending
        ;~  pose
          ;~(pfix wut yquy)
          (easy ~)
        ==

Parses query string, or lack thereof. Result type ++quay

    ~zod/try=> (scan "?sar=tok" yque:epur)
    [['sar' 'tok'] ~]
    ~zod/try=> (scan "?les=urbit%20sep&met=kam" yque:epur)
    [['les' 'urbit sep'] ~[['met' 'kam']]]
    ~zod/try=> (scan "" yque:epur)
    ~

### `++yquy`

      ++  yquy                                              ::  query
        ;~  pose                                            ::  proper query
          %+  more
            ;~(pose pam sem)
          ;~(plug fque ;~(pose ;~(pfix tis fquu) (easy '')))
        ::
          %+  cook                                          ::  funky query
            |=(a=tape [[%$ (crip a)] ~])
          (star pque)
        ==

Parse query string after `?`

    ~zod/try=> (scan "sar=tok" yquy:epur)
    [['sar' 'tok'] ~]
    ~zod/try=> (scan "les=urbit%20sep&met=kam" yquy:epur)
    [['les' 'urbit sep'] ~[['met' 'kam']]]
    ~zod/try=> (scan "" yquy:epur)
    ~

### `++zest`

      ++  zest                                              ::  2616 request-uri
        ;~  pose
          (stag %& (cook |=(a=purl a) auri))
          (stag %| ;~(plug apat yque))
        ==
      --

Parse ++quri absolute or relative request path

    ~zod/try=> (scan "http://www.google.com:80/search?q=foo" zest:epur)
    [%.y p=[p=%.n q=[~ 80] r=[%.y p=<|com google www|>]] q=[p=~ q=<|search|>] r=~[[p='q' q='foo']]]
    ~zod/try=> (scan "/rel/bat" zest:epur)
    [%.n [p=~ q=<|rel bat|>] ~]
