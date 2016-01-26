section 2eL, formatting (path)
==============================

### `++ab`

Primitive parser engine

    ++  ab
      |%

A core containing numeric parser primitives.

    ~zod/try=> ab
    <36.ecc 414.gly 100.xkc 1.ypj %164>

------------------------------------------------------------------------

### `++bix`

Parse hex pair

      ++  bix  (bass 16 (stun [2 2] six))

Parsing rule. Parses a pair of base-16 digits. Used in escapes.

    ~zod/try=> (scan "07" bix:ab)
    q=7
    ~zod/try=> (scan "51" bix:ab)
    q=81
    ~zod/try=> (scan "a3" bix:ab)
    q=163

------------------------------------------------------------------------

### `++hif`

Parse phonetic pair

      ++  hif  (boss 256 ;~(plug tip tiq (easy ~)))

Parsing rule. Parses an atom of odor [`@pE`](), a phrase of two bytes
encoded phonetically.

    ~zod/try=> (scan "doznec" hif:ab)
    q=256
    ~zod/try=> (scan "pittyp" hif:ab)
    q=48.626

------------------------------------------------------------------------

### `++huf`

Parse two phonetic pairs

      ++  huf  %+  cook
                   |=([a=@ b=@] (wred:un ~(zug mu ~(zag mu [a b]))))
                 ;~(plug hif ;~(pfix hep hif))

Parsing rule. Parses and unscrambles an atom of odor [@pF](), a phrase
of two two-byte pairs that are encoded (and scrambled) phonetically.

    ~zod/try=> (scan "pittyp-pittyp" huf:ab)
    328.203.557
    ~zod/try=> (scan "tasfyn-partyv" huf:ab)
    65.792
    ~zod/try=> `@ux`(scan "tasfyn-partyv" huf:ab)
    0x1.0100

------------------------------------------------------------------------

### `++hyf`

Parse 8 phonetic bytes

      ++  hyf  (bass 0x1.0000.0000 ;~(plug huf ;~(pfix hep huf) (easy ~)))

Parsing rule. Parses an atom of odor [@pG](), a phrase of eight of
phonetic bytes.

    ~zod/try=> (scan "sondel-forsut-tillyn-nillyt" hyf:ab)
    q=365.637.097.828.335.095
    ~zod/try=> `@u`~sondel-forsut-tillyn-nillyt
    365.637.097.828.335.095

------------------------------------------------------------------------

### `++pev`

Parse \<= 5 base-32

      ++  pev  (bass 32 ;~(plug sev (stun [0 4] siv)))

Parsing rule. Parses up to five base-32 digits without a leading zero.

    ~zod/try=> (scan "a" pev:ab)
    q=10
    ~zod/try=> (scan "290j" pev:ab)
    q=74.771
    ~zod/try=> (scan "123456" pev:ab)
    ! {1 6}
    ! exit
    ~zod/try=> (scan "090j" pev:ab)
    ~ <syntax error at [1 11]>

------------------------------------------------------------------------

### `++pew`

Parse \<= 5 base-64

      ++  pew  (bass 64 ;~(plug sew (stun [0 4] siw)))

Parsing rule. Parses up to five base-64 digits without a leading zero.

    ~zod/try=> (scan "Q" pew:ab)
    q=52
    ~zod/try=> (scan "aQ~9" pew:ab)
    q=2.838.473
    ~zod/try=> `@`0waQ~9
    2.838.473
    ~zod/try=> (scan "123456" pew:ab)
    ! {1 6}
    ! exit
    ~zod/try=> (scan "012345" pew:ab)
    ! {1 1}
    ! exit

------------------------------------------------------------------------

### `++piv`

Parse 5 base-32

      ++  piv  (bass 32 (stun [5 5] siv))

Parsing rule. Parses exactly five base-32 digits.

    ~zod/try=> (scan "10b3l" piv:ab)
    q=1.059.957
    ~zod/try=> (scan "1" piv:ab)
    ! {1 2}
    ! exit

------------------------------------------------------------------------

### `++piw`

Parse 5 base-64

      ++  piw  (bass 64 (stun [5 5] siw))

Parsing rule. Parses exactly five base-64 digits.

    ~zod/try=> (scan "2C-pZ" piw:ab)
    q=43.771.517
    ~zod/try=> (scan "2" piv:ab)
    ! {1 2}
    ! exit

------------------------------------------------------------------------

### `++qeb`

Parse \<= 4 binary

      ++  qeb  (bass 2 ;~(plug seb (stun [0 3] sib)))

Parsing rule. Parses a binary number of up to 4 digits in length without
a leading zero.

    ~zod/try=> (scan "1" qeb:ab)
    q=1
    ~zod/try=> (scan "101" qeb:ab)
    q=5
    ~zod/try=> (scan "1111" qeb:ab)
    q=15
    ~zod/try=> (scan "11111" qeb:ab)
    ! {1 5}
    ! exit
    ~zod/try=> (scan "01" qeb:ab)
    ! {1 1}
    ! exit

------------------------------------------------------------------------

### `++qex`

Parse \<= 4 hex

      ++  qex  (bass 16 ;~(plug sex (stun [0 3] hit)))

Parsing rule. Parses a hexadecimal number of up to 4 digits in length
without a leading zero.

    ~zod/try=> (scan "ca" qex:ab)
    q=202
    ~zod/try=> (scan "18ac" qex:ab)
    q=6.316
    ~zod/try=> (scan "18acc" qex:ab)
    ! {1 5}
    ! exit
    ~zod/try=> (scan "08ac" qex:ab)
    ! {1 1}
    ! exit

------------------------------------------------------------------------

### `++qib`

Parse 4 binary

      ++  qib  (bass 2 (stun [4 4] sib))

Parsing rule. Parses exactly four binary digits.

    ~zod/try=> (scan "0001" qib:ab)
    q=1
    ~zod/try=> (scan "0100" qib:ab)
    q=4
    ~zod/try=> (scan "110" qib:ab)
    ! {1 4}
    ! exit

------------------------------------------------------------------------

### `++qix`

Parse 4 hex

      ++  qix  (bass 16 (stun [4 4] six))

Parsing rule. Parses exactly four hexadecimal digits.

    ~zod/try=> (scan "0100" qix:ab)
    q=256
    ~zod/try=> (scan "10ff" qix:ab)
    q=4.351
    ~zod/try=> (scan "0" qix:ab)
    ! {1 2}
    ! exit

------------------------------------------------------------------------

### `++seb`

Parse 1

      ++  seb  (cold 1 (just '1'))

Parsing rule. Parses the number 1.

    ~zod/try=> (scan "1" seb:ab)
    1
    ~zod/try=> (scan "0" seb:ab)
    ! ~zod/try/~2014.10.23..22.34.21..bfdd/:<[1 1].[1 18]>
    ! {1 1}
    ~zod/try=> (scan "2" seb:ab)
    ! ~zod/try/~2014.10.23..22.34.29..d399/:<[1 1].[1 18]>
    ! {1 1}

------------------------------------------------------------------------

### `++sed`

Parse decimal

      ++  sed  (cook |=(a=@ (sub a '0')) (shim '1' '9'))

Parsing rule. Parses a nonzero decimal digit.

    ~zod/try=> (scan "5" sed:ab)
    5
    ~zod/try=> (scan "0" sed:ab)
    ! {1 1}
    ! exit

------------------------------------------------------------------------

### `++sev`

Parse base-32

      ++  sev  ;~(pose sed sov)

Parsing rule. Parses a nonzero base-32 digit

    ~zod/try=> (scan "c" sev:ab)
    12
    ~zod/socialnet=> (scan "0" sev:ab)
    ! {1 1}
    ! exit

------------------------------------------------------------------------

### `++sew`

Parse base-64

      ++  sew  ;~(pose sed sow)

Parsing rule. Parses a nonzero base-64 digit

    ~zod/try=> (scan "M" sew:ab)
    48
    ~zod/try=> (scan "0" sew:ab)
    ! {1 1}
    ! exit

------------------------------------------------------------------------

### `++sex`

Parse hex

      ++  sex  ;~(pose sed sox)

Parsing rule. Parses a nonzero hexadecimal digit.

    ~zod/try=> (scan "e" sex:ab)
    14
    ~zod/try=> (scan "0" sex:ab)
    ! {1 1}
    ! exit

------------------------------------------------------------------------

### `++sib`

Parse binary

      ++  sib  (cook |=(a=@ (sub a '0')) (shim '0' '1'))

Parsing rule. Parses a binary digit.

    ~zod/try=> (scan "1" sib:ab)
    1
    ~zod/socialnet=> (scan "0" sib:ab)
    0

------------------------------------------------------------------------

### `++sid`

Parse decimal

      ++  sid  (cook |=(a=@ (sub a '0')) (shim '0' '9'))

Parsing rule. Parses a decimal digit.

    ~zod/try=> (scan "5" sid:ab)
    5

------------------------------------------------------------------------

### `++siv`

Parse base-32

      ++  siv  ;~(pose sid sov)

Parsing rule. Parses a base-32 digit.

    ~zod/try=> (scan "c" siv:ab)
    12

------------------------------------------------------------------------

### `++siw`

Parse base-64

      ++  siw  ;~(pose sid sow)

Parsing rule. Parses a base-64 digit.

    ~zod/try=> (scan "M" siw:ab)
    48

------------------------------------------------------------------------

### `++six`

Parse hex

      ++  six  ;~(pose sid sox)

Parsing rule. Parses a hexadecimal digit.

    ~zod/try=> (scan "e" six:ab)
    14

------------------------------------------------------------------------

### `++sov`

Parse base-32

      ++  sov  (cook |=(a=@ (sub a 87)) (shim 'a' 'v'))

Parsing rule. Parses a base-32 letter.

    ~zod/try=> (scan "c" sov:ab)
    12

------------------------------------------------------------------------

### `++sow`

Parse base-64

      ++  sow  ;~  pose
                 (cook |=(a=@ (sub a 87)) (shim 'a' 'z'))
                 (cook |=(a=@ (sub a 29)) (shim 'A' 'Z'))
                 (cold 62 (just '-'))
                 (cold 63 (just '~'))
               ==

Parsing rule. Parses a base-64 letter/symbol.

    ~zod/try=> (scan "M" sow:ab)
    48

------------------------------------------------------------------------

### `++sox`

Parse hex letter

      ++  sox  (cook |=(a=@ (sub a 87)) (shim 'a' 'f'))

Parsing rule. Parses a hexadecimal letter.

    ~zod/try=> (scan "e" sox:ab)
    14

------------------------------------------------------------------------

### `++ted`

Parse \<= 3 decimal

      ++  ted  (bass 10 ;~(plug sed (stun [0 2] sid)))

Parsing rule. Parses a decimal number of up to 3 digits without a
leading zero.

    ~zod/try=> (scan "21" ted:ab)
    q=21
    ~zod/try=> (scan "214" ted:ab)
    q=214
    ~zod/try=> (scan "2140" ted:ab)
    {1 4}
    ~zod/try=> (scan "0" ted:ab)
    ! {1 1}
    ! exit

------------------------------------------------------------------------

### `++tip`

Leading phonetic byte

      ++  tip  (sear |=(a=@ (ins:po a)) til)

Parsing rule. Parses the leading phonetic byte, which represents a
syllable.

    ~zod/try=> (scan "doz" tip:ab)
    0
    ~zod/try=> (scan "pit" tip:ab)
    242

------------------------------------------------------------------------

### `++tiq`

Trailing phonetic syllable

      ++  tiq  (sear |=(a=@ (ind:po a)) til)

Parsing rule. Parses the trailing phonetic byte, which represents a
syllable.

    ~zod/try=> (scan "zod" tiq:ab)
    0
    ~zod/try=> (scan "nec" tiq:ab)
    1

------------------------------------------------------------------------

### `++tid`

Parse 3 decimal digits

      ++  tid  (bass 10 (stun [3 3] sid))

Parsing rule. Parses exactly three decimal digits.

    ~zod/try=> (scan "013" tid:ab)
    q=13
    ~zod/try=> (scan "01" tid:ab)
    ! {1 3}
    ! exit

------------------------------------------------------------------------

### `++til`

Parse 3 lowercase

      ++  til  (boss 256 (stun [3 3] low))

Parsing rule. Parses exactly three lowercase letters.

    ~zod/try=> (scan "mer" til:ab)
    q=7.497.069
    ~zod/try=> `@t`(scan "mer" til:ab)
    'mer'
    ~zod/try=> (scan "me" til:ab)
    ! {1 3}
    ! exit

------------------------------------------------------------------------

### `++urs`

Parse span characters

      ++  urs  %+  cook
                 |=(a=tape (rap 3 ^-((list ,@) a)))
               (star ;~(pose nud low hep dot sig cab))

Parsing rule. Parses characters from an atom of the span odor [`@ta`]().

    ~zod/try=> `@ta`(scan "asa-lom_tak" urs:ab)
    ~.asa-lom_tak 
    ~zod/try=> `@t`(scan "asa-lom_tak" urs:ab)
    'asa-lom_tak'

------------------------------------------------------------------------

### `++urt`

Parse non-`_` span

      ++  urt  %+  cook
                 |=(a=tape (rap 3 ^-((list ,@) a)))
               (star ;~(pose nud low hep dot sig))

Parsing rule. Parses all characters of the span odor [`@ta`]() except
for cab, `_`.

    ~zod/try=> `@t`(scan "asa-lom.t0k" urt:ab)
    'asa-lom.t0k'

------------------------------------------------------------------------

### `++voy`

Parse bas, soq, or bix

      ++  voy  ;~(pfix bas ;~(pose bas soq bix))

Parsing rule. Parses an escaped backslash, single quote, or hex pair
byte.

    ~zod/try=> (scan "\\0a" voy:ab)
    q=10
    ~zod/try=> (scan "\\'" voy:ab)
    q=39

------------------------------------------------------------------------

### `++ag`

Top-level atom parser engine

    ++  ag
      |%

A core containing top-level atom parsers.

    ~zod/try=> ag
    <14.vpu 414.mof 100.xkc 1.ypj %164>

------------------------------------------------------------------------

### `++ape`

Parse 0 or rule

      ++  ape  |*(fel=_rule ;~(pose (cold 0 (just '0')) fel))

Parser modifier. Parses 0 or the sample rule `fel`.

`fel` is a [`rule`]().

    ~zod/try=> (scan "202" (star (ape:ag (cold 2 (just '2')))))
    ~[2 0 2]

------------------------------------------------------------------------

### `++bay`

Parses binary number

      ++  bay  (ape (bass 16 ;~(plug qeb:ab (star ;~(pfix dog qib:ab)))))

Parsing rule. Parses a binary number without a leading zero.

    ~zod/try=> (scan "10.0110" bay:ag)
    q=38

------------------------------------------------------------------------

### `++bip`

Parse IPv6

      ++  bip  =+  tod=(ape qex:ab)
               (bass 0x1.0000 ;~(plug tod (stun [7 7] ;~(pfix dog tod))))

Parsing rule. Parses a [`@is`](), an IPv6 address.

    ~zod/try=> (scan "0.0.ea.3e6c.0.0.0.0" bip:ag)
    q=283.183.420.760.121.105.516.068.864
    ~zod/try=> `@is`(scan "0.0.ea.3e6c.0.0.0.0" bip:ag)
    .0.0.ea.3e6c.0.0.0.0

------------------------------------------------------------------------

### `++dem`

Parse decimal with dots

      ++  dem  (ape (bass 1.000 ;~(plug ted:ab (star ;~(pfix dog tid:ab)))))

Parsing rule. Parses a decimal number that includes dot separators.

    ~zod/try=> (scan "52" dem:ag)
    q=52
    ~zod/try=> (scan "13.507" dem:ag)
    q=13.507

------------------------------------------------------------------------

### `++dim`

Parse decimal number

      ++  dim  (ape (bass 10 ;~(plug sed:ab (star sid:ab))))

Parsing rule. Parses a decimal number without a leading zero.

    ~zod/try=> (scan "52" dim:ag)
    q=52
    ~zod/try=> (scan "013507" dim:ag)
    ! {1 2}
    ! exit

------------------------------------------------------------------------

### `++dum`

Parse decimal with leading `0`

      ++  dum  (bass 10 (plus sid:ab))

Parsing rule. Parses a decmial number with leading zeroes.

    ~zod/try=> (scan "52" dum:ag)
    q=52
    ~zod/try=> (scan "0000052" dum:ag)
    q=52
    ~zod/try=> (scan "13507" dim:ag)
    q=13.507

------------------------------------------------------------------------

### `++fed`

Parse phonetic base

      ++  fed  ;~  pose
                 (bass 0x1.0000.0000.0000.0000 (most doh hyf:ab))
                 huf:ab
                 hif:ab
                 tiq:ab
               ==

Parsing rule. Parses an atom of odor [`@p`](), the phonetic base.

    ~zod/try=> (scan "zod" fed:ag)
    0
    ~zod/try=> (scan "nec" fed:ag)
    1
    ~zod/try=> (scan "sondel" fed:ag)
    9.636
    ~zod/try=> ~tillyn-nillyt
    ~tillyn-nillyt
    ~zod/try=> (scan "tillyn-nillyt" fed:ag)
    3.569.565.175
    ~zod/try=> (scan "tillyn-nillyt-tasfyn-partyv" fed:ag)
    15.331.165.687.565.582.592
    ~zod/try=> (scan "tillyn-nillyt-tasfyn-partyv--novweb-talrud-talmud-sonfyr" fed:ag)
    282.810.089.790.159.633.869.501.053.313.363.681.181

------------------------------------------------------------------------

### `++hex`

Parse hex

      ++  hex  (ape (bass 0x1.0000 ;~(plug qex:ab (star ;~(pfix dog qix:ab)))))

Parsing rule. Parses a hexadecimal number

    ~zod/try=> (scan "4" hex:ag)
    q=4
    ~zod/try=> (scan "1a" hex:ag)
    q=26
    ~zod/try=> (scan "3.ac8d" hex:ag)
    q=240.781
    ~zod/try=> `@ux`(scan "3.ac8d" hex:ag)
    0x3.ac8d

------------------------------------------------------------------------

### `++lip`

Parse IPv4 address

      ++  lip  =+  tod=(ape ted:ab)
               (bass 256 ;~(plug tod (stun [3 3] ;~(pfix dog tod))))

Parsing rule. Parses an IPv4 address.

    ~zod/try=> (scan "127.0.0.1" lip:ag)
    q=2.130.706.433
    ~zod/try=> `@if`(scan "127.0.0.1" lip:ag)
    .127.0.0.1
    ~zod/try=> `@if`(scan "8.8.8.8" lip:ag)
    .8.8.8.8

------------------------------------------------------------------------

### `++viz`

Parse Base-32 with dots

      ++  viz  (ape (bass 0x200.0000 ;~(plug pev:ab (star ;~(pfix dog piv:ab)))))

Parsing rule. Parses a Base-32 number with dot separators.

    ~zod/try=> (scan "e2.ol4pm" viz:ag)
    q=15.125.353.270

------------------------------------------------------------------------

### `++vum`

Parse base-32 string

      ++  vum  (bass 32 (plus siv:ab))

Parsing rule. Parses a raw base-32 string.

    ~zod/try=> (scan "e2ol4pm" vum:ag)
    q=15.125.353.270

------------------------------------------------------------------------

### `++wiz`

Parse base-64

      ++  wiz  (ape (bass 0x4000.0000 ;~(plug pew:ab (star ;~(pfix dog piw:ab)))))
      --
    ::

Parsing rule. Parses a base-64 number.

    ~zod/try=> (scan "e2O.l4Xpm" wiz:ag)
    q=61.764.130.813.526

------------------------------------------------------------------------

### `++co`

Literal rendering engine

    ++  co
      =<  |_  lot=coin

A [door]() that contains arms that operate on the sample coin `lot`.

`lot` is a [`++coin`]().

    ~zod/try=> ~(. co many/~[`ta/'mo' `ud/5])
    < 3.dhd
      [ [ %many
          [%~ %ta @t]
          [%~ %ud @ud]
          %~
        ]
        <10.utz 3.zid [rex="" <414.hmb 100.xkc 1.ypj %164>]>
      ]
    >

------------------------------------------------------------------------

### `++rear`

Prepend & render as tape

          ++  rear  |=(rom=tape =>(.(rex rom) rend))

Renders a coin `lot` as a [tape]() prepended to the sample tape `rom`.

`rom` is a [`pe`]()

`lot` is a [`++coin`]().

    ~zod/try=> (~(rear co %$ %ux 200) "--ha")
    "0xc8--ha"

------------------------------------------------------------------------

### `++rent`

Render as span

          ++  rent  `@ta`(rap 3 rend)

Renders a coin `lot` as a span.

`lot` is a [`++coin`]().

    ~zod/try=> ~(rent co %$ %ux 200)
    ~.0xc8
    ~zod/try=> `@t`~(rent co %$ %ux 200)
    '0xc8'

------------------------------------------------------------------------

### `++rend`

Render as tape

          ++  rend
            ^-  tape
            ?:  ?=(%blob -.lot)
              ['~' '0' ((v-co 1) (jam p.lot))]
            ?:  ?=(%many -.lot)
              :-  '.'
              |-  ^-  tape
              ?~   p.lot
                ['_' '_' rex]
              ['_' (weld (trip (wack rent(lot i.p.lot))) $(p.lot t.p.lot))]
            =+  [yed=(end 3 1 p.p.lot) hay=(cut 3 [1 1] p.p.lot)]
            |-  ^-  tape
            ?+    yed  (z-co q.p.lot)
                %c   ['~' '-' (weld (rip 3 (wood (tuft q.p.lot))) rex)]
                %d
              ?+    hay  (z-co q.p.lot)
                  %a
                =+  yod=(yore q.p.lot)
                =>  ^+(. .(rex ?~(f.t.yod rex ['.' (s-co f.t.yod)])))
                =>  ^+  .
                    %=    .
                        rex
                      ?:  &(=(~ f.t.yod) =(0 h.t.yod) =(0 m.t.yod) =(0 s.t.yod))
                        rex
                      =>  .(rex ['.' (y-co s.t.yod)])
                      =>  .(rex ['.' (y-co m.t.yod)])
                      ['.' '.' (y-co h.t.yod)]
                    ==
                =>  .(rex ['.' (a-co d.t.yod)])
                =>  .(rex ['.' (a-co m.yod)])
                =>  .(rex ?:(a.yod rex ['-' rex]))
                ['~' (a-co y.yod)]
              ::
                  %r
                =+  yug=(yell q.p.lot)
                =>  ^+(. .(rex ?~(f.yug rex ['.' (s-co f.yug)])))
                :-  '~'
                ?:  &(=(0 d.yug) =(0 m.yug) =(0 h.yug) =(0 s.yug))
                  ['.' 's' '0' rex]
                =>  ^+(. ?:(=(0 s.yug) . .(rex ['.' 's' (a-co s.yug)])))
                =>  ^+(. ?:(=(0 m.yug) . .(rex ['.' 'm' (a-co m.yug)])))
                =>  ^+(. ?:(=(0 h.yug) . .(rex ['.' 'h' (a-co h.yug)])))
                =>  ^+(. ?:(=(0 d.yug) . .(rex ['.' 'd' (a-co d.yug)])))
                +.rex
              ==
            ::
                %f
              ?:  =(& q.p.lot)
                ['.' 'y' rex]
              ?:(=(| q.p.lot) ['.' 'n' rex] (z-co q.p.lot))
            ::
                %n   ['~' rex]
                %i
              ?+  hay  (z-co q.p.lot)
                %f  ((ro-co [3 10 4] |=(a=@ ~(d ne a))) q.p.lot)
                %s  ((ro-co [4 16 8] |=(a=@ ~(x ne a))) q.p.lot)
              ==
            ::
                %p
              =+  dyx=(met 3 q.p.lot)
              :-  '~'
              ?:  (lte dyx 1)
                (weld (trip (tod:po q.p.lot)) rex)
              ?:  =(2 dyx)
                ;:  weld
                  (trip (tos:po (end 3 1 q.p.lot)))
                  (trip (tod:po (rsh 3 1 q.p.lot)))
                  rex
                ==
              =+  [dyz=(met 5 q.p.lot) fin=|]
              |-  ^-  tape
              ?:  =(0 dyz)
                rex
              %=    $
                  fin      &
                  dyz      (dec dyz)
                  q.p.lot  (rsh 5 1 q.p.lot)
                  rex
                =+  syb=(wren:un (end 5 1 q.p.lot))
                =+  cog=~(zig mu [(rsh 4 1 syb) (end 4 1 syb)])
                ;:  weld
                  (trip (tos:po (end 3 1 p.cog)))
                  (trip (tod:po (rsh 3 1 p.cog)))
                  `tape`['-' ~]
                  (trip (tos:po (end 3 1 q.cog)))
                  (trip (tod:po (rsh 3 1 q.cog)))
                  `tape`?:(fin ['-' ?:(=(1 (end 0 1 dyz)) ~ ['-' ~])] ~)
                  rex
                ==
              ==
            ::
                %r
              ?+  hay  (z-co q.p.lot)
                %d  
              =+  r=(rlyd q.p.lot)
              ?~  e.r
                ['.' '~' (r-co r)]
              ['.' '~' u.e.r]
                %h  ['.' '~' '~' (r-co (rlyh q.p.lot))]
                %q  ['.' '~' '~' '~' (r-co (rlyq q.p.lot))]
                %s  ['.' (r-co (rlys q.p.lot))]
              ==
            ::
                %u
              =-  (weld p.gam ?:(=(0 q.p.lot) `tape`['0' ~] q.gam))
              ^=  gam  ^-  [p=tape q=tape]
              ?+  hay  [~ ((ox-co [10 3] |=(a=@ ~(d ne a))) q.p.lot)]
                %b  [['0' 'b' ~] ((ox-co [2 4] |=(a=@ ~(d ne a))) q.p.lot)]
                %i  [['0' 'i' ~] ((d-co 1) q.p.lot)]
                %x  [['0' 'x' ~] ((ox-co [16 4] |=(a=@ ~(x ne a))) q.p.lot)]
                %v  [['0' 'v' ~] ((ox-co [32 5] |=(a=@ ~(x ne a))) q.p.lot)]
                %w  [['0' 'w' ~] ((ox-co [64 5] |=(a=@ ~(w ne a))) q.p.lot)]
              ==
            ::
                %s
              %+  weld
                ?:((syn:si q.p.lot) "--" "-")
              $(yed 'u', q.p.lot (abs:si q.p.lot))
            ::
                %t
              ?:  =('a' hay)
                ?:  =('s' (cut 3 [2 1] p.p.lot))
                  
                  (weld (rip 3 q.p.lot) rex)
                ['~' '.' (weld (rip 3 q.p.lot) rex)]
              ['~' '~' (weld (rip 3 (wood q.p.lot)) rex)]
            ==
          --
      =+  rex=*tape
      =<  |%
          ++  a-co  |=(dat=@ ((d-co 1) dat))
          ++  d-co  |=(min=@ (em-co [10 min] |=([? b=@ c=tape] [~(d ne b) c])))
          ++  r-co
            |=  [syn=? nub=@ der=@ ign=(unit tape) ne=?]
            =>  .(rex ['.' (t-co ((d-co 1) der) ne)])
            =>  .(rex ((d-co 1) nub))
            ?:(syn rex ['-' rex])
          ++  t-co  |=  [a=tape n=?]  ^-  tape 
            ?:  n  a
            ?~  a  ~|(%empty-frac !!)  t.a
          ::
          ++  s-co
            |=  esc=(list ,@)  ^-  tape
            ~|  [%so-co esc]
            ?~  esc
              rex
            :-  '.'
            =>(.(rex $(esc t.esc)) ((x-co 4) i.esc))
            
        ::
          ++  v-co  |=(min=@ (em-co [32 min] |=([? b=@ c=tape] [~(v ne b) c])))
          ++  w-co  |=(min=@ (em-co [64 min] |=([? b=@ c=tape] [~(w ne b) c])))
          ++  x-co  |=(min=@ (em-co [16 min] |=([? b=@ c=tape] [~(x ne b) c])))
          ++  y-co  |=(dat=@ ((d-co 2) dat))
          ++  z-co  |=(dat=@ `tape`['0' 'x' ((x-co 1) dat)])
          --
      ~%  %co  +>  ~
      |%
      ++  em-co
        ~/  %emco
        |=  [[bas=@ min=@] [par=$+([? @ tape] tape)]]
        |=  hol=@
        ^-  tape
        ?:  &(=(0 hol) =(0 min))
          rex
        =+  [rad=(mod hol bas) dar=(div hol bas)]
        %=  $
          min  ?:(=(0 min) 0 (dec min))
          hol  dar
          rex  (par =(0 dar) rad rex)
        ==
      ::
      ++  ox-co
        ~/  %oxco
        |=  [[bas=@ gop=@] dug=$+(@ @)]
        %+  em-co
          [|-(?:(=(0 gop) 1 (mul bas $(gop (dec gop))))) 0]
        |=  [top=? seg=@ res=tape]
        %+  weld
          ?:(top ~ `tape`['.' ~])
        %.  seg
        %+  em-co(rex res)
          [bas ?:(top 0 gop)]
        |=([? b=@ c=tape] [(dug b) c])
      ::
      ++  ro-co
        ~/  %roco
        |=  [[buz=@ bas=@ dop=@] dug=$+(@ @)]
        |=  hol=@
        ^-  tape
        ?:  =(0 dop)
          rex
        =>  .(rex $(dop (dec dop)))
        :-  '.'
        %-  (em-co [bas 1] |=([? b=@ c=tape] [(dug b) c]))
        [(cut buz [(dec dop) 1] hol)]
      --
    ::

Renders a coin `lot` as a tape.

`lot` is a [`++coin`]().

    ~zod/try=> ~(rend co ~ %ux 200)
    "0xc8"
    ~zod/try=> ~(rend co %many ~[[%$ ux/200] [%$ p/40]])
    "._0xc8_~~tem__"
    ~zod/try=> ~(rend co ~ %p 32.819)
    "~pillyt"
    ~zod/try=> ~(rend co ~ %ux 18)
    "0x12"
    ~zod/try=> ~(rend co [~ p=[p=%if q=0x7f00.0001]])
    ".127.0.0.1"
    ~zod/try=> `@ux`.127.0.0.1
    2.130.706.433
    ~zod/try=> ~(rend co %many ~[[~ %ud 20] [~ %uw 133] [~ %tas 'sam']])
    "._20_0w25_sam__"
    ~zod/try=> ~(rend co %blob [1 1])
    "~0ph"
    ~zod/try=> ~0ph
    [1 1]
    ~zod/try=> `@uv`(jam [1 1])
    0vph

------------------------------------------------------------------------

### `++ne`

Digit rendering engine

    ++  ne
      |_  tig=@

A [door]() containing arms that render digits at bases 10, 16, 32, and
64.

`tig` is an [`atom`]().

    ~zod/try=> ~(. ne 20)
    <4.gut [@ud <414.hhh 100.xkc 1.ypj %164>]>

------------------------------------------------------------------------

### `++d`

Render decimal

      ++  d  (add tig '0')

Renders a decimal digit as an atom of an ACII byte value.

`tig` is an [`atom`]().

    ~zod/try=> `@t`~(d ne 7)
    '7'

------------------------------------------------------------------------

### `++x`

Render hex

      ++  x  ?:((gte tig 10) (add tig 87) d)

Renders a hexadecimal digit as an atom of an ASCII byte value.

`tig` is an [`atom`]().

    ~zod/try=> `@t`~(x ne 7)
    '7'
    ~zod/try=> `@t`~(x ne 14)
    'e'

------------------------------------------------------------------------

### `++v`

Render base-32

      ++  v  ?:((gte tig 10) (add tig 87) d)

Renders a base-32 digit as an atom of an ASCII byte value.

    ~zod/try=> `@t`~(v ne 7)
    '7'
    ~zod/try=> `@t`~(v ne 14)
    'e'
    ~zod/try=> `@t`~(v ne 25)
    'p'

------------------------------------------------------------------------

### `++w`

Render base-64

      ++  w  ?:(=(tig 63) '~' ?:(=(tig 62) '-' ?:((gte tig 36) (add tig 29) x)))
      --
    ::

Renders a base-64 digit as an atom of an ASCII byte value.

`tig` is an [`atom`]().

    ~zod/try=> `@t`~(w ne 7)
    '7'
    ~zod/try=> `@t`~(w ne 14)
    'e'
    ~zod/try=> `@t`~(w ne 25)
    'p'
    ~zod/try=> `@t`~(w ne 52)
    'Q'
    ~zod/try=> `@t`~(w ne 61)
    'Z'
    ~zod/try=> `@t`~(w ne 63)
    '~'
    ~zod/try=> `@t`~(w ne 62)
    '-'

------------------------------------------------------------------------

### `++mu`

Core used to scramble 16-bit atoms

    ++  mu
      |_  [top=@ bot=@]

A [door]() that contains arms that are used to scramble two atoms, `top`
and `bot`. Used especially in the phonetic base to disguise the
relationship between a destroyer and its cruiser.

`bot` is an [atom]().

`top` is an [atom]().

    ~zod/try=> ~(. mu 0x20e5 0x5901)
    <3.sjm [[@ux @ux] <414.hhh 100.xkc 1.ypj %164>]>

------------------------------------------------------------------------

### `++zag`

Add bottom into top

      ++  zag  [p=(end 4 1 (add top bot)) q=bot]

Produces the cell of `top` and `bot` with `top` scrambled to the result
of adding `bot` to `top` modulo 16. Used to scramble the name of a
destroyer.

`bot` is an [atom]().

`top` is an [atom]().

    ~zod/try=> `[@ux @ux]`~(zag mu 0x20e0 0x201)
    [0x22e1 0x201]

------------------------------------------------------------------------

### `++zig`

Subtract bottom from top

      ++  zig  [p=(end 4 1 (add top (sub 0x1.0000 bot))) q=bot]

The inverse of [`++zag`](). Produces the cell of `top` and `bot` with
`top` unscrambled. The unscrambled `top` is the sum of the sample `top`
and the 16-bit complement of `bot`. Used to unscramble the name of the
destroyer.

`bot` is an [atom]().

`top` is an [atom]().

    ~zod/try=> `[@ux @ux]`~(zig mu 0x2f46 0x1042)
    [0x1f04 0x1042]

------------------------------------------------------------------------

### `++zug`

Concatenate into atom

      ++  zug  (mix (lsh 4 1 top) bot)

Produces the concatenation of `top` and `bot`. Used to assemble a
destroyer name.

`bot` is an [atom]().

`top` is an [atom]().

    ~zod/try=> `@ux`~(zug mu 0x10e1 0xfa)
    0x10e1.00fa

------------------------------------------------------------------------

### `++so`

Coin parser engine

    ++  so
      |%

Core containing arms that parse [`++coin`]s.

    ~zod/try=> so
    <10.mkn 414.hhh 100.xkc 1.ypj %164>

------------------------------------------------------------------------

### `++bisk`

Parse odor-atom pair

      ++  bisk
        ;~  pose
          ;~  pfix  (just '0')
            ;~  pose
              (stag %ub ;~(pfix (just 'b') bay:ag))
              (stag %ui ;~(pfix (just 'i') dim:ag))
              (stag %ux ;~(pfix (just 'x') hex:ag))
              (stag %uv ;~(pfix (just 'v') viz:ag))
              (stag %uw ;~(pfix (just 'w') wiz:ag))
            ==
          ==
          (stag %ud dem:ag)
        ==

Parsing rule. Parses an unsigned integer of any permitted base,
producing a [`++dime`]().

    ~zod/try=> (scan "25" bisk:so)
    [%ud q=25]
    ~zod/try=> (scan "0x12.6401" bisk:so)
    [%ux q=1.205.249]

------------------------------------------------------------------------

### `++crub`

Parse `@da`, `@dr`, `@p`, `@t`

      ++  crub
        ;~  pose
          %+  cook
            |=(det=date `dime`[%da (year det)])
          ;~  plug
            %+  cook
              |=([a=@ b=?] [b a])
            ;~(plug dim:ag ;~(pose (cold | hep) (easy &)))
            ;~(pfix dot dim:ag)   ::  month
            ;~(pfix dot dim:ag)   ::  day
            ;~  pose
              ;~  pfix
                ;~(plug dot dot)
                ;~  plug
                  dum:ag
                  ;~(pfix dot dum:ag)
                  ;~(pfix dot dum:ag)
                  ;~(pose ;~(pfix ;~(plug dot dot) (most dot qix:ab)) (easy ~))
                ==
              ==
              (easy [0 0 0 ~])
            ==
          ==
        ::
          %+  cook
            |=  [a=(list ,[p=?(%d %h %m %s) q=@]) b=(list ,@)]
            =+  rop=`tarp`[0 0 0 0 b]
            |-  ^-  dime
            ?~  a
              [%dr (yule rop)]
            ?-  p.i.a
              %d  $(a t.a, d.rop (add q.i.a d.rop))
              %h  $(a t.a, h.rop (add q.i.a h.rop))
              %m  $(a t.a, m.rop (add q.i.a m.rop))
              %s  $(a t.a, s.rop (add q.i.a s.rop))
            ==
          ;~  plug
            %+  most
              dot
            ;~  pose
              ;~(pfix (just 'd') (stag %d dim:ag))
              ;~(pfix (just 'h') (stag %h dim:ag))
              ;~(pfix (just 'm') (stag %m dim:ag))
              ;~(pfix (just 's') (stag %s dim:ag))
            ==
            ;~(pose ;~(pfix ;~(plug dot dot) (most dot qix:ab)) (easy ~))
          ==
        ::
          (stag %p fed:ag)
          ;~(pfix dot (stag %ta urs:ab))
          ;~(pfix sig (stag %t (cook woad urs:ab)))
          ;~(pfix hep (stag %c (cook turf (cook woad urs:ab))))
        ==

Parsing rule. Parses any atom of any of the following odors after a
leading sig, `~` into a [`++dime`](): [`@da`](), [`@dr`](), [`@p`](),
and [`@t`](), producing a [`++dime`]().

    ~zod/try=> (scan "1926.5.12" crub:so)
    [p=~.da q=170.141.184.449.747.016.871.285.095.307.149.312.000]
    ~zod/try=> (,[%da @da] (scan "1926.5.12" crub:so))
    [%da ~1926.5.12]
    ~zod/try=> (scan "s10" crub:so)
    [p=~.dr q=184.467.440.737.095.516.160]
    ~zod/try=> (,[%dr @dr] (scan "s10" crub:so))
    [%dr ~s10]
    ~zod/try=> (scan "doznec" crub:so)
    [%p 256]
    ~zod/try=> (scan ".mas" crub:so)
    [%ta 7.561.581]

------------------------------------------------------------------------

### `++nuck`

Top-level coin parser

      ++  nuck
        %+  knee  *coin  |.  ~+
        %-  stew
        ^.  stet  ^.  limo
        :~  :-  ['a' 'z']  (cook |=(a=@ta [~ %tas a]) sym)
            :-  ['0' '9']  (stag ~ bisk)
            :-  '-'        (stag ~ tash)
            :-  '.'        ;~(pfix dot perd)
            :-  '~'        ;~(pfix sig ;~(pose twid (easy [~ %n 0])))
        ==

Parsing rule. Switches on the first character and applies the
corresponding [`++coin`]() parser.

    ~zod/try=> (scan "~pillyt" nuck:so)
    [% p=[p=~.p q=32.819]]
    ~zod/try=> (scan "0x12" nuck:so)
    [% p=[p=~.ux q=18]]
    ~zod/try=> (scan ".127.0.0.1" nuck:so)
    [% p=[p=~.if q=2.130.706.433]]
    ~zod/try=> `@ud`.127.0.0.1
    2.130.706.433
    ~zod/try=> (scan "._20_0w25_sam__" nuck:so)
    [ %many 
        p
      ~[[% p=[p=~.ud q=20]] [% p=[p=~.uw q=133]] [% p=[p=~.tas q=7.168.371]]]
    ]
    ~zod/try=> `@`%sam
    7.168.371
    ~zod/try=> (scan "~0ph" nuck:so)
    [%blob p=[1 1]]
    ~zod/try=> ~0ph
    [1 1]
    ~zod/try=> `@uv`(jam [1 1])
    0vph

------------------------------------------------------------------------

### `++nusk`

Parse coin literal with escapes

      ++  nusk
        (sear |=(a=@ta (rush (wick a) nuck)) urt:ab)

Parsing rule. Parses a coin literal with escapes. (See also: xx tuple
formatting).

    ~zod/try=> ~.asd_a
    ~.asd_a
    ~zod/try=> ._1_~~.asd~-a__
    [1 ~.asd_a]
    ~zod/try=> (scan "~~.asd~-a" nusk:so)
    [% p=[p=~.ta q=418.212.246.369]]
    ~zod/try=> (,[~ %ta @ta] (scan "~~.asd~-a" nusk:so))
    [~ %ta ~.asd_a]

------------------------------------------------------------------------

### `++perd`

Parsing rule.

      ++  perd
        ;~  pose
          (stag ~ zust)
          (stag %many (ifix [cab ;~(plug cab cab)] (more cab nusk)))
        ==

Parsing rule. Parses a dime or tuple without their respective standard
prefixes.

    ~zod/try=> (scan "y" perd:so)
    [~ [%f %.y]]
    ~zod/try=> (scan "n" perd:so)
    [~ [%f %.n]]
    ~zod/try=> |
    %.n
    ~zod/try=> (scan "_20_x__" perd:so)
    [%many [[% p=[p=~.ud q=20]] ~[[% p=[p=~.tas q=120]]]]]

------------------------------------------------------------------------

### `++royl`

Parse dime float

      ++  royl
        =+  ^=  zer
            (cook lent (star (just '0')))
        =+  ^=  voy
            %+  cook  royl-cell
            ;~  plug
              ;~(pose (cold | hep) (easy &))
              ;~(plug dim:ag ;~(pose ;~(pfix dot ;~(plug zer dim:ag)) (easy [0 0])))
              ;~  pose 
                ;~  pfix 
                  (just 'e') 
                  (cook some ;~(plug ;~(pose (cold | hep) (easy &)) dim:ag))
                == 
                (easy ~)  
              ==
            ==
        ;~  pose
          (stag %rh (cook rylh ;~(pfix ;~(plug sig sig) voy)))
          (stag %rq (cook rylq ;~(pfix ;~(plug sig sig sig) voy)))
          (stag %rd (cook ryld ;~(pfix sig voy)))
          (stag %rs (cook ryls voy)
        ==

Parsing rule. Parses a number into a [`++dime`]() float.

    ~zod/try=> (scan "~3.14" royl:so)
    [%rd .~3.13999999999999]
    ~zod/try=> .~3.14
    .~3.13999999999999

------------------------------------------------------------------------

### `++royl-cell`

XX still not fully functional

      ++  royl-cell
        |=  [a=? b=[c=@ d=@ e=@] f=(unit ,[h=? i=@])]  
        ^-  [? @ @ @ (unit ,@s)]
        ?~  f
          [a c.b d.b e.b ~]
        ?:  h.u.f
          [a c.b d.b e.b [~ (mul i.u.f 2)]]
        [a c.b d.b e.b [~ (dec (mul i.u.f 2))]]

Intermediate parsed float convereter

------------------------------------------------------------------------

### `++tash`

Parse signed dime

      ++  tash
        =+  ^=  neg
            |=  [syn=? mol=dime]  ^-  dime
            ?>  =('u' (end 3 1 p.mol))
            [(cat 3 's' (rsh 3 1 p.mol)) (new:si syn q.mol)]
        ;~  pfix  hep
          ;~  pose
            (cook |=(a=dime (neg | a)) bisk)
            ;~(pfix hep (cook |=(a=dime (neg & a)) bisk))
          ==
        ==

Parsing rule. Parses a signed number into a [`++dime`]().

    ~zod/try=> (scan "-20" tash:so)
    [p=~.sd q=39]
    ~zod/try=> (,[%sd @sd] (scan "-20" tash:so))
    [%sd -20]
    ~zod/try=> (,[%sd @sd] (scan "--20" tash:so))
    [%sd --20]
    ~zod/try=> (,[%sx @sx] (scan "--0x2e" tash:so))
    [%sx --0x2e]

------------------------------------------------------------------------

### `++twid`

Parse coins without `~` prefix

      ++  twid
        ;~  pose
          (cook |=(a=@ [%blob (cue a)]) ;~(pfix (just '0') vum:ag))
          (stag ~ crub)
        ==
      ::

Parsing rule. Parses coins after a leading sig, `~`.

    ~zod/try=> (scan "zod" twid:so)
    [~ [%p 0]]
    ~zod/try=> (scan ".sam" twid:so)
    [~ [%ta 7.168.371]]
    ~zod/try=> `@ud`~.sam
    7.168.371
    ~zod/try=> `@t`~.sam
    'sam'
    ~zod/try=> (scan "0ph" twid:so)
    [%blob [1 1]]

------------------------------------------------------------------------

### `++zust`

Parse prefixed dimes from `@if`, `@f`, `@rd`

      ++  zust
        ;~  pose
          (stag %is bip:ag)
          (stag %if lip:ag)
          (stag %f ;~(pose (cold & (just 'y')) (cold | (just 'n'))))
          royl
        ==
      --

Parsing rule. Parses an atom of either [`@if`]() (IP address), [`@f`]()
(loobean), or [`rf`]()(floating point) into a [`++dime`]().

    ~zod/try=> (scan "127.0.0.1" zust:so)
    [%if q=2.130.706.433]
    ~zod/try=> (scan "af.0.0.0.0.e7a5.30d2.7" zust:so)
    [%is q=908.651.950.243.594.834.993.091.554.288.205.831]
    ~zod/try=> (,[%is @is] (scan "af.0.0.0.0.e7a5.30d2.7" zust:so))
    [%is .af.0.0.0.0.e7a5.30d2.7]
    ~zod/try=> (,[%is @ux] (scan "af.0.0.0.0.e7a5.30d2.7" zust:so))
    [%is 0xaf.0000.0000.0000.0000.e7a5.30d2.0007]
    ~zod/try=> (scan "y" zust:so)
    [%f %.y]
    ~zod/try=> (scan "12.09" zust:so)
    [%rd .~12.00999999999999]

------------------------------------------------------------------------

### `++scot`

Render dime as cord

    ++  scot  |=(mol=dime ~(rent co %$ mol))

Renders a dime `mol` as a cord.

`mol` is a [`++dime`]().

    ~zod/try=> (scot %p ~pillyt)
    ~.~pillyt
    ~zod/try=> `@t`(scot %p ~pillyt)
    '~pillyt'
    ~zod/try=> (scot %ux 0x12)
    ~.0x12
    ~zod/try=> `@t`(scot %ux 0x12)
    '0x12'
    ~zod/try=> (scot %if .127.0.0.1)
    ~..127.0.0.1
    ~zod/try=> `@t`(scot %if .127.0.0.1)
    '.127.0.0.1'
    ~zod/try=> (scot %ta ~.asd_a)
    ~.~.asd_a
    ~zod/try=> `@t`(scot %ta ~.asd_a)
    '~.asd_a'

------------------------------------------------------------------------

### `++scow`

Render dime as tape

    ++  scow  |=(mol=dime ~(rend co %$ mol))

Renders `mol` as a tape.

`mol` is a [`++dime`]().

    ~zod/try=> (scow %p ~pillyt)
    "~pillyt"
    ~zod/try=> (scow %ux 0x12)
    "0x12"
    ~zod/try=> (scow %if .127.0.0.1)
    ".127.0.0.1"
    ~zod/try=> (scow %ta ~.asd_a)
    "~.asd_a"

------------------------------------------------------------------------

### `++slat`

Curried slaw

    ++  slat  |=(mod=@tas |=(txt=@ta (slaw mod txt)))

Produces a [`gate`]() that parses a [`term`]() `txt` to an atom of the
odor specified by `mod`.

`mod` is a term, an atom of odor [`@tas`]().

`txt` is a span, an atom of odor [`@ta`]().

    ~zod/try=> `(unit ,@p)`((slat %p) '~pillyt')
    [~ ~pillyt]
    ~zod/try=> `(unit ,@ux)`((slat %ux) '0x12')
    [~ 0x12]
    ~zod/try=> `(unit ,@if)`((slat %if) '.127.0.0.1')
    [~ .127.0.0.1]
    ~zod/try=> `(unit ,@ta)`((slat %ta) '~.asd_a')
    [~ ~.asd_a

------------------------------------------------------------------------

### `++slav`

Demand: parse span with input odor

    ++  slav  |=([mod=@tas txt=@ta] (need (slaw mod txt)))

Parses a span `txt` to an atom of the odor specificed by `mod`. Crashes
if it failes to parse.

`mod` is a term, an atom of odor [`@tas`]().

`txt` is a span, an atom of odor [`@ta`]().

    ~zod/try=> `@p`(slav %p '~pillyt')
    ~pillyt
    ~zod/try=> `@p`(slav %p '~pillam')
    ! exit
    ~zod/try=> `@ux`(slav %ux '0x12')
    0x12
    ~zod/try=> `@ux`(slav %ux '0b10')
    ! exit
    ~zod/try=> `@if`(slav %if '.127.0.0.1')
    .127.0.0.1
    ~zod/try=> `@if`(slav %if '.fe80.0.0.202')
    ! exit
    ~zod/try=> `@ta`(slav %ta '~.asd_a')
    ~.asd_a
    ~zod/try=> `@ta`(slav %ta '~~asd-a')
    ! exit

------------------------------------------------------------------------

### `++slaw`

Parse span to input odor

    ++  slaw
      |=  [mod=@tas txt=@ta]
      ^-  (unit ,@)
      =+  con=(slay txt)
      ?.(&(?=([~ %$ @ @] con) =(p.p.u.con mod)) ~ [~ q.p.u.con])
    ::

Parses a span `txt` to an atom of the odor specified by `mod`.

`mod` is a term, an atom of odor [`@tas`]().

`txt` is a span, an atom of odor [`@ta`]().

    ~zod/try=> `(unit ,@p)`(slaw %p '~pillyt')
    [~ ~pillyt]
    ~zod/try=> `(unit ,@p)`(slaw %p '~pillam')
    ~
    ~zod/try=> `(unit ,@ux)`(slaw %ux '0x12')
    [~ 0x12]
    ~zod/try=> `(unit ,@ux)`(slaw %ux '0b10')
    ~
    ~zod/try=> `(unit ,@if)`(slaw %if '.127.0.0.1')
    [~ .127.0.0.1]
    ~zod/try=> `(unit ,@if)`(slaw %if '.fe80.0.0.202')
    ~
    ~zod/try=> `(unit ,@ta)`(slaw %ta '~.asd_a')
    [~ ~.asd_a]
    ~zod/try=> `(unit ,@ta)`(slaw %ta '~~asd-a')
    ~

------------------------------------------------------------------------

### `++slay`

Parse span to coin

    ++  slay
      |=  txt=@ta  ^-  (unit coin)
      =+  vex=((full nuck:so) [[1 1] (trip txt)])
      ?~  q.vex
        ~
      [~ p.u.q.vex]
    ::

Parses a span `txt` to the unit of a [`++coin`]().

`txt` is a [`@ta`]().

    ~zod/try=> (slay '~pillyt')
    [~ [% p=[p=~.p q=32.819]]]
    ~zod/try=> (slay '0x12')
    [~ [% p=[p=~.ux q=18]]]
    ~zod/try=> (slay '.127.0.0.1')
    [~ [% p=[p=~.if q=2.130.706.433]]]
    ~zod/try=> `@ud`.127.0.0.1
    2.130.706.433
    ~zod/try=> (slay '._20_0w25_sam__')
    [ ~
      [ %many
        p=~[[% p=[p=~.ud q=20]] [% p=[p=~.uw q=133]] [% p=[p=~.tas q=7.168.371]]]
      ]
    ]
    ~zod/try=> `@`%sam
    7.168.371
    ~zod/try=> (slay '~0ph')
    [~ [%blob p=[1 1]]]
    ~zod/try=> 0ph
    ~ <syntax error at [1 2]>
    ~zod/try=> ~0ph
    [1 1]
    ~zod/try=> `@uv`(jam [1 1])
    0vph

------------------------------------------------------------------------

------------------------------------------------------------------------

### `++smyt`

Render path as tank

    ++  smyt
      |=  bon=path  ^-  tank
      :+  %rose  [['/' ~] ['/' ~] ['/' ~]]
      |-  ^-  (list tank)
      (turn bon |=(a=@ [%leaf (rip 3 a)]))

Renders the path `bon` as a [`tank`](), which is used for
pretty-printing.

`bon` is a [`++path`]().

    ~zod/try=> (smyt %)
    [ %rose
      p=[p="/" q="/" r="/"]
        q
      ~[ [%leaf p="~zod"]
         [%leaf p="try"] 
         [%leaf p="~2014.10.28..18.36.58..a280"]
       ]
    ]
    ~zod/try=> (smyt /as/les/top)
    [ %rose
      p=[p="/" q="/" r="/"]
      q=~[[%leaf p="as"] [%leaf p="les"] [%leaf p="top"]]
    ]

------------------------------------------------------------------------

### `++spat`

Render path as cord

    ++  spat  |=(pax=path (crip (spud pax)))               ::  path to cord

Renders a path `pax` as cord.

`pax` is a [`path`]().

    ~zod/try=> (spat %)
    '~zod/try/~2014.10.28..18.40.20..4287'
    ~zod/try=> (spat %/bin)
    '~zod/try/~2014.10.28..18.41.12..3bcd/bin'
    ~zod/try=> (spat /as/les/top)
    '/as/les/top'

------------------------------------------------------------------------

### `++spud`

Render path as tape

    ++  spud  |=(pax=path ~(ram re (smyt pax)))             ::  path to tape

Renders a path `pax` as [tape]().

`pax` is a [`path`]().

    ~zod/try=> (spud %)
    "~zod/try/~2014.10.28..18.40.46..e951"
    ~zod/try=> (spud %/bin)
    "~zod/try/~2014.10.28..18.41.05..16f2/bin"
    ~zod/try=> (spud /as/les/top)
    "/as/les/top"

------------------------------------------------------------------------

### `++stab`

Parse span to path

    ++  stab                                                ::  parse span to path
      |=  zep=@ta  ^-  path
      (rash zep ;~(pfix fas ;~(sfix (more fas urs:ab) fas)))

Parsing rule. Parses a span `zep` to a static [`++path`](/doc/hoon/library/1#++path).

    ~zod/try=> (stab '/as/lek/tor')
    /as/lek/tor
    ~zod/try=> `(pole ,@ta)`(stab '/as/lek/tor')
    [~.as [~.lek [~.tor ~]]]
    ~zod/try=> (stab '~zod/arvo/~2014.10.28..18.48.41..335f/zuse')
    ~zod/arvo/~2014.10.28..18.48.41..335f/zuse
    ~zod/try=> `(pole ,@ta)`(stab '~zod/arvo/~2014.10.28..18.48.41..335f/zuse')
    [~.~zod [~.arvo [~.~2014.10.28..18.48.41..335f [~.zuse ~]]]]
    ~zod/try=> (stab '/a/~pillyt/pals/1')
    /a/~pillyt/pals/1
    ~zod/try=> `(pole ,@ta)`(stab '/a/~pillyt/pals/1')
    [~.a [~.~pillyt [~.pals [~.1 ~]]]]
