section 2eZ, OLD rendering
==========================

<h3 id="++show"><code>++show</code></h3>

    ++  show                            ::  XX deprecated, use type
      |=  vem=*
      |^  ^-  tank
          ?:  ?=(@ vem)
            [%leaf (mesc (trip vem))]
          ?-    vem
              [s=~ c=*]
            [%leaf '\'' (weld (mesc (tape +.vem)) `tape`['\'' ~])]
          ::
              [s=%a c=@]        [%leaf (mesc (trip c.vem))]
              [s=%b c=*]        (shop c.vem |=(a=@ ~(rub at a)))
              [s=[%c p=@] c=*]
            :+  %palm
              [['.' ~] ['-' ~] ~ ~]
            [[%leaf (mesc (trip p.s.vem))] $(vem c.vem) ~]
          ::
              [s=%d c=*]        (shop c.vem |=(a=@ ~(rud at a)))
              [s=%k c=*]        (tank c.vem)
              [s=%h c=*]
            ?:  =(0 c.vem)      ::  XX remove after 220
              [%leaf '#' ~]
            :+  %rose
              [['/' ~] ['/' ~] ~]
            =+  yol=((list ,@ta) c.vem)
            (turn yol |=(a=@ta [%leaf (trip a)]))
          ::
              [s=%o c=*]
            %=    $
                vem
              :-  [%m '%h:<[%d %d].[%d %d]>']
              [-.c.vem +<-.c.vem +<+.c.vem +>-.c.vem +>+.c.vem ~]
            ==
          ::
              [s=%p c=*]        (shop c.vem |=(a=@ ~(rup at a)))
              [s=%q c=*]        (shop c.vem |=(a=@ ~(r at a)))
              [s=%r c=*]        $(vem [[%r ' ' '{' '}'] c.vem])
              [s=%t c=*]        (shop c.vem |=(a=@ ~(rt at a)))
              [s=%v c=*]        (shop c.vem |=(a=@ ~(ruv at a)))
              [s=%x c=*]        (shop c.vem |=(a=@ ~(rux at a)))
              [s=[%m p=@] c=*]  (shep p.s.vem c.vem)
              [s=[%r p=@] c=*]
            $(vem [[%r ' ' (cut 3 [0 1] p.s.vem) (cut 3 [1 1] p.s.vem)] c.vem])
          ::
              [s=[%r p=@ q=@ r=@] c=*]
            :+  %rose
              :*  p=(mesc (trip p.s.vem))
                  q=(mesc (trip q.s.vem))
                  r=(mesc (trip r.s.vem))
              ==
            |-  ^-  (list tank)
            ?@  c.vem
              ~
            [^$(vem -.c.vem) $(c.vem +.c.vem)]
          ::
              [s=%z c=*]        $(vem [[%r %$ %$ %$] c.vem])
              *                 !!
          ==

XX document

<h3 id="++shep"><code>++shep</code></h3>

      ++  shep
        |=  [fom=@ gar=*]
        ^-  tank
        =+  l=(met 3 fom)
        =+  i=0
        :-  %leaf
        |-  ^-  tape
        ?:  (gte i l)
          ~
        =+  c=(cut 3 [i 1] fom)
        ?.  =(37 c)
          (weld (mesc [c ~]) $(i +(i)))
        =+  d=(cut 3 [+(i) 1] fom)
        ?.  .?(gar)
          ['\\' '#' $(i (add 2 i))]
        (weld ~(ram re (show d -.gar)) $(i (add 2 i), gar +.gar))
      ::

XX document

<h3 id="++shop"><code>++shop</code></h3>

      ++  shop
        |=  [aug=* vel=$+(a=@ tape)]
        ^-  tank
        ?:  ?=(@ aug)
          [%leaf (vel aug)]
        :+  %rose
          [[' ' ~] ['[' ~] [']' ~]]
        =>  .(aug `*`aug)
        |-  ^-  (list tank)
        ?:  ?=(@ aug)
          [^$ ~]
        [^$(aug -.aug) $(aug +.aug)]
      --

XX document

<h3 id="++at"><code>++at</code></h3>

    ++  at
      |_  a=@

XX document

<h3 id="++r"><code>++r</code></h3>

      ++  r
        ?:  ?&  (gte (met 3 a) 2)
                |-
                ?:  =(0 a)
                  &
                =+  vis=(end 3 1 a)
                ?&  ?|(=('-' vis) ?&((gte vis 'a') (lte vis 'z')))
                    $(a (rsh 3 1 a))
                ==
            ==
          rtam
        ?:  (lte (met 3 a) 2)
          rud
        rux
      ::

XX document

<h3 id="++rf"><code>++rf</code></h3>

      ++  rf    `tape`[?-(a & '&', | '|', * !!) ~]

XX document

<h3 id="++rn"><code>++rn</code></h3>

      ++  rn    `tape`[?>(=(0 a) '~') ~]

XX document

<h3 id="++rt"><code>++rt</code></h3>

      ++  rt    `tape`['\'' (weld (mesc (trip a)) `tape`['\'' ~])]

XX document

<h3 id="++rta"><code>++rta</code></h3>

      ++  rta   rt

XX document

<h3 id="++rtam"><code>++rtam</code></h3>

      ++  rtam  `tape`['%' (trip a)]

XX document

<h3 id="++rub"><code>++rub</code></h3>

      ++  rub   `tape`['0' 'b' (rum 2 ~ |=(b=@ (add '0' b)))]

XX document

<h3 id="++rud"><code>++rud</code></h3>

      ++  rud   (rum 10 ~ |=(b=@ (add '0' b)))

XX document

<h3 id="++rum"><code>++rum</code></h3>

      ++  rum
        |=  [b=@ c=tape d=$+(@ @)]
        ^-  tape
        ?:  =(0 a)
          [(d 0) c]
        =+  e=0
        |-  ^-  tape
        ?:  =(0 a)
          c
        =+  f=&(!=(0 e) =(0 (mod e ?:(=(10 b) 3 4))))
        %=  $
          a  (div a b)
          c  [(d (mod a b)) ?:(f [?:(=(10 b) ',' '-') c] c)]
          e  +(e)
        ==
      ::

XX document

<h3 id="++rup"><code>++rup</code></h3>

      ++  rup
        =+  b=(met 3 a)
        ^-  tape
        :-  '-'
        |-  ^-  tape
        ?:  (gth (met 5 a) 1)
          %+  weld
            $(a (rsh 5 1 a), b (sub b 4))
          `tape`['-' '-' $(a (end 5 1 a), b 4)]
        ?:  =(0 b)
          ['~' ~]
        ?:  (lte b 1)
          (trip (tos:po a))
        |-  ^-  tape
        ?:  =(2 b)
          =+  c=(rsh 3 1 a)
          =+  d=(end 3 1 a)
          (weld (trip (tod:po c)) (trip (tos:po (mix c d))))
        =+  c=(rsh 3 2 a)
        =+  d=(end 3 2 a)
        (weld ^$(a c, b (met 3 c)) `tape`['-' $(a (mix c d), b 2)])
      ::

XX document

<h3 id="++ruv"><code>++ruv</code></h3>

      ++  ruv
        ^-  tape
        :+  '0'
          'v'
        %^    rum
            64
          ~
        |=  b=@
        ?:  =(63 b)
          '+'
        ?:  =(62 b)
          '-'
        ?:((lth b 26) (add 65 b) ?:((lth b 52) (add 71 b) (sub b 4)))
      ::

XX document

<h3 id="++rux"><code>++rux</code></h3>

      ++  rux  `tape`['0' 'x' (rum 16 ~ |=(b=@ (add b ?:((lth b 10) 48 87))))]
      --
      ::::::::::::::::::::::::::::::::::::::::::::::::::::::  ::

XX document

------------------------------------------------------------------------
