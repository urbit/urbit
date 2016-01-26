::
::::  /hoon/talk-report/mar
  ::
/?    314
/-    talk
/+    talk
!:
[talk .]
|_  rep/report
::
++  grab                                                ::  convert from
  |%
  ++  noun  report                                      ::  clam from %noun
  --
++  grow
  |%
  ++  mime  [/text/json (taco (crip (pojo json)))]
  ++  json
    =>  +
    |^  %+  joba  -.rep
        ?-  -.rep
          $cabal  (cabl +.rep)
          $house  a+(turn (~(tap by +.rep)) jose)
          $glyph  ((jome |=(a/char a) nack) +.rep)
          $grams  (jobe num+(jone p.rep) tele+[%a (turn q.rep gram)] ~)
          $group  (jobe local+(grop p.rep) global+%.(q.rep (jome parn grop)) ~)
        ==
    ++  joce  |=(a/span [%s a])
    ++  jose
      |=  {a/span b/posture c/cord}
      (jobe name+[%s a] posture+[%s a] caption+[%s b] ~)
    ::
    ++  jove
      |=  {a/envelope b/delivery}
      %-  jobe  :~
        envelope+(jobe visible+[%b p.a] sender+?~(q.a ~ s+(parn u.q.a)) ~)
        delivery+[%s b]
      ==
    ++  jope  |=(a/ship (jape +:<a>)) ::[%s (crip +:(scow %p a))])
    ++  joke  |=(a/tank [%s (role (turn (wash 0^80 a) crip))])
    ++  jode  |=(a/time (jone (div (mul (sub a ~1970.1.1) 1.000) ~s1)))
::     ++  jase
::       |*  a=,json
::       |=  b=(set ,$+<.a)  ^-  json
::       ~!  b
::       [%a (turn (~(tap in b)) a)]
    ::
    ++  jome                                            ::  stringify keys
      |*  {a/_cord b/_json}
      |=  c/(map _+<.a _+<.b)
      (jobe (turn (~(tap by c)) (both a b)))
    ::
    ++  both                                            ::  cons two gates
      |*  {a/_* b/_*}
      |=(c/_[+<.a +<.b] [(a -.c) (b +.c)])
    ::
    ::
    ++  nack  |=(a/(set (set partner)) [%a (turn (~(tap in a)) sorc)])
    ++  grop  (jome phon stas)                          ::  (map ship status)
    ++  phon  |=(a/ship (scot %p a))
    ++  stas  |=(status (jobe presence+(joce p) human+(huma q) ~))
    ++  gram  |=(telegram (jobe ship+(jope p) thought+(thot q) ~))
    ++  thot
      |=  thought
      (jobe serial+(jape <p>) audience+(audi q) statement+(stam r) ~)
    ::
    ++  audi  (jome parn jove)
    ++  bouq
      |=  a/bouquet
      a+(turn (~(tap in a)) |=(b/path a+(turn b |=(c/span s+c))))
    ::
    ++  parn
      |=  a/partner  ^-  cord
      ?-  -.a
        $&  (stat p.a)
        $|  %-  crip
            ?-  -.p.a
              $twitter  "{(trip -.p.a)}/{(trip p.p.a)}"
            ==
      ==
    ::
    ++  stat
      |=  a/station  ^-  cord
      (crip "{<p.a>}/{(trip q.a)}")
    ::
    ++  stam
      |=  statement
      (jobe date+(jode p) bouquet+(bouq q) speech+(spec r) ~)
    ::
    ++  spec
      |=  a/speech
      %+  joba  -.a
      ?+  -.a  ~|(stub+-.a !!)
        $lin  (jobe txt+[%s q.a] say+[%b p.a] ~)
        $url  (joba txt+[%s (crip (earf p.a))])
        $exp  (joba txt+[%s p.a])
        $tax  (joba txt+(jape (rend-work-duty p.a)))
        $app  (jobe txt+[%s q.a] src+[%s p.a] ~)
        $fat  (jobe tor+(tors p.a) taf+$(a q.a) ~)
        $mor  a+(turn p.a spec)
        ::  %inv  (jobe ship+(jope p.a) party+[%s q.a] ~)
      ==
    ::
    ++  tors
      |=  a/torso
      %+  joba  -.a
      ?-  -.a
        $text  [%s (role +.a)]
        $tank  [%a (turn +.a joke)]
        $name  (jobe nom+s+p.a mon+$(a q.a) ~)
      ==
    ::
    ++  huma
      |=  human
      %^  jobe
        hand+?~(hand ~ [%s u.hand])
        :-  %true
        ?~  true  ~
        =+  u.true
        (jobe first+[%s p] middle+?~(q ~ [%s u.q]) last+[%s r] ~)
      ~
    ::
    ++  cabl
      |=  cabal
      %-  jobe  :~
        loc+(conf loc)
        ham+((jome stat conf) ham)
      ==
    ::
    ++  sorc  
      |=  a/(set partner)  ^-  json
      [%a (turn (~(tap in a)) |=(b/partner s+(parn b)))]
    ::
    ++  conf
      |=  config
      %-  jobe  :~
        sources+(sorc sources)
        caption+[%s caption]
        =-  cordon+(jobe posture+[%s -.cordon] list+[%a -] ~)
        (turn (~(tap in q.cordon)) jope)                ::  XX  jase
      ==
  --
--  --

