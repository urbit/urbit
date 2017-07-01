::
::::  /hoon/telegrams/talk/mar
  ::
/?    310
/-  talk
/+  talk,map-to-json
!:
=+  talk
|_  gam/(list telegram)
::
++  grab
  |%
  ++  noun  (list telegram)
  ++  mime  |=(^mime (json (rash q.q apex:poja)))
  ++  json
    =>  [jo ..telegram]
    |=  a/json  ^-  (list telegram)
    =-  (need ((ar (ot ship+(su fed:ag) thought+thot ~)) a))
    |%
    ++  of
      |*  a/(pole {@tas fist})
      |=  b/json
      %.  ((of:jo a) b)
      %-  slog
      ?+  b  ~
          {$o *}
        %+  murn  `(list {@tas fist})`a
        |=  {c/term d/fist}  ^-  (unit tank)
        =+  (~(get by p.b) c)
        ?~  -  ~
        =+  (d u)
        ?~  -  (some >[c u]<)
        ~
      ==
    ++  op                                              ::  parse keys of map
      |*  {fel/rule wit/fist}
      %+  cu  malt
      %+  ci
        |=  a/(map cord _(need *wit))
        ^-  (unit (list _[(wonk *fel) (need *wit)]))
        (zl (turn (~(tap by a)) (head-rush fel)))
      (om wit)
    ::
    ++  as                                              ::  array as set
      |*  a/fist
      (cu ~(gas in *(set _(need *a))) (ar a))
    ::
    ++  ke                                              ::  callbacks
      |*  {gar/* sef/_|.(fist)}
      |=  jon/json
      ^-  (unit _gar)
      =-  ~!  gar  ~!  (need -)  -
      ((sef) jon)
    ::
    ++  lake  |*(a/_* $-(json (unit a)))
    ++  head-rush
      |*  a/rule
      |*  {cord *}
      =+  nit=(rush +<- a)
      ?~  nit  ~
      (some [u.nit +>->])
    ::
    ++  thot
      ^-  $-(json (unit thought))
      %-  ot  :~
        serial+`$-(json (unit serial))`(ci (slat %uv) so)
        audience+`$-(json (unit audience))`audi
        statement+`$-(json (unit statement))`stam
      ==
    ::
    ++  audi  `$-(json (unit audience))`(op parn memb)
    ++  auri  (op parn (ci (soft presence) so))
    ++  memb  ^-  $-(json (unit (pair envelope delivery)))
              (ot envelope+lope delivery+(ci (soft delivery) so) ~)
    ++  lope  (ot visible+bo sender+(mu (su parn)) ~)
    ::
    ++  parn
      ^-  $-(nail (like partner))
      %+  pick
        ;~((glue fas) ;~(pfix sig fed:ag) urs:ab)
      %+  sear  (soft passport)
      ;~((glue fas) sym urs:ab)                         ::  XX  [a-z0-9_]{1,15}
    ::
    ++  stam  (ot date+di speech+spec ~)
    ++  spec
      %+  ke  *speech  |.  ~+
      %-  of  :~
        lin+(ot say+bo txt+so ~)
        url+(ot txt+(su aurf:urlp) ~)
        exp+(ot txt+so ~)
        app+(ot txt+so src+so ~)
        fat+(ot tor+tors taf+spec ~)
        ext+(ot nom+so txe+blob ~)
        non+ul
        mor+(ar spec)
        ::  inv+(ot ship+(su fed:ag) party+(su urs:ab) ~)
      ==
    ++  tors
      %+  ke  *torso  |.  ~+
      %-  of  :~
        name+(ot nom+so mon+tors ~)
        text+(cu lore so)
        tank+(ot dat+(cu (hard (list tank)) blob) ~)
      ==
    ::
    ++  blob  (cu cue (su fel:ofis))
    --
  --
::
++  grow
  |%
  ++  mime  [/text/json (taco (crip (pojo json)))]
  ++  json
    =>  +
    |^
    :-  %a
    %+  turn  gam
    |=  telegram
    (jobe ship+(jope aut) thought+(thot tot) ~)
    ::
    ++  jove
      |=  {a/envelope b/delivery}
      %-  jobe  :~
        envelope+(jobe visible+[%b vis.a] sender+?~(sen.a ~ s+(parn u.sen.a)) ~)
        delivery+[%s b]
      ==
    ::
    ++  jope  |=(a/ship (jape +:<a>)) ::[%s (crip +:(scow %p a))])
    ++  joke  |=(a/tank [%s (role (turn (wash 0^80 a) crip))])
    ++  jode  |=(a/time (jone (div (mul (sub a ~1970.1.1) 1.000) ~s1)))
    ::
    ++  thot
      |=  thought
      (jobe serial+(jape <uid>) audience+(audi aud) statement+(stam sam) ~)
    ::
    ++  audi  (map-to-json parn jove)
    ++  bouq
      |=  a/bouquet
      a+(turn (~(tap in a)) |=(b/path a+(turn b |=(c/knot s+c))))
    ::
    ++  parn
      |=  a/partner  ^-  cord
      ?-  -.a
        $&  (circ p.a)
        $|  %-  crip
            ?-  -.p.a
              $twitter  "{(trip -.p.a)}/{(trip p.p.a)}"
            ==
      ==
    ::
    ++  circ
      |=  a/circle  ^-  cord
      (crip "{<hos.a>}/{(trip nom.a)}")
    ::
    ++  stam
      |=  statement
      (jobe date+(jode wen) bouquet+(bouq boq) speech+(spec sep) ~)
    ::
    ++  spec
      |=  a/speech
      %+  joba  -.a
      ?+  -.a  ~|(stub+-.a !!)
        $non  ~
        $lin  (jobe txt+[%s msg.a] say+[%b pat.a] ~)
        $url  (joba txt+(jape (earf url.a)))
        $exp  (joba txt+[%s exp.a])
        $fat  (jobe tor+(tors tac.a) taf+$(a sep.a) ~)
        $mor  :-  %a
              |-  ^-  (list json)
              ?~  ses.a  ~
              [^$(a i.ses.a) $(ses.a t.ses.a)]
        $ext  (jobe nom+[%s nom.a] txe+(jape (sifo (jam +.a))) ~)
        $app  (jobe txt+[%s msg.a] src+[%s app.a] ~)
        ::  $inv  (jobe ship+(jope p.a) party+[%s q.a] ~)
      ==
    ::
    ++  tors
      |=  a/torso
      %+  joba  -.a
      ?-  -.a
        $text  [%s (role +.a)]
        $tank  (jobe txt+[%a (turn +.a joke)] dat+(jape (sifo (jam +.a))) ~)
        $name  (jobe nom+s+nom.a mon+$(a tac.a) ~)
      ==
    ::
    --
  --
::
++  grad
  |%
  ++  form  %talk-telegrams
  ++  diff  |=((list telegram) +<)
  ++  pact  |=((list telegram) +<)
  ++  join  |=({(list telegram) (list telegram)} `(unit mime)`~)
  --
--
