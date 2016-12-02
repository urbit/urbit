::
::::  /hoon/telegrams/talk/mar
  ::
/?    310
/-  talk
/+  talk,map-to-json
::
=,  talk
=,  bytes:eyre
=,  format
=,  html
|_  gam/(list telegram)
::
++  grab  
  |%  
  ++  noun  (list telegram)
  ++  mime  |=(^mime (json (rash q.q apex:de-json)))
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
    ++  stam  (ot date+di bouquet+(as (ar so)) speech+spec ~)
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
        ::  inv+(ot ship+(su fed:ag) party+(su urs:ab) ~)
      ==
    ++  tors  
      %+  ke  *torso  |.  ~+
      %-  of  :~
        name+(ot nom+so mon+tors ~) 
        text+(cu to-wain so)
        tank+(ot dat+(cu (hard (list tank)) blob) ~)
      ==
    ::
    ++  blob  (cu cue (su fel:ofis))
    --
  --
::
++  grow
  |%
  ++  mime  [/text/json (taco (crip (en-json json)))]
  ++  json
    =>  +
    |^
    :-  %a
    %+  turn  gam
    |=  telegram
    (jobe ship+(jope p) thought+(thot q) ~)
    ::
    ++  jove
      |=  {a/envelope b/delivery}
      %-  jobe  :~
        envelope+(jobe visible+[%b p.a] sender+?~(q.a ~ s+(parn u.q.a)) ~)
        delivery+[%s b]
      ==
    ::
    ++  jope  |=(a/ship (jape +:<a>)) ::[%s (crip +:(scow %p a))])
    ++  joke  |=(a/tank (jape (of-wall (wash 0^80 a)))])
    ++  jode  |=(a/time (jone (div (mul (sub a ~1970.1.1) 1.000) ~s1)))
    ::
    ++  thot
      |=  thought
      (jobe serial+(jape <p>) audience+(audi q) statement+(stam r) ~)
    ::
    ++  audi  (map-to-json parn jove)
    ++  bouq
      |=  a/bouquet
      a+(turn (~(tap in a)) |=(b/path a+(turn b |=(c/knot s+c))))  
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
        $url  (joba txt+(jape (earf p.a)))
        $exp  (joba txt+[%s p.a])
        $app  (jobe txt+[%s q.a] src+[%s p.a] ~)
        $fat  (jobe tor+(tors p.a) taf+$(a q.a) ~)
        $ext  (jobe nom+[%s p.a] txe+(jape (sifo (jam +.a))) ~)
        $non  ~
        ::  $inv  (jobe ship+(jope p.a) party+[%s q.a] ~)
      ==
    ::
    ++  tors
      |=  a/torso
      %+  joba  -.a
      ?-  -.a
        $text  [%s (of-wain +.a)]
        $tank  (jobe txt+[%a (turn +.a joke)] dat+(jape (sifo (jam +.a))) ~)
        $name  (jobe nom+s+p.a mon+$(a q.a) ~)
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
