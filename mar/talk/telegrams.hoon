::
::::  /hoon/telegrams/talk/mar
  ::
/?    310
/-  talk
/+  talk, map-to-json, old-zuse
::
=,  talk
=,  mimes:html
=,  format
=,  html
=,  old-zuse
|_  gam/(list telegram)
::
++  grab  
  |%  
  ++  noun  (list telegram)
  ++  mime  |=(^mime (json (rash q.q apex:de-json)))
  ++  json
    =>  [dejs talk old-zuse]
    |=  a/json  ^-  (list telegram)
    %.  a
    |^  (ar (ot ship+(su fed:ag) thought+thot ~))
::     ++  of
::       |*  a/(pole {@tas fist})
::       |=  b/json
::       %.  ((of:jo a) b)
::       %-  slog
::       ?+  b  ~
::           {$o *} 
::         %+  murn  `(list {@tas fist})`a
::         |=  {c/term d/fist}  ^-  (unit tank)
::         =+  (~(get by p.b) c)
::         ?~  -  ~
::         =+  (d u)
::         ?~  -  (some >[c u]<)
::         ~  
::       ==
    ::
    ++  as                                              ::  array as set
      |*  a/fist 
      (cu ~(gas in *(set _*a)) (ar a))
    ::
    ++  ke                                              ::  callbacks
      |*  {gar/* sef/_|.(fist)}
      |=  jon/json
      ^+  gar
      =-  ~!  gar  ~!  (need -)  -
      ((sef) jon)
    ::
    ++  head-rush  
      |*  a/rule
      |*  {cord *}
      =+  nit=(rush +<- a) 
      ?~  nit  ~
      (some [u.nit +>->])
    ::
    ++  thot
      ^-  $-(json thought)
      %-  ot  :~
        serial+`$-(json serial)`(ci (slat %uv) so)  
        audience+`$-(json audience)`audi 
        statement+`$-(json statement)`stam  
      ==
    ::
    ++  audi  `$-(json audience)`(op parn memb)  
    ++  auri  (op parn (ci (soft presence) so))  
    ++  memb  ^-  $-(json (pair envelope delivery))
              (ot envelope+lope delivery+(cu (hard delivery) so) ~)
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
        mor+(ar spec)
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
  ++  mime  [/text/json (as-octs (crip (en-json json)))]
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
    ++  joke  |=(a/tank (jape (of-wall (wash 0^80 a))))
    ++  jode  |=(a/time (jone (div (mul (sub a ~1970.1.1) 1.000) ~s1)))
    ::
    ++  thot
      |=  thought
      (jobe serial+(jape <p>) audience+(audi q) statement+(stam r) ~)
    ::
    ++  audi  (map-to-json parn jove)
    ++  bouq
      |=  a/bouquet
      a+(turn ~(tap in a) |=(b/path a+(turn b |=(c/knot s+c))))
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
        $mor  :-  %a
              |-  ^-  (list json)
              ?~  p.a  ~
              [^$(a i.p.a) $(p.a t.p.a)]
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
