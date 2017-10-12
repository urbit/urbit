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
++  grab                                                ::  convert from
  |%
  ++  noun  (list telegram)
  ++  mime  |=(^mime (json (rash q.q apex:de-json)))
  ++  json
    =>  [dejs talk old-zuse]
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
        serial+`$-(json (unit serial))`(ci (slat %uv) so)
        audience+`$-(json (unit audience))`audi
        statement+`$-(json (unit statement))`stam
      ==
    ::
    ++  audi  `$-(json (unit audience))`circ
    ++  auri  (op circ (ci (soft presence) so))
    ++  lope  (ot visible+bo sender+(mu (su circ)) ~)
    ::
    ++  circ
      ^-  $-(nail (like circle))
      ;~((glue fas) ;~(pfix sig fed:ag) urs:ab)
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
++  grow                                                ::  convert to
  |%
  ++  mime  [/text/json (as-octs (crip (en-json json)))]
  ++  json
    =>  +
    |^
    :-  %a
    %+  turn  gam
    |=  telegram
    (jobe ship+(jope aut) thought+(thot tot) ~)
    ::
    ++  jope  |=(a/ship (jape +:<a>)) ::[%s (crip +:(scow %p a))])
    ++  joke  |=(a/tank (jape (of-wall (wash 0^80 a))))
    ++  jode  |=(a/time (jone (div (mul (sub a ~1970.1.1) 1.000) ~s1)))
    ::
    ++  thot
      |=  thought
      (jobe serial+(jape <uid>) audience+(audi aud) statement+(stam sam) ~)
    ::
    ++  audi  (set-to-json circ)
    ::
    ++  circ
      |=  a/circle  ^-  cord
      (crip "{<hos.a>}/{(trip nom.a)}")
    ::
    ++  stam
      |=  statement
      (jobe date+(jode wen) speech+(spec sep) ~)
    ::
    ++  spec
      |=  a/speech
      %+  joba  -.a
      ?+  -.a  ~|(stub+-.a !!)
        $lin  (jobe txt+[%s msg.a] say+[%b pat.a] ~)
        $url  (joba txt+(jape (earf url.a)))
        $exp  (joba txt+[%s exp.a])
        $fat  (jobe tor+(tors tac.a) taf+$(a sep.a) ~)
        $app  (jobe txt+[%s msg.a] src+[%s app.a] ~)
        ::  $inv  (jobe ship+(jope p.a) party+[%s q.a] ~)
      ==
    ::
    ++  tors
      |=  a/torso
      %+  joba  -.a
      ?-  -.a
        $text  [%s (of-wain +.a)]
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
