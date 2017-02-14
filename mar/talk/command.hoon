::
::::  /hoon/command/talk/mar
  ::
/?    310
/-    talk
/+    old-zuse
::
=,  talk
|_  cod/command
::
++  grab                                                ::  convert from
  |%
  ++  noun  command                                     ::  clam from %noun
  ++  json
    =,  old-zuse
    =,  jo
    |=  a/json  ^-  command
    =-  (need ((of -) a))
    =<  :~  publish+(ar thot)
            review+(ar thot)
            design+(ot party+so config+(mu conf) ~)
        ==
    |%
    ++  op                                              ::  parse keys of map
      |*  {fel/rule wit/fist}
      %+  cu  malt
      %+  ci
        |=  a/(map cord _(need *wit))
        ^-  (unit (list _[(wonk *fel) (need *wit)]))
        (zl (turn (~(tap by a)) (head-rush fel)))
      (om wit)
    ::
    ++  ke                                              ::  callbacks
      |*  {gar/* sef/_|.(fist)}
      |=  jon/json
      ^-  (unit _gar)
      =-  ~!  gar  ~!  (need -)  -
      ((sef) jon)
    ::
    ++  as                                              ::  array as set
      |*  a/fist
      (cu ~(gas in *(set _(need *a))) (ar a))
    ::
    ++  lake  |*(a/_* $-(json (unit a)))
    ++  peach
      |*  a/{rule rule}
      |=  tub/nail
      ^-  (like (each _(wonk (-.a)) _(wonk (+.a))))
      %.  tub
      ;~(pose (stag %& -.a) (stag %| +.a))
    ::
    ++  head-rush
      |*  a/rule
      |*  {cord *}
      =+  nit=(rush +<- a)
      ?~  nit  ~
      (some [u.nit +>->])
    ::
    ::
    ++  thot
      ^-  $-(json (unit thought))
      %-  ot  :~
        serial+ceri
        audience+audi
        statement+stam
      ==
    ::
    ++  ceri
      ^-  $-(json (unit serial))
      (ci (slat %uv) so)
    ::
    ++  audi
      ^-  $-(json (unit audience))
      (op parn memb)
    ::
    ++  auri  (op parn (ci (soft presence) so))
    ++  memb  (ot [envelope+lope delivery+(ci (soft delivery) so) ~])
    ++  lope  (ot [visible+bo sender+(mu (su parn)) ~])
    ::
    ++  parn
      ^-  $-(nail (like partner))
      %+  peach
        ;~((glue fas) ;~(pfix sig fed:ag) urs:ab)
      %+  sear  (soft passport)
      ;~((glue fas) sym urs:ab)                         ::  XX  [a-z0-9_]{1,15}
    ::
    ++  speech-or-eval  $?(speech {$eval p/@t} {$mor p/(list speech-or-eval)})
    ++  eval
      |=  a/(trel @da bouquet speech-or-eval)
      ^-  statement
      %=  a  r  ^-  speech
        |-
        ?:  ?=($mor -.r.a)
          [%mor (turn p.r.a |=(b/speech-or-eval ^$(r.a b)))]
        ?.  ?=($eval -.r.a)  r.a
        =-  [%fat tank+- %exp p.r.a]
        =+  pax=[&1:% &2:% (scot %da p.a) |3:%]
        p:(mule |.([(sell (slap !>(..zuse) (rain pax p.r.a)))]~))
      ==
    ::
    ++  stam
      ^-  $-(json (unit statement))
      %+  cu  eval
      (ot date+di bouquet+(as (ar so)) speech+spec ~)
    ::
    ++  spec
      %+  ke  *speech-or-eval  |.
      %-  of
      :~  lin+(ot say+bo txt+so ~)
          url+(su aurf:urlp)
          eval+so
          mor+(ar spec)
          ::  exp+(cu |=(a=cord [a ~]) so)
          ::  inv+(ot ship+(su fed:ag) party+(su urs:ab) ~)
      ==
    ::
    ++  conf
      ^-  $-(json (unit config))
      %-  ot  :~
        sources+(as (su parn))
        caption+so
        :-  %cordon
        (ot posture+(ci (soft posture) so) list+(as (su fed:ag)) ~)
      ==
  --
--  --
