::
::::  /hoon/command/talk/mar
  ::
/?    310
/-    talk
!:
[talk .]
|_  cod/command
::
++  grab                                                ::  convert from
  |%
  ++  noun  command                                     ::  clam from %noun
  ++  json
    =>  [jo ..command]
    |=  a/json  ^-  command
    =-  (need ((of -) a))
    =<  :~  publish+(ar thot)
            bearing+ul
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
      (as (su parn))
    ::
    ++  parn
      ^-  $-(nail (like partner))
      %+  peach
        ;~((glue fas) ;~(pfix sig fed:ag) urs:ab)
      %+  sear  (soft passport)
      ;~((glue fas) sym urs:ab)                         ::  XX  [a-z0-9_]{1,15}
    ::
    ++  speech-or-eval  $?(speech {$eval p/@t} {$mor ses/(list speech-or-eval)})
    ++  eval
      |=  a/(pair @da speech-or-eval)
      ^-  statement
      %=  a
          q
        |-  ^-  speech
        ?:  ?=($mor -.q.a)
          ::[%mor (turn ses.q.a |=(b/speech-or-eval ^$(q.a b)))]
          ~&  %todo-talk-command-mark  ::TODO  fix
          *speech
        ?.  ?=($eval -.q.a)  q.a
        =-  [%exp p.q.a -]
        =+  pax=[&1:% &2:% (scot %da p.a) |3:%]
        p:(mule |.([(sell (slap !>(..zuse) (rain pax p.q.a)))]~))
      ==
    ::
    ++  stam
      ^-  $-(json (unit statement))
      %+  cu  eval
      (ot date+di speech+spec ~)
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
  --
--  --
