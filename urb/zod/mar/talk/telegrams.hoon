::
::::  /hoon/talk-telegrams/mar
  ::
/?  314
/-  *talk
!:
|_  gam=(list telegram)
::
++  grab
  |%
  ++  noun  (list telegram)
  ++  mime  |=(^mime (json (rash q.q apex:poja)))
  ++  json
    =>  [jo ..telegram]
    |=  a=json  ^-  (list telegram)
    =-  (need ((ar (ot ship/(su fed:ag) thought/thot ~)) a))
    |%
    ++  op                                              ::  parse keys of map
      |*  [fel=_rule wit=fist]
      %+  cu  mo
      %-  ci  :_  (om wit)
      |=  a=(map cord ,_(need *wit))
      ^-  (unit (list ,_[(wonk *fel) (need *wit)]))
      (zl (turn (~(tap by a)) (head-rush fel)))
    ::
    ++  as                                              ::  array as set
      :: |*(a=fist (cu sa (ar a)))  ::  XX  types
      |*  a=fist 
      %-  cu  :_  (ar a)
      ~(gas in *(set ,_(need *a)))
    ::
    ++  lake  |*(a=_,* $+(json (unit a)))
    ++  peach
      |*  a=_[rule rule]
      |=  tub=nail  
      ^-  (like (each ,_(wonk (-.a)) ,_(wonk (+.a))))
      %.  tub
      ;~(pose (stag %& -.a) (stag %| +.a))
    ::
    ++  head-rush
      |*  a=_rule
      |*  [b=cord c=*]
      =+  nit=(rush b a) 
      ?~  nit  ~
      (some [u.nit c])
    ::
    ::
    ++  thot
      ^-  $+(json (unit thought))
      %-  ot  :~
        serial/(ci (slat %uv) so)
        audience/audi 
        statement/stam
      ==
    ::
    ++  audi  (op parn memb)                            ::  audience
    ++  auri  (op parn (ci (soft presence) so))
    ++  memb  (ot envelope/lope delivery/(ci (soft delivery) so) ~)
    ++  lope  (ot visible/bo sender/(mu (su parn)) ~)
    ::
    ++  parn
      ^-  $+(nail (like partner))
      %+  peach
        ;~((glue fas) ;~(pfix sig fed:ag) urs:ab)
      %+  sear  (soft passport)
      ;~((glue fas) sym urs:ab)                         ::  XX  [a-z0-9_]{1,15}
    ::
    ++  eval
      |=  a=(trel ,@da bouquet ?(speech [%eval p=@t]))  ^-  statement
      ?.  ?=(%eval -.r.a)  a
      =+  pax=[&1:% &2:% (scot %da p.a) |3:%]
      =-  a(r [%fat tank/- %exp p.r.a])
      p:(mule |.([(sell (slap !>(..zuse) (rain pax p.r.a)))]~))
    ::
    ++  stam
      ^-  $+(json (unit statement))
      %+  cu  eval
      =-  (ot date/di bouquet/(as (ar so)) speech/(of -) ~)
      :~  lin/(ot say/bo txt/so ~) 
          url/(su aurf:urlp)
          eval/so
          ::  exp/(cu |=(a=cord [a ~]) so)
          ::  inv/(ot ship/(su fed:ag) party/(su urs:ab) ~)
      ==
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
    (jobe ship/(jope p) thought/(thot q) ~)
    ::
    ++  jove
      |=  [a=envelope b=delivery]
      %-  jobe  :~
        envelope/(jobe visible/[%b p.a] sender/?~(q.a ~ s/(parn u.q.a)) ~)
        delivery/[%s b]
      ==
    ::
    ++  jope  |=(a=ship (jape +:<a>)) ::[%s (crip +:(scow %p a))])
    ++  joke  |=(a=tank [%s (role (turn (wash 0^80 a) crip))])
    ++  jode  |=(a=time (jone (div (mul (sub a ~1970.1.1) 1.000) ~s1)))
    ++  jome                                            ::  stringify keys
      |*  [a=_cord b=_json]
      |=  c=(map ,_+<.a ,_+<.b)
      (jobe (turn (~(tap by c)) (both a b)))
    ::
    ++  both                                            ::  cons two gates
      |*  [a=_,* b=_,*]
      |=(c=_[+<.a +<.b] [(a -.c) (b +.c)])
    ::
    ++  thot
      |=  thought
      (jobe serial/(jape <p>) audience/(audi q) statement/(stam r) ~)
    ::
    ++  audi  (jome parn jove)
    ++  bouq
      |=  a=bouquet
      a/(turn (~(tap in a)) |=(b=path a/(turn b |=(c=span s/c))))
    ::
    ++  parn
      |=  a=partner  ^-  cord
      ?-  -.a
        %&  (stat p.a)
        %|  %-  crip
            ?-  -.p.a
              %twitter  "{(trip -.p.a)}/{(trip p.p.a)}"
            ==
      ==
    ::
    ++  stat
      |=  a=station  ^-  cord
      (crip "{<p.a>}/{(trip q.a)}")
    ::
    ++  stam
      |=  statement
      (jobe date/(jode p) bouquet/(bouq q) speech/(spec r) ~)
    ::
    ++  spec
      |=  a=speech
      %+  joba  -.a
      ?+  -.a  ~|(stub/-.a !!)
        %lin  (jobe txt/[%s q.a] say/[%b p.a] ~)
        %url  (joba txt/[%s (crip (earf p.a))])
        %exp  (joba txt/[%s p.a])
        %tax  (joba txt/(jape <p.a>))
        %app  (jobe txt/[%s q.a] src/[%s p.a] ~)
        %fat  (jobe tor/(tors p.a) taf/$(a q.a) ~)
        ::  %inv  (jobe ship/(jope p.a) party/[%s q.a] ~)
      ==
    ::
    ++  tors
      |=  a=torso
      %+  joba  -.a
      ?-  -.a
        %text  [%s (role +.a)]
        %tank  [%a (turn +.a joke)]
        %name  (jobe nom/s/p.a mon/$(a q.a) ~)
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
  ++  join  |=([(list telegram) (list telegram)] `(unit mime)`~)
  --
--
