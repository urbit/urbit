::
::::  /hoon/talk-telegrams/mar
  ::
/?  314
/-  talk
/+  talk
!:
=+  talk
|_  gam=(list telegram)
::
++  grab-work-duty  =>  [jo work-stuff]
  |^  dute
  ++  as
    :: |*(a=fist (cu sa (ar a)))  ::  XX  types
    |*  a=fist 
    %-  cu  :_  (ar a)
    ~(gas in *(set ,_(need *a)))
  ++  ot
    |*  a=(pole ,[@tas fist])
    |=  b=json
    %.  ((^ot a) b)
    %-  slog
    ?+  b  ~
        [%o *] 
      %+  murn  `(list ,[@tas fist])`a
      |=  [c=term d=fist]  ^-  (unit tank)
      =+  (~(get by p.b) c)
      ?~  -  (some >[c (turn (~(tap by p.b)) head)]<)
      =+  (d u)
      ?~  -  (some >[c u]<)
      ~
    ==
  ++  of
    |*  a=(pole ,[@tas fist])
    |=  b=json
    %.  ((of:jo a) b)
    %-  slog
    ?+  b  ~
        [%o *] 
      %+  murn  `(list ,[@tas fist])`a
      |=  [c=term d=fist]  ^-  (unit tank)
      =+  (~(get by p.b) c)
      ?~  -  ~
      =+  (d u)
      ?~  -  (some >[c u]<)
      ~
    ==
  ++  id  (ci (slat %uv) so)
  ++  ship  (su fed:ag)
  ++  dute
    %-  of  :~
      create/task  change/(ot id/id meat/uppd ~)
      archive/id   update/(ot id/id version/ni her/(su fed:ag) meat/uppd ~)
    ==
  ++  task
    %-  ot  :~
      id/id           'date_created'^di
      version/ni      'date_modified'^di
      creator/ship    doer/(mu ship)
      tags/(as so)    'date_due'^(mu di)
      done/(mu di)    title/so
      description/so  discussion/(ar (ot date/di ship/ship body/so ~))
    ==
  ++  audi  (as stan)
  ++  stan  (su ;~((glue fas) ;~(pfix sig fed:ag) urs:ab))
  ++  uppd
    %-  of  :~
      set-doer/(mu (su fed:ag))
      set-date-due/(mu di)
      set-tags/(as so)
      set-title/so
      set-description/so
      set-done/bo
      add-comment/(ot ship/(su fed:ag) com/so ~)
    ==
  --
++  grow-work-duty
  =>  work-stuff
  =+  jope=|=(a=ship [%s (rsh 3 1 (scot %p a))])
  =+  jove=|=(a=@uvI [%s (scot %uv a)])
  =<  |=  duty
      %+  joba  +<-
      ?-  +<-
        %create   (task tax)
        %archive  (jove id)
        %change   (jobe id/(jove id) meat/(flesh meat) ~)
        %update
          %-  jobe  :~ 
            id/(jove id)
            version/(jone version)
            her/(jope her)
            meat/(flesh meat)
          ==
      ==
  |%
  ++  tags
    |=  a=(set ,@t)
    [%a (turn (sort (~(tap in a)) aor) |=(b=cord s/b))]
  ::
  ++  task
    |=  ^task
    %-  jobe  :~  id/[%s (scot %uv id)]
                tags/(^tags tags)
                doer/?~(doer ~ (jope u.doer))
               title/[%s title]
             creator/(jope creator)
             version/(jone version)
      'date_created'^(jode date-created)
     'date_modified'^(jode date-modified)
         description/[%s description]
      =<  discussion/[%a (turn discussion .)]
      |=(comment (jobe date/(jode date) ship/(jope ship) body/[%s body] ~))
          'date_due'^?~(date-due ~ (jode u.date-due))
                done/?~(done ~ (jode u.done))
    ==
  ++  flesh  
    |=  ^flesh
    %+  joba  +<-
    ?-  +<-
      %set-doer         ?~(her ~ (jope u.her))
      %set-date-due     ?~(wen ~ (jode u.wen))
      %set-tags         (tags tag)
      %set-title        [%s til]
      %set-description  [%s des]
      %set-done         [%b don]
      %add-comment      (jobe ship/(jope who) com/[%s com] ~)
    ==
  --
++  grab
  |%
  ++  noun  (list telegram)
  ++  mime  |=(^mime (json (rash q.q apex:poja)))
  ++  json
    =>  [jo ..telegram dute=grab-work-duty]
    |=  a=json  ^-  (list telegram)
    =-  (need ((ar (ot ship/(su fed:ag) thought/thot ~)) a))
    |%
    ++  of
      |*  a=(pole ,[@tas fist])
      |=  b=json
      %.  ((of:jo a) b)
      %-  slog
      ?+  b  ~
          [%o *] 
        %+  murn  `(list ,[@tas fist])`a
        |=  [c=term d=fist]  ^-  (unit tank)
        =+  (~(get by p.b) c)
        ?~  -  ~
        =+  (d u)
        ?~  -  (some >[c u]<)
        ~
      ==
    ++  op                                              ::  parse keys of map
      |*  [fel=rule wit=fist]
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
    ++  ke                                              ::  callbacks
      |*  [gar=* sef=_|.(fist)]
      |=  jon=json
      ^-  (unit ,_gar)
      =-  ~!  gar  ~!  (need -)  -
      ((sef) jon)
    ::
    ++  lake  |*(a=_,* $+(json (unit a)))
    ++  head-rush
      |*  a=rule
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
      %+  pick
        ;~((glue fas) ;~(pfix sig fed:ag) urs:ab)
      %+  sear  (soft passport)
      ;~((glue fas) sym urs:ab)                         ::  XX  [a-z0-9_]{1,15}
    ::
    ++  stam  (ot date/di bouquet/(as (ar so)) speech/spec ~)
    ++  spec
      %+  ke  *speech  |.  ~+
      %-  of  :~
        lin/(ot say/bo txt/so ~) 
        url/(ot txt/(su aurf:urlp) ~)
        exp/(ot txt/so ~)
        tax/(ot xat/dute ~)
        app/(ot txt/so src/so ~)
        fat/(ot tor/tors taf/spec ~)
        ext/(ot nom/so txe/blob ~)
        non/ul
        ::  inv/(ot ship/(su fed:ag) party/(su urs:ab) ~)
      ==
    ++  tors  
      %+  ke  *torso  |.  ~+
      %-  of  :~
        name/(ot nom/so mon/tors ~) 
        text/(cu lore so)
        tank/(ot dat/(cu (hard (list tank)) blob) ~)
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
        %url  (joba txt/(jape (earf p.a)))
        %exp  (joba txt/[%s p.a])
        %tax  (jobe txt/(jape (rend-work-duty p.a)) xat/(grow-work-duty p.a) ~)
        %app  (jobe txt/[%s q.a] src/[%s p.a] ~)
        %fat  (jobe tor/(tors p.a) taf/$(a q.a) ~)
        %ext  (jobe nom/[%s p.a] txe/(jape (sifo (jam +.a))) ~)
        %non  ~
        ::  %inv  (jobe ship/(jope p.a) party/[%s q.a] ~)
      ==
    ::
    ++  tors
      |=  a=torso
      %+  joba  -.a
      ?-  -.a
        %text  [%s (role +.a)]
        %tank  (jobe txt/[%a (turn +.a joke)] dat/(jape (sifo (jam +.a))) ~)
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
