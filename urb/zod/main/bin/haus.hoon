!:
::  /=main=/bin/haus/hoon
::
=>  %=    .
        +
      =>  +
      =>  ^/===/lib/pony
      =>  ^/===/lib/chat
      =>  |%
          ++  fool
            $:  num=(map room ,@ud)                     ::  number received
                nym=@t                                  ::  text name
                sec=sect                                ::  banner
                liv=?                                   ::  officially live
                elf=@da                                 ::  last ping
                roo=(list room)                         ::  room memberships
            ==                                          ::
          ++  loft                                      ::  room log
            $:  num=@ud                                 ::  (lent meg)
                meg=(list zong)                         ::  messages backward
            ==                                          ::
          ++  part                                      ::  party
            $:  lov=(map room loft)                     ::  rooms
                pod=(map ,@p fool)                      ::  individual status
            ==                                          ::
          ++  chub  (list ,[p=@p q=(list zong)])        ::  delivery report
          --                                            ::
      |%
      ++  fu
        |_  [now=@da par=part]
        ++  of
          |_  [her=@p ful=fool]
          ++  abet  ..of(pod.par (~(put by pod.par) her ful))
          ++  call  [her nym.ful]
          ++  push
            ^-  [(list zong) _.]
            ?.  liv.ful  [~ .]
            ^-  [(list zong) _.]
            :-
                ^-  (list zong)
                %-  flop
                %-  ^zing
                %+  turn  roo.ful
                |=  roo=room
                =+  lov=(~(get by lov.par) roo)
                =+  lov=?~(lov *loft u.lov)
                =+  num=(~(get by num.ful) roo)
                =+  num=?~(num 0 u.num)
                %+  skim  (scag (sub num.lov num) meg.lov)
                |=  a=zong
                ?<  ?=(%who -.a)
                ?&  (lien roo.ful |=(* =(q.a +<)))
                    ?.  ?=(%all -.a)  &
                    &(!=(her p.s.a) |(=(%white r.a) =(sec.ful r.a)))
                ==
            ^-  _.
            %=  .
              num.ful
                %-  ~(gas by num.ful)
                %+  turn  roo.ful
                |=  roo=room
                =+  lov=(~(get by lov.par) roo)
                =+  lov=?~(lov *loft u.lov)
                [roo num.lov]
            ==
          --
        ::
        ++  rolf
          ^-  [chub _.]
          =^  zal  pod.par
              %+  ~(rib by pod.par)  *chub
              |=  [[her=@p ful=fool] fug=chub]
              ^-  [chub [@p fool]]
              =+  lol=~(push of her ful)
              [[[her -.lol] fug] [her ful.+.lol]]
          [zal +]
        ::
        ++  tell
          |=  zog=(map room (list zong))
          %=  +>
            lov.par
              ^-  (map room loft)
              =+  ^=  m  ^-  (map room loft)
                  %+  ~(rep by lov.par)
                    *(map room loft)
                  |=  [p=[p=room q=loft] q=(map room loft)]
                  =+  g=(~(get by zog) p.p)
                  =+  g=?~(g ~ u.g)
                  %+  ~(put by q)  p.p
                  [(add (lent g) num.q.p) (weld (flop g) meg.q.p)]
              %-  ~(gas by m)
              %+  turn
                %+  skip  (~(tap by zog) ~)
                |=  [p=room q=(list zong)]
                (~(has by lov.par) p)
              |=  [p=room q=(list zong)]
              ^-  [room loft]
              [p [0 q]]
          ==
        ::
        ++  tilt
          |=  zew=(map room (list zong))
          ^-  [chub part]
          =.  +>.$  (tell zew)
          =^  yeq  +>.$  rolf
          [yeq par]
        ::
        ++  wake
          ^-  [chub part]
          =^  zew  pod.par
              %+  ~(rib by pod.par)  *(map room (list zong))
              |=  [[her=@p ful=fool] fug=(map room (list zong))]
              ^-  [(map room (list zong)) [@p fool]]
              ?.  &(liv.ful (gth now (add ~m2 elf.ful)))  [fug her ful]
              =+  ^=  outs  ^-  (list ,[room (list zong)])
              (turn roo.ful |=(c=room [c ~[[%out now c [her nym.ful]]]]))
              [(~(gas by fug) outs) [her ful(liv |)]]
          (tilt zew)
        ::
        ++  yelp
          |=  [our=@p her=@p zig=zing]
          ^-  [chub part]
          ?:  ?=(%who -.zig)
            :_  par
            :~  :-  her
                :~  :+  %who
                      now
                    =+  ^=  all
                    ^-  (map room (list user))
                    =<  -
                    %+  ~(rib by pod.par)  *(map room (list user))
                    |=  [[her=@p ful=fool] fug=(map room (list user))]
                    ^-  [(map room (list user)) [@p fool]]
                    :_  [her ful]
                    ?.  liv.ful  fug
                    %-  ~(gas by fug)
                    %+  turn  roo.ful
                    |=  r=room
                    =+  c=(~(get by fug) r)
                    [r [[her nym.ful] ?~(c ~ u.c)]]
                    ?~  p.zig
                      all
                    %-  ~(gas by *(map room (list user)))
                    (turn u.p.zig |=(r=room [r (fall (~(get by all) r) ~)]))
                ==
            ==
          =+  pof=(yowl our her)
          =+  m=*(map room (list zong))
          =^  zew  +>.$
              ?-    -.zig
                  %all
                :_  abet:pof
                %-  ~(gas by m)
                ?.  (lien roo.ful:pof |=(* =(p.zig +<)))  ~
                :_  ~
                :-  p.zig
                ~[[%all now p.zig ?:(q.zig %white sec.ful.pof) call:pof r.zig]]
              ::
                  %ego
                :_  abet:pof(liv.ful &, elf.ful now)
                %-  ~(gas by m)
                =.  elf.ful.pof  now
                ?:  liv.ful.pof  ~
                (turn roo.ful:pof |=(c=room [c ~[[%new now c call:pof]]]))
              ::
                  %out
                :_  abet:pof(liv.ful |)
                %-  ~(gas by m)
                ?.  liv.ful.pof  ~
                (turn roo.ful:pof |=(c=room [c ~[[%out now c call:pof]]]))
              ::
                  %lus
                =+  ^=  oldroo
                    %+  skip  roo.ful:pof
                    |=(a=room (lien p.zig |=(a=room =(a ^a))))
                :_  abet:pof(roo.ful (weld p.zig oldroo))
                %-  ~(gas by m)
                %+  turn
                (skip p.zig |=(a=room (lien roo.ful:pof |=(a=room =(a ^a)))))
                |=(r=room [r ~[[%new now r call:pof]]])
              ::
                  %hep
                =+  ^=  oldroo
                    %+  skip  roo.ful:pof
                    |=(a=room (lien p.zig |=(a=room =(a ^a))))
                :_  abet:pof(roo.ful oldroo)
                %-  ~(gas by m)
                %+  turn
                (skip p.zig |=(a=room (levy roo.ful:pof |=(a=room =(a ^a)))))
                |=(r=room [r ~[[%out now r call:pof]]])
              ==
          (tilt zew)
        ::
        ++  yowl
          |=  [our=@p her=@p]
          ^+  of
          =+  nog=(~(get by pod.par) her)
          =+  ^=  ful  ^-  fool
              ?^  nog  u.nog
              =+  ^=  gos
                  %-  (hard (unit gcos))
                  .^(%a (scot %p our) %gcos (scot %da now) (scot %p her) ~)
              ^-  fool
              :*  *(map room ,@ud)
                  ^-  @t
                  =+  yow=(scot %p her)
                  =+  woy=((hard ,@t) .^(%a (scot %p our) %name (scot %da now) (scot %p her) ~))
                  ?:  =(%$ woy)  yow
                  (cat 3 yow (cat 3 ' ' woy))
                  ^-  sect
                    ?.  &(?=(^ gos) ?=(%duke -.u.gos))  %white
                    ?:  ?=(?(%lord %lady) -.p.u.gos)
                      r.p.p.u.gos
                    ?:(?=(%punk -.p.u.gos) p.p.u.gos %white)
                  |
                  now
                  ~[coci]
              ==
          ~(. of her ful)
        --
      --
    ==
=>  %=    .
        -
      :-  :*  par=[(mo `(list ,[room loft])`~[[coci *loft]]) *(map ,@p fool)]
              wak=_@da
          ==
      [who=`@p`-< how=`path`->]
    ==
|=  [est=time *]
|=  ~
=.  wak  est
=<  `bowl`[~ ~ hope vent]
|%
++  care
  |=  [you=@p meg=(list zong) mor=(list gift)]
  =+  len=(lent meg)
  |-  ^-  (list gift)
  ?:  =(0 len)  mor
  =+  hob=(min len 256)
  :-  [%sq you %ob /re (scag hob meg)]
  $(meg (slag hob meg), len (sub len hob))
::
++  cede
  |=  cub=chub
  ^-  (list gift)
  ?~(cub ~ (care p.i.cub q.i.cub $(cub t.cub)))
::
++  hope
  ^-  (list slip)
  :~  [/bo [%lq %bo]]
      [/wa [%wa wak]]
      [/re [%ow ~]]
  ==
::
++  take
  |-  ^-  [chub _+]
  =^  cub  par  ~(wake fu est par)
  [cub +.$]
::
++  talk
  |=  [her=@p zig=zing]
  ^-  [chub _+>]
  =^  cub  par  (~(yelp fu est par) who her zig)
  [cub +>.$]
::
++  vent
  |=  [now=@da pax=path nut=note]
  ^-  bowl
  =.  est  now
  =^  cub  +>
    ?+  -.pax  !!
      %re  ?>(?=(%ow -.nut) [~ +>])
      %wa  ?>(?=(%wa -.nut) take(wak (add ~m1 est)))
      %bo  ?>(?=(%lq -.nut) (talk p.nut ((hard zing) r.nut)))
    ==
  [(cede cub) ~ hope ..$]
--
