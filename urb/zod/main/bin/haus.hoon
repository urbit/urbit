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
            $:  nym=@t                                  ::  text name
                sec=sect                                ::  banner
                elf=(map room ,[d=@da n=@ud])           ::  ping/mess received
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
            :-
                ^-  (list zong)
                %-  flop
                ^-  (list zong)
                %+  ~(rep by elf.ful)  *(list zong)
                |=  [[r=room *] q=(list zong)]
                =+  lov=(fall (~(get by lov.par) r) *loft)
                =+  num=+:(fall (~(get by elf.ful) r) *[@da @ud])
                %+  weld
                  %+  skim  (scag (sub num.lov num) meg.lov)
                  |=  a=zong
                  ?<  ?=(%who -.a)
                  ?&  (~(has by elf.ful) q.a)
                      ?.  ?=(%all -.a)  &
                      &(!=(her p.s.a) |(=(%white r.a) =(sec.ful r.a)))
                  ==
                q
            ^-  _.
            %=  .
              elf.ful
                %-  ~(tur by elf.ful)
                |=  [r=room [d=@da n=@ud]]
                =+  lov=(fall (~(get by lov.par) r) *loft)
                [d num.lov]
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
              =+  ^=  outs  ^-  (list room)
                  %+  ~(rep by elf.ful)  *(list room)
                  |=  [[r=room [d=@da n=@ud]] q=(list room)]
                  ?.  (gth now (add ~m2 d))  q
                  [r q]
              =+  ^=  outmes  ^-  (list ,[room (list zong)])
                  %+  turn  outs
                  |=  r=room
                  [r [%out now r [her nym.ful]]~]
              =+  ^=  f
              |=  [p=room q=(map room ,[@da @ud])]  (~(del by q) p)
              =+  ^=  newelf  ^-  (map room ,[@da @ud])
                  %+  roll  outs  f(q elf.ful)
              [(~(gas by fug) outmes) [her ful(elf newelf)]]
          (tilt zew)
        ::
        ++  yelp
          |=  [our=@p her=@p zig=zing]
          ^-  [chub part]
          ?:  ?=(%who -.zig)
            :_  par
            :_  ~
            :-  her
              ^-  (list zong)
              :_  ~  
              :^  %who  now  p.zig
              =+  ^=  all  ^-  (map room (list user))
                  =<  -
                  %+  ~(rib by pod.par)  *(map room (list user))
                  |=  [[her=@p ful=fool] fug=(map room (list user))]
                  ^-  [(map room (list user)) [@p fool]]
                  :_  [her ful]
                  %+  ~(rep by elf.ful)  fug
                  |=  [[r=room *] q=(map room (list user))]
                  %+  ~(put by q)  r
                  [[her nym.ful] (fall (~(get by fug) r) ~)]
              ?~  q.zig
                all
              %-  ~(gas by *(map room (list user)))
              (turn u.q.zig |=(r=room [r (fall (~(get by all) r) ~)]))
          =+  pof=(yowl our her)
          =+  m=*(map room (list zong))
          =^  zew  +>.$
              ?-    -.zig
                  %all
                :_  abet:pof
                %-  ~(gas by m)
                ?.  (~(has by elf.ful:pof) p.zig)  ~
                :_  ~
                :-  p.zig
                ~[[%all now p.zig ?:(q.zig %white sec.ful.pof) call:pof r.zig]]
              ::
                  %ego
                =+  num=n:(fall (~(get by elf.ful.pof) p.zig) [_@ n=_@ud])
                :_  abet:pof(elf.ful (~(put by elf.ful.pof) p.zig [now num]))
                %-  ~(gas by m)
                =+  liv=(~(has by elf.ful.pof) p.zig)
                =.  elf.ful.pof  (~(put by elf.ful.pof) p.zig [now num])
                ?:  liv  ~
                [p.zig ~[[%new now p.zig call:pof]]]~
              ::
                  %out
                  :-  ~  abet:pof
                  ::  :_  abet:pof(elf.ful (~(del by elf.ful.pof) p.zig))
                  ::  %-  ~(gas by m)
                  ::  ?.  liv.ful.pof  ~
                  ::  (turn roo.ful:pof |=(c=room [c ~[[%out now c call:pof]]]))
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
              :*  ^-  @t
                  =+  yow=(scot %p her)
                  =+  woy=((hard ,@t) .^(%a (scot %p our) %name (scot %da now) (scot %p her) ~))
                  ?:  =(%$ woy)  yow
                  (cat 3 yow (cat 3 ' ' woy))
                  ^-  sect
                    ?.  &(?=(^ gos) ?=(%duke -.u.gos))  %white
                    ?:  ?=(?(%lord %lady) -.p.u.gos)
                      r.p.p.u.gos
                    ?:(?=(%punk -.p.u.gos) p.p.u.gos %white)
                  *(map room ,[@da @ud])
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
      %bo
        ?>  ?=(%lq -.nut)
        =+  n=((soft zing) r.nut)
        ?~  n
          ~&  %haus-zing-fail  [~ +>+]
        (talk p.nut u.n)
    ==
  [(cede cub) ~ hope ..$]
--
