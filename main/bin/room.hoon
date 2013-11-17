!:
::  /=main=/bin/room/hoon
::
=>  %=    .
        +
      =>  +
      =>  ^/===/lib/pony
      =>  ^/===/lib/chat
      =>  |%  
          ++  fool
            $:  num=@ud                                 ::  number received
                nym=@t                                  ::  text name
                sec=sect                                ::  banner
                liv=?                                   ::  officially live
                elf=@da                                 ::  last ping
            ==                                          ::
          ++  loft                                      ::
            $:  num=@ud                                 ::  (lent meg)
                meg=(list zong)                         ::  messages backward
                pod=(map ,@p fool)                      ::  individual status
            ==                                          ::
          ++  chub  (list ,[p=@p q=(list zong)])        ::  delivery report
          --                                            ::
      |%
      ++  fu
        |_  [now=@da lov=loft]
        ++  of
          |_  [her=@p ful=fool]
          ++  abet  ..of(pod.lov (~(put by pod.lov) her ful))
          ++  call  [her nym.ful]
          ++  push
            ^-  [(list zong) _.]
            ?.  liv.ful  [~ .]
            :-  %-  flop
                %+  skim  (scag (sub num.lov num.ful) meg.lov)
                |=  a=zong
                ?.  ?=(%all -.a)  &
                &(!=(her p.q.a) |(=(%white p.a) =(sec.ful p.a)))
            .(num.ful num.lov)
          --
        ::
        ++  rolf
          ^-  [chub _.]
          =^  zal  pod.lov
              %+  ~(rib by pod.lov)  *chub
              |=  [[her=@p ful=fool] fug=chub]
              ^-  [chub [@p fool]]
              =+  lol=~(push of her ful)
              [[[her -.lol] fug] [her ful.+.lol]]
          [zal +]
        ::
        ++  tell
          |=  zog=(list zong)
          ^+  +>
          %=  +>
            num.lov  (add (lent zog) num.lov)
            meg.lov  (weld (flop zog) meg.lov) 
          ==
        ::
        ++  tilt
          |=  zew=(list zong)
          =.  +>.$  (tell zew)
          =^  yeq  +>.$  rolf
          [yeq lov]
        ::
        ++  wake
          ^-  [chub loft]
          =^  zew  pod.lov
              %+  ~(rib by pod.lov)  *(list zong)
              |=  [[her=@p ful=fool] fug=(list zong)]
              ^-  [(list zong) [@p fool]]
              ?.  &(liv.ful (gth now (add ~m2 elf.ful)))  [fug her ful]
              [[`zong`[%out [her nym.ful]] fug] [her ful(liv |)]]
          (tilt zew) 
        ::
        ++  yelp
          |=  [her=@p zig=zing]
          ^-  [chub loft]
          ?:  ?=(%who -.zig)
            :_  lov
            :~  :-  her 
                :~  :-  %who
                    ^-  (list user)
                    =<  -
                    %+  ~(rib by pod.lov)  *(list user)
                    |=  [[her=@p ful=fool] fug=(list user)]
                    ^-  [(list user) [@p fool]]
                    :_  [her ful]
                    ?.  liv.ful  fug
                    [[her nym.ful] fug]
                ==
            ==
          =+  pof=(yowl her)
          =^  zew  +>.$
              ?-    -.zig
                  %all 
                :_  abet:pof
                [[%all ?:(p.zig %white sec.ful.pof) call:pof q.zig] ~]
              ::
                  %ego
                :_  abet:pof(liv.ful &, elf.ful now)
                =.  elf.ful.pof  now
                ?:(liv.ful.pof ~ [[%new call:pof] ~])
              ::
                  %out
                :_  abet:pof(liv.ful |)
                ?.(liv.ful.pof ~ [[%out call:pof] ~])
              ==
          (tilt zew)
        ::
        ++  yowl
          |=  her=@p
          ^+  of
          =+  nog=(~(get by pod.lov) her)
          =+  ^=  ful  ^-  fool
              ?^  nog  u.nog
              =+  ^=  gos
                  %-  (hard (unit gcos)) 
                  .^(%a (scot %p her) %gcos (scot %da now) ~)
              ^-  fool
              :*  0
                  (numb her now)
                  ^-  sect
                    ?.  &(?=(^ gos) ?=(%duke -.u.gos))  %white
                    ?:  ?=(?(%lord %lady) -.p.u.gos) 
                      r.p.p.u.gos 
                    ?:(?=(%punk -.p.u.gos) p.p.u.gos %white)
                  |
                  now
              ==
          ~(. of her ful)
        --
      --
    ==
=>  %=    .
        -
      :-  :*  lov=*loft
              wak=@da
          ==
      [who=`@p`-< how=`path`->]
    ==
|=  [est=time *]
|=  ~
=.  wak  est
=<  `bowl`[~ ~ hope vent]
|%
++  cede
  |=  cub=chub
  ^-  (list gift)
  ?~  cub  ~
  =+  mor=$(cub t.cub)
  ?~(q.i.cub mor [[%sq p.i.cub %oy /re q.i.cub] mor])
::
++  hope
  ^-  (list slip)
  :~  [/yo [%lq %yo]]
      [/wa [%wa wak]]
      [/re [%ow ~]]
  ==
::
++  take
  |-  ^-  [chub _+]
  =^  cub  lov  ~(wake fu est lov)
  [cub +.$]
::
++  talk
  |=  [her=@p zig=zing]
  ^-  [chub _+>]
  =^  cub  lov  (~(yelp fu est lov) her zig)
  [cub +>.$]
::
++  vent
  |=  [now=@da pax=path nut=note]
  ^-  bowl
  =.  est  now
  =^  cub  +>
    ?+  pax  !!
      /re  ?>(?=(%ow -.nut) [~ +>])
      /wa  ?>(?=(%wa -.nut) take(wak (add ~m1 est)))
      /yo  ?>(?=(%lq -.nut) (talk p.nut ((hard zing) r.nut)))
    ==
  [(cede cub) ~ hope ..$]
--
