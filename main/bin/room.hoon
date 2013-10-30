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
          --                                            ::
      |%
      ++  fu
        |_  [now=@da lov=loft]
        ++  of
          |_  [her=@p ful=fool]
          ++  abet  ..of(pod.lov (~(put by pod.lov) her ful))
          ++  push
            ^+  [(list zong) _.]
            ?.  (lth (add ~m2 elf.ful) now)  [~ .]
            :-  %-  flip
                %+  skim  (scag (sub num.lov num.ful) meg.lov)
                |=  a=zong
                ?.  ?=(%all -.a)  &
                &(!=(her p.q.a) |(=(%white p.a) =(sec.ful p.a)))
            .(num.ful num.lov)
          --
        ::
        ++  rolf
          ^+  [(list ,[p=@p q=(list zong)]) _.]
          =^  zal  meg.luv
              %+  ~(rib by meg.luv)  *(list ,[p=@p q=(list zong)])
              |=  [her=@p ful=fool]
              ^-  ,[[p=@p q=(list zong)] fool]
              =+  lol=~(push of her ful)
              [[her -.lol] ful.+.lol]
          [zal +]
        ::
        ++  wake
          ^+  [(list zong) _.]
          =^  zal  meg.luv
              %+  ~(rib by meg.luv)  *(list (unit zong))
              |=  [her=@p ful=fool]
              ^ [(unit zong) fool]
              ?.  &(liv.ful (gth now (add ~m2 elf.ful)))  [~ ful]
              [[~ `zong`[%out [her nym.ful]]] ful(liv |)]
          :_  +
          |-  ^-  (list zong)
          ?~(zal ~ =+(laz=$(zal t.zal) ?~(i.zal laz [u.i.zal laz])))
        ::
        ++  yowl
          |=  her=@p
          ^+  ..of
          =+  nog=(~(get by pod) her)
          %~  .  ..of
          :-  her
          ?^  nog  u.
          =+  ^=  gos
              %-  (hard (unit gcos)) 
              .^(%a (scot %p him) %gcos (scot %da now) ~)
          ^-  fool
          :*  0
              (numb him now)
              ^-  sect
                ?.  &(?=(^ gos) ?=(%duke -.u.gos))  %white
                ?.(?=(?(%lord %lady) -.p.u.gos) %white r.p.u.gos)
              &
              now
          ==
        ::
        ++  yelp
          |=  [her=@p zig=zing]
          ^+  [p=(list ,[p=@p q=zong]) q=loft]
          ?>  ?=(%who -.zig)
            :-  :~  [her
          ?-    -.zig
              %all 
             
              %ego
              %out
              %who
            :_  lov
            =+  all
          ==
        --
      --
    ==
=>  %=    .
        -
      :-  lov=loft
      [who=`@p`-< how=`path`->]
    ==
|=  [est=time *]
|=  ~
^-  bowl
:-  ~  :-  ~
:-  ^-  (list slip)
    :~  [/yo [%lq %yo]]
        [/re [%ow ~]]
    ==
|=  [now=@da pax=path nut=note]
=.  est  now
^-  bowl
%-  pogo
:_  ^$
^-  bowl
?+    pax  !!
    /re  ?>(?=(%ow -.nut) [~ ~])
    /yo
  :_  ~
  ?>  ?=(%lq -.nut)
  =+  zig=((hard zing) r.nut)
  =^  vey  lov  abet:(~(yelp fu lov) p.nut now zig)
  |-  ^-  (list gift)
  ?~  vey  ~
  =+  mor=$(vey t.vey)
  ?~  q.i.vey  mor
  [

  (turn vey |=(a=(list ,[p=who zong) [%sq
  [vey ~]
==
