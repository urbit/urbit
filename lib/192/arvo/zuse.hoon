!:
::          %zuse, main loop.   This file is in the public domain.
::
=<  =+  ^=  duc                                         ::  external entrance
        |=  cud=*  ^-  *
        ?:  =(0 cud)
          ..peek
        ?>(?=(@ cud) (make cud))
    ^-  *
    [8 [1 duc] [9 2 [[0 4] [[0 3] [0 11]]]]]
=<  |%
    ++  come
      |=  *
      ^+  +>
      !!
    ++  load
      |=  *
      ^+  +>
      !!
    ++  keep                                            ::  wakeup delay
      |=  *  ^-  (unit ,@da)
      =>  .(+< ((hard ,[now=@da hap=path]) +<))
      (~(doos is now) hap)
    ::
    ++  peek                                            ::  external inspect
      |=  *
      ^-  (unit)
      =>  .(+< ((hard ,[our=@p now=@da hap=path]) +<))
      ::  ~&  [%zuse-peek hap]
      ?~  hap  ~
      ((~(beck is now) [~ our]) hap)
    ::
    ++  poke                                            ::  external apply
      |=  *
      ^-  [p=(list ovum) q=_+>]
      =>  .(+< ((hard ,[now=@da ovo=ovum]) +<))
      ::  ~&  [%zuse-poke ovo]
      =^  zef  fan
        (~(hurl is now) ovo)
      [zef +>.$]
    ::
    ++  wish                                            ::  external compute
      |=  *
      ^-  *
      =>  .(+< ((hard ,[txt=@]) +<))
      =+  gen=(ream txt)
      q:(slap `vase`!>(+>) (ream txt))
    --
=|  eny=@
=+  ^=  fan  ^-  (list ,[p=@tas q=vane])
    :~  a/ames
        b/bede
        c/cary
        d/dill
        e/eyre
    ==
|%
++  is                                                  ::  operate in time
  |_  now=@da 
  ++  beck  
    |=  owr=(unit flag)
    |+  hap=*
    ^-  (unit)
    =>  .(hap ((hard path) hap))
    ?~  owr  ~
    ?.  ?=([@ @ @ *] hap)  ~
    =+  :*  hyr=(slay i.hap) 
            ved=(slay i.t.hap) 
            fal=(slay i.t.t.hap)
            tyl=t.t.t.hap
        ==
    ?.  ?=([~ %% %tas @] hyr)  ~
    ?.  ?=([~ %% %p @] fal)  ~
    =+  his=`@p`q.p.u.fal
    =>  .(owr [~ u=his])                                ::  XX no!
    =+  dis=(end 3 1 q.p.u.hyr)
    =+  rem=(rsh 3 1 q.p.u.hyr)
    |-  ^-  (unit)
    ?~  fan  ~
    ?.  =(dis p.i.fan)  $(fan t.fan)
    %-  scry:(q.i.fan now (shax now) ..^$)
    [u.owr rem his u.ved tyl]
  ::
  ++  dink                                              ::  vane by char
    |=  din=@tas  ^-  vane
    ?~(fan !! ?:(=(din p.i.fan) q.i.fan $(fan t.fan)))
  ::
  ++  dint                                              ::  name to vane
    |=  hap=path  ^-  @tas
    ?+  hap  !!
      [@ %ames *]  %a
      [@ %sync *]  %c
      [@ %term *]  %d
      [@ %http *]  %e
    ==
  ::
  ++  doos
    |=  hap=path
    ^-  (unit ,@da)
    =+  van=`vane`(dink (dint hap))
    =+  vug=(van now 0 (beck ~))
    =+  don=(doze:vug [hap ~])
    don
  ::
  ++  hurl                                              ::  start loop
    |=  ovo=ovum
    ^-  [p=(list ovum) q=(list ,[p=@tas q=vane])]
    =+  des=(dint p.ovo)
    (kick [[~ [[des ~] p.ovo ~] q.ovo] ~])
  ::
  ++  kick                                              ::  complete loop
    |=  mor=(list move)
    =|  ova=(list ovum)
    |-  ^-  [p=(list ovum) q=(list ,[p=@tas q=vane])]
    ?~  mor
      [(flop ova) fan]
    ::  ~&  [%kick-move q.i.mor -.r.i.mor]
    ?>  ?=(^ q.i.mor)
    ?~  t.q.i.mor
      $(mor t.mor, ova [[i.q.i.mor r.i.mor] ova])
    ?>  ?=(^ i.q.i.mor)
    =-  $(mor (weld p.nyx t.mor), fan q.nyx)
    ^=  nyx
    =+  naf=fan
    |-  ^-  [p=(list move) q=_fan]
    ?~  naf  [~ ~]
    ?.  =(i.i.q.i.mor p.i.naf)
      =+  tuh=$(naf t.naf)
      [p.tuh [i.naf q.tuh]]
    =+  ^=  yub
        %-  beat:(q.i.naf now (shax now) (beck p.i.mor))
        [p.i.mor t.i.q.i.mor t.q.i.mor r.i.mor]
    [p.yub [[p.i.naf q.yub] t.naf]]
  --
--
