:-  %say
|=  $:  {now/@da eny/@uvJ bec/beak}
        *
    ==
:-  %noun
=>
|%
::  fundamental hierarchical node  XX s/b +arch
::
++  node
  =<  |$  [item]
      [fil=(unit item) dir=(map @ta $)]
  ::
  |%
  ++  de
    =|  fat=(node)
    |@
    ::
    ++  get-node
      |=  pax=path
      ^+  fat
      ?~  pax  fat
      =/  kid  (~(get by dir.fat) i.pax)
      ?~  kid  [~ ~]
      $(fat u.kid, pax t.pax)
    ::
    ++  get
      |=  pax=path
      ^+  fil.fat
      fil:(get-node pax)
    ::
    ++  got
      |=  pax=path
      (need (get pax))
    ::
    ++  has
      |=  pax=path
      ^-  ?
      !=(~ (get pax))
    ::
    ++  put
      |*  [pax=path dat=*]
      =>  .(dat `_?>(?=(^ fil.fat) u.fil.fat)`dat)
      ^+  fat
      ?~  pax  fat(fil `dat)
      =/  kid  (~(get by dir.fat) i.pax)
      =/  new  (fall kid fat(fil ~, dir ~))
      fat(dir (~(put by dir.fat) i.pax $(fat new, pax t.pax)))
    ::
    ++  gas
      |=  lit=(list (pair path _?>(?=(^ fil.fat) u.fil.fat)))
      ^+  fat
      ?~  lit  fat
      $(fat (put p.i.lit q.i.lit), lit t.lit)
    ::
    ++  tap
      =|  pax=path
      =|  out=(list (pair path _?>(?=(^ fil.fat) u.fil.fat)))
      |-  ^+   out
      =?  out  ?=(^ fil.fat)  :_(out [pax u.fil.fat])
      =/  dir  ~(tap by dir.fat)
      |-  ^+   out
      ?~  dir  out
      %=  $
        dir  t.dir
        out  ^$(pax (weld pax /[p.i.dir]), fat q.i.dir)
      ==
    -- :: de
  -- :: node
--
=>
|%
+$  hoof  @t                                            ::  hoon source file
+$  news
    $:  ::  use: non-system files
        ::  new: new set
        ::  sys: installs + replacements
        ::
        use=(map path plum)
        new=(set path)
        sys=(map path plum)
    ==
+$  plum  (cask)
+$  seed  [hoon=(unit hoof) arvo=hoof]
+$  sprig
  $:  lull=(unit hoof)
      zuse=(unit hoof)
      vane=(list (pair term hoof))
  ==
::
++  wilt                                              ::  deep file as source
  |=  =plum
  ^-  hoof
  ?>(?=([%hoon @t] plum) q.plum)
::
++  adapt
  =*  de  de:node
  |_  fat=(node plum)
  ::
  ::  +group: collate changes
  ::
  ++  group
    |=  fal=(list (pair path plum))
    =|  del=news
    |-  ^+  del
    ?~  fal
      ~?  !=(~ use.del)
        [%what-use ~(tap in ~(key by use.del))]
      ~?  !=(~ new.del)
        [%what-new ~(tap in new.del)]
      ~?  !=(~ sys.del)
        [%what-sys ~(tap in ~(key by sys.del))]
      del
    ::
    =*  pax  p.i.fal
    =*  dat  q.i.fal
    =/  old  (~(get de fat) pax)
    ::
    ::  ignore unchanged data
    ::
    ?:  =(old `dat)
      $(fal t.fal)
    ::
    ::  classify as user, system install or replacement
    ::
    ?.  ?=([%sys *] pax)
      =.  use.del  (~(put by use.del) pax dat)
      $(fal t.fal)
    =?  new.del  ?=(~ old)
      (~(put in new.del) pax)
    =.  sys.del   (~(put by sys.del) pax dat)
    $(fal t.fal)
  ::  +usurp: consider self-replacement
  ::
  ++  usurp
    |=  del=news
    ^-  (unit seed)
    ::
    ::  when we get new hoon and arvo system files,
    ::  we assume they match what's running now
    ::
    =*  adopt-new
      |=(=path ?:((~(has in new.del) path) ~ (~(get by sys.del) path)))
    ::
    =/  hun  (adopt-new /sys/hoon)
    =/  arv  (adopt-new /sys/arvo)
    ?~  hun
      ?~  arv  ~
      ::
      ::  light reboot, arvo only
      ::
      ~&  %light-reboot
      ``(wilt u.arv)
    ::
    ::  heavy reboot, hoon and arvo
    ::
    ~&  %heavy-reboot
    :+  ~
      `(wilt u.hun)
    (wilt ?^(arv u.arv (~(got de fat) /sys/arvo)))
  ::  +adorn: augment capabilities
  ::
  ++  adorn
    |=  [del=news force=?]
    ^-  sprig
    ::  lull: shared structures
    ::
    =/  lull
      ?^  lul=(~(get by sys.del) /sys/lull)
        `(wilt u.lul)
      ?.(force ~ `(wilt (~(got de fat) /sys/lull)))
    ::
    ::  zuse: shared library
    ::
    ::    %lull is the subject of %zuse; force %zuse if we have a new %lull
    ::
    =/  zuse
      =.  force  ?=(^ lull)
      ?^  zus=(~(get by sys.del) /sys/zuse)
        `(wilt u.zus)
      ?.(force ~ `(wilt (~(got de fat) /sys/zuse)))
    ::
    :+  lull
      zuse
    ::
    ::  kernel modules
    ::
    ::    %zuse is the subject of the vanes; force all if we have a new %zuse
    ::
    =/  current=(list (pair term hoof))
      =.  force  ?=(^ zuse)
      ?.  force  ~
      %+  turn
        ~(tap by dir:(~(get-node de fat) /sys/vane))
      |=([name=@ta (node plum)] [`@tas`name (wilt (need fil))])
    ::
    =/  updates=(list (pair term hoof))
      %+  turn
        %+  skim
          ~(tap by sys.del)
        |=([=path *] ?=([%sys %vane @tas ~] path))
      |=  [=path =plum]
      ?>  ?=([%sys %vane @tas ~] path)
      =*  name  i.t.t.path
      ?>  ((sane %tas) name)
      [`@tas`name (wilt plum)]
    ::
    %+  sort
      %~  tap  by
      %-  ~(gas by *(map term hoof))
      (weld current updates)
    |=([[a=@tas *] [b=@tas *]] (aor a b))
  -- :: adapt
::
++  what
  |=  fat=(node plum)
  ::
  |=  fal=(list (pair path plum))
  =/  del  (~(group adapt fat) fal)
  =/  but  (~(usurp adapt fat) del)
  =/  job  (~(adorn adapt fat) del force=?=(^ but))
  ::
  ::  adopt system changes
  ::
  =.  fat  (~(gas de:node fat) ~(tap by sys.del))
  ::
  ::  just adopt user changes, which have no systems impact
  ::
  ::    XX or ignore? useful for bootstrap?
  ::
  =.  fat  (~(gas de:node fat) ~(tap by use.del))
  ::
  [[but job] fat]
--
::
=|  fat=(node plum)
=^  one  fat
  %-  (what fat)
  :~  /sys/hoon^hoon+%a
      /sys/arvo^hoon+%a
      /sys/lull^hoon+%a
      /sys/zuse^hoon+%a
      /sys/vane/foo^hoon+%a
      /sys/vane/bar^hoon+%a
      /sys/vane/baz^hoon+%a
      /app/dojo^hoon+%a
      /lib/drum^hoon+%a
  ==
=^  two  fat
  %-  (what fat)
  :~  /sys/arvo^hoon+%b
      /sys/lull^hoon+%b
      /sys/vane/foo^hoon+%b
  ==
[one two fat]
