/-  *mass
/+  default-agent, dbug
::
|%
+$  card  card:agent:gall
++  orm  ((on @da @ud) lth)
+$  state-0
  $:  %0
    poll=(unit [every=$@(@dr [d=@ud h=@ud m=@ud]) next=@da])
    last=(unit (pair @da (list quac:dill)))
    data=(map (list @t) ((mop @da @ud) lth))
  ==
+$  versioned-state
  $%  state-0
  ==
--
::
=|  state-0
=*  state  -
::
%-  agent:dbug
^-  agent:gall
=<
|_  =bowl:gall
+*  this  .
    def  ~(. (default-agent this %|) bowl)
    hc   ~(. +> bowl)
::
++  on-init  on-init:def
++  on-save   !>(state)
++  on-load
  |=  =vase
  ^-  (quip card _this)
  =+  !<(old=versioned-state vase)
  ?-  old
    [%0 *]  `this(state old)
  ==
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?.  ?=(%mass-action mark)  (on-poke:def mark vase)
  =+  !<(act=action vase)
  ?-    act
      [%mass *]
    :_  this
    [%pass /mass %arvo %d %mass ~]~
  ::
      [%poll *]
    =/  cards=(list card)  [%give %fact ~[/poll] mass-update+!>(`update`act)]~
    =?  cards  ?=(^ poll)
      :_  cards  [%pass /timer/stop %arvo %b %rest next.u.poll]
    ?~  every.act
      [cards this(poll ~)]
    =/  t=@da  (calc-time:hc u.every.act)
    :_  this(poll [~ u.every.act t])
    :_  cards  [%pass /timer/set %arvo %b %wait t]
  ::
      [%free *]
    ?~  path.act
      ?~  before.act
        `this(data ~)
      =.  data
        %-  ~(run by data)
        |=  m=((mop @da @ud) lth)
        ^-  ((mop @da @ud) lth)
        (lot:orm m `u.before.act ~)
      `this
    ?~  before.act
      `this(data (~(del by data) path.act))
    ?~  got=(~(get by data) path.act)
      `this
    `this(data (~(put by data) path.act (lot:orm u.got `u.before.act ~)))
  ==
::
++  on-arvo
  |=  [=wire s=sign-arvo]
  ^-  (quip card _this)
  ?+    wire  (on-arvo:def wire s)
      [%timer %set ~]
    ?.  ?=([%behn %wake *] s)  (on-arvo:def wire s)
    :_  this
    ?~  poll  ~
    ?:  &(?=(^ error.s) (lth now.bowl next.u.poll))
      [%pass /timer/set %arvo %b %wait next.u.poll]~
    [%pass /mass/repeat %arvo %d %mass ~]~
  ::
      [%mass *]
    ?.  ?=([%dill %meme *] s)  (on-arvo:def wire s)
    =/  l=(list (trel (list @t) @da @ud))   (tap-quacs:hc p.s)
    =/  cards=(list card)
      %+  turn  l
      |=  (trel (list @t) @da @ud)
      [%give %fact [%one (turn p wood)]~ mass-update+!>(`update`[%new q p r])]
    =.  cards
      =/  =(map (list @t) @ud)
        %-  ~(gas by *(map (list @t) @ud))
        %+  turn  l
        |=  (trel (list @t) @da @ud)
        [p r]
      :_  cards
      [%give %fact ~[/all] mass-update+!>(`update`[%new-all now.bowl map])]
    =.  cards
      :_  cards
      [%give %fact ~[/raw] mass-update+!>(`update`[%raw ~ now.bowl p.s])]
    =.  last  [~ now.bowl p.s]
    =.  data
      |-
      ?~  l  data
      ?~  got=(~(get by data) p.i.l)
        =/  m  (put:orm *((mop @da @ud) lth) q.i.l r.i.l)
        $(l t.l, data (~(put by data) p.i.l m))
      $(l t.l, data (~(put by data) p.i.l (put:orm u.got q.i.l r.i.l)))
    ?~  t.wire  [cards this]
    ?~  poll  [cards this]
    =/  t=@da  (calc-time:hc every.u.poll)
    =.  cards  :_  cards  [%pass /timer/set %arvo %b %wait t]
    [cards this(poll `u.poll(next t))]
  ==
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  ?+    path  (on-watch:def path)
      [%raw ~]
    :_  this
    [%give %fact ~ mass-update+!>(`update`[%raw last])]~
  ::
      [%one *]
    =/  pax=(list @t)  (turn t.path woad)
    =/  dat=(list [=time size=@ud])  (tap:orm (fall (~(get by data) pax) ~))
    :_  this
    [%give %fact ~ mass-update+!>(`update`[%old pax dat])]~
  ::
      [%all ~]
    :_  this
    :~  :^  %give  %fact  ~
        :-  %mass-update
        !>  ^-  update
        :-  %old-all
        %-  ~(run by data)
        |=  m=((mop @da @ud) lth)
        ^-  (list [=time size=@ud])
        (tap:orm m)
    ==
  ==
::
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  ?+    path  (on-peek:def path)
      [%x %raw ~]
    ``mass-update+!>(`update`[%raw last])
  ::
      [%x %newest *]
    =/  pax=(list @t)  (turn t.t.path woad)
    :^  ~  ~  %mass-update
    !>  ^-  update
    ?~  got=(~(get by data) pax)
      [%newest ~]
    ?~  ram=(ram:orm u.got)
      [%newest ~]
    [%newest ~ key.u.ram pax val.u.ram]
  ::
      [%x %one *]
    =/  pax=(list @t)  (turn t.t.path woad)
    :^  ~  ~  %mass-update
    !>  ^-  update
    [%old pax (tap:orm (fall (~(get by data) pax) ~))]
  ::
      [%x %all ~]
    :^  ~  ~  %mass-update
    !>  ^-  update
    :-  %old-all
    %-  ~(run by data)
    |=  m=((mop @da @ud) lth)
    ^-  (list [=time size=@ud])
    (tap:orm m)
  ==
::
++  on-agent  on-agent:def
++  on-fail   on-fail:def
++  on-leave  on-leave:def
--
::
|_  =bowl:gall
++  calc-time
  |=  t=$@(@dr [d=@ud h=@ud m=@ud])
  ^-  @da
  ?@  t
    (add now.bowl t)
  ?.  &((lth h.t 24) (lth m.t 60))
    ~|(%invalid-time-of-day !!)
  =/  =tarp  [d:(yell now.bowl) h.t m.t 0 ~]
  ?:  =(0 d.t)
    =/  =time  (yule tarp)
    ?:  (gth time now.bowl)
      time
    (yule tarp(d +(d.tarp)))
  (yule tarp(d (add d.tarp d.t)))
::
++  tap-quacs
  |=  q=(list quac:dill)
  =|  out=(list (trel (list @t) @da @ud))
  =|  pax=(list @t)
  |-  ^+  out
  ?~  q  out
  =.  out  [[(flop [name.i.q pax]) now.bowl size.i.q] out]
  $(q t.q, out $(q quacs.i.q, pax [name.i.q pax]))
--

