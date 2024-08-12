::  /hoon/mass/agent
::    Run and record memory reports, maybe on a timer.
::    Subscribe to and scry reports, either for a particular
::    entry or all records. Includes a whitelist for remote
::    ships and records base hash and time of each report.
::    Records for a particular path can also be scried out
::    as a CSV. Sizes are in bytes.
::
::  Subscription endpoints:
::
::    /acl: changes to whitelist for remote ships
::    /raw: raw reports from dill
::    /one/[path]: reports for a particular path
::    /all: reports for all paths
::
::  Scry endpoints:
::
::    /x/acl: remote ship whitelist
::    /x/poll: current polling frequency/period
::    /x/raw: last raw memory report from dill
::    /x/newest/[path]: latest report for path
::    /x/one/[path]: all reports for path
::    /x/all: all reports for all paths
::    /x/csv/[path]: csv file of reports for path 
::
/-  *mass
/+  default-agent, dbug
::
|%
+$  card  card:agent:gall
++  orm  ((on @da (pair @ud @uvI)) lth)
+$  state-0
  $:  %0
    poll=(unit [every=$@(@dr [d=@ud h=@ud m=@ud]) next=@da])
    last=(unit (trel @da @uvI (list quac:dill)))
    data=(map (list @t) ((mop @da (pair @ud @uvI)) lth))
    obey=(set @p)
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
      [%obey *]
    ?>  =(our.bowl src.bowl)
    =^  cards=(list card)  obey
      ?~  p.act
        :_  ~
        %+  murn  ~(tap in obey)
        |=  =ship
        ^-  (unit card)
        ?:  =(our.bowl src.bowl)
          ~
        `[%give %kick ~ `ship]
      :_  (~(dif in (~(uni in obey) add.u.p.act)) rm.u.p.act)
      %+  murn  ~(tap in rm.u.p.act)
      |=  =ship
      ^-  (unit card)
      ?:  =(our.bowl src.bowl)
        ~
      `[%give %kick ~ `ship]
    :_  this
    :_  cards
    [%give %fact ~[/acl] mass-update+!>(`update`[%acl +.act])]
  ::
      [%mass *]
    ?>  |(=(our.bowl src.bowl) (~(has in obey) src.bowl))
    :_  this
    [%pass /mass %arvo %d %mass ~]~
  ::
      [%poll *]
    ?>  |(=(our.bowl src.bowl) (~(has in obey) src.bowl))
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
    ?>  =(our.bowl src.bowl)
    ?~  path.act
      ?~  before.act
        `this(data ~)
      =.  data
        %-  ~(run by data)
        |=  m=((mop @da (pair @ud @uvI)) lth)
        ^-  ((mop @da (pair @ud @uvI)) lth)
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
    =+  .^(base=@uvI %cz /(scot %p our.bowl)/base/(scot %da now.bowl))
    =/  cards=(list card)
      %+  turn  l
      |=  (trel (list @t) @da @ud)
      [%give %fact [%one (turn p wood)]~ mass-update+!>(`update`[%new q p r base])]
    =.  cards
      =/  =(map (list @t) (pair @ud @uvI))
        %-  ~(gas by *(map (list @t) (pair @ud @uvI)))
        %+  turn  l
        |=  (trel (list @t) @da @ud)
        [p r base]
      :_  cards
      [%give %fact ~[/all] mass-update+!>(`update`[%new-all now.bowl map])]
    =.  cards
      :_  cards
      [%give %fact ~[/raw] mass-update+!>(`update`[%raw ~ now.bowl base p.s])]
    =.  last  [~ now.bowl base p.s]
    =.  data
      |-
      ?~  l  data
      ?~  got=(~(get by data) p.i.l)
        =/  m  (put:orm *((mop @da (pair @ud @uvI)) lth) q.i.l r.i.l base)
        $(l t.l, data (~(put by data) p.i.l m))
      $(l t.l, data (~(put by data) p.i.l (put:orm u.got q.i.l r.i.l base)))
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
      [%acl ~]
    ?>  =(src.bowl our.bowl)
    :_  this
    [%give %fact ~ mass-update+!>(`update`[%acl ~ obey ~])]~
  ::
      [%raw ~]
    ?>  |(=(our.bowl src.bowl) (~(has in obey) src.bowl))
    :_  this
    [%give %fact ~ mass-update+!>(`update`[%raw last])]~
  ::
      [%one *]
    ?>  |(=(our.bowl src.bowl) (~(has in obey) src.bowl))
    =/  pax=(list @t)  (turn t.path woad)
    =/  dat=(list [=time size=@ud base=@uvI])  (tap:orm (fall (~(get by data) pax) ~))
    :_  this
    [%give %fact ~ mass-update+!>(`update`[%old pax dat])]~
  ::
      [%all ~]
    ?>  |(=(our.bowl src.bowl) (~(has in obey) src.bowl))
    :_  this
    :~  :^  %give  %fact  ~
        :-  %mass-update
        !>  ^-  update
        :-  %old-all
        %-  ~(run by data)
        |=  m=((mop @da (pair @ud @uvI)) lth)
        ^-  (list [=time size=@ud base=@uvI])
        (tap:orm m)
    ==
  ==
::
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  ?+    path  (on-peek:def path)
      [%x %acl ~]
    ``mass-update+!>(`update`[%acl ~ obey ~])
  ::
      [%x %poll ~]
    ``mass-update+!>(`update`[%poll ?~(poll ~ `every.u.poll)])
  ::
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
    |=  m=((mop @da (pair @ud @uvI)) lth)
    ^-  (list [=time size=@ud base=@uvI])
    (tap:orm m)
  ::
      [%x %csv *]
    :^  ~  ~  %atom
    !>  ^-  @
    =/  pax=(list @t)  (turn t.t.path woad)
    (make-csv:hc (tap:orm (fall (~(get by data) pax) ~)))
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
::
++  make-csv
  |=  dat=(list [=time size=@ud base=@uvI])
  |^  ^-  @t
  %-  of-wain:format
  :-  'time,bytes,basehash'
  %+  turn  dat
  |=  [=time size=@ud base=@uvI]
  (rap 3 (format-date time) ',' (crip (a-co:co size)) ',' (scot %uv base) ~)
  ++  format-date
    |=  t=time
    ^-  @t
    =/  d=date  (yore t)
    %+  rap  3
    :~  (crip ((d-co:co 4) ?.(a.d 0 (min y.d 9.999))))
        '-'
        (crip ((d-co:co 2) m.d))
        '-'
        (crip ((d-co:co 2) d.t.d))
        ' '
        (crip ((d-co:co 2) h.t.d))
        ':'
        (crip ((d-co:co 2) m.t.d))
        ':'
        (crip ((d-co:co 2) s.t.d))
    ==
  --
--

