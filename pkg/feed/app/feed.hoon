/-  post, feed, feed-ui
/+  mall, dbug, verb, agentio, log
|%
++  orm  orm:feed
++  gorm  gorm:feed
++  lorm  lorm:feed
+$  card  card:agent:gall
::
+$  persistent-state-0
  $:  %0
      feeds=(map ship feed:feed)
      watching=(set ship)
  ==
::
+$  cache
  $:  =aggregate:feed
      children=(map gid:post post)
  ==
::
+$  state-0
  [persistent-state-0 cache]
--
%-  agent:dbug
%+  verb  &
%-  adapt:mall
=|  state-0
=*  state  -
=<
  |_  =bowl:gall
  +*  this  .
      event-core    ~(. +> [bowl ~])
  ++  init  
    :-  ~
    this(feeds (~(put by feeds) our.bowl *feed:feed))
  ++  stay  !>(-.state)
  ++  load
    |=  =vase
    =.  -.state  !<(persistent-state-0 vase)
    =?  feeds  !(~(has by feeds) our.bowl)
      (~(put by feeds) our.bowl *feed:feed)
    =.  +.state  rebuild-cache:event-core
    `this
  ++  call
    |=  [=path =task:mall]
    =^  cards  state
      abet:(call:event-core path task)
    [cards this]
  ::
  ++  take 
    |=  [=wire =sign:mall]
    =^  cards  state
      abet:(take:event-core wire sign)
    [cards this]
  ::
  ++  peek  peek:event-core
  ++  poke
    |=  [=mark =vase]
    =^  cards  state
      abet:(on-poke:event-core mark vase)
    [cards this]
  ++  goof
    |=  [=term =tang]
    %-  (slog leaf/"error in {<dap.bowl>}" >term< tang)
    `this
  --
|_  [=bowl:gall cards=(list card)]
++  io  ~(. agentio bowl)
++  pas  pass:io
++  log  ~(. ^log [bowl 0 "feed: "])
++  event-core  .
++  abet
  ?>  (gth now.bowl *time)
  [(flop cards) state]
::
++  emit  |=(=card event-core(cards [card cards]))
++  emil  |=(crds=(list card) event-core(cards (welp (flop crds) cards)))
::
++  call
  |=  [=path =task:mall]
  ?>  ?=([%feed @ *] path)
  abet:(call:(from-wire:fe-core path) t.t.path task)
::
++  take
  |=  [=wire =sign:mall]
  ~|  bad-take/wire
  ?>  ?=([%feed @ *] wire)
  abet:(take:(from-wire:fe-core wire) t.t.wire sign)
::
++  peek
  |=  =path
  |^  ^-  (unit (unit cage))
  ?+  path  [~ ~]
    [%x %dbug %state ~]  ``noun+!>(state)
    [%x %feed @ *]  (peek:(from-wire:fe-core t.path) t.t.t.path)
    [%x %feeds ~]   ``feed-ui-update+!>([%feeds ~(key by feeds)])
    ::
      [%x %global %newest @ ~]
    =/  count=@ud  (slav %ud i.t.t.t.path)
    ``(ui-upd list/(scag count (turn (tap:gorm aggregate) tail)))
    ::  
      [%x %global %older @ @ ~]
    =/  count=@ud  (slav %ud i.t.t.t.path)
    =/  index=@da  (slav %da i.t.t.t.t.path)
    ``(ui-upd list/(turn (tab:gorm aggregate `[~zod index] count) tail))
  ==
  ++  ui-upd
    |=  =update:feed-ui
    ^-  cage
    feed-ui-update+!>(update)
  --
::
++  on-poke
  |=  [=mark =vase]
  ^+  event-core
  ?+  mark  !!
  ::
      %feed-action
    =+  !<(=action:feed vase)
    abet:(~(on-action fe-core p.action) q.action)
  ::
      %feed-diff
    =+  !<(=diff:feed vase)
    abet:(~(on-diff fe-core our.bowl) now.bowl diff)
  ::
     %feed-ui-update
    =+  !<(=update:feed-ui vase)
    =;   [=ship =diff:feed]
      abet:(~(on-action fe-core ship) diff)
    ?+  -.update  ~|(bad-ui-update/-.update !!)
    ::
        %like
      =/  post-update=update:post  [%stamps like/~]
      =/  =diff:feed  post/[time.update post-update]
      [ship.update diff]
    ::
        %add-post
      =/  =missive:post
        [[now.bowl ~] q.update]
      =/  p=post:post
        [missive *stamps:post]
      =/  up=update:post  add-post/p
      =/  =diff:feed  post/[now.bowl up]
      [p.update diff]
    ==

  ::
      %ship  
    =+  !<(=ship vase)
    =/  fe  ~(. fe-core ship)
    ?:  (~(has in watching) ship)
      abet:unfollow:fe
    abet:follow:fe
  ::
      %noun  
    ?+  q.vase  ~|(%bad-noun-poke !!)
    ::
        %print
      %-  (slog leaf/<+.state> ~)
      event-core
    ::
        %kick   
      %+  roll  ~(val by sup.bowl)
      |=  [[=ship =path] out=_event-core]
      (emit:out `card`(kick-only:io ship path ~))
    ==
  ==
++  rebuild-cache
  =/  feeds=(list [=ship =feed:feed])  ~(tap by feeds)
  |-  ^+  +.state
  ?~  feeds  +.state
  =,  i.feeds
  %_    $
    feeds  t.feeds
    ::
      aggregate  
    %+  gas:gorm  aggregate
    %+  turn  (tap:orm tl.feed)
    |=  [=id:post pos=post]
    ^-  [gid:post post]
    [[ship id] pos]
  ==
++  fe-core
  |_  =ship
  ++  fe-core  .
  ++  fed  `feed:feed`(~(got by feeds) ship)
  ++  from-wire
    |=  =wire
    ^+  fe-core
    ?>  ?=([%feed @ *] wire)
    (abed (slav %p i.t.wire))
  ::
  ++  abed
    |=  s=^ship
    ^+  fe-core
    fe-core(ship s)
  ::
  ++  abet  event-core
  ::
  ++  here  /feed/(scot %p ship)
  ++  area
    :-  here
    %~  tap  in
    %-  ~(gas in *(set path))
    %+  murn  ~(val by sup.bowl)
    |=  [s=^ship =path]
    ?.  =(here (scag 2 path))  ~
    `path
  ::
  ++  allowed  
    |%
    ++  read   (allowed:feed src.bowl read:fed)
    ++  write  (allowed:feed src.bowl write:fed)
    --
  ++  pass
    |%
    ++  proxy  |=(=cage (poke-clone:pass ship cage))
    ++  pass  ~(. pass:io here)
    ++  when  (fall (bind (pry:lorm log:fed) head) *time)
    ++  watch  
      %+  watch-clone:pass  ship
      %+  welp  here
      ?.  (~(has by feeds) ship)  ~
      /log/(scot %da when)
    ++  leave  (leave-clone:pass ship)
    --
  ++  give
    |%
    ++  init  (fact-init:io feed-update+!>(fed))
    ++  replay  
      |=  t=(unit time)
      =/  =log:feed  (lot:lorm log:fed ~ t)
      (fact-init:io feed-update+!>([when:pass %replay log]))
    ++  update
      |=  =update:feed
      (fact:io feed-update+!>(update) area)
    ::
    ++  recheck-perms
      ^-  (list card)
      %+  murn  ~(val by sup.bowl)
      |=  [s=^ship =path]
      ?.  =(here (scag 2 path))  ~
      ?:  (allowed:feed s read:fed)  ~
      `(kick-only:io s path ~)
    --
  ::  +|  %entrypoints
  ++  follow
    =.  watching  (~(put in watching) ship)
    =.  event-core  (emit watch:pass)
    fe-core
  ++  unfollow
    =:  feeds     (~(del by feeds) ship)
        watching  (~(del in watching) ship)
      ==
    =.  event-core  (emit leave:pass)
    fe-core
  ::
  ++  edit
    |=  f=$-(feed:feed feed:feed)
    ^+  fe-core
    fe-core(feeds (~(jab by feeds) ship f))
  ::
  ++  on-action
    |=  =diff:feed
    ?:  =(our.bowl ship)  (on-diff now.bowl diff)
    fe-core(event-core (emit (proxy:pass feed-diff+!>(diff))))
  ::
  ++  on-update
    |=  [=time =diff:feed]
    ^+  fe-core
    ?.  ?=(%replay -.diff)  (on-diff time diff)
    %+  roll  (bap:lorm log.diff)
    |=  [[t=^time =diff:feed] out=_fe-core]
    (on-diff:out t diff)
  ::
  ++  on-diff
    |=  [=time =diff:feed]
    |^  ^+  fe-core
    ?>  write:allowed
    =.  fe-core
      (edit |=(f=feed:feed f(log (put:lorm log.f time diff))))
    =.  event-core
      (emit (update:give time diff))
    ?+  -.diff  ~|(bad-diff/-.diff !!)
    ::
        %post   
      abet:(~(on-update po-core id.diff) update.diff)
    ::
      %policy  ~!  +.diff  (diff-policy +.diff)
    ==
    ++  diff-policy
      |=  [=kind:policy:feed =diff:policy:feed]
      |^  ^+  fe-core
      ?>  |(=(our.bowl ship) =(src.bowl ship))
      =?  fe-core  ?=(%read kind)
        (edit |=(f=feed:feed f(read (ap read.f))))
      =?  fe-core  ?=(%write kind)
        (edit |=(f=feed:feed f(write (ap write.f))))
      fe-core(event-core (emil recheck-perms:give))
      ::
      ++  ap
        |=  =policy:feed
        ^-  policy:feed
        ?-  -.diff
        ::
          %replace  +.diff
        ::
            %white  
          ?>  ?=(%white -.policy)
          ?-  +<.diff
            %add  policy(p (~(uni in p.policy) p.diff))
            %del  policy(p (~(dif in p.policy) p.diff))
          ==
        ::
            %black  
          ?>  ?=(%black -.policy)
          ?-  +<.diff
            %allow-ranks  policy(ranks (~(dif in ranks.policy) p.diff))
            %ban-ranks    policy(ranks (~(uni in ranks.policy) p.diff))
            %ban-ships    policy(ships (~(uni in ships.policy) p.diff))
            %allow-ships  policy(ships (~(dif in ships.policy) p.diff))
          ==
        ==
      --
    --
  ::
  ++  take
    |=  [=wire =sign:mall]
    ^+  fe-core
    ?+  wire  ~|(bad-feed-take/wire !!)
    ::
        ~
      ?+  sign  fe-core
      ::
          [%agent %kick *]  
        =.  event-core  (emit watch:pass)
        fe-core

      ::
          [%agent %fact *]
        ?.  ?=(%feed-update p.cage.p.sign)  fe-core
        =+  !<(=update:feed q.cage.p.sign)
        (on-update update)
      ::
          [%agent %watch-ack *]
        ?~  p.p.sign
          =?  feeds  !(~(has by feeds) ship)
            (~(put by feeds) ship *feed:feed)
          fe-core
        %-  (slog leaf/"nacked {<wire>}" u.p.p.sign)
        =.  watching  (~(del in watching) ship)
        fe-core
      ==
    ==
  ::
  ++  call
    |=  [=path =task:mall] 
    ^+  fe-core
    ?+  path  ~|(bad-feed-call/path !!)
    ::
        ~
      ?+  -.task  !!
        %leave  fe-core
      ::
          %watch  
        ?>  read:allowed
        =.  event-core  (emit (replay:give ~))
        fe-core
      ==
    ::
        [%log @ ~]
      =/  start=time
        (slav %da i.t.path)
      ?+  -.task  !!
        %leave  fe-core
      ::
          %watch  
        ?>  read:allowed
        =.  event-core  (emit (replay:give `start))
        fe-core
      ==
    ==
  ::
  ++  peek
    |=  =path
    ^-  (unit (unit cage))
    ?+  path  [~ ~]
    ::
        [%newest ?(%all %orphans) @ ~]  
      =/  count=@ud  (slav %ud i.t.t.path)
      =/  orphaned   =(%orphans i.t.path)
      =-  ``feed-ui-update+!>(-)
      ^-  update:feed-ui
      :-  %list
      %+  scag  count
      %+  murn  (tap:orm tl:fed)
      |=  [=id:post p=post:post]
      ^-  (unit post:post)
      ?.  orphaned  `p
      ?.  =(~ parent.p.p)  ~
      `p
    ::
        [%next ?(%all %orphans) @ @ ~]
      =/  index=@da  (slav %da i.t.t.t.path)
      =/  count=@ud  (slav %ud i.t.t.path)
      =/  orphaned   =(%orphans i.t.path)
      =-  ``feed-ui-update+!>(-)
      ^-  update:feed-ui
      :-  %list
      %+  murn  (tab:orm tl:fed `index count)
      |=  [=id:post p=post:post]
      ^-  (unit post:post)
      ?.  orphaned  `p
      ?.  =(~ parent.p.p)  ~
      `p
    ==
  ::
  ++  po-core
    |_  =id:post
    ++  p   (got:orm tl:fed id)
    ++  po-core  .
    ++  here  `path`(welp here:fe-core /post/(scot %da id))
    ++  abet   fe-core
    ++  abed
      |=  =wire
      ?>  ?=([%post @ *] wire)
      =/  i=id:post  (slav %da i.t.wire)
      po-core(id i)
    ::
    ++  fmt
      |=  mes=tape
      ^-  tape
      (welp "post: ({<ship>}@{<id>}): " mes)
    ::
    ++  edit
      |=  f=$-(post:post post:post)
      ^+  po-core
      =.  fe-core
        %-  edit:fe-core
        |=  fe=feed:feed
        =/  old=post:post  (got:orm tl.fe id)
        fe(tl (put:orm tl.fe id (f p)))
      po-core
    ::
    ++  reply
      |=  [parent=id:post child=id:post]
      ^+  fe-core
      =.  id  parent
      =.  po-core  
        %-  edit
        |=(=post:post post(children.p (~(put in children.p.post) child)))
      abet
    ::
    ++  on-update
      |=  =update:post
      ^+  po-core
      ?-  -.update
        %add-post  (add +.update)
        %del-post  del
        %stamps    (philate +.update)
      ==
    ++  philate
      |=  =update:stamps:post
      ^+  po-core
      =*  ship  src.bowl
      |^ 
      ?+  -.update  !!
        %like  like
        %unlike  unlike
        %react  (react react.update)
        %unreact  unreact
      ==
      ++  like
        ^+  po-core
        ?<  (~(has in likes.q:p) src.bowl)
        %-  edit
        |=(=post:post post(likes.q (~(put in likes.q.post) src.bowl)))
      ::
      ++  unlike
        ^+  po-core
        %-  edit
        |=(=post:post post(likes.q (~(del in likes.q.post) ship)))
      ::
      ++  react
        |=  r=react:stamps:post
        ^+  po-core
        %-  edit
        |=(=post:post post(reacts.q (~(put by reacts.q.post) ship r)))
      ::
      ++  unreact
        ^+  po-core
        %-  edit
        |=(=post:post post(reacts.q (~(del by reacts.q.post) ship)))
      --
    ::
    ++  add
      |=  po=post
      ?>  |(?=(~ parent.p.po) (has:orm tl:fed u.parent.p.po))
      ?>  |(?=(^ parent.p.po) =(ship src.bowl)) 
      =?  fe-core  ?=(^ parent.p.po)
        (reply u.parent.p.po id)
      =.  fe-core
        %-  edit:fe-core
        |=  f=feed:feed
        f(tl (put:orm tl.f id po))
      =.  aggregate  (put:gorm aggregate ship^id po)
      po-core
    ::
    ++  del
      =^  val  aggregate
        (del:gorm aggregate [ship id])
      =.  fe-core
        %-  edit:fe-core
        |=  f=feed:feed
        =^  unused  tl.f
          (del:orm tl.f id)
        f
      po-core
    --
  --
--
