::  /app/collection/hoon
::
/-  hall, *collections
/+  hall, rekey
/=  cols
  /:  /===/web/collections
  /^  collections
  /;  (rekey %da)
  /_  /.  /=  conf  /collections-config/
          /=  tops
        /;  (rekey %da)
        /_  /.  /collections-topic/
                /=  comt
              /;  (rekey %da)
              /_  /collections-comment/
      ==    ==
::
::    things to keep in sync, unique by date:
::  collections: files, circles,       
::  topics:      files, circles, notify
::  comments:    files,          notify
::
::    filepaths:
::  /web/collections/my-coll.config
::  /web/collections/my-coll/some.topic
::  /web/collections/my-coll/some/1.comment
::
::    notification circles:
::  ~.collections_blog-date               new/changed post notifications
::  ~.collections_blog-date__post-date    new/changed comments notifications
::
::
::::
  ::
:: XX belongs back in zuse
|%
++  pack                                                ::  light path encoding
  |=  {a/term b/path}  ^-  knot
  %+  rap  3  :-  (wack a)
  (turn b |=(c/knot (cat 3 '_' (wack c))))
::
++  pick                                                ::  light path decoding
  =+  fel=(most cab (sear wick urt:ab))
  |=(a/knot `(unit {p/term q/path})`(rush a fel))
::
--
=>  |%
    ++  move  (pair bone card)                          ::  all actions
    ++  poke                                            ::
      $%  {$hall-action action:hall}                    ::
      ==                                                ::
    ++  card                                            ::
      $%  {$info wire ship term nori:clay}              ::
          {$poke wire dock poke}                        ::
          {$pull wire dock $~}                          ::
          {$warp wire sock riff:clay}                   ::
          {$quit $~}                                    ::
      ==                                                ::
    --
::
::::
  ::
=,  wired
=,  space:userlib
|_  {bol/bowl:gall upd/@da}
::
++  prep                                                ::<  prepare state
  ::>  adapts state.
  ::
  ::REVIEW this seems like not the ideal place to get the ford update
  |=  old/(unit @da)
  ^-  (quip move _..prep)
  =^  mow  ..prep
    ta-done:(ta-update:ta (fall old *@da))
  [mow ..prep(upd now.bol)]
::
++  poke-noun
  |=  a=$@(?(~ @da) [p=@da q=@da])
  ^-  (quip move _+>)
  ~&  %poked
  =<  ta-done
  ?~  a  
    (ta-create:ta %fora ['a description' pub=& vis=& [~palzod ~ ~]])
  ?@  a
    (ta-submit:ta a 'a topic' ~['with contents'])
  (ta-comment:ta p.a q.a now.bol ~['a comment' 'yo'])
::
++  writ
  |=  {wir/wire rit/riot:clay}
  ^-  (quip move _+>)
  [~ +>]
  ::TODO  watch for file changes. create on new files, update on change, delete
  ::      on remove. we want to watch /web/collections recursively if possible,
  ::      or /web/collections/[col] for each collection and then
  ::      /web/collections/[col]/[top] for each topic as they get created.
::
++  ignore-action
  |=  act=action:api  ^-  ?
  ?-    -.act
      ?($create $delete)
    ?:  (team:title our.bol src.bol)  |
    ~|([%unauthorized -.act src.bol] !!)
  ::
      ?($submit $comment)
    =/  col  (~(get by cols) col.act)
    ?~  col  &
    ?:  publ.conf.u.col
      (~(has in mems.conf.u.col) src.bol)  :: not on blacklist
    !(~(has in mems.conf.u.col) src.bol)   :: is on whitelist
  ==
::
++  poke-collections-action
  |=  act=action:api
  ^-  (quip move _+>)      
  ?:  (ignore-action act)
    [~ +>]
  =<  ta-done
  ?-  -.act
    $create   (ta-create:ta +.act)
    $submit   (ta-submit:ta +.act)
    $comment  (ta-comment:ta +.act)
    $delete   (ta-delete:ta +.act)
  ==
::
++  ta
  |_  moves/(list move)
  ++  ta-done  [(flop moves) +>]
  ++  ta-emit  |=(mov/move %_(+> moves [mov moves]))
  ++  ta-emil  |=(mos/(list move) %_(+> moves (welp (flop mos) moves)))
  ++  ta-hall-action
    |=  act=action:hall
    %-  ta-emit
    :^  ost.bol  %poke  /  ::TODO  wire, handle ++coup.
    :+  [our.bol %hall]  %hall-action
    act
  ::
  ++  ta-hall-actions
    |=  act=(list ?(~ action:hall))  ^+  +>
    ?~  act  +>
    ?~  i.act  $(act t.act)
    $(act t.act, +> (ta-hall-action i.act))  ::TODO group at all?
  ::
  ::  %performing-actions
  ::
  ++  ta-create
    |=  {wat/kind:api cof/config}
    ^+  +>
    ::XX unhandled kind
    (ta-write-config now.bol cof)
  ::
  ++  ta-submit
    |=  {col/time tit/cord wat/wain}
    ::TODO %resubmit topic edit command?
    =/  top/topic  [tit src.bol now.bol wat]
    (ta-write-topic col now.bol top)
  ::
  ++  ta-comment
    |=  {col/time top/@da com/@da wat/wain}
    ^+  +>
    =;  res/$@(~ _+>.$)  ?^(res res +>.$)
    %+  biff  (~(get by cols) col)
    |=  cos=collection
    %+  biff   (~(get by tops.cos) top)
    |=  [topic cos=(map @da comment) ~]
    =/  old/comment
      %+  fall  (~(get by cos) com)
      [src.bol now.bol wat]
    ?.  =(who.old src.bol)  ..ta-comment  :: error?
    %^  ta-write-comment  col  top
    :-  com
    [who.old now.bol wat]
  ::
  ++  ta-delete
    |=  col/time
    ^+  +>
    +>
    ::TODO  - delete files
    ::      - unsubscribe from clay
    ::      - send delete action to hall
    ::      - remove from state
  ::
  ::  %writing-files
  ::
  ++  ta-write
    |=  [wir=[term ~] pax=path cay=cage]  ^+  +>
    =/  pax=path
      :(weld base-path pax /[p.cay])
    %+  ta-emit  ost.bol
    [%info (weld wir pax) our.bol (foal pax cay)]
  ::
  ++  ta-write-config
    |=  {col/time cof/config}
    ^+  +>
    %^  ta-write  /config
      (dray /[%da] col)
    [%collections-config !>(cof)]
  ::
  ++  ta-write-topic
    |=  {col/time wen/@da top/topic}
    ^+  +>
    %^  ta-write  /topic
      (dray /[%da]/[%da] col wen)
    [%collections-topic !>(top)]
  ::
  ++  ta-write-comment
    |=  {col/time top/@da wen/@da com/comment}
    ^+  +>
    %^  ta-write  /comment
      (dray /[%da]/[%da]/[%da] col top wen)
    [%collections-comment !>(com)]
  ::
  ::  %applying-changes
  ::
  ++  ta-this  .
  ++  ta-update
    |=  wen=@da
    =.  upd  wen
    =/  cos  ~(tap by cols)
    |-  ^+  ta-this
    ?~  cos  ta-this
    =.  ta-done  $(cos t.cos)
    =+  `[col=@da collection]`i.cos
    =?  ta-this  (gth col upd)      ::TODO mtime
      (ta-change-config col conf)
    =/  tos  ~(tap by tops)
    |-  ^+  ta-this
    ?~  tos  ta-this
    =.  ta-done  $(tos t.tos)
    =+  `[top=@da topicful]`i.tos
    =?  ta-this  (gth top upd)      ::TODO mtime
      (ta-change-topic col top info)
    =/  mos  ~(tap by comt)
    |-  ^+  ta-this
    ?~  mos  ta-this
    =.  ta-done  $(mos t.mos)
    =+  `[com=@da cot=comment]`i.mos
    =?  ta-this  (gth com upd)      ::TODO mtime
      (ta-change-comment col top com cot)
    ta-this
  ::
  ++  ta-change-config
    |=  {col/time new/config}
    ^+  +>
    ::
    ::  if we don't have it yet, add to hall.
    =/  old  (old-config col)  ::TODO convert other two
    ?~  old
      (ta-hall-create col new)
    ::  update config in hall.
    =/  nam  (circle-for col)
    %-  ta-hall-actions  :~
      ?:  =(desc.new desc.u.old)  ~
      [%depict nam desc.new]
    ::
      ?:  =(visi.new visi.u.old)  ~
      [%public visi.new our.bol nam]
    ::
      (hall-permit nam & (~(dif in mems.new) mems.u.old))
      (hall-permit nam | (~(dif in mems.u.old) mems.new))
    ==
  ::
  ++  ta-change-topic
    |=  {col/time wen/@da top/topic}
    ^+  +>
    =^  top  +>.$
      ?:  =(wed.top now.bol)  [top +>.$]
      =.  wed.top  now.bol                ::  change last edit date
      [top (ta-write-topic col wen top)]
    ::
    =/  new  ?=(~ (old-topic col wen))
    =?  +>.$  new
      =/  cos  (~(got by cols) col)
      ~|  cos
      (ta-hall-create-topic col wen conf.cos)
    (ta-hall-notify col wen ~ new wat.top)
  ::
  ++  ta-change-comment
    |=  {col/time top/@da wen/@da com/comment}
    ^+  +>
    =^  com  +>.$
      ?:  =(wed.com now.bol)  [com +>.$]
      =.  wed.com  now.bol                ::  change last edit date
      [com (ta-write-comment col top wen com)]
    ::
    =/  new  =(~ (old-comment col top wen))
    (ta-hall-notify col top `wen new wat.com)
  ::
  ::  %hall-changes
  ::
  ++  ta-hall-create
    |=  {col/time cof/config}
    ^+  +>
    =+  nam=(circle-for col)
    (ta-hall-configure nam cof)
  ::
  ++  ta-hall-create-topic
    |=  {col/time top/@da cof/config}
    ^+  +>
    =+  nam=(circle-for-topic col top)
    =.  +>.$  (ta-hall-configure nam cof)
    %-  ta-hall-action
    ::NOTE  %source also subs to local config & presence, but
    ::      that generally won't result in visible notifications.
    :^  %source  (circle-for col)  &
    (sy `source:hall`[our.bol nam]~ ~)
  ::
  ++  ta-hall-configure
    |=  [nam=term cof=config]  ^+  +>
    ^+  +>
    %-  ta-hall-actions  :~
      [%create nam desc.cof ?:(publ.cof %journal %village)]
      ?.(visi.cof ~ [%public & our.bol nam])
      (hall-permit nam & mems.cof)
    ==
  ::
  ::
  ++  ta-hall-notify
    |=  {col/time top/@da com/(unit @da) new/? wat/wain}
    ^+  +>
    %-  ta-hall-action
    =-  :+  %phrase  [[our.bol tar] ~ ~]
        [%fat [%text wat] [%lin | msg]]~
    ^-  {tar/naem:hall msg/cord}
    ::TODO
    [(circle-for col) 'TODO']
  --
::
++  hall-permit
  |=  [nam=term inv=? sis=(set ship)]
  ?~  sis  ~
  [%permit nam inv sis]
::
::
++  circle-for
  |=(col/time (pack %collection (dray /[%da] col)))
::
++  circle-for-topic
  |=({col/time top/time} (pack %collection (dray /[%da]/[%da] col top)))
::
++  base-path  (en-beam:format byk.bol(r da+upd) /collections/web)
::
++  old-config
  |=  col/time
  ^-  (unit config)
  =/  pax  :(weld base-path (dray /[%da] col) /collections-config)
  ::
  ?~  (file pax)  ~
  `.^(config %cx pax)
::
++  old-topic
  |=  {col/time top/@da}
  ^-  (unit topic)
  =/  pax  :(weld base-path (dray /[%da]/[%da] col top) /collections-topic)
  ::
  ?~  (file pax)  ~
  `.^(topic %cx pax)
::
++  old-comment
  |=  {col/time top/@da com/@da}
  ^-  (unit comment)
  =/  pax
    :(weld base-path (dray /[%da]/[%da]/[%da] col top com) /collections-comment)
  ::
  ?~  (file pax)  ~
  `.^(comment %cx pax)
--
