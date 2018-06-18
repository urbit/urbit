::  /app/collection/hoon
::
/-  hall, *collections
/+  hall, rekey, colls
/=  cols   /:  /===/web/collections  /collections/
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
++  ships-to-whoms
  |=  a=(set ship)
  ^-  (set whom:clay)
  %-  (hard (set whom:clay))
  %-  ~(run in a)
  |=  b=@p
  [& b]
::
++  whoms-to-ships 
  |=  a=(set whom:clay)
  ^-  (set ship)
  %-  (hard (set ship))
  %-  ~(rep in a)
  |=  [b=whom:clay out=(set ship)]
  :: no we don't support permissions groups because we can't scry them
  ?:  -.b
    ?>  ?=(@p +.b)
    (~(put in out) +.b)
  out
::
--
=>  |%
    ++  move  (pair bone card)                          ::  all actions
    ++  poke                                            ::
      $%  {$hall-action action:hall}                    ::
          {$collections-action action:api}              ::
      ==                                                ::
    ++  card                                            ::
      $%  {$info wire ship term nori:clay}              ::
          {$poke wire dock poke}                        ::
          {$perm wire ship desk path rite:clay}         ::
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
++  now-id
  ::  HACK "sanitized" now for id use, can't get mistaken for file with extension in url
  `@da`(sub now.bol (div (dis now.bol ~s0..fffe) 2))
::
++  base-path  (en-beam:format byk.bol(r da+upd) /collections/web)
::
++  full-path
  |=  $@(col=time [col=time $@(top=@da [top=@da com=@da])])
  ^-  path
  %+  weld  base-path
  ?-  +<
    @        (dray /[%da] col)
    {@ @}    (dray /[%da]/[%tas]/[%da] col %post top)
    {@ @ @}  (dray /[%da]/[%tas]/[%da]/[%tas]/[%da] col %post top %comment com) 
  ==
::
++  poke-noun
  |=  [who=@p wat=$?(~ $@(@da [@da @da]))]
  ^-  (quip move _+>)
  :_  +>
  :~
  :*  ost.bol  %poke  /noun  
      [our.bol dap.bol]
      %collections-action
      ?~  wat
        =|  con=config
        =/  rul=rule:clay
          [%black (ships-to-whoms (sy ~[~bus ~zod ~marzod]))]
        [%create con(desc (scot %uv eny.bol), write-post rul, write-reply rul)]
      ?@  wat
        [%submit who wat (scot %uv eny.bol) ~['this' 'is' 'a' 'post' 'from' (scot %p our.bol)]]
      [%comment who -.wat +.wat ~ ~['this' 'is' 'a' 'comment' 'from' (scot %p our.bol)]]
    ==
  ==
::
::  +mack: acknowledgement for permissions
::
++  mack
  |=  [wir=wire err=(unit tang)]
  ^-  (quip move _+>)
  ?~  err
    [~ +>]
  (mean u.err)
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
++  check-permissions
  |=  act=action:api
  ^-  ?
  ?>  !=(our.bol src.bol)
  ?>  !?=(?(%create %change-config) -.act)
  =/  pax
    ?+  -.act  !!
      $submit          (weld (full-path col.act) /post)
      $resubmit        (full-path col.act top.act)
      $comment         (weld (full-path col.act top.act) /comment)
      $delete          (full-path col.act)
      $delete-topic    (full-path col.act top.act)
      $delete-comment  (full-path col.act top.act com.act)
    ==
  =/  perms=[read=dict:clay writ=dict:clay]  .^([dict:clay dict:clay] %cp pax)
  =/  in-list=?  (~(has in (whoms-to-ships who.rul.writ.perms)) src.bol)
  ?:  =(%white mod.rul.writ.perms)
    in-list
  !in-list
::
++  poke-collections-action :: Review: should we fail silently or loudly for some of these?
  |=  act=action:api
  ^-  (quip move _+>)
  ?:  =(our.bol src.bol)
    ?:  ?=(?(%create %change-config) -.act)
      :: these can only be for us - no host field
      ta-done:(ta-collections-action:ta act)
    ?:  =(our.bol host.act)
      :: from us and to us
      :: no need to check permissions
      ta-done:(ta-collections-action:ta act)
    :: from us but not to us
    :: forward poke
    :_  +>
    :_  ~
    :*  ost.bol  %poke  /foreign-poke
        [host.act %collections]
        [%collections-action act]
    ==
  ?:  ?=(?(%create %change-config) -.act)
    :: we don't allow other people to create or modify our configurations
    [~ +>]
  ?:  =(our.bol host.act)
    :: not from us but to us
    :: check permissions
    ?:  (check-permissions act)
      ta-done:(ta-collections-action:ta act)
    [~ +>]
  :: not from us and not to us
  :: should never happen, crash
  [~ +>]
::
++  ta
  |_  moves/(list move)
  ++  ta-this  .
  ++  ta-done  [(flop moves) +>]
  ++  ta-emit  |=(mov/move %_(+> moves [mov moves]))
  ++  ta-emil  |=(mos/(list move) %_(+> moves (welp (flop mos) moves)))
  ++  ta-collections-action
    |=  act=action:api
    ?-  -.act
      $create          (ta-create +.act)
      $change-config   !!  :::(ta-create +.act)
      $submit          (ta-submit +.act)
      $resubmit        (ta-resubmit +.act)
      $comment         (ta-comment +.act)
      $delete          (ta-delete +.act)
      $delete-topic    (ta-delete-topic +.act)
      $delete-comment  (ta-delete-comment +.act)
    ==
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
    ::|=  {wat/kind:api cof/config}
    |=  cof/config
    ^+  +>
    =/  pax  (full-path now-id)
    =/  latest-pax  (weld pax /latest/hoon)
    =.  ta-this  (ta-write /config now-id %collections-config !>(cof))
    %-  ta-emit
    [ost.bol %info latest-pax our.bol (foal latest-pax [%hoon !>(latest-post:colls)])]
  ::
  ++  ta-submit
    |=  {host/@p col/@da tit/cord wat/wain}
    =/  top/topic  [tit src.bol wat]
    (ta-write /topic [col now-id] %collections-topic !>(top))
  ::
  ++  ta-resubmit
    |=  {host/@p col/@da wen/@da tit/cord wat/wain}
    ?:  (new-topic col wen)  ta-this  ::REVIEW error?
    =/  top/topic  [tit src.bol wat]
    (ta-write /topic [col wen] %collections-topic !>(top))
  ::
  ++  ta-comment
    |=  {host/@p col/@da top/@da com/?(~ @da) wat/wain}
    ^+  +>
    ?~  com  $(com now-id)  :: new comment
    =;  res/$@(~ _+>.$)  ?^(res res +>.$)
    %+  biff  (ta-get-topic col top)
    |=  [^ cos=(map @da {@da comment})]
    =/  old/{@da comment}
      (fall (~(get by cos) com) [now-id src.bol wat])
    ?.  =(who.old src.bol)  ..ta-comment  ::REVIEW error?
    %^  ta-write  /comment
      [col top com]
    [%collections-comment !>(`comment`+.old(wat wat))]
  ::
  ++  ta-get-topic
    |=  {col/time top/@da}  ^-  (unit topicful)
    %+  biff  (~(get by cols) col)
    |=  [^ tos=(map @da topicful)]
    (~(get by tos) top)
  ::
  ++  ta-get-comment
    |=  {col/time top/@da com/@da}  ^-  (unit [@da comment])
    %+  biff  (ta-get-topic col top)
    |=  [^ cos=(map @da {@da comment})]
    (~(get by cos) com)
  ::
  ++  ta-delete
    |=  [host/@p col/@da]
    ^+  +>
    =+  (~(get by cols) col)
    ?~  -  ta-this  ::REVIEW error?
    =.  ta-this  (ta-remove /config col %collections-config)
    =/  cyc  (circle-for col)
    =.  ta-this  (ta-hall-action %delete cyc `'Collection deleted')
    =/  tops=(list [top=@da topicful])  ~(tap by tops.u)
    |-  ^+  ta-this
    ?~  tops  ta-this
    =.  ta-this  $(tops t.tops)
    (ta-delete-topic-inf 'Collection deleted' col i.tops)
  ::
  ++  ta-delete-topic
    |=  {host/@p col/@da top/@da}  ^+  ta-this
    =+  (ta-get-topic col top)
    ?~  -  ta-this  ::REVIEW error?
    (ta-delete-topic-inf 'Topic deleted' col top u)
  ::
  ++  ta-delete-topic-inf  ::REVIEW name
    |=  {inf/@t col/time top/@da tof/topicful}
    =.  ta-this  (ta-remove /topic [col top] %collections-topic)
    =/  cyt  (circle-for-topic col top)
    =.  ta-this  (ta-hall-action %delete cyt `inf)
    =/  coms=(list [com=@da @ comment])  ~(tap by coms.tof)
    |-  ^+  ta-this
    ?~  coms  ta-this
    =.  ta-this  $(coms t.coms)
    (ta-remove /comment [col top com.i.coms] %collections-comment)
  ::
  ++  ta-delete-comment
    |=  {host/@p col/@da top/@da com/@da}  ^+  +>
    =+  (ta-get-comment col top com)
    ?~  -  ta-this  ::REVIEW error?
    (ta-remove /comment [col top com] %collections-comment)
  ::
  ::  permissions
  ::
  ++  ta-set-permissions  ::  TODO: add /post, /comments
    |=  $:  rit=rite:clay
            loc=$@(col=@da [col=@da $@(top=@da [top=@da com=@da])])
        ==
    =/  pax  (full-path loc)
    =.  pax  (slag 3 pax)
    =.  pax
      ?@  loc  (weld pax /post)
      (weld pax /comment)
    %-  ta-emit
    [ost.bol %perm (weld /perms pax) our.bol q.byk.bol pax rit]
  ::
  ::  %writing-files
  ::
  ++  ta-write
    |=  [wir=[term ~] loc=?(@ {@ @} {@ @ @}) cay=cage]  ^+  +>
    =/  pax  (full-path loc)
    =.  pax
      ?-  loc
        @        (weld pax /collections-config)
        [@ @]    (weld pax /collections-topic)
        [@ @ @]  (weld pax /collections-comment)
      ==
    %-  ta-emit
    [ost.bol %info (weld wir pax) our.bol (foal pax cay)]
  ::
  ++  ta-remove
    |=  [wir=[term ~] loc=?(@ {@ @} {@ @ @}) mar=mark]  ^+  +>
    =/  pax  (full-path loc)
    =.  pax
      ?-  loc
        @        (weld pax /collections-config)
        [@ @]    (weld pax /collections-topic)
        [@ @ @]  (weld pax /collections-comment)
      ==
    ?>  =(mar -:(flop pax))
    %+  ta-emit  ost.bol
    [%info (weld wir pax) our.bol (fray pax)]
  ::
  ::  %applying-changes
  ::
  ++  ta-update
    |=  wen=@da
    =.  upd  wen
    =/  cos  ~(tap by cols)
    |-  ^+  ta-this
    ?~  cos  ta-this
    =.  ta-done  $(cos t.cos)
    =+  `[col=@da collection]`i.cos
    =?  ta-this  (gth mod.conf upd)
      (ta-change-config col +.conf)
    =/  tos  ~(tap by tops)
    |-  ^+  ta-this
    ?~  tos  ta-this
    =.  ta-done  $(tos t.tos)
    =+  `[top=@da topicful]`i.tos
    =?  ta-this  (gth mod.info upd)
      (ta-change-topic col top +.info +.conf)
    =/  mos  ~(tap by coms)
    |-  ^+  ta-this
    ?~  mos  ta-this
    =.  ta-done  $(mos t.mos)
    =+  `[com=@da cot=[mod=@da comment]]`i.mos
    =?  ta-this  (gth mod.cot upd)
      (ta-change-comment col top com cot)
    ta-this
  ::
  ++  ta-change-config
    |=  {col/@da conf/config}
    ^+  +>
    ::
    ::  if we don't have it yet, add to hall.
    =.  ta-this
    =/  old  !(new-config col)  ::TODO keep old configs in state
    ?.  old
      (ta-hall-create col conf)
    ::  update config in hall.
    =/  nam  (circle-for col)
    %-  ta-hall-actions  :~
::       ?:  =(desc.new desc.u.old)  ~
      [%depict nam desc.conf]
    ::
::       ?:  =(visi.new visi.u.old)  ~
      [%public visi.conf our.bol nam]
    ::
::       (hall-permit nam & (~(dif in mems.new) mems.u.old))
::       (hall-permit nam | (~(dif in mems.u.old) mems.new))
    ==
    =/  rit=rite:clay  [%rw `read.conf `write-post.conf]
    (ta-set-permissions rit col)
  ::
  ++  ta-change-topic
    |=  {col/time wen/@da top/topic conf/config}
    ^+  +>
    =/  new=?  (new-topic col wen)
    =?  ta-this  new
      =/  cos  (~(got by cols) col)
      (ta-hall-create-topic col wen +.conf.cos)
    =.  ta-this  (ta-hall-notify col wen ~ new wat.top)
    =/  rit=rite:clay  [%rw `read.conf `write-reply.conf]
    (ta-set-permissions rit col wen)
  ::
  ++  ta-change-comment
    |=  {col/time top/@da wen/@da @da com/comment}
    ^+  +>
    =/  new  (new-comment col top wen)
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
  ::
  ++  ta-hall-configure
    |=  [nam=term cof=config]  ^+  +>
    ^+  +>
    %-  ta-hall-actions  :~
    ::
      [%create nam desc.cof %journal]
    ::
      ?.(visi.cof ~ [%public & our.bol nam])
    ::
      [%source %inbox & (sy `source:hall`[our.bol nam]~ ~)]
    ::
::      (hall-permit nam & mems.cof)
    ==
  ::
  ::
  ++  ta-hall-notify
    |=  {col/time top/@da com/(unit @da) new/? wat/wain}
    ^+  +>
    %-  ta-hall-action
    =-  :+  %phrase  [[our.bol tar] ~ ~]
        [%fat [%text wat] [%lin | msg]]~
    ^-  {tar/name:hall msg/cord}
    ::TODO
    ::
    ::  put post id and title in post message
    =/  lin   %-  crip
              ;:  weld
                  (scow %da top)
                  "|"
                  %-  trip  
                  =<  tit.info  
                  %-  need
                  (ta-get-topic col top)
              ==
    :: this can't be the best way to switch on top v com?
    ?~  com
      [(circle-for col) lin]
    [(circle-for-topic col top) lin]
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
::
++  new-config
  |=  col/time
  ^-  ?
  =/  pax  :(weld base-path (dray /[%da] col) /collections-config)
  ::
  ?=(~ [fil]:.^(arch %cy pax))
::
++  new-topic
  |=  {col/time top/@da}
  ^-  ?
  =/  pax  :(weld base-path (dray /[%da]/[%da] col top) /collections-topic)
  ::
  ?=(~ [fil]:.^(arch %cy pax))
::
++  new-comment
  |=  {col/time top/@da com/@da}
  ^-  ?
  =/  pax
    :(weld base-path (dray /[%da]/[%da]/[%da] col top com) /collections-comment)
  ::
  ?=(~ [fil]:.^(arch %cy pax))
--
