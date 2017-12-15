::  /app/collection/hoon
::
/-  hall
/+  hall ::, rekey
:: /=  cols
::   /:  /===/web/collections
::   /_  /.  /=  conf  /coll-config/
::           /=  tops
::         /;  (rekey %da)
::         /_  /.  /coll-topic/
::                 /=  comt
::               /;  (rekey %da)
::               /_  /coll-comment/
::       ==    ==
::
::    things to keep in sync, unique by date:
::  collections: state, files,       
::  topics:      state, files, notify
::  comments:    state, files, notify
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
    ++  state                                           ::
      $:  cols/(map time collection)                    ::  collections by name
      ==                                                ::
    ++  collection                                      ::
      $:  conf/config                                   ::  configuration
          tops/(map @da topic)                          ::  parent-level content
      ==                                                ::
    ++  topic                                           ::
      $:  tit/cord                                      ::  title
          comment                                       ::
      ==                                                ::
    ++  comment                                         ::
      $:  who/ship                                      ::  author
          wed/@da                                       ::  editted
          wat/wain                                      ::  content
      ==                                                ::
    ++  config                                          ::
      $:  desc/cord                                     ::  description
          publ/?                                        ::  public or private
          visi/?                                        ::  visible or hidden
          mems/(set ship)                               ::  ships on list
      ==                                                ::
    ++  action                                          ::
      $%  $:  $create                                   ::  create a collection
              wat/kind                                  ::  collection kind
              des/cord                                  ::  name
              pub/?                                     ::  public or private
              vis/?                                     ::  visible or hidden
              ses/(set ship)                            ::  black/whitelist
          ==                                            ::
          ::TODO  probably want to specify @da here too.
          {$submit col/time tit/cord wat/wain}          ::  submit a post/note
          {$comment col/time top/@da com/@da wat/wain}  ::  submit a comment
          {$delete col/time}                            ::  delete a collection
      ==                                                ::
    ++  kind  ?($blog $fora $note)                      ::
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
|_  {bol/bowl:gall state}
::
++  prep                                                ::<  prepare state
  ::>  adapts state.
  ::
  |=  old/(unit *)
  ^-  (quip move _..prep)
  ::?~  old
    [~ ..prep]  ::TODO  init, start clay subs
  ::[~ ..prep(+<+ u.old)]
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
  |=  act/action  ^-  ?
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
  |=  act/action
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
    |=  {wat/kind cof/config}
    ^+  +>
    ::XX unhandled kind
    (ta-change-config now.bol cof %poke)
  ::
  ++  ta-submit
    |=  {col/time tit/cord wat/wain}
    =/  top/topic  [tit src.bol now.bol wat]
    (ta-change-topic col now.bol top %poke)
  ::
  ++  ta-comment
    |=  {col/time top/@da com/@da wat/wain}
    ^+  +>
    ?.  (~(has by cols) col)  +>.$
    =/  cos=collection  (~(got by cols) col)
    ?.  (~(has by tops.cos) top)  +>.$
    =/  old/comment
      %+  fall  (get-comment col top com)
      [src.bol now.bol wat]
    ?.  =(who.old src.bol)  +>.$  :: error?
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
  ::  %applying-changes
  ::
  ++  ta-change-config
    |=  {col/time new/config src/?($file $poke)}
    ^+  +>
    ::
    ::REVIEW I think clay writes are idempotent?
    ::  if not changed on disk, update the file.
    =?  +>  !?=($file src)  
      (ta-write-config col new)
    =+  ole=(~(get by cols) col)
    ::  if we don't have it yet, add to state and hall.
    ?~  ole
      =.  cols  (~(put by cols) col new ~)
      (ta-hall-create col new)
    =/  old  conf.u.ole
    ::  make sure publ stays unchanged.
    =.  +>.$
      ?:  =(publ.new publ.old)  +>.$
      =.  publ.new  publ.old
      (ta-write-config col new)
    ::  update config in state.
    =.  cols  (~(put by cols) col u.ole(conf new))
    ::  update config in hall.
    =/  nam  (circle-for col)
    %-  ta-hall-actions  :~
      ?:  =(desc.old desc.new)  ~
      [%depict nam desc.new]
    ::
      ?:  =(visi.old visi.new)  ~
      [%public visi.new our.bol nam]
    ::
      (hall-permit nam | (~(dif in mems.old) mems.new))
      (hall-permit nam & (~(dif in mems.new) mems.old))
    ==
  ::
  ++  ta-change-topic
    |=  {col/time wen/@da top/topic src/?($file $poke)}
    ^+  +>
    =/  old  (get-topic col wen)
    ::  only original poster and host can edit.
    ?.  |(?=(~ old) =(who.u.old src.bol) ?=($file src))  +>.$
    :: 
    ::REVIEW this was just set in ta-submit?
    =?  who.top  ?=($poke src)  src.bol   ::  ensure legit author
    =.  wed.top  now.bol                  ::  change last edit date
    ::  store in state
    ~|  ~(key by cols)
    =/  cos  (~(got by cols) col)
    =.  tops.cos  (~(put by tops.cos) wen top)
    =.  cols  (~(put by cols) col cos)
    ::
    =/  new  =(~ old)
    =?  +>.$  new
      (ta-hall-create-topic col wen conf.cos)
    =.  +>.$  (ta-write-topic col wen top)
    (ta-hall-notify col wen ~ new wat.top)
  ::
  ::REVIEW never called
  ::++  ta-change-comment
  ::  |=  {col/time top/@da com/comment src/?($file $poke)}
  ::  ^+  +>
  ::  =/  old  (get-comment col top wen.com)
  ::  ::  only original poster and host can edit.
  ::  ?.  |(?=(~ old) =(who.u.old src.bol) ?=($file src))
  ::    +>.$
  ::  =?  who.com  ?=($poke src)  src.bol  ::  ensure legit author
  ::  =.  wed.com  now.bol                 ::  change last edit date.
  ::  ::
  ::  =.  +>.$  (ta-write-comment col top com)
  ::  (ta-hall-notify col top `wen.com =(~ old) wat.com)
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
++  base-path  (en-beam:format byk.bol(r da+now.bol) /collections/web)
::
++  get-config
  |=  col/time
  ^-  (unit config)
  =/  pax  :(weld base-path (dray /[%da] col) /collections-config)
  ::
  ?~  (file pax)  ~
  `.^(config %cx pax)
::
++  get-topic
  |=  {col/time top/@da}
  ^-  (unit topic)
  =/  pax  :(weld base-path (dray /[%da]/[%da] col top) /collections-topic)
  ::
  ?~  (file pax)  ~
  `.^(topic %cx pax)
::
++  get-comment
  |=  {col/time top/@da com/@da}
  ^-  (unit comment)
  =/  pax
    :(weld base-path (dray /[%da]/[%da]/[%da] col top com) /collections-comment)
  ::
  ?~  (file pax)  ~
  `.^(comment %cx pax)
--
