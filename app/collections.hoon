::  /app/collection/hoon
::
/-  hall
/+  hall
::
::    things to keep in sync:
::  collections: state, files, hall          unique by name
::  topics:      state, files, hall, notify  unique by date
::  comments:    s a e, files,       notify  unique by date/number?
::
::    filepaths:
::  /web/collections/my-coll.config
::  /web/collections/my-coll/some.topic
::  /web/collections/my-coll/some/1.comment
::
::    notification circles:
::  %collections--my-blog               new/changed post notifications
::  %collections--my-blog--post-title   new/changed comments notifications
::
=>  |%
    ++  state                                           ::
      $:  cols/(map term collection)                    ::  collections by name
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
          wen/@da                                       ::  created
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
          {$submit col/term tit/cord wat/wain}          ::  submit a post/note
          {$comment col/term top/@da com/@da wat/wain}  ::  submit a comment
          {$delete col/term}                            ::  delete a collection
      ==                                                ::
    ++  kind  ?($blog $fora $note)                      ::
    ++  move  (pair bone card)                          ::  all actions
    ++  lime                                            ::  diff fruit
      $%  {$hall-prize prize:hall}                      ::
          {$hall-rumor rumor:hall}                      ::
      ==                                                ::
    ++  poke                                            ::
      $%  {$hall-action action:hall}                    ::
      ==                                                ::
    ++  card                                            ::
      $%  {$diff lime}                                  ::
          {$info wire ship term nori:clay}              ::
          {$peer wire dock path}                        ::
          {$poke wire dock poke}                        ::
          {$pull wire dock $~}                          ::
          {$warp wire sock riff:clay}                   ::
          {$quit $~}                                    ::
      ==                                                ::
    --
::
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
  |=  a/@
  ^-  (quip move _+>)
  ~&  %poked
  ta-done:(ta-write-config:ta %test ['a description' & & [~palzod ~ ~]])
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
++  poke-collections-action
  |=  act/action
  ^-  (quip move _+>)
  ?.  ?-  -.act
          ?($create $delete)
        (team:title our.bol src.bol)
      ::
          ?($submit $comment)
        =+  col=(~(get by cols) nom.act)
        ?~  col  |
        ?!  .=  publ.conf.u.col
        (~(has in mems.conf.u.col) src.bol)
      ==
    [~ +>]
  =<  ta-done
  ?-  -.act
    $create   (ta-create:ta +.act)
    $submit   (ta-submit:ta +.act)
    $comment  (ta-comment:ta +.act)
    $delete   (ta-delete:ta +.act)
  ==
::
++  diff-hall-prize
  |=  {wir/wire piz/prize:hall}
  ^-  (quip move _+>)
  [~ +>]
::
++  diff-hall-rumor
  |=  {wir/wire rum/rumor:hall}
  ^-  (quip move _+>)
  ?>  ?=({$hall @tas $~} wir)
  =+  nom=i.t.wir
  ?>  ?=({$circle $config *} rum)
  ta-done:(ta-apply-config-diff:ta nom dif.rum.rum)
::
++  ta
  |_  moves/(list move)
  ::
  ++  ta-done
    [(flop moves) +>]
  ::
  ++  ta-emit
    |=  mov/move
    %_(+> moves [mov moves])
  ::
  ++  ta-emil
    |=  mos/(list move)
    %_(+> moves (welp (flop mos) moves))
  ::
  ++  ta-hall-action
    |=  act/action:hall
    %-  ta-emit
    :^  ost.bol  %poke  /  ::TODO  wire, handle ++coup.
    :+  [our.bol %hall]  %hall-action
    act
  ::
  ::  %performing-actions
  ::
  ++  ta-create
    |=  {wat/kind des/cord pub/? vis/? ses/(set ship)}
    ^+  +>
    =+  nom=(sane-cord des)
    =-  (ta-change-config nom - %coll)
    [des pub vis ses]
  ::
  ++  ta-submit
    |=  {col/term tit/cord wat/wain}
    %+  ta-change-topic  col
    [[tit src.bol now.bol now.bol wat] %coll]
  ::
  ++  ta-comment
    |=  {col/term top/@da com/@da wat/wain}
    ^+  +>
    =+  col=(~(get by cols) col)
    ?~  col  +>.$
    =+  tob=(~(get by tops.u.col) top)
    ?~  tob  +>.$
    =+  old=(get-comment col top com)
    ?~  old
      %^  ta-write-comment  col  top
      [src.bol now.bol now.bol wat]
    ?.  =(who.u.old src.bol)  +>.$
    %^  ta-write-comment  col  top
    u.old(wat wat, wed now.bol)
  ::
  ++  ta-delete
    |=  col/term
    ^+  +>
    +>
    ::TODO  - delete files
    ::      - unsubscribe from clay
    ::      - unsubscribe from hall
    ::      - send delete action to hall
    ::      - remove from state
  ::
  ::  %applying-changes
  ::
  ++  ta-apply-config-diff
    |=  {col/term dif/diff-config:hall}
    ^+  +>
    =+  cof=conf:(~(got by cols) col)
    =;  new/(unit config)
      ?~  new  +>.$
      (ta-change-config col u.new %hall)
    ?+  -.dif  ~
        $caption
      `cof(desc cap.dif)
    ::
        $permit
      %-  some
      %_  cof
          mems
        %.  mems.cof
        ?:  add.dif
          ~(dif in sis.dif)
        ~(int in sis.dif)
      ==
    ::
        $remove
      `cof  ::TODO/REVIEW  ignore/recreate? *don't* remove files.
    ==
  ::
  ++  ta-change-config
    |=  {col/term new/config src/?($file $hall $coll)}
    ^+  +>
    ::  if not changed on disk, update the file.
    =?  +>  !?=($file src)
      (ta-write-config col new)
    =+  ole=(~(get by cols) col)
    ::  if we don't have it yet, add to state and hall.
    ?~  ole
      =.  cols  (~(put by cols) col new ~)
      (ta-hall-create col ~ new)
    ::  make sure publ stays unchanged.
    =.  +>.$
      ?:  =(publ.conf.u.ole publ.new)  +>.$
      =.  new  new(publ publ.conf.u.ole)
      (ta-write-config col new)
    ::  update config in state.
    =.  cols  (~(put by cols) col u.ole(conf new))
    ::  update config in hall.
    =+  dif=(ta-config-diff conf.u.ole new)
    =?  +>.$  ?=(^ des.dif)
      (ta-hall-set-description col u.des.dif)
    =?  +>.$  ?=(^ vis.dif)
      (ta-hall-set-visible col u.vis.dif)
    ::TODO  do below for all topic circles as well.
    =?  +>.$  ?=(^ rem.dif)
      (ta-hall-set-permissions col | rem.dif)
    =?  +>.$  ?=(^ add.dif)
      (ta-hall-set-permissions col & add.dif)
    +>.$
  ::
  ++  ta-config-diff
    |=  {old/config new/config}
    ^-  $:  des/(unit cord)
            vis/(unit ?)
            rem/(set ship)
            add/(set ship)
        ==
    :+  ?:  =(desc.old desc.new)  ~
        `desc.new
      ?:  =(visi.old visi.new)  ~
      `visi.new
    :-  (~(dif in mems.old) mems.new)
        (~(dif in mems.new) mems.old)
  ::
  ++  ta-change-topic
    |=  {col/term top/topic src/?($file $coll)}
    ^+  +>
    =+  old=(get-topic col wen.top)
    ::  only original poster and host can edit.
    ?.  |(?=($~ old) =(who.u.old src.bol) ?=($file src))  +>.$
    ::  ensure legit author.
    =?  top  ?=($coll src)
      top(who src.bol)
    ::  change last edit date.
    =.  top  top(wed now.bol)
    ::  store in state.
    =.  cols
      %+  ~(put by cols)  col
      =+  col=(~(got by cols) col)
      col(tops (~(put by tops.col) wen.top top))
    =+  new=?=($~ old)
    =?  +>.$  new
      (ta-hall-create col `wen.top conf:(~(got by cols) col))
    =.  +>.$
      (ta-write-topic col top)
    (ta-hall-notify col wen.top ~ new wat.top)
  ::
  ++  ta-change-comment
    |=  {col/term top/@da com/comment src/?($file $coll)}
    ^+  +>
    =+  old=(get-comment col top wen.com)
    ::  only original poster and host can edit.
    ?.  |(?=($~ old) =(who.u.old src.bol) ?=($file src))
      +>.$
    ::  ensure legit author.
    =?  com  ?=($coll src)
      com(who src.bol)
    ::  change last edit date.
    =.  com  com(wed now.bol)
    =.  +>.$
      (ta-write-comment col top com)
    (ta-hall-notify col top `wen.com ?=($~ old) wat.com)
  ::
  ::  %writing-files
  ::
  ++  ta-write-config
    |=  {col/term cof/config}
    ^+  +>
    %-  ta-emit
    =/  paf/path
      %-  make-path
      /[col]/collections-config
    :*  ost.bol
        %info
        /config/[col]
        our.bol
        (foal:space:userlib paf [%collections-config !>(cof)])
    ==
  ::
  ++  ta-write-topic
    |=  {col/term top/topic}
    ^+  +>
    =+  wan=(scot %da wen.top)
    =+  pax=(make-path /[col]/[wan]/collections-topic)
    %-  ta-emit
    :*  ost.bol
        %info
        /topic/[col]/[wan]
        our.bol
        (foal:space:userlib pax [%collections-topic !>(top)])
    ==
  ::
  ++  ta-write-comment
    |=  {col/term top/@da com/comment}
    ^+  +>
    =+  tap=(scot %da top)
    =+  wan=(scot %da wen.com)
    =/  pax/path
      %-  make-path
      /[col]/[tap]/[wan]/collections-comment
    %-  ta-emit
    :*  ost.bol
        %info
        /comment/[col]/[tap]/[wan]
        our.bol
        (foal:space:userlib pax [%collections-comment !>(com)])
    ==
  ::
  ::  %hall-changes
  ::
  ++  ta-hall-create
    |=  {col/term top/(unit @da) cof/config}
    ^+  +>
    =+  nam=(make-circle col top)
    =.  +>.$
      %-  ta-hall-action
      [%create nam desc.cof ?:(publ.cof %journal %village)]
    =?  +>.$  ?=($~ top)
      %-  ta-emit
      :*  0
          %peer
          /hall/[col]
          [our.bol %hall]
          /circle/[nam]/config-l
      ==
    =?  +>.$  visi.cof
      (ta-hall-set-visible nam &)
    =?  +>.$  ?=(^ top)
      %-  ta-hall-action
      ::NOTE  %source also subs to local config & presence, but
      ::      that generally won't result in visible notifications.
      :^  %source  (make-circle col ~)  &
      [`source:hall`[[our.bol nam] ~] ~ ~]
    ?~  mems.cof  +>.$
    (ta-hall-set-permissions nam & mems.cof)
  ::
  ++  ta-hall-set-description
    |=  {col/term des/cord}
    ^+  +>
    %-  ta-hall-action
    [%depict (make-circle col ~) des]
  ::
  ++  ta-hall-set-visible
    |=  {col/term vis/?}
    ^+  +>
    %-  ta-hall-action
    [%public vis our.bol (make-circle col ~)]
  ::
  ++  ta-hall-set-permissions
    |=  {col/term inv/? sis/(set ship)}
    ^+  +>
    %-  ta-hall-action
    [%permit (make-circle col ~) inv sis]
  ::
  ++  ta-hall-notify
    |=  {col/term top/@da com/(unit @da) new/? wat/wain}
    ^+  +>
    %-  ta-hall-action
    =-  :+  %phrase  [[our.bol tar] ~ ~]
        [%fat [%text wat] [%lin | msg]]~
    ^-  {tar/naem:hall msg/cord}
    ::TODO
    [(make-circle col ~) 'TODO']
  --
::
++  sane-cord
  |=  c/cord
  ^-  term
  %-  crip
  %-  zing
  %+  scan  (cass (trip c))
  =+  val=;~(pose low nud)
  =+  inv=(plus ;~(less ;~(pose low nud) next))
  %+  ifix  [(star inv) (star inv)]
  %-  star  ;~  pose
    ;~(plug val (easy ~))
    ;~(plug (cold '-' inv) val (easy ~))
  ==
::
++  make-circle
  |=  {n/term t/(unit @da)}
  ^-  term
  ;:  (cury cat 3)
    %collection--
    n
    ?~(t %$ '--')
    ?~(t %$ (sane-cord (scot %da u.t)))
  ==
::
++  beak-now
  byk.bol(r [%da now.bol])
::
++  make-path
  |=  pax/path
  :(welp (en-beam:format beak-now ~) /web/collections pax)
::
++  has-file
  |=  pax/path
  ::NOTE  %cu not implemented, so we use %cy instead.
  =-  ?=(^ fil.-)
  .^(arch %cy pax)
::
++  get-config
  |=  col/term
  ^-  (unit config)
  =/  pax
    %-  make-path
    /[col]/collections-config
  ?.  (has-file pax)  ~
  `.^(config %cx pax)
::
++  get-topic
  |=  {col/term top/@da}
  ^-  (unit topic)
  =/  pax
    %-  make-path
    /[col]/(scot %da top)/collections-topic
  ?.  (has-file pax)  ~
  `.^(topic %cx pax)
::
++  get-comment
  |=  {col/term top/@da wen/@da}
  ^-  (unit comment)
  =/  pax
    %-  make-path
    /[col]/(scot %da top)/(scot %da wen)/collections-comment
  ?.  (has-file pax)  ~
  `.^(comment %cx pax)
--
