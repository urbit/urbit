::  /app/collection/hoon
::
/-  hall
/+  hall
::
::  collections: state, files, hall
::  topics:      state, files, hall, notify
::  comments:    s a e, files,       notify
::
::  /web/collections/my-coll.config
::  /web/collections/my-coll/some.topic
::  /web/collections/my-coll/some/1.comment
::
::TODO  nom/term too ambiguous, use col/term where applicable.
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
          {$submit nom/term tit/cord wat/wain}          ::  submit a post/note
          {$comment nom/term top/@da com/@da wat/wain}  ::  submit a comment
          {$delete nom/term}                            ::  delete a collection
      ==
    ++  kind  ?($blog $fora $note)
    ++  command
      $%  {$write wer/term wat/wain}  ::TODO  parent? title? etc.
      ==
    ::
    ++  move  (pair bone card)                          ::<  all actions
    ++  lime                                            ::>  diff fruit
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
          {$quit $~}                                    ::
      ==                                                ::
    --
::
|_  {bol/bowl:gall state}
::
::TODO  ++prep watch %desk
::
++  poke-noun
  |=  a/@
  ^-  (quip move _+>)
  ~&  %poked
  ta-done:(ta-write-config:ta %test ['a description' & & [~palzod ~ ~]])
::
++  poke-collections-action
  |=  act/action
  ^-  (quip move _+>)
  ?.  (team:title our.bol src.bol)  [~ +>]
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
++  diff-filesystem  ::TODO  ?
  |=  {wir/wire *}
  ^-  (quip move _+>)
  [~ +>]
  ::TODO  - extract nom from path
  ::      - determine if config file or content
  ::        - if config: determine changes, then send to hall for nom and comms
  ::        - if content: determine if comment or op, then
  ::                      post notification, noting whether new or changed.
  ::                      if op, also create comments channel (and add to state)
  ::
  ::  /web/collections/
  ::  /web/collections/my-blog.config
  ::  /web/collections/my-blog/post-title.txt
  ::  /web/collections/my-blog/post-title/1.txt   or:
  ::  /web/collections/my-blog/post-title/~2017.12.01..12.34.56..abcd.txt
  ::
  ::  %collections--my-blog
  ::  %collections--my-blog--post-title
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
    :^  ost.bol  %poke  /  ::TODO  and handle returns correctly.
    :+  [our.bol %hall]  %hall-action
    act
  ::
  ::  %performing-actions
  ::
  ++  ta-create
    |=  {wat/kind des/cord pub/? vis/? ses/(set ship)}
    ^+  +>
    =+  nom=(sane-cord des)
    =.  +>.$
      =-  (ta-change-config nom - %coll)
      [des pub vis ses]
    %-  ta-emit
    :*  0
        %peer
        /hall/[nom]
        [our.bol %hall]
        /circle/(make-circle nom ~)/config-l
    ==
  ::
  ++  ta-submit
    |=  {nom/term tit/cord wat/wain}
    %+  ta-change-topic  nom
    [[tit src.bol now.bol now.bol wat] %coll]
  ::
  ++  ta-comment
    |=  {nom/term top/@da com/@da wat/wain}
    ^+  +>
    =+  col=(~(get by cols) nom)
    ?~  col  +>.$
    =+  tob=(~(get by tops.u.col) top)
    ?~  tob  +>.$
    =+  old=(get-comment nom top com)
    ?~  old
      %^  ta-write-comment  nom  top
      [src.bol now.bol now.bol wat]
    ?.  =(who.u.old src.bol)  +>.$
    %^  ta-write-comment  nom  top
    u.old(wat wat, wed now.bol)

  ::
  ++  ta-delete
    |=  nom/term
    ^+  +>
    +>
    ::TODO  - delete files
    ::      - unsubscribe from the thing
    ::      - send delete action to hall
    ::      - remove from state
  ::
  ::  %applying-changes
  ::
  ++  ta-apply-config-diff
    |=  {nom/term dif/diff-config:hall}
    ^+  +>
    +>
    ::TODO  find error?
    ::=-  (ta-change-config nom - %hall)
    ::=+  cof=conf:(~(got by cols) nom)
    ::?+  -.dif  ta
    ::    $caption
    ::  cof(desc cap.dif)
    ::::
    ::    $permit
    ::  cof  ::TODO
    ::::
    ::    $remove
    ::  cof  ::TODO/REVIEW  ignore/recreate? *don't* remove files.
    ::==
  ::
  ++  ta-change-config
    |=  {nom/term new/config src/?($file $hall $coll)}
    ^+  +>
    ::  if not changed on disk, update the file.
    =?  +>  !?=($file src)
      (ta-write-config nom new)
    =+  ole=(~(get by cols) nom)
    ::  if we don't have it yet, add to state and hall.
    ?~  ole
      =.  cols  (~(put by cols) nom new ~)
      (ta-hall-create nom ~ new)
    ::  make sure publ stays unchanged.
    =.  +>.$
      ?:  =(publ.conf.u.ole publ.new)  +>.$
      =.  new  new(publ publ.conf.u.ole)
      (ta-write-config nom new)
    ::  update config in state.
    =.  cols  (~(put by cols) nom u.ole(conf new))
    ::  update config in hall.
    =+  dif=(ta-config-diff conf.u.ole new)
    =?  +>.$  ?=(^ des.dif)
      (ta-hall-set-description nom u.des.dif)
    =?  +>.$  ?=(^ vis.dif)
      (ta-hall-set-visible nom u.vis.dif)
    ::TODO  do below for all coms as well.
    =?  +>.$  ?=(^ rem.dif)
      (ta-hall-set-permissions nom | rem.dif)
    =?  +>.$  ?=(^ add.dif)
      (ta-hall-set-permissions nom & add.dif)
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
    |=  {nom/term top/topic src/?($file $coll)}
    ^+  +>
    =+  old=(get-topic nom wen.top)
    ::  only original poster and host can edit.
    ?.  |(?=($~ old) =(who.u.old src.bol) ?=($file src))  +>.$
    ::  ensure legit author.
    =?  top  ?=($coll src)
      top(who src.bol)
    ::  change last edit date.
    =.  top  top(wed now.bol)
    ::  store in state.
    =.  cols
      %+  ~(put by cols)  nom
      =+  col=(~(got by cols) nom)
      col(tops (~(put by tops.col) wen.top top))
    =+  new=?=($~ old)
    =?  +>.$  new
      (ta-hall-create nom `wen.top conf:(~(got by cols) nom))
    =.  +>.$
      (ta-write-topic nom top)
    (ta-hall-notify nom wen.top ~ new wat.top)
  ::
  ++  ta-change-comment
    |=  {nom/term top/@da com/comment src/?($file $coll)}
    ^+  +>
    =+  old=(get-comment nom top wen.com)
    ::  only original poster and host can edit.
    ?.  |(?=($~ old) =(who.u.old src.bol) ?=($file src))
      +>.$
    ::  ensure legit author.
    =?  com  ?=($coll src)
      com(who src.bol)
    ::  change last edit date.
    =.  com  com(wed now.bol)
    =.  +>.$
      (ta-write-comment nom top com)
    (ta-hall-notify nom top `wen.com ?=($~ old) wat.com)
  ::
  ::  %writing-files
  ::
  ++  ta-write-config
    |=  {nom/term cof/config}
    ^+  +>
    %-  ta-emit
    =/  paf/path
      %-  make-path
      /[nom]/collections-config
    :*  ost.bol
        %info
        /config/[nom]
        our.bol
        (foal:space:userlib paf [%collections-config !>(cof)])
    ==
  ::
  ++  ta-write-topic
    |=  {nom/term top/topic}
    ^+  +>
    =+  wan=(scot %da wen.top)
    =+  pax=(make-path /[nom]/[wan]/collections-topic)
    %-  ta-emit
    :*  ost.bol
        %info
        /topic/[nom]/[wan]
        our.bol
        (foal:space:userlib pax [%collections-topic !>(top)])
    ==
  ::
  ++  ta-write-comment
    |=  {nom/term top/@da com/comment}
    ^+  +>
    =+  tap=(scot %da top)
    =+  wan=(scot %da wen.com)
    =/  pax/path
      %-  make-path
      /[nom]/[tap]/[wan]/collections-comment
    %-  ta-emit
    :*  ost.bol
        %info
        /comment/[nom]/[tap]/[wan]
        our.bol
        (foal:space:userlib pax [%collections-comment !>(com)])
    ==
  ::
  ::  %hall-changes
  ::
  ++  ta-hall-create
    |=  {nom/term top/(unit @da) cof/config}
    ^+  +>
    =+  nam=(make-circle nom top)
    =.  +>.$
      %-  ta-hall-action
      [%create nam desc.cof ?:(publ.cof %journal %village)]
    ::TODO  if ?~ top, sub to config changes
    =?  +>.$  visi.cof
      (ta-hall-set-visible nam &)
    =?  +>.$  ?=(^ top)
      %-  ta-hall-action
      ::NOTE  %source also subs to local config & presence, but
      ::      that generally won't result in visible notifications.
      :^  %source  (make-circle nom ~)  &
      [`source:hall`[[our.bol nam] ~] ~ ~]
    ?~  mems.cof  +>.$
    (ta-hall-set-permissions nam & mems.cof)
  ::
  ++  ta-hall-set-description
    |=  {nom/term des/cord}
    ^+  +>
    %-  ta-hall-action
    [%depict (make-circle nom ~) des]
  ::
  ++  ta-hall-set-visible
    |=  {nom/term vis/?}
    ^+  +>
    %-  ta-hall-action
    [%public vis our.bol (make-circle nom ~)]
  ::
  ++  ta-hall-set-permissions
    |=  {nom/term inv/? sis/(set ship)}
    ^+  +>
    %-  ta-hall-action
    [%permit (make-circle nom ~) inv sis]
  ::
  ++  ta-hall-notify
    |=  {nom/term top/@da com/(unit @da) new/? wat/wain}
    ^+  +>
    %-  ta-hall-action
    =-  :+  %phrase  [[our.bol tar] ~ ~]
        [%fat [%text wat] [%lin | msg]]~
    ^-  {tar/naem:hall msg/cord}
    ::TODO
    [(make-circle nom ~) 'TODO']
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
  |=  nom/term
  ^-  (unit config)
  =/  pax
    %-  make-path
    /[nom]/collections-config
  ?.  (has-file pax)  ~
  `.^(config %cx pax)
::
++  get-topic
  |=  {nom/term top/@da}
  ^-  (unit topic)
  =/  pax
    %-  make-path
    /[nom]/(scot %da top)/collections-topic
  ?.  (has-file pax)  ~
  `.^(topic %cx pax)
::
++  get-comment
  |=  {nom/term top/@da wen/@da}
  ^-  (unit comment)
  =/  pax
    %-  make-path
    /[nom]/(scot %da top)/(scot %da wen)/collections-comment
  ?.  (has-file pax)  ~
  `.^(comment %cx pax)
--
