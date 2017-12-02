::  /app/collection/hoon
::
/-  hall
/+  hall
::
::TODO  nom/term too ambiguous, use col/term where applicable.
::
=>  |%
    ++  state                                           ::
      $:  cols/(map term collection)                    ::  collections by name
      ==                                                ::
    ++  collection                                      ::
      $:  conf/config                                   ::  configuration
          coms/(set term)                               ::  comment circles
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
              ses/(set ship)                            ::  black/whitelist
              vis/?                                     ::  visible or hidden
            ==                                          ::
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
  [~ +>]
::
++  poke-collections-action
  |=  act/action
  ^-  (quip move _+>)
  ?.  (team:title our.bol src.bol)  [~ +>]
  ?-  -.act
      $create
    =+  nom=(sane-cord des.act)
    =^  mos  +>.$
      =<  ta-done
      =-  (ta-change-config:ta nom - %coll)
      [des.act pub.act vis.act ses.act]
    =-  [[- mos] +>.$]
    :*  0
        %peer
        /hall/[nom]
        [our.bol %hall]
        /circle/(make-circle nom ~)/config-l
    ==
  ::
      $delete
    [~ +>]
    ::TODO  - delete files
    ::      - unsubscribe from the thing
    ::      - send delete action to hall
    ::      - remove from state
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
  ++  ta-change-comment
    |=  {nom/term top/term num/@ud wat/wain}
    ^+  +>
    =+  old=(some *comment)  ::TODO  get existing comment
    %-  ta-write-comment
    :^  nom  top  num
    ::?~  old
    ::  [src.bol now.bol now.bol wat]
    u.old(wat wat, wed now.bol)
  ::
  ++  ta-write-comment
    |=  {nom/term top/term num/@ud com/comment}
    ^+  +>
    %-  ta-emit
    =+  mun=(scot %ud num)
    =/  paf/path
      %+  en-beam:format  [our.bol %home da+now.bol]
      (flop /web/collections/[nom]/[top]/[mun]/collections-comment)
    :*  ost.bol
        %info
        /comment/[nom]/[top]/[mun]
        our.bol
        (foal:space:userlib paf [%collections-comment !>(com)])
    ==
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
      (ta-hall-create nom new)
    ::  update config in state.
    =.  cols  (~(put by cols) nom u.ole(conf new))
    ::  if we got it from file, update config in hall.
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
  ++  ta-write-config
    |=  {nom/term cof/config}
    ^+  +>
    %-  ta-emit
    =/  paf/path
      %+  en-beam:format  [our.bol %home da+now.bol]
      (flop /web/collections/[nom]/collections-config)
    :*  ost.bol
        %info
        /config/[nom]
        our.bol
        (foal:space:userlib paf [%collections-config !>(cof)])
    ==
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
  ++  ta-hall-create
    |=  {nom/term cof/config}
    ^+  +>
    =+  nam=(make-circle nom ~)
    =.  +>.$
      %-  ta-hall-action
      [%create nam desc.cof ?:(publ.cof %journal %village)]
    =?  +>.$  visi.cof
      (ta-hall-set-visible nam &)
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
  |=  {n/term t/(unit term)}
  ^-  term
  ;:  (cury cat 3)
    %collection--
    n
    ?~(t %$ '--')
    ?~(t %$ u.t)
  ==
::
++  read-config
  |=  nom/term
  .^  config
      %cx
      ::TODO  current desk, not always %home!
      /(scot %p our.bol)/home/(scot %da now.bol)/web/collections/[nom]/collections-config
  ==
--
