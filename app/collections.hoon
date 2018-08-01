::  /app/collections/hoon
::
/?  309
/-  hall
/+  collections
/=  cols
  /^  collection:collections
  /;  |=  a=(map knot item:collections)
      [*config:collections a]
  /:  /===/web/collections  /_  /collections-item/
::
=,  collections
=,  space:userlib
::
|%
+=  move  [bone card]
+=  card
  $%  [%info wire ship toro:clay]
      [%poke wire dock poke]
      [%perm wire ship desk path rite:clay]
  ==
+=  poke
  $%  [%hall-action action:hall]
      [%collections-action action:collections]
      [%json json]
  ==
--
::
::::
  ::
|_  [bol=bowl:gall state=collection]
::
++  this  .
::
::::
++  prep
  |=  old=(unit *)
  ^-  (quip move _this)
  ?~  old
    =<  ta-done
    (ta-hall-create-circle:ta /c 'collections')
  =/  old-col  ((soft collection) u.old)
  ?~  old-col
    [~ this(state cols)]
  =^  mow  this
    =<  ta-done
    (ta-update:ta u.old-col)
  [mow this(state cols)]
::
::  mack: acknowledgement for permissions
::
++  mack
  |=  [wir=wire err=(unit tang)]
  ^-  (quip move _this)
  ?~  err
    [~ this]
  (mean u.err)
::::::
::  utilities
::
++  base-spur  `spur`/web/collections
++  base-beam  `beam`[byk.bol (flop base-spur)]
++  base-path  `path`(en-beam:format base-beam)
::
++  path-to-circle
  |=  pax=path
  ^-  circle:hall
  =.  pax
    ?:  ?=([%web %collections *] pax)
      (weld /c (slag 2 `path`pax))
    ?:  ?=([%collections *] pax)
      (weld /c (slag 1 `path`pax))
    ?:  ?=([%c *] pax)
      `path`pax
    `path`(weld /c pax)
  =/  nam=term
  %+  roll  `(list @ta)`pax
  |=  [seg=@ta out=term]
  %^  cat  3
    ?:(=(%$ out) out (cat 3 out '-'))
    ((hard @tas) seg)
  [our.bol nam]
::
++  allowed-by
  |=  [who=@p dic=dict:clay]
  =/  in-list=?
    ?|  (~(has in p.who.rul.dic) who)
      ::
        %-  ~(rep by q.who.rul.dic)
        |=  [[@ta cru=crew:clay] out=_|]
        ?:  out  &
        (~(has in cru) who)
    ==
  ?:  =(%black mod.rul.dic)
    !in-list
  in-list
::
++  collection-notify
  |=  [pax=path conf=config]
  ^-  json
  %-  pairs:enjs:format
  :~  ['owner' [%s (crip (scow %p owner.conf))]]
      ['path' [%a (turn pax |=(a=@ta `json`[%s a]))]]
      ['name' [%s name.conf]]
      ['date' [%s (crip (scow %da last-modified.conf))]]
      ['type' [%s type.conf]]
  ==
::
++  item-notify
  |=  [pax=path raw=raw-item]
  ^-  json
  =/  owner  (fall (~(get by meta.raw) %owner) ~.anon)
  =/  dat  (fall (~(get by meta.raw) %last-modified) (scot %da now.bol))
  =/  nom  (fall (~(get by meta.raw) %name) ~.no-title)
  =/  typ  (fall (~(get by meta.raw) %type) ~.no-type)
  %-  pairs:enjs:format
  :~  ['owner' [%s owner]]
      ['path' [%a (turn pax |=(a=@ta `json`[%s a]))]]
      ['name' [%s nom]]
      ['date' [%s dat]]
      ['type' [%s typ]]
      ['content' [%s data.raw]]
  ==
::
::
::
++  poke-noun
  |=  a=*
  ^-  (quip move _this)
::  =/  pax  ((hard path) a)
  =/  yo=item  (~(got by data.cols) ~.yoyo)
  ?>  ?=(%both -.yo)
  =/  new-front  (~(put by meta.raw.yo) %new-field ~.fuck-off)
  =/  new-body   (crip (weld (trip data.raw.yo) "new line\0a"))
  =/  new-contents  (update-umd-front new-front data.raw.yo)

  =/  act=action:collections
    :*  our.bol  %home
      :~  [%write /web/collections/yoyo/umd %umd new-body]
      ==
    ==
  =/  mow=move
    [ost.bol %poke /poke-act [our.bol %collections] %collections-action act]
  [[mow]~ this]
::
++  poke-collections-action
  |=  act=action:collections
  ^-  (quip move _this)
::  ~&  act
  ?.  =(who.act our.bol)
    ::
    ::  forward poke if its not meant for us
    :_  this
    :_  ~
    :*  ost.bol  %poke  
        /forward-collections-action  
        [who.act %collections]
        %collections-action  act
    ==
  ::
  ::  resolve %collection, %post, or %comment to %write action
  =.  acts.act
  %+  turn  acts.act
  |=  a=sub-action
  ^-  sub-action
  ?:  ?=(%collection -.a)
    =/  conf=config
      :*  [byk.bol (flop (weld pax.a /[name.a]/collections-config))]
          name.a
          desc.a
          our.bol
          now.bol
          now.bol
          type.a
          comments.a
          ~
          visible.a
      ==
    [%write (weld pax.a /[name.a]/collections-config) %collections-config conf]
  ::
  ?:  ?=(%post -.a)
    =/  front=(map knot cord)
      %-  my
      :~  [%name name.a]
          [%comments ?:(comments.a ~..y ~..n)]
          [%owner (scot %p src.bol)]
          [%date-created (scot %da now.bol)]
          [%last-modified (scot %da now.bol)]
          [%type type.a]
      ==
    [%write (weld pax.a /[name.a]/umd) %umd (update-umd-front front content.a)]
  ::
  ?:  ?=(%comment -.a)
    =/  dat  (scot %da now.bol)
    =/  front=(map knot cord)
      %-  my
      :~  [%owner (scot %p src.bol)]
          [%date-created dat]
          [%last-modified dat]
          [%type %comments]
      ==
    [%write (weld pax.a /[dat]/umd) %umd (update-umd-front front content.a)]
  ::
  a
  ::
  ::  if its from us no need to check permissions
  ?:  =(src.bol our.bol)
    ta-done:(ta-act:ta act)
  ::
  ::  if not from us, check if every action in the list is permitted
  ::  if even one isn't, don't reject the whole list
  =/  success=?
  %+  levy  acts.act
  |=  a=sub-action
  =/  pax  (path +<.a)
  ?+    -.a    !!
      %write  
    =/  perms  .^([dict:clay dict:clay] %cp pax)
    (allowed-by src.bol +.perms)
      %delete
    =/  perms  .^([dict:clay dict:clay] %cp pax)
    (allowed-by src.bol +.perms)
      %perms  |
  ==
  ::
  ?:  success
    ta-done:(ta-act:ta act)
  [~ this]  :: XX TODO: report failure
::
::::::
::::::
::
++  ta
  |_  moves=(list move)
  ::
  ::  core control
  ::
  ++  ta-this  .
  ++  ta-done  [(flop moves) this]
  ++  ta-emit
    |=  mov=move
    %_  ta-this
      moves  [mov moves]
    ==
  ++  ta-emil
    |=  mos=(list move)
    %_  ta-this
      moves  (welp (flop mos) moves)
    ==
  ::
  ::  interface
  ::
  ++  ta-act
    |=  act=action:collections
    ^+  ta-this
    |-
    ?~  acts.act  ta-this
    =*  a  i.acts.act
    =.  ta-this
    ?+    -.a
      !!
      %write   (ta-write pax.a `cage`[-.for.a !>(+.for.a)])
      %delete  (ta-remove pax.a)
      %perms   (ta-set-permissions pax.a r.a w.a)
    ==
    $(acts.act t.acts.act)
  ::
  ::  clay updates
  ::
  ++  ta-update
    |=  old=collection
    ^+  ta-this
::    ~&  old+old
::    ~&  new+cols
    ?:  =(old cols)
::      ~&  %no-update
      ta-this
    (ta-update-collection old cols /web/collections)
  ::
  ++  ta-insert-item
    |=  [new=item pax=path]
    ^+  ta-this
::    ~&  insert+[-.new pax]
    =/  parent-path  (scag (dec (lent pax)) pax)
    ::
    ?-    -.new
    ::
        %collection
      =.  ta-this
        (ta-hall-json parent-path 'new collection' (collection-notify pax meta.col.new))
      ::
      =.  ta-this  (ta-hall-create-circle pax description.meta.col.new)
      =/  items=(list [nom=@ta =item])  ~(tap by data.col.new)
      |-
      ?~  items  ta-this
      =.  ta-this  (ta-insert-item item.i.items (weld pax [nom.i.items ~]))
      $(items t.items)
    ::
        %both
      =.  ta-this  
        (ta-hall-json parent-path 'new collection' (collection-notify pax meta.col.new))
      =.  ta-this  
        (ta-hall-json parent-path 'new item' (item-notify pax raw.new))
      =.  ta-this  (ta-hall-create-circle pax description.meta.col.new)
      =/  items=(list [nom=@ta =item])  ~(tap by data.col.new)
      =.  ta-this
      |-
      ?~  items  ta-this
      =.  ta-this  (ta-insert-item item.i.items (weld pax [nom.i.items ~]))
      $(items t.items)
      ::
      ?:  ?&  (~(has by meta.raw.new) %comments)
              =('.y' (~(got by meta.raw.new) %comments))
          ==
        (ta-generate-comments pax)
      ta-this
    ::
        %raw
      =.  ta-this
        (ta-hall-json parent-path 'new item' (item-notify pax raw.new))
      ?:  ?&  (~(has by meta.raw.new) %comments)
              =('.y' (~(got by meta.raw.new) %comments))
          ==
        (ta-generate-comments pax)
      ta-this
    ::
    ==
  ::
  ++  ta-remove-item
    |=  [old=item pax=path]
    ^+  ta-this
::    ~&  remove+[-.old pax]
    ::  flush permissions
    ::  notify parent of deletion
    =/  parent  (scag (dec (lent pax)) pax)
    ::  recurse for children
    ?-    -.old
    ::
        %collection
      =.  ta-this  
        (ta-hall-json parent 'deleted collection' (collection-notify pax meta.col.old))
      =.  ta-this  (ta-flush-permissions (weld pax /collections-config))
      =/  items=(list [nom=@ta =item])  ~(tap by data.col.old)
      |-
      ?~  items  ta-this
      =.  ta-this  (ta-remove-item item.i.items (weld pax [nom.i.items ~]))
      $(items t.items)
    ::
        %both
      =.  ta-this  
        (ta-hall-json parent 'deleted collection' (collection-notify pax meta.col.old))
      =.  ta-this  
        (ta-hall-json parent 'deleted item' (item-notify pax raw.old))
      =.  ta-this  (ta-flush-permissions pax)
      =.  ta-this  (ta-flush-permissions (weld pax /collections-config))
      =/  items=(list [nom=@ta =item])  ~(tap by data.col.old)
      |-
      ?~  items  ta-this
      =.  ta-this  (ta-remove-item item.i.items (weld pax [nom.i.items ~]))
      $(items t.items)
    ::
        %raw
      =.  ta-this  (ta-flush-permissions pax)
      (ta-hall-json parent 'deleted item' (item-notify pax raw.old))
    ::
    ==
  ::
  ::
  :: 
  ++  ta-update-item
    ::  always make sure removals happen first and insertions happen last
    ::  because removals flush permissions and insertions set them
    ::
    |=  [old=item new=item pax=path]
    ^+  ta-this
::    ~&  update-item+[-.old -.new pax]
    ?:  =(old new)
      ta-this
    ::
    ::  check for changes in item type
    ?:  &(?=(%collection -.old) ?=(%collection -.new))
      (ta-update-collection col.old col.new pax)
    ?:  &(?=(%raw -.old) ?=(%raw -.new))
      (ta-update-raw-item raw.old raw.new pax)
    ?:  &(?=(%both -.old) ?=(%both -.new))
      ::  update raw item
      =.  ta-this  (ta-update-collection col.old col.new pax)
      (ta-update-raw-item raw.old raw.new pax)
    ::
    ?:  &(?=(%collection -.old) ?=(%raw -.new))
      :: remove collection
      :: insert raw item
      =.  ta-this  (ta-remove-item old pax)
      (ta-insert-item new pax)
    ::
    ?:  &(?=(%collection -.old) ?=(%both -.new))
      ::  insert raw item
      ::  update-collection
      =.  ta-this  (ta-update-collection col.old col.new pax)
      (ta-insert-item new pax)
    ::
    ?:  &(?=(%raw -.old) ?=(%collection -.new))
      :: remove raw item
      :: insert collection
      =.  ta-this  (ta-remove-item old pax)
      (ta-insert-item new pax)
    ::
    ?:  &(?=(%raw -.old) ?=(%both -.new))
      ::  insert collection
      ::  update raw item
      =.  ta-this  (ta-update-raw-item raw.old raw.new pax)
      (ta-insert-item new pax)
    ::
    ?:  &(?=(%both -.old) ?=(%raw -.new))
      ::  remove collection
      ::  update raw item
      =.  ta-this  (ta-remove-item [%collection col.old] pax)
      (ta-update-raw-item raw.old raw.new pax)
    ::
    ?:  &(?=(%both -.old) ?=(%collection -.new))
      ::  remove raw item
      ::  update collection
      =.  ta-this  (ta-remove-item [%raw raw.old] pax)
      (ta-update-collection col.old col.new pax)
    ::
    ta-this
  ::
  ++  ta-update-raw-item
    |=  [old=raw-item new=raw-item pax=path]
    ^+  ta-this
::    ~&  update-raw-item+[-.old -.new pax]
    ?:  =(old new)
      ta-this
    ::
    =?  ta-this  !=(data.old data.new)
      =/  parent-path  (scag (dec (lent pax)) pax)
      (ta-hall-json parent-path 'edited item' (item-notify pax new)) :: XX fil 
    ::
    =?  ta-this
      ?&  =('.y' (fall (~(get by meta.new) %comments) '.n'))
          =('.n' (fall (~(get by meta.old) %comments) '.n'))
      ==
      ::  create comments
      (ta-generate-comments pax)
    ::
    =?  ta-this
      ?&  =('.n' (fall (~(get by meta.new) %comments) '.n'))
          =('.y' (fall (~(get by meta.old) %comments) '.n'))
      ==
      ::  delete comments
      (ta-remove (weld pax /collections-config))
    ::
    ::  check if file has been modified
    ::  and if so update last modified field
    =/  told  (trip data.old)
    =/  newt  (trip data.new)
    =/  old-con  (slag (need (find ";>" told)) told)
    =/  new-con  (slag (need (find ";>" newt)) newt)
    =?  ta-this  !=(old-con new-con)
      =/  contents=@t
        %+  update-umd-front
        (~(put by meta.new) %last-modified (scot %da now.bol))
        data.new

::      ~&  %update-last-modified
::      ~&  contents
::      ~&  (weld pax /umd)
      (ta-write (weld pax /umd) %umd !>(contents))
    ::
    ta-this
  ::
  ++  ta-update-collection
    |=  $:  old=collection
            new=collection
            pax=path
        ==
    ^+  ta-this
::    ~&  update-collection+pax
    ::
    =?  ta-this  !=(meta.old meta.new)
      =/  parent-path  (scag (dec (lent pax)) pax)
      (ta-hall-json parent-path 'edited collection' (collection-notify pax meta.new))
    ::
    ?:  =(data.old data.new)
      ta-this
    ::
    ::  new values of all changed items
    =/  upd-new  (~(dif in (~(int by data.old) data.new)) data.old)
    ::  old values of all changed items
    =/  upd-old  (~(dif in (~(int by data.new) data.old)) data.new)
    ::  all totally new entries
    =/  ins-new  (~(dif by data.new) data.old)
    ::  all deleted entries
    =/  del-old  (~(dif by data.old) data.new)
    ::
    =/  upd-new=(list [nom=knot =item])  ~(tap by upd-new)
    =/  upd-old=(list [nom=knot =item])  ~(tap by upd-old)
    =/  ins-new=(list [nom=knot =item])  ~(tap by ins-new)
    =/  del-old=(list [nom=knot =item])  ~(tap by del-old)
    ::
    =/  lam  |=([[a=knot item] out=(list path)] [(weld pax [a ~]) out])
::    ~&  upd-new+(roll upd-new lam)
::    ~&  upd-old+(roll upd-old lam)
::    ~&  ins-new+(roll ins-new lam)
::    ~&  del-old+(roll del-old lam)
    ::
    =.  ta-this  |-
    ?~  upd-new
      ta-this
    ?<  ?=(~ upd-old)
    =*  new-item  i.upd-new
    =*  old-item  i.upd-old
    =/  new-pax  (weld pax [nom.new-item ~])
    =.  ta-this  (ta-update-item item.old-item item.new-item new-pax)
    ::
    %=  $
      upd-new  t.upd-new
      upd-old  t.upd-old
    ==
    ::
    =.  ta-this  |-
    ?~  ins-new
      ta-this
    =*  new-item  i.ins-new
    =/  new-pax  (weld pax [nom.new-item ~])
    =.  ta-this  (ta-insert-item +.new-item (weld pax [-.new-item ~]))
    $(ins-new t.ins-new)
    ::
    =.  ta-this  |-
    ?~  del-old
      ta-this
    =*  old-item  i.del-old
    =/  old-pax  (weld pax [nom.old-item ~])
    =.  ta-this  (ta-remove-item +.old-item (weld pax [-.old-item ~]))
    $(del-old t.del-old)
    ::
    ta-this
  ::
  ++  ta-generate-comments
    |=  pax=path
    ^+  ta-this
    =/  sup=path  [%collections-config (flop pax)]
    =/  pat  (en-beam:format [byk.bol sup])
    =/  cay=config
      :*  [byk.bol sup]
          'comments'
          'comments'
          our.bol
          now.bol
          now.bol
          %comments
          |
          ~
          |
      ==
    (ta-write (flop sup) %collections-config !>(cay))
  ::
  ::  writing files
  ::
  ++  ta-write
    |=  [pax=path cay=cage]
::    ~&  writing+pax
    ^+  ta-this
    =.  pax  (en-beam:format byk.bol (flop pax))
    %+  ta-emit  ost.bol
    [%info (weld /ta-write pax) our.bol (foal pax cay)]
  ::
  ++  ta-remove
    |=  pax=path
    =.  pax  (en-beam:format byk.bol (flop pax))
    ^+  ta-this
    %+  ta-emit  ost.bol
    [%info (weld /ta-remove pax) our.bol (fray pax)]
  ::
  ::  permissions
  ::
  ++  ta-set-permissions
    |=  [pax=path r=rule:clay w=rule:clay]
::    =.  pax  (weld base-spur pax)
    ^+  ta-this
    %+  ta-emit  ost.bol
    [%perm (weld /perms pax) our.bol q.byk.bol pax [%rw `r `w]]
  ::
  ++  ta-flush-permissions
    |=  pax=path
::    =.  pax  (weld base-spur pax)
    ^+  ta-this
    %+  ta-emit  ost.bol
    [%perm (weld /perms pax) our.bol q.byk.bol pax [%rw ~ ~]]
  ::
  ::  hall
  ::
  ++  ta-hall-action
    |=  act=action:hall
    ^+  ta-this
    %+  ta-emit  ost.bol
    [%poke /col-hall-action [our.bol %hall] %hall-action act]
  ::
  ++  ta-hall-actions
    |=  act=(list $?(~ action:hall))
    ^+  ta-this
    ?~  act  ta-this
    ?~  i.act  $(act t.act)
    %=  $
      ta-this  (ta-hall-action i.act)
      act  t.act
    ==
  ::
  ++  ta-hall-create-circle  ::
    |=  [pax=path description=@t]
    ^+  ta-this
    =/  circ=circle:hall  (path-to-circle pax)
    =/  parent=circle:hall
      ?:  =(nom.circ %c)
        [our.bol %inbox]
      (path-to-circle (scag (dec (lent pax)) pax))
::    ~&  create+circ
::    ~&  source+parent
    %-  ta-hall-actions
    :: XX TODO make this depend on clay perms
    :~  [%create nom.circ description %journal]  
        [%source nom.parent & (sy `source:hall`[circ ~] ~)]
    ==
  ::
  ++  ta-hall-lin
    |=  [pax=path msg=cord]
    ^+  ta-this
    =/  circ=circle:hall  (path-to-circle pax)
    %-  ta-hall-action
    [%phrase [circ ~ ~] [%lin | msg]~]
  ::
  ++  ta-hall-json
    |=  [pax=path header=@t jon=json]
    ^+  ta-this
    =/  circ=circle:hall  (path-to-circle pax)
    %-  ta-hall-action
    :+  %phrase   [circ ~ ~]
    [%fat [%text ~[header]] [%lin | (crip (en-json:html jon))]]~
  ::
  --
--
