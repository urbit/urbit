::
::::  /app/collections/hoon
  ::
/?  309
/-  hall
/+  collections, cram
::
::  cols:
::
::    run collections-item renderer on children of /web/collections
::    combine with a bunted config in a +collection structure defined in 
::    /lib/collections because the top level collection has no config file
::
::    whenever any of the clay files that compose this renderer change, this app
::    will recompile and the +prep arm will fire. we then check which files 
::    changed and notify the corresponding hall circle of that change
::
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
::  state: 
::    
::    stores the collection built by above by :cols so that we can compare old
::    and new versions whenever the rendered data changes
::
|_  [bol=bowl:gall state=collection]
::
::  +this: app core subject
::
++  this  .
::
::  +prep:
::
::    on initial boot, create top level hall circle for collections, called %c
::
::    on subsequent compiles, call +ta-update:ta on the old collection data,
::    then update state to store the new collection data
::
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
::  +mack: 
::
::    recieve acknowledgement for permissions changes, print error if it failed
::
++  mack
  |=  [wir=wire err=(unit tang)]
  ^-  (quip move _this)
  ?~  err
    [~ this]
  (mean u.err)
::
::  +coup: recieve acknowledgement for poke, print error if it failed
::
++  coup
  |=  [wir=wire err=(unit tang)]
  ^-  (quip move _this)
  ?~  err
    [~ this]
  (mean u.err)
::
::  +path-to-circle:
::
::    takes a clay path and returns a hall circle
::    for a path /foo/bar it returns a circle with a :name %c-foo-bar
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
::  +allowed-by: checks if ship :who is allowed by the permission rules in :dic
::
++  allowed-by
  |=  [who=@p dic=dict:clay]
  ^-  ?
  ?:  =(who our.bol)  &
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
::  +collection-notify: XX
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
::  +item-notify: XX
::
++  item-notify
  |=  [pax=path raw=raw-item]
  ^-  json
  =/  owner  (fall (~(get by meta.raw) %owner) ~.anon)
  =/  dat    (fall (~(get by meta.raw) %last-modified) (scot %da now.bol))
  =/  nom    (fall (~(get by meta.raw) %name) ~.no-title)
  =/  typ    (fall (~(get by meta.raw) %type) ~.no-type)
  ::
  =/  elm=manx   elm:(static:cram (ream data.raw))
  =/  snip=marl  tal:(hedtal +.elm)
  =/  inner
    ?~  snip 
      (crip (en-xml:html elm)) 
    (crip (en-xml:html i.snip))    :: inner html
  ::
  =/  parent-spur  (slag 1 (flop pax))
  =/  bek=beak     byk.bol(r [%da now.bol])
  =/  parent-path  (en-beam:format [bek parent-spur])
  =/  parent-dir   .^(arch %cy parent-path)
  ::
  =/  parent-conf=json
    ?:  (~(has in dir.parent-dir) ~.umd ~)
      %-  meta-to-json
      %-  umd-to-front 
      .^(@t %cx (weld parent-path /umd))
    ?:  (~(has in dir.parent-dir) ~.collections-config ~)
      %-  config-to-json
      .^(config %cx (weld parent-path /collections-config))
    ~
  ::
  %-  pairs:enjs:format
  :~  ['owner' [%s owner]]
      ['path' [%a (turn pax |=(a=@ta `json`[%s a]))]]
      ['name' [%s nom]]
      ['date' [%s dat]]
      ['type' [%s typ]]
      ['content' [%s data.raw]]
      ['snip' [%s inner]]
      ['parent-config' parent-conf]
  ==
::
::  +front-to-wain: XX
::
++  front-to-wain
  |=  a=(map knot cord)
  ^-  wain
  =/  entries=wain
    %+  turn  ~(tap by a)
    |=  b=[knot cord]
    =/  c=[term cord]  ((hard ,[term cord]) b)
    (crip "  [{<-.c>} {<+.c>}]")
  ::
  ?~  entries  ~
  ;:  weld
    [':-  :~' ~]
    entries
    ['    ==' ~]
  ==
::
::  +update-umd-front: XX
::
++  update-umd-front
  |=  [fro=(map knot cord) umd=@t]
  ^-  @t
  %-  of-wain:format
  =/  tum  (trip umd)
  =/  id  (find ";>" tum)
  ?~  id
    %+  weld  (front-to-wain fro)
    (to-wain:format (crip (weld ";>\0a" tum)))
  %+  weld  (front-to-wain fro)
  (to-wain:format (crip (slag u.id tum)))
::
::  +poke-collections-action:
::
::    the main interface for creating and deleting collections and items
::
++  poke-collections-action
  |=  act=action:collections
  ^-  (quip move _this)
  ?:  =(who.act our.bol)
    ta-done:(ta-act:ta act)
  ::  forward poke if its not meant for us
  ::
  :_  this
  :_  ~
  :*  ost.bol  %poke  
      /forward-collections-action  
      [who.act %collections]
      %collections-action  act
  ==
::
::  +poke-json
::
::    utility for setting whether or not to display the onboarding page
::
++  poke-json
  |=  jon=json
  ^-  (quip move _this)
  ?:  ?=([%o [[%onboard %b ?] ~ ~]] jon)
    =<  ta-done
    (ta-write:ta /web/landscape/onboard/json [%json !>(jon)])
  [~ this]
::
::  +ta: main event core for collections
::
++  ta
  |_  moves=(list move)
  ::
  ::  +ta-this: ta core subject
  ::
  ++  ta-this  .
  ::
  ::  +ta-done: 
  ::    
  ::    flop :moves for finalization, since moves are prepended to the list
  ::
  ++  ta-done  [(flop moves) this]
  ::
  ::  +ta-emit: add a +move to :moves
  ::
  ++  ta-emit
    |=  mov=move
    %_  ta-this
      moves  [mov moves]
    ==
  ::
  ::  +ta-emil: add a list of +move to :moves
  ::
  ++  ta-emil
    |=  mos=(list move)
    %_  ta-this
      moves  (welp (flop mos) moves)
    ==
  ::
  ::  +ta-act: process collection-action
  ::
  ++  ta-act
    |=  act=action:collections
    ^+  ta-this
    ::
    ::  iterate through list of +sub-action of +action
    ::
    |-
    ?~  acts.act  ta-this
    =*  a  i.acts.act
    ::
    =/  now-id=@da  (sub now.bol (div (dis now.bol ~s0..fffe) 2))
    =/  dat  (scot %da now-id)
    =/  bek=beak  byk.bol(r [%da now-id])
    =/  sap  (en-beam:format [bek (flop (path +<.a))])
    ::
    =.  ta-this
    ?-    -.a
        %write
      =/  perms  .^([dict:clay dict:clay] %cp sap)
      ?:  (allowed-by src.bol +.perms)
        ?-  -.for.a
          %umd                 (ta-write pax.a `cage`[-.for.a !>(+.for.a)])
          %collections-config  (ta-write pax.a `cage`[-.for.a !>(+.for.a)])
        ==
      ta-this
    ::
        %delete
      =/  perms  .^([dict:clay dict:clay] %cp sap)
      ?:  (allowed-by src.bol +.perms)
        (ta-remove pax.a)
      ta-this 
    ::
        %perms
      ?:  =(src.bol our.bol)  :: XX admin privileges for other users?
        (ta-set-permissions pax.a r.a w.a)
      ta-this
    ::
    ::
    ::  XX some of this is redunant
    ::
        %collection
      =/  perms  
        .^([dict:clay dict:clay] %cp (weld sap /[dat]/collections-config))
      ?.  (allowed-by src.bol +.perms)
        ta-this
      =/  conf=config
        :*  [bek (flop (weld pax.a /[dat]/collections-config))]
            name.a
            desc.a
            our.bol
            now-id
            now-id
            type.a
            comments.a
            ~
            visible.a
        ==
      =.  ta-this  
        %+  ta-write  (weld pax.a /[dat]/collections-config) 
        [%collections-config !>(conf)]
      ::  restrict permissions on config file
      =.  ta-this  
        %^  ta-set-permissions  (weld pax.a /[dat]/collections-config) 
        [%white ((set whom:clay) [[& src.bol] ~ ~])]   ::  read
        [%white ((set whom:clay) [[& src.bol] ~ ~])]   ::  write
      ::  open permissions on collection items
      =.  ta-this  
        %^  ta-set-permissions  (weld pax.a /[dat]) 
        [%black ((set whom:clay) ~)]                   ::  read
        [%black ((set whom:clay) ~)]                   ::  write
      ta-this
    ::
        %post
      =?  pax.a  !edit.a
        (weld pax.a /[dat])
      =?  sap  !edit.a
        (en-beam:format [bek (flop pax.a)])
      =/  perms  .^([dict:clay dict:clay] %cp (weld sap /umd))
      ?.  (allowed-by src.bol +.perms)
        ta-this
      =.  content.a  (crip (weld (trip content.a) "\0a"))
      =/  front=(map knot cord)
        %-  my
        :~  [%name name.a]
            [%comments ?:(comments.a ~..y ~..n)]
            [%owner (scot %p src.bol)]
            [%date-created (snag 0 (flop pax.a))]
            [%last-modified dat]
            [%type type.a]
        ==
      =.  ta-this  
        %+  ta-write  (weld pax.a /umd) 
        [%umd !>((update-umd-front front content.a))]
      ::  restrict permissions on umd file
      =.  ta-this  
        %^  ta-set-permissions  (weld pax.a /umd) 
        [%black ((set whom:clay) ~)]                   ::  read
        [%white ((set whom:clay) [[& src.bol] ~ ~])]   ::  write
      ::  open permissions on comments
      =.  ta-this  
        %^  ta-set-permissions  pax.a 
        [%black ((set whom:clay) ~)]                   ::  read
        [%black ((set whom:clay) ~)]                   ::  write
      ta-this
    ::
        %comment
      =/  perms  .^([dict:clay dict:clay] %cp (weld sap /[dat]/umd))
      ?.  (allowed-by src.bol +.perms)
        ta-this
      =.  content.a  (crip (weld (trip content.a) "\0a"))
      =/  front=(map knot cord)
        %-  my
        :~  [%owner (scot %p src.bol)]
            [%date-created dat]
            [%last-modified dat]
            [%type %comments]
        ==
      =.  ta-this  
        %+  ta-write  (weld pax.a /[dat]/umd) 
        [%umd !>((update-umd-front front content.a))]
      ::  restrict permissions on umd file
      =.  ta-this  
        %^  ta-set-permissions  (weld pax.a /[dat]/umd) 
        [%black ((set whom:clay) ~)]                   ::  read
        [%white ((set whom:clay) [[& src.bol] ~ ~])]   ::  write
      ta-this
    ::
    ==
    $(acts.act t.acts.act)
  ::
  ::  +ta-update:
  ::
  ::
  ::
  ++  ta-update
    |=  old=collection
    ^+  ta-this
    ?:  =(old cols)
      ta-this
    (ta-update-collection old cols /web/collections)
  ::
  ++  ta-insert-item
    |=  [new=item pax=path]
    ^+  ta-this
    =/  parent-path  (scag (dec (lent pax)) pax)
    ::
    ?-    -.new
    ::
        %error
      (ta-hall-lin parent-path 'error')
    ::
        %collection
      =.  ta-this
        %^  ta-hall-json  parent-path  'new collection' 
        (collection-notify pax meta.col.new)
      ::
      =.  ta-this  (ta-hall-create-circle pax description.meta.col.new)
      =/  items=(list [nom=@ta =item])  ~(tap by data.col.new)
      |-
      ?~  items  ta-this
      =.  ta-this  (ta-insert-item item.i.items (weld pax [nom.i.items ~]))
      $(items t.items)
    ::
        %both
      =.  ta-this  (ta-hall-create-circle pax description.meta.col.new)
      =/  items=(list [nom=@ta =item])  ~(tap by data.col.new)
      =.  ta-this
      |-
      ?~  items  ta-this
      =.  ta-this  (ta-insert-item item.i.items (weld pax [nom.i.items ~]))
      $(items t.items)
      ::
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
    ::  flush permissions
    ::  notify parent of deletion
    =/  parent  (scag (dec (lent pax)) pax)
    ::  recurse for children
    ?-    -.old
    ::
        %error
      (ta-hall-lin parent 'error')
    ::
        %collection
      =.  ta-this  
        %^  ta-hall-json  parent  'deleted collection' 
        (collection-notify pax meta.col.old)
      =.  ta-this  (ta-flush-permissions (weld pax /collections-config))
      =/  items=(list [nom=@ta =item])  ~(tap by data.col.old)
      |-
      ?~  items  ta-this
      =.  ta-this  (ta-remove-item item.i.items (weld pax [nom.i.items ~]))
      $(items t.items)
    ::
        %both
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
    ::
    ?:  &(?=(%error -.old) ?=(%error -.new))
      ta-this
    ?:  &(?=(%error -.old) ?=(%collection -.new))
      (ta-insert-item new pax)
    ?:  &(?=(%error -.old) ?=(%raw -.new))
      (ta-insert-item new pax)
    ?:  &(?=(%error -.old) ?=(%both -.new))
      (ta-insert-item new pax)
    ?:  ?=(%error -.new)
      (ta-hall-lin pax 'error')
    ::
    ta-this
  ::
  ++  ta-update-raw-item
    |=  [old=raw-item new=raw-item pax=path]
    ^+  ta-this
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
    ta-this
  ::
  ++  ta-update-collection
    |=  $:  old=collection
            new=collection
            pax=path
        ==
    ^+  ta-this
    ::
    =?  ta-this  !=(meta.old meta.new)
      =/  parent-path  (scag (dec (lent pax)) pax)
      %^  ta-hall-json  parent-path  'edited collection'
      (collection-notify pax meta.new)
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
    =/  bek  byk.bol(r [%da now.bol])
    =/  pat  (en-beam:format [bek sup])
    =/  dat=@da  (slav %da (snag 0 (flop pax)))
    =/  cay=config
      :*  [bek sup]
          'comments'
          'comments'
          our.bol
          dat
          dat
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
    ^+  ta-this
    =/  bek  byk.bol(r [%da now.bol])
    =.  pax  (en-beam:format bek (flop pax))
    %+  ta-emit  ost.bol
    [%info (weld /ta-write pax) our.bol (foal pax cay)]
  ::
  ++  ta-remove
    |=  pax=path
    =/  bek  byk.bol(r [%da now.bol])
    =.  pax  (en-beam:format bek (flop pax))
    ^+  ta-this
    %+  ta-emit  ost.bol
    [%info (weld /ta-remove pax) our.bol (fray pax)]
  ::
  ::  permissions
  ::
  ++  ta-set-permissions
    |=  [pax=path r=rule:clay w=rule:clay]
    ^+  ta-this
    %+  ta-emit  ost.bol
    [%perm (weld /perms pax) our.bol q.byk.bol pax [%rw `r `w]]
  ::
  ++  ta-flush-permissions
    |=  pax=path
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
  ++  ta-hall-create-circle
    |=  [pax=path description=@t]
    ^+  ta-this
    =/  circ=circle:hall  (path-to-circle pax)
    =/  parent=circle:hall
      ?:  =(nom.circ %c)
        [our.bol %inbox]
      (path-to-circle (scag (dec (lent pax)) pax))
    %-  ta-hall-actions
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
