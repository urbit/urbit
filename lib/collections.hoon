::
::::  /hoon/collections/lib
  ::
/?  309
/-  hall
/+  cram, elem-to-react-json
:: ::
~%  %collections-lib  ..is  ~
|%
+$  move  [bone card]
::
+$  card
  $%  [%info wire toro:clay]
      [%poke wire dock poke]
      [%perm wire desk path rite:clay]
      [%peer wire dock path]
      [%pull wire dock ~]
      [%diff diff]
  ==
::
+$  diff
  $%  [%collections-prize prize]
      [%collections-rumor rumor]
      [%hall-rumor rumor:hall]
  ==
::
+$  poke
  $%  [%hall-action action:hall]
      [%collections-action action]
      [%json json]
  ==
::
+$  state
  $%  [%0 col=collection str=streams]
  ==
::
+$  streams
  $:  ::  inbox config and messages
      ::
      inbox=[con=(unit config:hall) env=(list envelope:hall)]
      ::  names and configs of all circles we know about
      ::
      circles=(map circle:hall (unit config:hall))
      ::  names of all circles we own
      ::
      our-circles=(set name:hall)
      ::  list of messages in all our DM circles
      ::
      dms=(map name:hall [ini=ship env=(list envelope:hall)])
      ::  all the DM invites we've received
      ::
      invites=(list envelope:hall)
  ==
::
+$  prize
  $:  ::  inbox config and messages
      ::
      inbox=[con=(unit config:hall) env=(list envelope:hall)]
      ::  names and configs of all circles we know about
      ::
      circles=(map circle:hall (unit config:hall))
      ::  names of all circles we own
      ::
      our-circles=(set name:hall)
      ::  list of messages in all our DM circles
      ::
      dms=(map name:hall [ini=ship env=(list envelope:hall)])
      ::  all the DM invites we've received
      ::
      invites=(list envelope:hall)
  ==
::
+$  rumor
  $%  ::  if config is given, either add new circle or update existing one
      ::  if config is nil then delete circle
      ::
      [%config-change cir=circle:hall con=(unit config:hall)]
      ::  recieved a new inbox message or DM invite
      ::
      [%new-msg nom=?(%inbox %invites) env=envelope:hall]
  ==
::
+$  command
  $%  [%chat-invite nom=name:hall who=(set ship)]
      [%collection-invite nom=name:hall col=term who=(set ship)]
  ==
+$  collection  [meta=config data=(map nom=knot =item)]
::
+$  item
  $~  [%error ~]
  $%  [%collection col=collection]
      [%raw raw=raw-item]
      [%both col=collection raw=raw-item]
      [%error ~]
  ==
::
+$  raw-item
  $%  [%udon meta=(map knot cord) data=@t]
  ==
::
+$  config
  $:  full-path=beam
      name=@t
      description=@t
    ::
      author=@p
    ::
      date-created=@da
      last-modified=@da
    ::
      type=@tas
      comments=?
      sort-key=(unit @)
      visible=?
    ::
  ==
::
+$  action
  $:  who=ship
      dek=desk
      acts=(list sub-action)
  ==
::
+$  sub-action
  $%  [%write pax=path for=form]
      [%delete pax=path]
      [%perms pax=path r=rule:clay w=rule:clay]
    ::
      [%collection pax=path name=@t desc=@t comments=? visible=? type=@tas]
      [%post pax=path name=@t type=@tas comments=? content=@t edit=?]
      [%comment pax=path content=@t]
  ==
::
+$  form
  $%  [%udon @t]
      [%collections-config config]
  ==
::
++  collection-error
  ~/  %coll-collection-error
  |=  col=collection
  ^-  ?
  |-
  =/  vals=(list item)  ~(val by data.col)
  %+  roll  vals
  |=  [i=item out=_|]
  ^-  ?
  ?:  out  out
  ?+  -.i
    %.n
    %error       %.y
    %collection  ^$(col col.i)
    %both        ^$(col col.i)
  ==
::::
::::  /mar/snip
::::
++  words  1
++  hedtal
  =|  met/marl
  |=  a/marl  ^-  {hed/marl tal/marl}
  ?~  a  [~ ~]
  :: looks like it only terminates if it finds an h1?
  ?.  ?=($h1 n.g.i.a)
    ?:  ?=($meta n.g.i.a)
      $(a t.a, met [i.a met])
    =+  had=$(a c.i.a)
    ?^  -.had  had
    $(a t.a)
  [c.i.a (weld (flop met) (limit words t.a))]
  ::
::
++  limit
  ~/  %coll-limit
  |=  {lim/@u mal/marl}
  =<  res
  |-  ^-  {rem/@u res/marl}
  ?~  mal  [lim ~]
  ?~  lim  [0 ~]
  =+  ^-  {lam/@u hed/manx}
    ?:  ?=(_;/(**) i.mal)
      [lim ;/(tay)]:(deword lim v.i.a.g.i.mal)
    [rem ele(c res)]:[ele=i.mal $(mal c.i.mal)]
  [rem - res]:[hed $(lim lam, mal t.mal)]
::
++  deword
  ~/  %coll-deword
  |=  {lim/@u tay/tape}  ^-  {lim/@u tay/tape}
  ?~  tay  [lim tay]
  ?~  lim  [0 ~]
  =+  wer=(dot 1^1 tay)
  ?~  q.wer
    [lim - tay]:[i.tay $(tay t.tay)]
  =+  nex=$(lim (dec lim), tay q.q.u.q.wer)
  [-.nex [(wonk wer) +.nex]]
::
::  json
::
++  item-to-json
  ~/  %coll-item-to-json
  |=  itm=item
  ^-  json
  ?-    -.itm
      %error  (frond:enjs:format %error ~)
  ::
      %collection
    %+  frond:enjs:format
    %collection  (collection-to-json col.itm)
  ::
      %raw
    %-  frond:enjs:format
    [%item (raw-to-json raw.itm)]
  ::
      %both
    %-  pairs:enjs:format
    :~  [%item (raw-to-json raw.itm)]
        [%collection (collection-to-json col.itm)]
    ==
  ==
::
++  collection-to-json
  ~/  %coll-collection-to-json
  |=  col=collection
  ^-  json
  %-  pairs:enjs:format
  :~  [%meta (config-to-json meta.col)]
      :+  %data  %a
      %+  turn  ~(tap by data.col)
      |=  [nom=knot ite=item]
      ^-  json
      %-  pairs:enjs:format
      :~  [%filename %s nom]
          [%item (item-to-json ite)]
      ==
  ==
::
++  raw-to-json
  ~/  %coll-raw-to-json
  |=  raw=raw-item
  ^-  json
  =/  elm=manx  elm:(static:cram (ream data.raw))
  =/  rec=json  (elem-to-react-json elm)
  %-  pairs:enjs:format
  :~  [%data rec]
      [%meta (meta-to-json meta.raw)]
  ==
::
++  config-to-json
  ~/  %coll-config-to-json
  |=  con=config
  ^-  json
  ?:  =(con *config)
    ~
  %-  pairs:enjs:format
  :~  :-  %full-path
        :-  %a
        %+  turn  (en-beam:format full-path.con)
        |=  a=@ta
        [%s a]
      :-  %name           [%s name.con]
      :-  %desc           [%s description.con]
      :-  %author          (ship:enjs:format author.con)
      :-  %date-created   (time:enjs:format date-created.con)
      :-  %last-modified  (time:enjs:format last-modified.con)
      :-  %type           [%s type.con]
      :-  %comments       [%b comments.con]
      :-  %sort-key       ?~(sort-key.con ~ (numb:enjs:format u.sort-key.con))
      :-  %visible        [%b visible.con]
  ==
::
++  meta-to-json
  ~/  %coll-meta-to-json
  |=  meta=(map knot cord)
  ^-  json
  %-  pairs:enjs:format
  %+  turn  ~(tap by meta)
  |=  [key=@t val=@t]
  ^-  [@t json]
  [key [%s val]]
::
++  udon-to-front
  ~/  %coll-udon-to-front
  |=  u=@t
  ^-  (map knot cord)
  %-  ~(run by inf:(static:cram (ream u)))
  |=  a=dime  ^-  cord
  ?+  (end 3 1 p.a)  (scot a)
    %t  q.a
  ==
::
::  +path-to-circle:
::
::    takes a clay path and returns a hall circle
::    for a path /foo/bar it returns a circle with a :name %c-foo-bar
::
++  path-to-circle
  ~/  %coll-path-to-circle
  |=  [pax=path our=@p]
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
  [our nam]
::
::  +allowed-by: checks if ship :who is allowed by the permission rules in :dic
::
++  allowed-by
  ~/  %coll-allowed-by
  |=  [who=@p dic=dict:clay our=@p]
  ^-  ?
  ?:  =(who our)  &
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
  ~/  %coll-collection-notify
  |=  [pax=path conf=config]
  ^-  json
  %-  pairs:enjs:format
  :~  ['author' [%s (crip (scow %p author.conf))]]
      ['host' [%s (crip (scow %p p.full-path.conf))]]
      ['path' [%a (turn pax |=(a=@ta `json`[%s a]))]]
      ['name' [%s name.conf]]
      ['date' [%s (crip (scow %da last-modified.conf))]]
      ['type' [%s type.conf]]
  ==
::
::  +item-notify: XX
::
++  item-notify
  ~/  %coll-item-notify
  |=  [pax=path raw=raw-item now=@da byk=beak]
  ^-  json
  =/  author  (fall (~(get by meta.raw) %author) ~.anon)
  =/  host  (fall (~(get by meta.raw) %host) ~.anon)
  =/  dat    (fall (~(get by meta.raw) %last-modified) (scot %da now))
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
  =/  bek=beak     byk(r [%da now])
  =/  parent-path  (en-beam:format [bek parent-spur])
  =/  parent-dir   .^(arch %cy parent-path)
  ::
  =/  parent-conf=json
    ?:  (~(has in dir.parent-dir) ~.udon ~)
      %-  meta-to-json
      %-  udon-to-front
      .^(@t %cx (weld parent-path /udon))
    ?:  (~(has in dir.parent-dir) ~.collections-config ~)
      %-  config-to-json
      .^(config %cx (weld parent-path /collections-config))
    ~
  ::
  %-  pairs:enjs:format
  :~  ['author' [%s author]]
      ['host' [%s host]]
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
  ~/  %coll-front-to-wain
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
::  +update-udon-front: XX
::
++  update-udon-front
  ~/  %coll-update-udon-front
  |=  [fro=(map knot cord) udon=@t]
  ^-  @t
  %-  of-wain:format
  =/  tum  (trip udon)
  =/  id  (find ";>" tum)
  ?~  id
    %+  weld  (front-to-wain fro)
    (to-wain:format (crip (weld ";>\0a" tum)))
  %+  weld  (front-to-wain fro)
  (to-wain:format (crip (slag u.id tum)))
::
::  _ta: main event core for collections
::
++  ta
  ~/  %coll-ta
  |_  $:  moves=(list move)
          bol=bowl:gall
      ==
  ::
  ::  +ta-this: ta core subject
  ::
  ++  ta-this  .
  ::
  ::  +ta-done:
  ::
  ::    flop :moves for finalization, since moves are prepended to the list
  ::
  ++  ta-done  (flop moves)
  ::
  ::  +ta-emit: add a +move to :moves
  ::
  ++  ta-emit
    ~/  %coll-ta-emit
    |=  mov=move
    %_  ta-this
      moves  [mov moves]
    ==
  ::
  ::  +ta-emil: add a list of +move to :moves
  ::
  ++  ta-emil
    ~/  %coll-ta-emil
    |=  mos=(list move)
    %_  ta-this
      moves  (welp (flop mos) moves)
    ==
  ::
  ::  +ta-act: process collection-action
  ::
  ++  ta-act
    ~/  %coll-ta-act
    |=  act=action
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
      ?:  (allowed-by src.bol +.perms our.bol)
        ?-  -.for.a
          %udon                (ta-write pax.a `cage`[-.for.a !>(+.for.a)])
          %collections-config  (ta-write pax.a `cage`[-.for.a !>(+.for.a)])
        ==
      ta-this
    ::
        %delete
      =/  perms  .^([dict:clay dict:clay] %cp sap)
      ?:  (allowed-by src.bol +.perms our.bol)
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
      ?.  (allowed-by src.bol +.perms our.bol)
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
      =/  perms  .^([dict:clay dict:clay] %cp (weld sap /udon))
      ?.  (allowed-by src.bol +.perms our.bol)
        ta-this
      =.  content.a  (crip (weld (trip content.a) "\0a"))
      =/  front=(map knot cord)
        %-  my
        :~  [%name name.a]
            [%comments ?:(comments.a ~..y ~..n)]
            [%author (scot %p src.bol)]
            [%host (scot %p our.bol)]
            [%date-created (snag 0 (flop pax.a))]
            [%last-modified dat]
            [%type type.a]
        ==
      =.  ta-this
        %+  ta-write  (weld pax.a /udon)
        [%udon !>((update-udon-front front content.a))]
      ::  restrict permissions on udon file
      =.  ta-this
        %^  ta-set-permissions  (weld pax.a /udon)
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
      =/  perms  .^([dict:clay dict:clay] %cp (weld sap /[dat]/udon))
      ?.  (allowed-by src.bol +.perms our.bol)
        ta-this
      =.  content.a  (crip (weld (trip content.a) "\0a"))
      =/  front=(map knot cord)
        %-  my
        :~  [%author (scot %p src.bol)]
            [%host (scot %p our.bol)]
            [%date-created dat]
            [%last-modified dat]
            [%type %comments]
        ==
      =.  ta-this
        %+  ta-write  (weld pax.a /[dat]/udon)
        [%udon !>((update-udon-front front content.a))]
      ::  restrict permissions on udon file
      =.  ta-this
        %^  ta-set-permissions  (weld pax.a /[dat]/udon)
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
    ~/  %coll-ta-update
    |=  [old=collection new=collection]
    ^+  ta-this
    ?:  =(old new)
      ta-this
    (ta-update-collection old new /web/collections)
  ::
  ++  ta-insert-item
    ~/  %coll-ta-insert-item
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
      =.  ta-this  (ta-hall-create-circle pax name.meta.col.new)
      =/  items=(list [nom=@ta =item])  ~(tap by data.col.new)
      |-
      ?~  items  ta-this
      =.  ta-this  (ta-insert-item item.i.items (weld pax [nom.i.items ~]))
      $(items t.items)
    ::
        %both
      =.  ta-this  (ta-hall-create-circle pax name.meta.col.new)
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
        %^  ta-hall-json
          parent-path
          'new item'
          (item-notify pax raw.new now.bol byk.bol)
      ?:  ?&  (~(has by meta.raw.new) %comments)
              =('.y' (~(got by meta.raw.new) %comments))
          ==
        =/  author=(unit @ta)  (~(get by meta.raw.new) %author)
        =/  author-p=@p
          ?~  author  our.bol
          (fall (rush u.author ;~(pfix sig fed:ag)) our.bol)
        (ta-generate-comments pax author-p)
      ta-this
    ::
    ==
  ::
  ++  ta-remove-item
    ~/  %coll-ta-remove-item
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
      %^  ta-hall-json
        parent
        'deleted item'
        (item-notify pax raw.old now.bol byk.bol)
    ::
    ==
  ::
  ::
  ::
  ++  ta-update-item
    ::  always make sure removals happen first and insertions happen last
    ::  because removals flush permissions and insertions set them
    ::
    ~/  %coll-ta-update-item
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
    ~/  %coll-ta-update-raw-item
    |=  [old=raw-item new=raw-item pax=path]
    ^+  ta-this
    ?:  =(old new)
      ta-this
    ::
    =?  ta-this  !=(data.old data.new)
      =/  parent-path  (scag (dec (lent pax)) pax)
      %^  ta-hall-json
        parent-path
        'edited item'
        (item-notify pax new now.bol byk.bol)
    ::
    =?  ta-this
      ?&  =('.y' (fall (~(get by meta.new) %comments) '.n'))
          =('.n' (fall (~(get by meta.old) %comments) '.n'))
      ==
      =/  author=(unit @ta)  (~(get by meta.new) %author)
      =/  author-p=@p
        ?~  author  our.bol
        (fall (rush u.author ;~(pfix sig fed:ag)) our.bol)
      (ta-generate-comments pax author-p)
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
    ~/  %coll-ta-update-collection
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
    ~/  %coll-ta-generate-comments
    |=  [pax=path author=ship]
    ^+  ta-this
    =/  sup=path  [%collections-config (flop pax)]
    =/  bek  byk.bol(r [%da now.bol])
    =/  pat  (en-beam:format [bek sup])
    =/  dat=@da  (slav %da (snag 0 (flop pax)))
    =/  cay=config
      :*  [bek sup]
          'comments'
          'comments'
          author
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
    ~/  %coll-ta-write
    =,  space:userlib
    |=  [pax=path cay=cage]
    ^+  ta-this
    =/  bek  byk.bol(r [%da now.bol])
    =.  pax  (en-beam:format bek (flop pax))
    %+  ta-emit  ost.bol
    [%info (weld /ta-write pax) (foal pax cay)]
  ::
  ++  ta-remove
    =,  space:userlib
    ~/  %coll-ta-remove
    |=  pax=path
    =/  bek  byk.bol(r [%da now.bol])
    =.  pax  (en-beam:format bek (flop pax))
    ^+  ta-this
    %+  ta-emit  ost.bol
    [%info (weld /ta-remove pax) (fray pax)]
  ::
  ::  permissions
  ::
  ++  ta-set-permissions
    ~/  %coll-ta-set-permissions
    |=  [pax=path r=rule:clay w=rule:clay]
    ^+  ta-this
    %+  ta-emit  ost.bol
    [%perm (weld /perms pax) q.byk.bol pax [%rw `r `w]]
  ::
  ++  ta-flush-permissions
    ~/  %coll-ta-flush-permissions
    |=  pax=path
    ^+  ta-this
    %+  ta-emit  ost.bol
    [%perm (weld /perms pax) q.byk.bol pax [%rw ~ ~]]
  ::
  ::  hall
  ::
  ++  ta-hall-action
    ~/  %coll-ta-hall-action
    |=  act=action:hall
    ^+  ta-this
    %+  ta-emit  ost.bol
    [%poke /col-hall-action [our.bol %hall] %hall-action act]
  ::
  ++  ta-hall-actions
    ~/  %coll-ta-hall-actions
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
    ~/  %coll-ta-hall-create-circle
    |=  [pax=path name=@t]
    ^+  ta-this
    =/  circ=circle:hall  (path-to-circle pax our.bol)
    =/  parent=circle:hall
      ?:  =(nom.circ %c)
        [our.bol %inbox]
      (path-to-circle (scag (dec (lent pax)) pax) our.bol)
    =/  acts=(list action:hall)
    :~  [%source nom.parent & (sy `source:hall`[circ ~] ~)]
        [%create nom.circ name %journal]
    ==
    ::  XX should we also source comment circles?
    =?  acts  =(nom.parent %c)
      [[%source %inbox & (sy `source:hall`[circ ~] ~)] acts]
    (ta-hall-actions (flop acts))
  ::
  ++  ta-hall-lin
    ~/  %coll-ta-hall-lin
    |=  [pax=path msg=cord]
    ^+  ta-this
    =/  circ=circle:hall  (path-to-circle pax our.bol)
    %-  ta-hall-action
    [%phrase [circ ~ ~] [%lin | msg]~]
  ::
  ++  ta-hall-json
    ~/  %coll-ta-hall-json
    |=  [pax=path header=@t jon=json]
    ^+  ta-this
    =/  circ=circle:hall  (path-to-circle pax our.bol)
    %-  ta-hall-action
    :+  %phrase   [circ ~ ~]
    [%fat [%text ~[header]] [%lin | (crip (en-json:html jon))]]~
  ::
  --
--
