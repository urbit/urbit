::
::::
  ::
/?  309
/+  collections, cram
/=  gas  /$  fuel:html
/=  itm  /%  /collections-web-item/
::
::
/=  collection-post     
::  /^  $-(raw-item:collections manx)
  /:  /===/web/landscape/collections/post     /%  /!noun/
/=  collection-details
  /^  manx
  /:  /===/web/landscape/collections/details  /%  /!hymn/
::
::
=<  (item-to-elem itm)
|%
++  item-to-elem
  |=  itm=item:collections
  ^-  manx
  =/  sho  (fall (~(get by qix.gas) %show) %default)
  ;div.container
  ;input
    =type  "hidden"
    =name  "urb-header"
    =value  "collection"
    =station  "{(scow %p p.bem.gas)}/{<(flop s.bem.gas)>}"
    =ship  "{(scow %p p.bem.gas)}";
  ;div.row
  ;div.col-sm-10.col-sm-offset-2
  ;div.collection-index.mt-12
  ;+
    ?-    -.itm
    ::
        %collection
      ?+  sho     !!
        %default  (collection-to-elem col.itm)
        %post     (collection-post ~ (flop s.bem.gas))
        %edit     !!
        %details  collection-details
      ==
    ::
        %raw
      ?+  sho     !!
        %default  (raw-to-elem raw.itm)
        %post     !!
        %edit     (collection-post `raw.itm (flop s.bem.gas))
        %details  collection-details
      ==
    ::
        %both
      ?+  sho     !!
        %default  (both-to-elem col.itm raw.itm)
        %post     !!
        %edit     (collection-post `raw.itm (flop s.bem.gas))
        %details  collection-details
      ==
    ::
    ==
  ==
  ==
  ==
  ==
++  collection-to-elem
  |=  col=collection:collections
  ^-  manx
  ;ul
    ;*  %+  turn  ~(tap by data.col)
        |=  [nom=knot ite=item:collections]
        ^-  manx
        ;li.collection-post.mt-6
          ;+  (item-to-snip nom ite)
        ==
  ==
::
++  raw-to-elem
  |=  raw=raw-item:collections
  ^-  manx
  =/  elm  elm:(static:cram (ream data.raw))
  =/  ht  (hedtal +.elm)
  =/  title  (fall (~(get by meta.raw) %name) -.s.bem.gas)
  =/  date   (fall (~(get by meta.raw) %date-created) 'missing date')
  =/  owner  (fall (~(get by meta.raw) %owner) 'anonymous')
  ::
  ;div
    ;div.collection-date: {(trip date)}
    ::
    ;div#show
      ;div.row.tit.mt-6.collection-title
        ;+  ?~  hed.ht
              ;h3: {(trip title)}
            ;h3: *{hed.ht}
      ==
    ==
    ::
    ;div.who.text-mono.text-600: {(trip owner)}
    ;div.row.content.mb-18.mt-6
      ;+  elm
    ==
    ::
    ::  if comments are enabled it should be a %both not a %raw
    ::  XX REVIEW ^^ not robust enough?
  ==
::
++  both-to-elem
  |=  [col=collection:collections raw=raw-item:collections]
  ^-  manx
  ;div
    ;+  (raw-to-elem raw)
    ::
    ;div
      ;div.mb-2
        ;span(urb-component "IconComment");
        ;span: {<~(wyt by data.col)>}
      ==
      ::
      ;ul
      ;*  %+  turn  ~(tap by data.col)  :: XX TODO: sort
          |=  [nom=knot ite=item:collections]
          ^-  manx
          ::  XX TODO: accept types other than comments
          ?>  ?=(%raw -.ite)
          ?>  =(%comments (~(got by meta.raw.ite) %type))
          =/  owner  (fall (~(get by meta.raw.ite) %owner) 'anonymous')
          =/  date  (fall (~(get by meta.raw.ite) %date-created) 'missing date')
          ;li.collection-comment
            ;div.collection-comment-avatar
              ;div(urb-component "AvatarSample1");
            ==
            ;div
              ;a.collection-comment-author.text-mono
                =href  "/~~/landscape/profile"
                ; {(trip owner)}
              ==
              ;+  elm:(static:cram (ream data.raw.ite))
            ==
            ;span.collection-date: {(trip date)}
          ==
      ==
      ::
      ;div
        =urb-component  "CommentCreate"
        =urb-pax        "{<(flop s.bem.gas)>}"
        =urb-ship       "{(scow %p p.bem.gas)}";
    ==
  ==
::
::
++  item-to-snip
  |=  [nom=knot itm=item:collections]
  ^-  manx
  ?-    -.itm
      %collection
    (collection-to-snip nom col.itm)
      %raw
    (raw-to-snip nom raw.itm)
      %both
    (both-to-snip nom col.itm raw.itm)
  ==
::
++  collection-to-snip
  |=  [nom=knot col=collection:collections]
  ^-  manx
  ;div
    ;div.collection-date: {<date-created.meta.col>}
    ;h3
      ;a(href "{(trip -.s.bem.gas)}/{(trip nom)}"): {(trip name.meta.col)}
    ==
    ;div.who.text-mono.text-600: {<owner.meta.col>}
    ;div.meta-cont
      ;div.com-count.ml-12
        ; {(trip (scot %ud ~(wyt by data.col)))} comments
      ==
    ==
  ==
::
++  raw-to-snip
  |=  [nom=knot raw=raw-item:collections]
  ^-  manx
  =/  elm=manx  elm:(static:cram (ream data.raw))
  =/  ht  (hedtal +.elm)
  =?  tal.ht  ?=(~ hed.ht)
    (scag 5 c.elm)
  =/  title  (fall (~(get by meta.raw) %name) nom)
  =/  date   (fall (~(get by meta.raw) %date-created) 'missing date')
  =/  owner  (fall (~(get by meta.raw) %owner) 'anonymous')
  ::
  ;div
    ;div.collection-date: {(trip date)}
    ;h3
      ;+  ?~  hed.ht
            ;a(href "{(trip -.s.bem.gas)}/{(trip nom)}"): {(trip title)}
          ;a(href "{(trip -.s.bem.gas)}/{(trip nom)}"): *{hed.ht}
    ==
    ;div.who.text-mono.text-600: {(trip owner)}
    ;div.snippet
      ;*  tal.ht
    ==
  ==
::
++  both-to-snip
  |=  [nom=knot col=collection:collections raw=raw-item:collections]
  ^-  manx
  =/  elm=manx  elm:(static:cram (ream data.raw))
  =/  ht  (hedtal +.elm)
  =?  tal.ht  ?=(~ hed.ht)
    (scag 5 c.elm)
  =/  title  (fall (~(get by meta.raw) %name) nom)
  ::
  ;div
    ;div.collection-date: {<date-created.meta.col>}
    ;h3
      ;+  ?~  hed.ht
            ;a(href "{(trip -.s.bem.gas)}/{(trip nom)}"): {(trip title)}
          ;a(href "{(trip -.s.bem.gas)}/{(trip nom)}"): *{hed.ht}
    ==
    ;div.who.text-mono.text-600: {<owner.meta.col>}
    ;div.snippet: *{tal.ht}
    ;div.meta-cont
      ;div.com-count.ml-12
        ; {(trip (scot %ud ~(wyt by data.col)))} comments
      ==
    ==
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
  |=  {lim/@u tay/tape}  ^-  {lim/@u tay/tape}
  ?~  tay  [lim tay]
  ?~  lim  [0 ~]
  =+  wer=(dot 1^1 tay)
  ?~  q.wer
    [lim - tay]:[i.tay $(tay t.tay)]
  =+  nex=$(lim (dec lim), tay q.q.u.q.wer)
  [-.nex [(wonk wer) +.nex]]
::
::
::
--
