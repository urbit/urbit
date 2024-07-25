/-  mast
/*  mast-js
=<
^-  kook:neo
|%
++  state  [%pro %sig]
++  poke   (sy %eyre-task %eyre-chan-task %gift ~)
++  kids
  :+  ~  %y
  %-  my
  :~  [[|/%p |] [%pro %manx] (sy %sig ~)] :: fix
      [[&/%aft |/%p |] [%pro %manx] ~]
  ==
++  deps   *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo =pail:neo]
  ::
  ++  init
    |=  pal=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    ?>  ?=(^ pal)
    =+  !<(prow:mast q.u.pal)
    =/  =pith:neo      #/[p/our.bowl]/$/eyre
    =/  =binding:eyre  [~ url]
    =/  =req:eyre:neo  [%connect binding here.bowl]
    =/  =rig:mast
      :*  urth
          made
          /eyre-chan/mast/(scot %ud ^-(@ud (mug here.bowl)))
          ~
      ==
    :_  sig/!>(rig)
    :~  [pith %poke eyre-req/!>(req)]
        [(snoc here.bowl p/ship.src.bowl) %make made.rig]
    ==
  ::
  ++  poke
    |=  [sud=stud:neo vaz=vase]
    ^-  (quip card:neo pail:neo)
    :: ~&  mast-poke/sud
    =+  !<(=rig:mast q.pail)
    ?+  sud  ~|(bad-stud/sud !!)
      ::
        %eyre-task
      =+  !<([rid=@ta req=inbound-request:eyre] vaz)
      ?.  authenticated.req
        :_  pail
        (~(make-auth-redirect res bowl) rid)
      ?+  method.request.req
        [(~(make-400 res bowl) rid) pail]
        ::
          %'GET'
        ?.  |(urth:rig =(our.bowl ship.src.bowl))
          :_  pail
          (~(make-403 res bowl) rid)
        =/  jig=(unit idea:neo)
          =/  g  (~(get by kid.kids.bowl) p/ship.src.bowl)
          ?~  g  ~
          fil.u.g
        ?~  jig
          =.  open-http.rig
            (~(put by open-http.rig) ship.src.bowl rid)
          ~&  open-http/rid
          :_  sig/!>(rig)
          :~  [(snoc here.bowl p/ship.src.bowl) %make made.rig]
          ==
        =/  sail=manx
          (hoist !<(manx q.pail.u.jig))
        :_  pail
        (~(gale res bowl) rid sail rig)
        ::
      ==
      ::
        %gift
      =/  rum=(list [=pith:neo =loot:neo])
        ~(tap of:neo !<(gift:neo vaz))
      =^  cards  rig
        =|  cards=(list card:neo)
        |-  ^-  (quip card:neo rig:mast)
        ?~  rum
          [cards rig]
        ?:  ?=([%aft *] pith.i.rum)
          $(rum t.rum)
        =/  jig=(unit idea:neo)
          (~(get of:neo kids.bowl) pith.i.rum)
        ?~  jig
          $(rum t.rum)
        =/  sail=manx
          (hoist !<(manx q.pail.u.jig))
        =/  boat=@p
          =/  p  (rear pith.i.rum)
          ?>  &(?=(^ p) ?=(%p -.p))
          +.p
        =/  rid=(unit @ta)
          (~(get by open-http.rig) boat)
        ?^  rid
          ~&  close-http/u.rid
          %=  $
            open-http.rig  (~(del by open-http.rig) boat)
            rum            t.rum
            cards          
              %+  weld
                cards
              %+  snoc
                (~(gale res bowl) u.rid sail rig)
              (new-aft-sail sail boat here.bowl)
          ==
        ?+  mode.loot.i.rum
          $(rum t.rum)
          ::
            %add
          %=  $
            rum    t.rum
            cards  (snoc cards (new-aft-sail sail boat here.bowl))
          ==
          ::
            %dif
          =/  aft=manx
            =/  ide=(unit idea:neo)
              (~(get of:neo kids.bowl) #/aft/[p/boat])
            ?~  ide
              ~&  >>>  missing-aft-sail/[here.bowl aft/boat]
              (hoist [[%html ~] [[%head ~] ~] [[%body ~] ~] ~])
            !<(manx q.pail.u.ide)
          %=  $
            rum    t.rum
            cards
              %+  weld
                cards
              %+  snoc
                (~(gust res bowl) rig aft sail)
              (new-aft-sail sail boat here.bowl)
          ==
          ::
        ==
      [cards sig/!>(rig)]
      ::
        %eyre-chan-task
      =+  !<(jon=json vaz)
      =/  dat
        (parse-channel-data jon)
      :_  pail
      :~  [(snoc here.bowl p/ship.src.bowl) %poke ui-event/!>(dat)]
      ==
      ::
    ==
  ::
  --
--
::
|%
::
++  res
  |_  =bowl:neo
  ::
  ++  gale
    |=  [rid=@ta sail=manx =rig:mast]
    ^-  (list card:neo)
    ?>  ?=(^ c.sail)
    %^    make-direct-http-cards
        rid
      [200 ['Content-Type' 'text/html'] ~]
    :-  ~
    ^-  octs
    %-  as-octt:mimes:html
    %-  en-xml:html
    %_    sail
        a.g
      ^-  mart
      :*  [%pith <^-(path (pout:neo here.bowl))>]  :: destination path to shrub from neo
          [%path <^-(path (snoc base-sub.rig (scot %p ship.src.bowl)))>]  :: sub path, shrub to eyre
          [%ship +:(scow %p our.bowl)]
          [%app "neo"]
          a.g.sail
      ==
        c.i.c
      (marl [script-node c.i.c.sail])
    ==
  ::
  ++  gust
    |=  [=rig:mast old=manx new=manx]
    ^-  (list card:neo)
    ?~  c.old  !!
    ?~  c.new  !!
    ?~  t.c.old  !!  :: fix
    ?~  t.c.new  !!
    ?~  a.g.new  !!
    :_  ~
    :-  #/[p/our.bowl]/$/eyre
    :-  %poke
    :-  %eyre-chan-gift
    !>
    ^-  chan-gift:eyre:neo
    :-  (snoc base-sub.rig (scot %p ship.src.bowl))
    %-  tape:enjs:format
    %-  en-xml:html
    :: ~&  >>  gust/(algo c.i.t.c.old c.i.t.c.new)
    ^-  manx
    ;g
      =url  v.i.a.g.new
      ;*  %+  algo
        c.i.t.c.old
      c.i.t.c.new
    ==
  ::
  ++  make-400
    |=  rid=@ta
    ^-  (list card:neo)
    %^    make-direct-http-cards
        rid
      [400 ~]
    ~
  ::
  ++  make-403
    |=  rid=@ta
    ^-  (list card:neo)
    %^    make-direct-http-cards
        rid
      [403 ~]
    ~
  ::
  ++  make-auth-redirect
    |=  rid=@ta
    ^-  (list card:neo)
    %^    make-direct-http-cards
        rid
      [307 ['Location' '/~/login?redirect='] ~]
    ~
  ::
  ++  make-direct-http-cards
    |=  [rid=@ta hed=response-header:http dat=(unit octs)]
    ^-  (list card:neo)
    =/  eyre=pith:neo       #/[p/our.bowl]/$/eyre
    =/  head=sign:eyre:neo  [rid %head hed]
    =/  data=sign:eyre:neo  [rid %data dat]
    =/  done=sign:eyre:neo  [rid %done ~]
    :~  [eyre %poke eyre-sign/!>(head)]
        [eyre %poke eyre-sign/!>(data)]
        [eyre %poke eyre-sign/!>(done)]
    ==
  ::
  --
::
++  new-aft-sail
  |=  [sail=manx boat=@p base=pith:neo]
  ^-  card:neo
  [(welp base #/aft/[p/boat]) %make [%manx [~ manx/!>(sail)] ~]]
::
++  parse-channel-data
  |=  jon=json
  ^-  crow:mast
  ((ot ~[path+pa data+(om so)]):dejs:format jon)
::
++  script-node
  ^-  manx
  ;script: {(trip mast-js)}
::
++  hoist
  |=  root=manx
  |^  ^-  manx
  (tanx root "0" "~")
  ++  tanx
    |=  [m=manx key=tape pkey=tape]
    =/  fkey=tape  (getv a.g.m %key)
    =/  nkey=tape  ?~(fkey key fkey)
    ?:  =(%$ n.g.m)
      ;span
        =mast  "text"
        =key    nkey
        =pkey   pkey
        ;+  m
      ==
    =:  a.g.m  %-  mart  
          ?~  fkey
            [[%key nkey] [[%pkey pkey] a.g.m]]
          [[%pkey pkey] a.g.m]
        c.m  (tarl c.m nkey)
    ==
    m
  ++  tarl
    |=  [m=marl key=tape]
    =/  i=@ud  0
    |-  ^-  marl
    ?~  m
      ~
    :-  %^  tanx  
          (manx i.m) 
        (weld (scow %ud i) (weld "-" key))
      key
    $(m t.m, i +(i))
  --
::
++  algo
  |=  [old=marl new=marl]
  ^-  marl
  =/  i=@ud  0
  =/  acc=marl  ~
  |-
  ?~  new
    ?.  =(~ old)
      ?:  =(%skip -.-.-.old)
        $(old +.old)
      :_  acc
      :_  ~
      :-  %d
      =/  c=@ud  0
      |-  ^-  mart
      ?~  old
        ~
      :-  :-  (crip (weld "d" <c>)) 
        (getv a.g.i.old %key)
      $(old t.old, c +(c))
    acc
  ?:  &(?=(^ old) =(%skip -.-.-.old))
    $(old t.old)
  ?:  =(%m n.g.i.new)
    $(new t.new, i +(i), acc (snoc acc i.new))
  =/  j=@ud  0
  =/  jold=marl  old
  =/  nkey=[n=mane k=tape]  [n.g.i.new (getv a.g.i.new %key)]
  |-
  ?~  new
    !!
  ?~  jold
    %=  ^$
      new  t.new
      i    +(i)
      acc  %+  snoc  acc
        ;n(id <i>)
          ;+  i.new
        ==
    ==
  ?~  old
    !!
  ?:  =(%skip n.g.i.jold)
    $(jold t.jold, j +(j))
  ?:  .=(nkey [n.g.i.jold (getv a.g.i.jold %key)])
    ?.  =(0 j)
      =/  n=@ud  0
      =/  nnew=marl  new
      =/  okey=[n=mane k=tape]  [n.g.i.old (getv a.g.i.old %key)]
      |-
      ?~  nnew
        ^^$(old (snoc t.old i.old))
      ?:  =(%m n.g.i.nnew)
        $(nnew t.nnew, n +(n))
      =/  nnky=[n=mane k=tape]  [n.g.i.nnew (getv a.g.i.nnew %key)]
      ?.  .=(okey nnky)
        $(nnew t.nnew, n +(n))
      ?:  (gte n j)
        =/  aupd=mart  (upda a.g.i.old a.g.i.nnew)
        ?~  aupd
          %=  ^^$
            old  c.i.old
            new  c.i.nnew
            i    0
            acc
              %=  ^^$
                old  t.old
                new  %^  newm  new  n
                  ;m(id <(add n i)>, key k.nnky);
              ==
          ==
        %=  ^^$
          old  c.i.old
          new  c.i.nnew
          i    0
          acc
            %=  ^^$
              old  t.old
              new  %^  newm  new  n
                ;m(id <(add n i)>, key k.nnky);
              acc  :_  acc
                [[%c [[%key k.nnky] aupd]] ~]
            ==
        ==
      =/  aupd=mart  (upda a.g.i.jold a.g.i.new)
      ?~  aupd
        %=  ^^$
          old  c.i.jold
          new  c.i.new
          i    0
          acc
            %=  ^^$
              old  (newm old j ;skip;)
              new  t.new
              i    +(i)
              acc  %+  snoc  acc
                ;m(id <i>, key k.nkey);
            ==
        ==
      %=  ^^$
        old  c.i.jold
        new  c.i.new
        i    0
        acc
          %=  ^^$
            old  (newm old j ;skip;)
            new  t.new
            i    +(i)
            acc  :-  [[%c [[%key k.nkey] aupd]] ~]
              %+  snoc
                acc
              ;m(id <i>, key k.nkey);
          ==
      ==
    ?:  =("text" (getv a.g.i.new %mast))
      ?:  =(+.-.+.-.-.+.-.old +.-.+.-.-.+.-.new)
        ^$(old t.old, new t.new, i +(i))
      %=  ^$
        old  t.old
        new  t.new
        i    +(i)
        acc  [i.new acc]
      ==
    =/  aupd=mart  (upda a.g.i.old a.g.i.new)
    ?~  aupd
      %=  ^$
        old  c.i.old
        new  c.i.new
        i    0
        acc  ^$(old t.old, new t.new, i +(i))
      ==
    %=  ^$
      old  c.i.old
      new  c.i.new
      i    0
      acc
        %=  ^$
          old  t.old
          new  t.new
          i    +(i)
          acc  :_  acc
            [[%c [[%key k.nkey] aupd]] ~]
        ==
    ==
  $(jold t.jold, j +(j))
::
++  getv
  |=  [m=mart tag=@tas]
  ^-  tape
  ?~  m
    ~
  ?:  =(n.i.m tag)
    v.i.m
  $(m t.m)
::
++  upda
  |=  [om=mart nm=mart]
  =/  acc=mart  ~
  |-  ^-  mart
  ?~  nm
    ?~  om
      acc
    :_  acc
    :-  %rem
    =/  omom=mart  om
    |-
    ?~  omom
      ~
    =/  nom=tape  +:<n.i.omom>
    |-
    ?~  nom
      [' ' ^$(omom t.omom)]
    [i.nom $(nom t.nom)]
  =/  i=@ud  0
  =/  com=mart  om
  |-
  ?~  nm
    !!
  ?~  com
    ^$(nm t.nm, acc [i.nm acc])
  ?~  om
    !!
  ?:  =(n.i.com n.i.nm)
    ?:  =(v.i.com v.i.nm)
      ^$(om (oust [i 1] (mart om)), nm t.nm)
    %=  ^$
      om  (oust [i 1] (mart om))
      nm  t.nm
      acc  [i.nm acc]
    ==
  $(com t.com, i +(i))
::
++  newm
  |=  [ml=marl i=@ud mx=manx]
  =/  j=@ud  0
  |-  ^-  marl
  ?~  ml
    ~
  :-  ?:  =(i j)
      mx
    i.ml
  $(ml t.ml, j +(j))
::
--
