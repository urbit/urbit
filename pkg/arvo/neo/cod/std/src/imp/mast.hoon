/@  ui-event
/@  mast-bind
/*  mast-js
=<
^-  kook:neo
|%
++  state  [%pro %sig]
++  poke   (sy %mast-bind %eyre-task %eyre-chan-task %gift ~)
++  deps   *deps:neo
++  kids
  :+  ~  %y
  %-  my
  :~  [[|/%ud |/%p |] [%pro %manx] (sy %ui-event %rely ~)]
  ==
++  form
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo =pail:neo]
  ::
  ++  init
    |=  pal=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    ~&  >  %mast-init
    =/  =pith:neo      #/[p/our.bowl]/$/eyre
    =/  =binding:eyre  [~ /mast]
    =/  =req:eyre:neo  [%connect binding ~(here moor our.bowl)]
    :_  sig/!>(*rig)
    :~  [pith %poke eyre-req/!>(req)]
    ==
  ::
  ++  poke
    |=  [sud=stud:neo vaz=vase]
    ^-  (quip card:neo pail:neo)
    :: ~&  mast-poke/sud
    =+  !<(=rig q.pail)
    ?+  sud  !!
      ::
        %mast-bind                 :: bind outside of sky
      ?>  =(our.bowl ship.src.bowl)
      =+  !<(bind=mast-bind vaz)
      =/  =pith:neo      #/[p/our.bowl]/$/eyre
      =/  =binding:eyre  [~ url.bind]
      =/  =req:eyre:neo  [%connect binding ~(here moor our.bowl)]
      =/  =rope          (mug view.bind src.bind)
      =.  endpoints.rig  (~(put by endpoints.rig) url.bind [view.bind src.bind])
      =?  public.rig  public.bind
        (~(put in public.rig) rope)
      :_  sig/!>(rig)
      :~  [pith %poke eyre-req/!>(req)]
      ==
      ::
        %eyre-task                 :: session creation via http
      =+  !<([rid=@ta req=inbound-request:eyre] vaz)
      ?.  authenticated.req   [(~(make-auth-redirect res bowl) rid) pail]
      ?+  method.request.req  [(~(make-400 res bowl) rid) pail]
        ::
          %'GET'
        =/  url=path  (stab url.request.req)
        =/  =bind
          ?:  ?=([%mast ^] url)
            [i.t.url (pave:neo t.t.url)]
          (~(got by endpoints.rig) url)
        =/  =rope  (mug view.bind src.bind)
        =/  =boat  ship.src.bowl
        ?.  ?|  =(our.bowl boat)
                (~(has in public.rig) rope)
            ==
          [(~(make-403 res bowl) rid) pail]
        =/  at=pith:neo    (~(session moor our.bowl) rope boat)
        =/  =made:neo      [view.bind ~ (my [%src src.bind] ~)]
        =.  open-http.rig  (~(put by open-http.rig) [rope boat] rid)
        :: ~&  >>  open-http/rid
        :_  sig/!>(rig)
        :~  [at %cull ~]
            [at %make made]
        ==
        ::
      ==
      ::
        %gift
      =/  rum=(list [=pith:neo =loot:neo])  ~(tap of:neo !<(gift:neo vaz))
      =/  rng  ~(. og eny.bowl)
      =^  cards  rig
        =|  cards=(list card:neo)
        |-  ^-  (quip card:neo ^rig)
        ?~  rum
          [cards rig]
        =/  jig=(unit idea:neo)  (~(get of:neo kids.bowl) pith.i.rum)
        ?~  jig
          $(rum t.rum)
        =/  =sail  (hoist !<(sail q.pail.u.jig))
        =/  =rope  =/(ud (rear (snip pith.i.rum)) ?>(&(?=(^ ud) ?=(%ud -.ud)) +.ud))
        =/  =boat  =/(p (rear pith.i.rum) ?>(&(?=(^ p) ?=(%p -.p)) +.p))
        =/  rid=(unit @ta)  (~(get by open-http.rig) [rope boat])
        ?^  rid
          :: ~&  >  close-http/u.rid
          =^  =buoy  rng  (rads:rng 1.000.000.000.000)
          %=  $
            open-http.rig  (~(del by open-http.rig) [rope boat])
            subs.rig       (~(put by subs.rig) [rope boat] buoy)
            aft.rig        (~(put by aft.rig) [rope boat] sail)
            cards          (weld cards (~(gale res bowl) u.rid rope buoy sail))
            rum            t.rum
          ==
        ?+  mode.loot.i.rum  $(rum t.rum)
          ::
            %dif
          =/  aft=^sail
            =/  sal=(unit ^sail)  (~(get by aft.rig) [rope boat])
            ?^  sal  u.sal
            (hoist [[%html ~] [[%head ~] ~] [[%body ~] ~] ~])
          =/  sub=path  (sub-path (~(got by subs.rig) [rope boat]))
          %=  $
            aft.rig  (~(put by aft.rig) [rope boat] sail)
            cards    (weld cards (~(gust res bowl) sub aft sail))
            rum      t.rum
          ==
          ::
        ==
      [cards sig/!>(rig)]
      ::
        %eyre-chan-task
      =+  !<(jon=json vaz)
      =/  =crow  (parse-channel-data jon)
      :_  pail
      :~  :-  (~(session moor our.bowl) rope.crow ship.src.bowl)
          [%poke ui-event/!>(`ui-event`[path.crow data.crow])]
      ==
      ::
    ==
  ::
  --
--
::
|%
::
+$  crow                           :: client to mast channel poke data
  [=rope =path data=(map @t @t)]
+$  view  @tas                     :: view imp
+$  bind  [=view src=pith:neo]     :: view to src binding
+$  rope  @                        :: view+src bind id
+$  buoy  @                        :: channel subscription id
+$  boat  @p                       :: src ship session id
+$  sail  manx
+$  rig                            ::  ::  :: mast state
  $:  open-http=(map [rope boat] @ta)  :: eyre ids pending session creation
      endpoints=(map path bind)        :: urls to view+src bindings (non-sky)
      public=(set rope)                :: view+src bindings served beyond =(ship.src our)
      subs=(map [rope boat] buoy)      :: eyre channel subscription ids by session
      aft=(map [rope boat] sail)       :: most recent sail state by session
  ==
::
++  moor                           :: assumes mast shrub location at /our-ship/mast
  |_  our=@p
  ::
  ++  here
    ^-  pith:neo
    #/[p/our]/mast
  ::
  ++  session
    |=  [=rope =boat]
    ^-  pith:neo
    =/  here  here
    ?>  &(?=(^ here) ?=(^ t.here))
    %_  here
      t.t  #/[ud/rope]/[p/boat]
    ==
  ::
  --
::
++  sub-path
  |=  =buoy
  ^-  path
  /eyre-chan/mast/(scot %ud buoy)
::
++  script-node
  ^-  manx
  ;script: {(trip mast-js)}
::
++  parse-channel-data
  |=  jon=json
  ^-  crow
  ((ot ~[rope+ni path+pa data+(om so)]):dejs:format jon)
::
++  res
  |_  =bowl:neo
  ::
  ++  gale                         ::  ::  :: send a full page
    |=  [rid=@ta =rope =buoy =sail]
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
      :*  [%rope (y-co:co rope)]                            :: id of bind target for an event poke
          [%pith (en-tape:pith:neo ~(here moor our.bowl))]  :: destination path, neo to shrub
          [%path (spud (sub-path buoy))]                    :: sub path, shrub to eyre
          [%ship +:(scow %p our.bowl)]
          [%app "neo"]
          a.g.sail
      ==
        c.i.c
      (marl [script-node c.i.c.sail])
    ==
  ::
  ++  gust                         ::  ::  :: send a diff update
    |=  [sub=path old=sail new=sail]
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
    !>  ^-  chan-gift:eyre:neo
    :-  sub
    ^-  json
    [%a (algo c.i.t.c.old c.i.t.c.new)]
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
++  hoist
  |_  root=manx
  ++  $
    ^-  manx
    (anx root ["" ~])
  ++  anx
    |=  [m=manx key=(pair tape (list @))]
    ^-  manx
    =/  fkey=@t  (getv %key a.g.m)
    =/  nkey=(pair tape (list @))  ?~(fkey key [((w-co:co 1) `@uw`(mug fkey)) ~])
    =/  ntap=tape  (weld p.nkey ((w-co:co 1) `@uw`(jam q.nkey)))
    ?:  =(%$ n.g.m)
      ;t-
        =key  ntap
        ;+  m
      ==
    %_    m
        a.g
      ^-  mart  
      ?~  fkey
        [[%key ntap] a.g.m]
      a.g.m
        c
      (arl c.m nkey)
    ==
  ++  arl
    |=  [m=marl key=(pair tape (list @))]
    =|  i=@
    |-  ^-  marl
    ?~  m  ~
    :-  %+  anx
          i.m
        key(q [i q.key])
    $(m t.m, i +(i))
  --
::
++  algo
  |=  [old=marl new=marl]
  =|  i=@ud
  =|  pkey=@t
  =|  acc=(list json)
  |-  ^-  (list json)
  ?~  new
    ?~  old
      acc
    ?:  =(%skip- n.g.i.old)
      %=  $
        old  t.old
      ==
    :_  acc
    ^-  json
    :-  %o
    %-  my
    :~  ['p' [%s 'd']]
        ['q' [%a (turn old |=(m=manx [%s (getv %key a.g.m)]))]]
    ==
  ?:  &(?=(^ old) =(%skip- n.g.i.old))
    %=  $
      old  t.old
    ==
  ?:  =(%move- n.g.i.new)
    %=  $
      new  t.new
      i    +(i)
      acc
        %+  snoc  acc
        ^-  json
        :-  %o
        %-  my
        :~  ['p' [%s 'm']]
            ['q' [%s (getv %key a.g.i.new)]]
            ['r' [%n (getv %i a.g.i.new)]]
        ==
    ==
  =|  j=@ud
  =/  jold=marl  old
  =/  nkey=[n=mane k=@t]  [n.g.i.new (getv %key a.g.i.new)]
  |-  ^-  (list json)
  ?~  new
    !!
  ?~  jold
    %=  ^$
      new  t.new
      i    +(i)
      acc
        %+  snoc  acc
        ^-  json
        :-  %o
        %-  my
        :~  ['p' [%s 'n']]
            ['q' [%s pkey]]
            ['r' [%n (scot %ud i)]]
            ['s' [%s (crip (en-xml:html i.new))]]
        ==
    ==
  ?~  old
    !!
  ?:  =(%skip- n.g.i.jold)
    %=  $
      jold  t.jold
      j     +(j)
    ==
  ?:  =(nkey [n.g.i.jold (getv %key a.g.i.jold)])
    ?.  =(0 j)
      =|  n=@ud
      =/  nnew=marl  new
      =/  okey=[n=mane k=@t]  [n.g.i.old (getv %key a.g.i.old)]
      |-  ^-  (list json)
      ?~  nnew
        %=  ^^$
          old  (snoc t.old i.old)
        ==
      ?:  =(%move- n.g.i.nnew)
        %=  $
          nnew  t.nnew
          n     +(n)
        ==
      =/  nnky=[n=mane k=@t]  [n.g.i.nnew (getv %key a.g.i.nnew)]
      ?.  =(okey nnky)
        %=  $
          nnew  t.nnew
          n     +(n)
        ==
      ?:  (gte n j)
        =/  aupd  (upda a.g.i.old a.g.i.nnew)
        %=  ^^$
          old   c.i.old
          new   c.i.nnew
          pkey  k.nnky
          i     0
          acc
            %=  ^^$
              old  t.old
              new
                %^  newm  new  n
                ;move-(i (scow %ud (add n i)), key (trip k.nnky));
              acc
                ?:  &(?=(~ del.aupd) ?=(~ new.aupd))
                  acc
                :_  acc
                ^-  json
                :-  %o
                %-  my
                :~  ['p' [%s 'c']]
                    ['q' [%s k.nnky]]
                    ['r' [%a del.aupd]]
                    ['s' [%a new.aupd]]
                ==
            ==
        ==
      =/  aupd  (upda a.g.i.jold a.g.i.new)
      %=  ^^$
        old   c.i.jold
        new   c.i.new
        pkey  k.nkey
        i     0
        acc
          %=  ^^$
            old  (newm old j ;skip-;)
            new  t.new
            i    +(i)
            acc
              =.  acc
                %+  snoc  acc
                ^-  json
                :-  %o
                %-  my
                :~  ['p' [%s 'm']]
                    ['q' [%s k.nkey]]
                    ['r' [%n (scot %ud i)]]
                ==
              ?:  &(?=(~ del.aupd) ?=(~ new.aupd))
                acc
              :_  acc
              ^-  json
              :-  %o
              %-  my
              :~  ['p' [%s 'c']]
                  ['q' [%s k.nkey]]
                  ['r' [%a del.aupd]]
                  ['s' [%a new.aupd]]
              ==
          ==
      ==
    ?:  =(%t- n.g.i.new)
      ?:  ?&  ?=(^ c.i.old)  ?=(^ c.i.new)
              ?=(^ a.g.i.c.i.old)  ?=(^ a.g.i.c.i.new)
              =(v.i.a.g.i.c.i.old v.i.a.g.i.c.i.new)
          ==
        %=  ^$
          old  t.old
          new  t.new
          i    +(i)
        ==
      =/  txt=@t
        ?.  &(?=(^ c.i.new) ?=(^ a.g.i.c.i.new))
          ''
        (crip v.i.a.g.i.c.i.new)
      %=  ^$
        old  t.old
        new  t.new
        i    +(i)
        acc
          :_  acc
          ^-  json
          :-  %o
          %-  my
          :~  ['p' [%s 't']]
              ['q' [%s (getv %key a.g.i.new)]]
              ['r' [%s txt]]
          ==
      ==
    =/  aupd  (upda a.g.i.old a.g.i.new)
    %=  ^$
      old   c.i.old
      new   c.i.new
      pkey  k.nkey
      i     0
      acc
        %=  ^$
          old  t.old
          new  t.new
          i    +(i)
          acc
            ?:  &(?=(~ del.aupd) ?=(~ new.aupd))
              acc
            :_  acc
            ^-  json
            :-  %o
            %-  my
            :~  ['p' [%s 'c']]
                ['q' [%s k.nkey]]
                ['r' [%a del.aupd]]
                ['s' [%a new.aupd]]
            ==
        ==
    ==
  %=  $
    jold  t.jold
    j     +(j)
  ==
::
++  getv
  |=  [t=@tas m=mart]
  ^-  @t
  ?~  m  ''
  ?:  =(n.i.m t)
    (crip v.i.m)
  $(m t.m)
::
++  upda
  |=  [om=mart nm=mart]
  =|  acc=[del=(list json) new=(list json)]
  |-  ^+  acc
  ?~  nm
    ?~  om
      acc
    %_    acc
        del
      %+  turn  om
      |=  [n=mane *]
      [%s `@t`?>(?=(@ n) n)]
    ==
  =|  i=@ud
  =/  com=mart  om
  |-  ^+  acc
  ?~  nm
    !!
  ?~  com
    %=  ^$
      nm  t.nm
      new.acc
        :_  new.acc
        :-  %a
        :~  [%s `@t`?>(?=(@ n.i.nm) n.i.nm)]
            [%s (crip v.i.nm)]
        ==
    ==
  ?~  om
    !!
  ?:  =(n.i.com n.i.nm)
    ?:  =(v.i.com v.i.nm)
      %=  ^$
        om  (oust [i 1] (mart om))
        nm  t.nm
      ==
    %=  ^$
      om   (oust [i 1] (mart om))
      nm   t.nm
      new.acc
        :_  new.acc
        :-  %a
        :~  [%s `@t`?>(?=(@ n.i.nm) n.i.nm)]
            [%s (crip v.i.nm)]
        ==
    ==
  %=  $
    com  t.com
    i    +(i)
  ==
::
++  newm
  |=  [ml=marl i=@ud mx=manx]
  =|  j=@ud
  |-  ^-  marl
  ?~  ml
    ~
  :-  ?:  =(i j)
      mx
    i.ml
  $(ml t.ml, j +(j))
::
--
