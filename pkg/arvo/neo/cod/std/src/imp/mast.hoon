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
    =|  =rig
    =.  brig.rig       eny.bowl
    =/  =pith:neo      #/[p/our.bowl]/$/eyre
    =/  =binding:eyre  [~ /mast]
    =/  =req:eyre:neo  [%connect binding ~(here moor our.bowl)]
    :_  sig/!>(rig)
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
      =/  =rope          (mug bind)
      =.  endpoints.rig  (~(put by endpoints.rig) url.bind [view.bind src.bind])
      =?  public.rig  public.bind
        (~(put in public.rig) rope)
      :_  sig/!>(rig)
      :~  [pith %poke eyre-req/!>(req)]
      ==
      ::
        %eyre-chan-task            :: channel poke from the client
      =+  !<(jon=json vaz)
      =/  =crow  (parse-channel-data jon)
      :_  pail
      :~  :-  (~(session moor our.bowl) rope.crow ship.src.bowl)
          [%poke ui-event/!>(`ui-event`[path.crow data.crow])]
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
        =/  =rope  (mug bind)
        =/  =boat  ship.src.bowl
        ?.  ?|  =(our.bowl boat)
                (~(has in public.rig) rope)
            ==
          [(~(make-403 res bowl) rid) pail]
        =/  at=pith:neo    (~(session moor our.bowl) rope boat)
        =/  =made:neo      [view.bind ~ (my [%src src.bind] ~)]
        =/  wat            (~(get by waiting.rig) [rope boat])
        =.  waiting.rig
          (~(put by waiting.rig) [rope boat] [rid ?~(wat ~ u.wat)])
        :_  sig/!>(rig)
        :~  [at %cull ~]
            [at %make made]
        ==
        ::
      ==
      ::
        %gift
      =/  rum=(list [=pith:neo =loot:neo])  ~(tap of:neo !<(gift:neo vaz))
      =^  cards  rig
        =|  cards=(list card:neo)
        |-  ^-  (quip card:neo ^rig)
        ?~  rum
          [cards rig]
        =/  jig=(unit idea:neo)  (~(get of:neo kids.bowl) pith.i.rum)
        ?~  jig
          $(rum t.rum)
        =/  =rope  =/(ud (rear (snip pith.i.rum)) ?>(&(?=(^ ud) ?=(%ud -.ud)) +.ud))
        =/  =boat  =/(p (rear pith.i.rum) ?>(&(?=(^ p) ?=(%p -.p)) +.p))
        =/  =buoy  (mug [brig.rig rope boat])
        =/  =sail  (hoist buoy brig.rig rope boat !<(sail q.pail.u.jig))
        =/  aft=(unit ^sail)  (~(get by aft.rig) [rope boat])
        ?:  =(%dif mode.loot.i.rum)
          :: handle sail component diff
          ?~  aft
            $(rum t.rum)
          =/  sub=path   (sub-path buoy)
          =/  =diff      (luff u.aft sail)
          =^  kid-cards  waiting.rig
            (make-kids our.bowl boat p.diff [[buoy rope] ~] waiting.rig)
          %=  $
            aft.rig  (~(put by aft.rig) [rope boat] sail)
            cards    (welp cards ?~(q.diff kid-cards [(~(gust res bowl) sub [%a q.diff]) kid-cards]))
            rum      t.rum
          ==
        :: handle sail component creation
        =/  build-keys   (~(got by waiting.rig) [rope boat])
        =/  kid-els      (find-kid-els sail)
        =/  kid-ropes    (turn kid-els |=(=bind (mug bind)))
        =^  kid-cards  waiting.rig
          (make-kids our.bowl boat kid-els build-keys waiting.rig)
        =:  waiting.rig  (~(del by waiting.rig) [rope boat])
            aft.rig      (~(put by aft.rig) [rope boat] sail)
            cards        ?~(kid-cards cards (weld cards kid-cards))
          ==
        |-  ^-  (quip card:neo ^rig)
        ?~  build-keys
          ^$(rum t.rum)
        =/  buil  (~(get by building.rig) i.build-keys)
        ?~  buil
          ?^  kid-els
            %=  $
              build-keys    t.build-keys
              building.rig  (~(put by building.rig) i.build-keys [(silt kid-ropes) sail])
            ==
          %=  $
            build-keys  t.build-keys
            cards
              %+  weld  cards
              ?@  i.build-keys
                (~(gale res bowl) i.build-keys sail)
              [(~(gust res bowl) [(sub-path buoy.i.build-keys) (make-gust-kid sail)]) ~]
          ==
        =:  sail.u.buil       (insert-kid-sail buoy sail.u.buil sail)
            remaining.u.buil  (~(del in remaining.u.buil) rope)
          ==
        =?  remaining.u.buil  ?=(^ kid-els)
          (~(gas in remaining.u.buil) kid-ropes)
        ?^  remaining.u.buil
          $(build-keys t.build-keys)
        %=  $
          build-keys    t.build-keys
          building.rig  (~(del by building.rig) i.build-keys)
          cards
            %+  weld  cards
            ?@  i.build-keys
              (~(gale res bowl) i.build-keys sail.u.buil)
            [(~(gust res bowl) [(sub-path buoy.i.build-keys) (make-gust-kid sail.u.buil)]) ~]
        ==
      [cards sig/!>(rig)]
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
+$  rope  @                        :: view+src bind id (mug bind)
+$  buoy  @                        :: channel subscription id (mug [brig rope boat])
+$  brig  @uvJ                     :: channel subscription secret
+$  boat  @p                       :: src ship session id
+$  sail  manx
+$  diff  (pair (list bind) (list json))
::
+$  waiting    (map [rope boat] (list to-build))  :: sail component makes waiting to be handled
+$  building   (map to-build [=remaining =sail])  :: sail component updates being built
+$  to-build   $@(@ta [=buoy =rope])              :: eyre-id, or parent sail component's sub id and root's rope
+$  remaining  (set rope)                         :: remaining nodes to complete a sail component update
::
+$  rig                            :: mast state
  $:  =brig
      =waiting
      =building
      endpoints=(map path bind)    :: urls to view+src bindings (non-sky)
      public=(set rope)            :: view+src bindings served beyond =(ship.src our)
      aft=(map [rope boat] sail)   :: most recent sail state by session
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
  /eyre-chan/mast/(crip (y-co:co buoy))
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
  ++  gale                         :: send a full page
    |=  [rid=@ta =sail]
    ^-  (list card:neo)
    %^    make-direct-http-cards
        rid
      [200 ['Content-Type' 'text/html'] ~]
    :-  ~
    ^-  octs
    %-  as-octt:mimes:html
    %-  en-xml:html
    =/  =mart
      :~  [%pith (en-tape:pith:neo ~(here moor our.bowl))]
          [%ship +:(scow %p our.bowl)]
      ==
    ?:  ?&  =(%html n.g.sail)
            ?=(^ c.sail)  ?=(^ t.c.sail)
            =(%body n.g.i.t.c.sail)
        ==
      %_  sail
        a.g    (weld mart a.g.sail)
        c.i.c  (snoc c.i.c.sail script-node)
      ==
    ^-  manx
    :-  [%html ~]
    :~  [[%head mart] [script-node ~]]
        ?:(=(%body n.g.sail) sail [[%body ~] [sail ~]])
    ==
  ::
  ++  gust                         :: send a diff update
    |=  [sub=path dif=json]
    ^-  card:neo
    :-  #/[p/our.bowl]/$/eyre
    :-  %poke
    :-  %eyre-chan-gift
    !>  ^-  chan-gift:eyre:neo
    [sub dif]
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
++  find-kid-els
  |=  m=manx
  =|  acc=(list bind)
  |-  ^-  (list bind)
  ?:  =(%kid n.g.m)
    :_  acc
    :-  (getv %view a.g.m)
    (pave:neo (stab (getv %pith a.g.m)))
  |-  ^-  (list bind)
  ?~  c.m  acc
  $(c.m t.c.m, acc ^$(m i.c.m))
::
++  make-kids
  |=  $:  our=@p
          =boat
          bin=(list bind)
          new=(list to-build)
          wat=waiting
      ==
  =|  car=(list card:neo)
  |-  ^-  (quip card:neo waiting)
  ?~  bin  [car wat]
  =/  =rope         (mug i.bin)
  =/  rest          (~(get by wat) [rope boat])
  =/  at=pith:neo   (~(session moor our) rope boat)
  %=  $
    bin  t.bin
    car
      :+  [at %cull ~]
        [at %make [view.i.bin ~ (my [%src src.i.bin] ~)]]
      car
    wat
      (~(put by wat) [rope boat] ?~(rest new (weld new u.rest)))
  ==
::
++  prepare-kid-sail
  |=  m=manx
  ^-  manx
  ?:  &(=(%html n.g.m) ?=(^ c.m) ?=(^ t.c.m) =(%body n.g.i.t.c.m))
    i.t.c.m(n.g %div)
  m
::
++  make-gust-kid
  |=  m=manx
  ^-  json
  =.  m  (prepare-kid-sail m)
  :-  %a
  :_  ~
  :-  %o
  %-  my
  :~  ['p' [%s 'k']]
      ['q' [%s (getv %key a.g.m)]]
      ['r' [%s (crip (en-xml:html m))]]
  ==
::
++  insert-kid-sail
  |=  [=buoy par=manx kid=manx]
  ^-  manx
  =.  kid       (prepare-kid-sail kid)
  =/  key=tape  (y-co:co buoy)
  ?:  &(=(%kid n.g.par) ?=(^ (find [key/key ~] a.g.par)))
    kid
  %_    par
      c
    |-  ^-  marl
    ?~  c.par  ~
    ?:  &(=(%kid n.g.i.c.par) ?=(^ (find [key/key ~] a.g.i.c.par)))
      [kid t.c.par]
    [i.c.par(c $(c.par c.i.c.par)) $(c.par t.c.par)]
  ==
::
++  prepare-root-mart
  |=  [=rope m=mart]
  ^-  mart
  :-  [%rope (y-co:co rope)]
  |-  ^-  mart
  ?~  m  ~
  ?:  |(=(%key n.i.m) =(%rope n.i.m))
    $(m t.m)
  [i.m $(m t.m)]
::
++  hoist                          :: process gifted sail
  |_  [=buoy =brig =rope =boat =sail]
  ++  $
    ^-  manx
    =/  root-key=tape  (y-co:co buoy)
    ?.  ?&  =(%html n.g.sail)
            ?=(^ c.sail)  ?=(^ t.c.sail)
            =(%body n.g.i.t.c.sail)
        ==
      (anx sail(a.g (prepare-root-mart rope a.g.sail)) [root-key ~])
    %_  sail
      i.t.c  (anx i.t.c.sail(a.g (prepare-root-mart rope a.g.i.t.c.sail)) [root-key ~])
    ==
  ++  anx
    |=  [m=manx key=(pair tape (list @))]
    ^-  manx
    =/  fkey=@t  (getv %key a.g.m)
    =/  nkey=(pair tape (list @))  ?~(fkey key [((w-co:co 1) `@uw`(mug fkey)) ~])
    =/  ntap=tape
      ?~  q.nkey  p.nkey
      (weld p.nkey ((w-co:co 1) `@uw`(jam q.nkey)))
    ?:  =(%kid n.g.m)
      =/  =view         (getv %view a.g.m)
      =/  pit=@t        (getv %pith a.g.m)
      =/  src=pith:neo  (pave:neo (stab pit))
      =/  kid-rope      (mug view src)
      =/  kid-buoy      (mug [brig kid-rope boat])
      %_    m
          c
        ~
          a.g
        :~  [%key (y-co:co kid-buoy)]
            [%view (trip view)]
            [%pith (trip pit)]
            [%rope (y-co:co kid-rope)]
        ==
      ==
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
      ?:  ?|  =(%input n.g.m)  =(%textarea n.g.m)
              =(%script n.g.m)  =(%img n.g.m)
              =(%link n.g.m)  =(%hr n.g.m)
              =(%meta n.g.m)  =(%base n.g.m)
          ==
        c.m
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
++  luff                           :: produce a sail diff for the client
  |=  [oldx=manx newx=manx]
  =/  [old=marl new=marl]
    :-  ?.  ?&  =(%html n.g.oldx)
              ?=(^ c.oldx)  ?=(^ t.c.oldx)
              =(%body n.g.i.t.c.oldx)
            ==
          [oldx ~]
        [i.t.c.oldx ~]
    ?.  ?&  =(%html n.g.newx)
            ?=(^ c.newx)  ?=(^ t.c.newx)
            =(%body n.g.i.t.c.newx)
        ==
      [newx ~]
    [i.t.c.newx ~]
  =|  i=@ud
  =|  pkey=@t
  =|  acc=diff
  |-  ^-  diff
  ?~  new
    ?~  old
      acc
    ?:  =(%skip- n.g.i.old)
      %=  $
        old  t.old
      ==
    %_    acc
        q
      :_  q.acc
      ^-  json
      :-  %o
      %-  my
      :~  ['p' [%s 'd']]
          ['q' [%a (turn old |=(m=manx [%s (getv %key a.g.m)]))]]
      ==
    ==
  ?:  =(%$ n.g.i.new)
    acc
  ?:  &(?=(^ old) =(%skip- n.g.i.old))
    %=  $
      old  t.old
    ==
  ?:  =(%move- n.g.i.new)
    %=  $
      new  t.new
      i    +(i)
      q.acc
        %+  snoc  q.acc
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
  |-  ^-  diff
  ?~  new
    !!
  ?~  jold
    %=  ^$
      new  t.new
      i    +(i)
      p.acc
        ?.  |(=(%kid n.g.i.new) ?=(^ c.i.new))
          p.acc
        (weld p.acc (find-kid-els i.new))
      q.acc
        %+  snoc  q.acc
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
      |-  ^-  diff
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
                ;move-(i (y-co:co (add n i)), key (trip k.nnky));
              q.acc
                ?:  &(?=(~ del.aupd) ?=(~ new.aupd))
                  q.acc
                :_  q.acc
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
            q.acc
              =.  q.acc
                %+  snoc  q.acc
                ^-  json
                :-  %o
                %-  my
                :~  ['p' [%s 'm']]
                    ['q' [%s k.nkey]]
                    ['r' [%n (scot %ud i)]]
                ==
              ?:  &(?=(~ del.aupd) ?=(~ new.aupd))
                q.acc
              :_  q.acc
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
        q.acc
          :_  q.acc
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
          q.acc
            ?:  &(?=(~ del.aupd) ?=(~ new.aupd))
              q.acc
            :_  q.acc
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
++  upda                           :: produce an attribute list diff
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
