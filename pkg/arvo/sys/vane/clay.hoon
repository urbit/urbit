::  clay (4c), revision control
::
::  The way to understand Clay is to take it section-by-section:
::
::  - Data structures.  You *must* start here; make sure you understand
::  the entire contents of +raft.
::
::  - Individual reads.  +aver is the entry point, follow it through
::  +read-at-tako to understand each kind of read.
::
::  - Subscriptions.  +wake is the center of this mechanism; nothing
::  else responds to subscriptions.  +wake has no arguments, which means
::  every subscription response happens when something in Clay's *state*
::  has changed.  No edge-triggered responses.
::
::  - Receiving foreign data.  For individual requests, this is
::  +take-foreign-answer.  For sync requests (%many, which is %sing %v
::  for a foreign desk), this is +foreign-update.
::
::  - Ford.  +ford builds hoon files and gives files their types.
::  Read +build-file for the first, and +read-file is the second.
::
::  - Writing to a desk.  Every write to a desk goes through +park, read
::  it thoroughly.
::
::  - Merges.  Control flow starts at +start-merge, then +merge, but
::  everything is scaffolding for +merge-by-germ, which is the ideal of
::  a merge function: it takes two commits and a merge strategy and
::  produces a new commit.
::
::  - Tombstoning.  This is in +tomb.
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::
::  We use a system of "invariant footnotes", where nonlocal invariants
::  are tagged with notes to construct a distributed argument that the
::  invariant is maintained.  For example, see [wake].
::
::  Each one should be described somewhere, and then it should be
::  referenced any time it's touched.  For example, any code which might
::  fill a subscription should be tagged with [wake], and if +wake is
::  not called by the end of that function, the function itself should
::  be tagged with [wake].
::
::  The tagged code should constitute an argument that the invariant is
::  maintained everywhere.  While this is vulnerable to omission ("I
::  forgot that X could fill a subscription", it provides a good minimum
::  bar.
::
::  Tag the specific line of code which affects the invariant.  You do
::  not need to tag every function in a call stack if the invariant is
::  guaranteed to be maintained by the time the function returns.
::
::  Some invariant references get tagged with whether they "open" or
::  "close" the invariant.  For example, adding a commit to the dome
::  "opens" the [wake] invariant, while calling +wake closes it.  When
::  an invariant opens, you should be able to scan down and find why it
::  closes in each possible flow of control.  For wake, these are
::  labeled like this:
::
::    open: [wake] <
::    close: [wake] >
::    open and almost immediately close: [wake] <>
::
::  This system is best used for nonlocal invariants and is not
::  necessary when a function can guarantee its own invariants.  For
::  example, consider a set alongside a @ud representing its size.
::  There is an invariant that any time you add or remove an item from
::  the set you must update its size.  If you're operating on these
::  directly, it could be beneficial to tag each line of code which
::  might modify the set and make it clear where the size is modified.
::
::  Sometimes code can be restructured so that many fewer tags are
::  needed.  In the above example, if the set is modified in many
::  places, it may be worth factoring out set+size into a data structure
::  with its own arms for put, del, uni, int, etc.  Then the invariant
::  only needs to be maintained within that data structure, and call
::  sites do not need to be tagged.
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::
::  Here are the structures.  `++raft` is the formal arvo state.  It's
::  also worth noting that many of the clay-related structures are
::  defined in lull.
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
=/  bud
  ^~
  =/  zuse  !>(..zuse)
  :*  zuse=zuse
      nave=(slap zuse !,(*hoon nave:clay))
      cork=(slap zuse !,(*hoon cork))
      same=(slap zuse !,(*hoon same))
      mime=(slap zuse !,(*hoon mime))
      cass=(slap zuse !,(*hoon cass:clay))
  ==
::
|=  our=ship
=,  clay
=>  |%
+$  aeon  @ud                                           ::  version number
::
::  Part of ++mery, representing the set of changes between the mergebase and
::  one of the desks being merged.
::
::  --  `new` is the set of files in the new desk and not in the mergebase.
::  --  `cal` is the set of changes in the new desk from the mergebase except
::      for any that are also in the other new desk.
::  --  `can` is the set of changes in the new desk from the mergebase and that
::      are also in the other new desk (potential conflicts).
::  --  `old` is the set of files in the mergebase and not in the new desk.
::
+$  cane
  $:  new=(map path lobe)
      cal=(map path lobe)
      can=(map path cage)
      old=(map path ~)
  ==
::
::  Type of request.
::
::  %d produces a set of desks, %p gets file permissions, %t gets all paths
::  with the specified prefix, %u checks for existence, %v produces a ++dome
::  of all desk data, %w gets @ud and @da variants for the given case, %x
::  gets file contents, %y gets a directory listing, and %z gets a recursive
::  hash of the file contents and children.
::
:: ++  care  ?(%d %p %t %u %v %w %x %y %z)
::
::  Keeps track of subscribers.
::
::  A map of requests to a set of all the subscribers who should be notified
::  when the request is filled/updated.
::
+$  cult  (jug wove duct)
::
::  State for ongoing %fuse merges. `con` maintains the ordering,
::  `sto` stores the data needed to merge, and `bas` is the base
::  beak for the merge.
::
+$  melt  [bas=beak con=(list [beak germ]) sto=(map beak (unit domo))]
::
::  Domestic desk state.
::
::  Includes subscriber list, dome (desk content), possible commit state (for
::  local changes), possible merge state (for incoming merges), and permissions.
::
+$  dojo
  $:  qyx=cult                                          ::  subscribers
      dom=dome                                          ::  desk state
      per=regs                                          ::  read perms per path
      pew=regs                                          ::  write perms per path
      fiz=melt                                          ::  state for mega merges
  ==
::
::  Over-the-wire backfill request/response
::
+$  fill
  $%  [%0 =desk =lobe]
      [%1 =desk =lobe]
  ==
::
::  All except %1 are deprecated
::
+$  fell
  $%  [%direct p=lobe q=page]
      [%delta p=lobe q=[p=mark q=lobe] r=page]
      [%dead p=lobe ~]
      [%1 peg=(unit page)]
  ==
::
::  New desk data.
::
::  Sent to other ships to update them about a particular desk.
::  Includes a map of all new aeons to hashes of their commits, the most
::  recent aeon, and sets of all new commits and data.  `bar` is always
::  empty now because we expect you to request any data you don't have
::  yet
::
+$  nako                                                ::  subscription state
  $:  gar=(map aeon tako)                               ::  new ids
      let=aeon                                          ::  next id
      lar=(set yaki)                                    ::  new commits
      bar=~                                             ::  new content
  ==                                                    ::
::
::
::  Formal vane state.
::
::  --  `rom` is our domestic state.
::  --  `hoy` is a collection of foreign ships where we know something about
::      their clay.
::  --  `ran` is the object store.
::  --  `mon` is a collection of mount points (mount point name to urbit
::      location).
::  --  `hez` is the unix duct that %ergo's should be sent to.
::  --  `cez` is a collection of named permission groups.
::  --  `pud` is an update that's waiting on a kernel upgrade
::
+$  raft                                                ::  filesystem
  $:  rom=room                                          ::  domestic
      hoy=(map ship rung)                               ::  foreign
      ran=rang                                          ::  hashes
      fad=flow                                          ::  ford cache
      mon=(map term beam)                               ::  mount points
      hez=(unit duct)                                   ::  sync duct
      cez=(map @ta crew)                                ::  permission groups
      tyr=(set duct)                                    ::  app subs
      tur=rock:tire                                     ::  last tire
      pud=(unit [=desk =yoki])                          ::  pending update
      sad=(map ship @da)                                ::  scry known broken
      bug=[veb=@ mas=@]                                 ::  verbosity
  ==                                                    ::
::
::  Unvalidated response to a request.
::
::  Like a +$rant, but with a page of data rather than a cage of it.
::
+$  rand                                                ::  unvalidated rant
          $:  p=[p=care q=case r=@tas]                  ::  clade release book
              q=path                                    ::  spur
              r=page                                    ::  data
          ==                                            ::
::
::  Generic desk state.
::
::  --  `lim` is the most recent date we're confident we have all the
::      information for.  For local desks, this is always `now`.  For foreign
::      desks, this is the last time we got a full update from the foreign
::      urbit.
::  --  `ref` is a possible request manager.  For local desks, this is null.
::      For foreign desks, this keeps track of all pending foreign requests
::      plus a cache of the responses to previous requests.
::  --  `qyx` is the set of subscriptions, with listening ducts. These
::      subscriptions exist only until they've been filled.
::  --  `dom` is the actual state of the filetree.  Since this is used almost
::      exclusively in `++ze`, we describe it there.
::
+$  rede                                                ::  universal project
          $:  lim=@da                                   ::  complete to
              ref=(unit rind)                           ::  outgoing requests
              qyx=cult                                  ::  subscribers
              dom=dome                                  ::  revision state
              per=regs                                  ::  read perms per path
              pew=regs                                  ::  write perms per path
              fiz=melt                                  ::  domestic mega merges
          ==                                            ::
::
::  Foreign request manager.
::
::  When we send a request to a foreign ship, we keep track of it in here.  This
::  includes a request counter, a map of request numbers to requests, a reverse
::  map of requesters to request numbers, a simple cache of common %sing
::  requests, and a possible nako if we've received data from the other ship and
::  are in the process of validating it.
::
+$  rind                                                ::  request manager
  $:  nix=@ud                                           ::  request index
      bom=(map @ud update-state)                        ::  outstanding
      fod=(map duct @ud)                                ::  current requests
      haw=(map mood (unit cage))                        ::  simple cache
  ==                                                    ::
::
+$  bill  (list dude:gall)
::
::  Active downloads
::
+$  update-state
  $:  =duct
      =rave
      have=(map lobe fell)
      need=(list $@(lobe [=tako =path =lobe]))          ::  opt deets for scry
      nako=(qeu (unit nako))
      busy=(unit $@(%ames [kind=@ta =time =path]))      ::  pending request
  ==
::
::  Domestic ship.
::
::  `hun` is the duct to dill, and `dos` is a collection of our desks.
::
+$  room                                                ::  fs per ship
          $:  hun=duct                                  ::  terminal duct
              dos=(map desk dojo)                       ::  native desk
          ==                                            ::
::
::  Stored request.
::
::  Like a +$rave but with caches of current versions for %next and %many.
::  Generally used when we store a request in our state somewhere.
::
::  TODO: remove lobes from %many
::
+$  cach  (unit (unit cage))                            ::  cached result
+$  wove  [for=(unit [=ship ver=@ud]) =rove]            ::  stored source + req
+$  rove                                                ::  stored request
          $%  [%sing =mood]                             ::  single request
              [%next =mood aeon=(unit aeon) =cach]      ::  next version of one
              $:  %mult                                 ::  next version of any
                  =mool                                 ::  original request
                  aeon=(unit aeon)                      ::  checking for change
                  old-cach=(map [=care =path] cach)     ::  old version
                  new-cach=(map [=care =path] cach)     ::  new version
              ==                                        ::
              [%many track=? =moat lobes=(map path lobe)] ::  change range
          ==                                            ::
::
::  Foreign desk data.
::
+$  rung
          $:  rus=(map desk rede)                       ::  neighbor desks
          ==
::
+$  card  (wind note gift)                              ::  local card
+$  move  [p=duct q=card]                               ::  local move
+$  note                                                ::  out request $->
  $~  [%b %wait *@da]                                   ::
  $%  $:  %$                                            ::  to arvo
          $>(%what waif)                                ::
      ==                                                ::
      $:  %a                                            ::  to %ames
          $>(?(%plea %keen %yawn) task:ames)            ::
      ==                                                ::
      $:  %b                                            ::  to %behn
          $>  $?  %drip                                 ::
                  %rest                                 ::
                  %wait                                 ::
              ==                                        ::
          task:behn                                     ::
      ==                                                ::
      $:  %c                                            ::  to %clay
          $>  $?  %info                                 ::  internal edit
                  %merg                                 ::  merge desks
                  %fuse                                 ::  merge many
                  %park                                 ::
                  %perm                                 ::
                  %pork                                 ::
                  %warp                                 ::
                  %werp                                 ::
              ==                                        ::
          task                                          ::
      ==                                                ::
      $:  %d                                            ::  to %dill
          $>  $?  %flog                                 ::
                  %text                                 ::
              ==                                        ::
          task:dill                                     ::
      ==                                                ::
      $:  %g                                            ::  to %gall
          $>  $?  %deal
                  %jolt
                  %load
              ==
          task:gall
      ==                                                ::
      $:  %j                                            ::  by %jael
          $>(%public-keys task:jael)                    ::
  ==  ==                                                ::
+$  riot  (unit rant)                                   ::  response+complete
+$  sign                                                ::  in result $<-
  $~  [%behn %wake ~]                                   ::
  $%  $:  %ames                                         ::
          $>  $?  %boon                                 ::  response
                  %done                                 ::  (n)ack
                  %lost                                 ::  lost boon
                  %tune                                 ::  scry response
              ==                                        ::
          gift:ames                                     ::
      ==                                                ::
      $:  %behn                                         ::
          $%  $>(%wake gift:behn)                       ::  timer activate
              $>(%writ gift)                            ::
      ==  ==                                            ::
      $:  %clay                                         ::
          $>  $?  %mere                                 ::
                  %writ                                 ::
                  %wris                                 ::
              ==                                        ::
          gift                                          ::
      ==                                                ::
      $:  %gall
          $>  $?  %unto
              ==
          gift:gall
      ==
      $:  %jael                                         ::
          $>(%public-keys gift:jael)                    ::
  ==  ==                                                ::
--  =>
~%  %clay-utilities  ..part  ~
::  %utilities
::
|%
++  scry-timeout-time  ~m5
++  scry-retry-time    ~h1
::  +sort-by-head: sorts alphabetically using the head of each element
::
++  sort-by-head
  |=([a=(pair path *) b=(pair path *)] (aor p.a p.b))
::
::  By convention: paf == (weld pax pat)
::
++  mode-to-commit
  |=  [hat=(map path lobe) pax=path all=? mod=mode]
  ^-  [deletes=(set path) changes=(map path cage)]
  =/  deletes
    %-  silt
    %+  turn
      ^-  (list path)
      %+  weld
        ^-  (list path)
        %+  murn  mod
        |=  [pat=path mim=(unit mime)]
        ^-  (unit path)
        ?^  mim
          ~
        `pat
      ^-  (list path)
      ?.  all
        ~
      =+  mad=(malt mod)
      =+  len=(lent pax)
      =/  descendants=(list path)
        %+  turn
          %+  skim  ~(tap by hat)
          |=  [paf=path lob=lobe]
          =(pax (scag len paf))
        |=  [paf=path lob=lobe]
        (slag len paf)
      %+  skim
        descendants
      |=  pat=path
      (~(has by mad) pat)
    |=  pat=path
    (weld pax pat)
  ::
  =/  changes
    %-  malt
    %+  murn  mod
    |=  [pat=path mim=(unit mime)]
    ^-  (unit [path cage])
    ?~  mim
      ~
    `[(weld pax pat) %mime !>(u.mim)]
  ::
  [deletes changes]
::
++  pour-to-mist
  |=  =pour
  ^-  mist
  ?+    -.pour  pour
      %vale  [%vale path.pour]
      %arch  [%arch path.pour]
  ==
::
++  fell-to-page
  |=  =fell
  ^-  (unit page)
  ?-  -.fell
    %dead    ~
    %direct  `q.fell
    %delta   ~
    %1       peg.fell
  ==
::
++  rave-to-rove
  |=  rav=rave
  ^-  rove
  ?-  -.rav
    %sing  rav
    %next  [- mood ~ ~]:rav
    %mult  [- mool ~ ~ ~]:rav
    %many  [- track moat ~]:rav
  ==
::
++  rove-to-rave
  |=  rov=rove
  ^-  rave
  ?-  -.rov
    %sing  rov
    %next  [- mood]:rov
    %mult  [- mool]:rov
    %many  [- track moat]:rov
  ==
--  =>
~%  %clay  +  ~
|%
::  Printable form of a wove; useful for debugging
::
++  print-wove
  |=  =wove
  :-  for.wove
  ?-  -.rove.wove
    %sing  [%sing mood.rove.wove]
    %next  [%next [mood aeon]:rove.wove]
    %mult  [%mult [mool aeon]:rove.wove]
    %many  [%many [track moat]:rove.wove]
  ==
::
::  Printable form of a cult; useful for debugging
::
++  print-cult
  |=  =cult
  %+  turn  ~(tap by cult)
  |=  [=wove ducts=(set duct)]
  [ducts (print-wove wove)]
::
++  fusion
  ~%  %fusion  ..fusion  ~
  |%
  ::  +wrap: external wrapper
  ::
  ++  wrap
    |*  [* state:ford]
    [+<- +<+< +<+>-]  ::  [result cache.state flue]
  ::
  ++  with-face  |=([face=@tas =vase] vase(p [%face face p.vase]))
  ++  with-faces
    =|  res=(unit vase)
    |=  vaz=(list [face=@tas =vase])
    ^-  vase
    ?~  vaz  (need res)
    =/  faz  (with-face i.vaz)
    =.  res  `?~(res faz (slop faz u.res))
    $(vaz t.vaz)
  ::
  ++  ford
    !.
    =>  |%
        +$  state
          $:  cache=flow
              flue
              cycle=(set mist)
              drain=(map mist leak)
              stack=(list (set leak))
          ==
        +$  args
          $:  files=(map path (each page lobe))
              file-store=(map lobe page)
              verb=@
              cache=flow
              flue
          ==
        --
    ~%  %ford-gate  ..ford  ~
    |=  args
    ::  nub: internal mutable state for this computation
    ::
    =|  nub=state
    =.  cache.nub  cache
    =.  spill.nub  spill
    =.  sprig.nub  sprig
    ~%  %ford-core  ..$  ~
    |%
    ::  +read-file: retrieve marked, validated file contents at path
    ::
    ++  read-file
      ~/  %read-file
      |=  =path
      ^-  [cage state]
      ~|  %error-validating^path
      %-  soak-cage
      %+  gain-sprig  vale+path  |.
      =.  stack.nub  [~ stack.nub]
      ?:  (~(has in cycle.nub) vale+path)
        ~|(cycle+vale+path^cycle.nub !!)
      =.  cycle.nub  (~(put in cycle.nub) vale+path)
      %+  gain-leak  vale+path
      |=  nob=state
      =.  nub  nob
      %-  (trace 1 |.("read file {(spud path)}"))
      =/  file
        ~|  %file-not-found^path
        (~(got by files) path)
      =/  page
        ?:  ?=(%& -.file)
          p.file
        ~|  %tombstoned-file^path^p.file
        (~(got by file-store) p.file)
      =^  =cage  nub  (validate-page path page)
      [[%cage cage] nub]
    ::
    ::  +build-nave: build a statically typed mark core
    ::
    ++  build-nave
      ~/  %build-nave
      |=  mak=mark
      ^-  [vase state]
      ~|  %error-building-mark^mak
      %-  soak-vase
      %+  gain-sprig  nave+mak  |.
      =.  stack.nub  [~ stack.nub]
      ?:  (~(has in cycle.nub) nave+mak)
        ~|(cycle+nave+mak^cycle.nub !!)
      =.  cycle.nub  (~(put in cycle.nub) nave+mak)
      %-  (trace 1 |.("make mark {<mak>}"))
      =^  cor=vase  nub  (build-fit %mar mak)
      =/  gad=vase  (slap cor limb/%grad)
      ?@  q.gad
        =+  !<(mok=mark gad)
        =^  deg=vase  nub  ^$(mak mok)
        =^  tub=vase  nub  (build-cast mak mok)
        =^  but=vase  nub  (build-cast mok mak)
        %+  gain-leak  nave+mak
        |=  nob=state
        =.  nub  nob
        :_  nub  :-  %vase
        ^-  vase  ::  vase of nave
        %+  slap
          (with-faces deg+deg tub+tub but+but cor+cor nave+nave.bud ~)
        !,  *hoon
        =/  typ  _+<.cor
        =/  dif  _*diff:deg
        ^-  (nave typ dif)
        |%
        ++  diff
          |=  [old=typ new=typ]
          ^-  dif
          (diff:deg (tub old) (tub new))
        ++  form  form:deg
        ++  join  join:deg
        ++  mash  mash:deg
        ++  pact
          |=  [v=typ d=dif]
          ^-  typ
          (but (pact:deg (tub v) d))
        ++  vale  noun:grab:cor
        --
      %+  gain-leak  nave+mak
      |=  nob=state
      =.  nub  nob
      :_  nub  :-  %vase
      ^-  vase  ::  vase of nave
      %+  slap  (slop (with-face cor+cor) zuse.bud)
      !,  *hoon
      =/  typ  _+<.cor
      =/  dif  _*diff:grad:cor
      ^-  (nave:clay typ dif)
      |%
      ++  diff  |=([old=typ new=typ] (diff:~(grad cor old) new))
      ++  form  form:grad:cor
      ++  join
        |=  [a=dif b=dif]
        ^-  (unit (unit dif))
        ?:  =(a b)
          ~
        `(join:grad:cor a b)
      ++  mash
        |=  [a=[=ship =desk =dif] b=[=ship =desk =dif]]
        ^-  (unit dif)
        ?:  =(dif.a dif.b)
          ~
        `(mash:grad:cor a b)
      ++  pact  |=([v=typ d=dif] (pact:~(grad cor v) d))
      ++  vale  noun:grab:cor
      --
    ::  +build-dais: build a dynamically typed mark definition
    ::
    ++  build-dais
      ~/  %build-dais
      |=  mak=mark
      ^-  [dais state]
      ~|  %error-building-dais^mak
      %-  soak-dais
      %+  gain-sprig  dais+mak  |.
      =.  stack.nub  [~ stack.nub]
      ?:  (~(has in cycle.nub) dais+mak)
        ~|(cycle+dais+mak^cycle.nub !!)
      =.  cycle.nub  (~(put in cycle.nub) dais+mak)
      =^  nav=vase  nub  (build-nave mak)
      %+  gain-leak  dais+mak
      |=  nob=state
      =.  nub  nob
      %-  (trace 1 |.("make dais {<mak>}"))
      :_  nub  :-  %dais
      ^-  dais
      =>  [nav=nav ..zuse]
      |_  sam=vase
      ++  diff
        |=  new=vase
        (slam (slap nav limb/%diff) (slop sam new))
      ++  form  !<(mark (slap nav limb/%form))
      ++  join
        |=  [a=vase b=vase]
        ^-  (unit (unit vase))
        =/  res=vase  (slam (slap nav limb/%join) (slop a b))
        ?~  q.res    ~
        ?~  +.q.res  [~ ~]
        ``(slap res !,(*hoon ?>(?=([~ ~ *] .) u.u)))
      ++  mash
        |=  [a=[=ship =desk diff=vase] b=[=ship =desk diff=vase]]
        ^-  (unit vase)
        =/  res=vase
          %+  slam  (slap nav limb/%mash)
          %+  slop
            :(slop [[%atom %p ~] ship.a] [[%atom %tas ~] desk.a] diff.a)
          :(slop [[%atom %p ~] ship.b] [[%atom %tas ~] desk.b] diff.b)
        ?~  q.res
          ~
        `(slap res !,(*hoon ?>((^ .) u)))
      ++  pact
        |=  diff=vase
        (slam (slap nav limb/%pact) (slop sam diff))
      ++  vale
        |:  noun=q:(slap nav !,(*hoon *vale))
        (slam (slap nav limb/%vale) noun/noun)
      --
    ::  +build-cast: produce gate to convert mark .a to, statically typed
    ::
    ++  build-cast
      ~/  %build-cast
      |=  [a=mark b=mark]
      ^-  [vase state]
      ~|  error-building-cast+[a b]
      %-  soak-vase
      %+  gain-sprig  cast+a^b  |.
      =.  stack.nub  [~ stack.nub]
      ?:  (~(has in cycle.nub) cast+[a b])
        ~|(cycle+cast+[a b]^cycle.nub !!)
      ?:  =(a b)
        %+  gain-leak  cast+a^b
        |=  nob=state
        %-  (trace 4 |.("identity shortcircuit"))
        =.  nub  nob
        :_(nub vase+same.bud)
      ?:  =([%mime %hoon] [a b])
        %-  (trace 4 |.("%mime -> %hoon shortcircuit"))
        :_(nub [%vase =>(..zuse !>(|=(m=mime q.q.m)))])
      ::  try +grow; is there a +grow core with a .b arm?
      ::
      %-  (trace 1 |.("make cast {<a>} -> {<b>}"))
      =^  old=vase  nub  (build-fit %mar a)
      ?:  =/  ram  (mule |.((slap old !,(*hoon grow))))
          ?:  ?=(%| -.ram)  %.n
          =/  lab  (mule |.((slob b p.p.ram)))
          ?:  ?=(%| -.lab)  %.n
          p.lab
        ::  +grow core has .b arm; use that
        ::
        %+  gain-leak  cast+a^b
        |=  nob=state
        %-  (trace 4 |.("{<a>} -> {<b>}: +{(trip b)}:grow:{(trip a)}"))
        =.  nub  nob
        :_  nub  :-  %vase
        %+  slap  (with-faces cor+old ~)
        ^-  hoon
        :+  %brcl  !,(*hoon v=+<.cor)
        :+  %tsgl  limb/b
        !,(*hoon ~(grow cor v))
      ::  try direct +grab
      ::
      =^  new=vase  nub  (build-fit %mar b)
      =/  rab  (mule |.((slap new tsgl/[limb/a limb/%grab])))
      ?:  &(?=(%& -.rab) ?=(^ q.p.rab))
        %+  gain-leak  cast+a^b
        |=  nob=state
        %-  (trace 4 |.("{<a>} -> {<b>}: +{(trip a)}:grab:{(trip b)}"))
        =.  nub  nob
        :_(nub vase+p.rab)
      ::  try +jump
      ::
      =/  jum  (mule |.((slap old tsgl/[limb/b limb/%jump])))
      ?:  ?=(%& -.jum)
        =/  via  !<(mark p.jum)
        %-  (trace 4 |.("{<a>} -> {<b>}: via {<via>} per +jump:{(trip a)}"))
        (compose-casts a via b)
      ?:  ?=(%& -.rab)
        =/  via  !<(mark p.rab)
        %-  (trace 4 |.("{<a>} -> {<b>}: via {<via>} per +grab:{(trip b)}"))
        (compose-casts a via b)
      ?:  ?=(%noun b)
        %+  gain-leak  cast+a^b
        |=  nob=state
        %-  (trace 4 |.("{<a>} -> {<b>} default"))
        =.  nub  nob
        :_(nub vase+same.bud)
      ~|(no-cast-from+[a b] !!)
    ::
    ++  compose-casts
      |=  [x=mark y=mark z=mark]
      ^-  [soak state]
      =^  uno=vase  nub  (build-cast x y)
      =^  dos=vase  nub  (build-cast y z)
      %+  gain-leak  cast+x^z
      |=  nob=state
      =.  nub  nob
      :_  nub  :-  %vase
      %+  slap
        (with-faces uno+uno dos+dos ~)
      !,(*hoon |=(_+<.uno (dos (uno +<))))
    ::  +build-tube: produce a $tube mark conversion gate from .a to .b
    ::
    ++  build-tube
      |=  [a=mark b=mark]
      ^-  [tube state]
      ~|  error-building-tube+[a b]
      %-  soak-tube
      %+  gain-sprig  tube+a^b  |.
      =.  stack.nub  [~ stack.nub]
      ?:  (~(has in cycle.nub) tube+[a b])
        ~|(cycle+tube+[a b]^cycle.nub !!)
      =^  gat=vase  nub  (build-cast a b)
      %+  gain-leak  tube+a^b
      |=  nob=state
      =.  nub  nob
      %-  (trace 1 |.("make tube {<a>} -> {<b>}"))
      :_(nub [%tube =>([gat=gat ..zuse] |=(v=vase (slam gat v)))])
    ::
    ++  validate-page
      |=  [=path =page]
      ^-  [cage state]
      ~|  validate-page-fail+path^from+p.page
      =/  mak=mark  (head (flop path))
      ?:  =(mak p.page)
        (page-to-cage page)
      =^  [mark vax=vase]  nub  (page-to-cage page)
      =^  =tube  nub  (build-tube p.page mak)
      :_(nub [mak (tube vax)])
    ::
    ++  page-to-cage
      |=  =page
      ^-  [cage state]
      ?:  =(%hoon p.page)
        :_(nub [%hoon [%atom %t ~] q.page])
      ?:  =(%mime p.page)
        :_(nub [%mime =>([;;(mime q.page) ..zuse] !>(-))])
      =^  =dais  nub  (build-dais p.page)
      :_(nub [p.page (vale:dais q.page)])
    ::
    ++  cast-path
      |=  [=path mak=mark]
      ^-  [cage state]
      =/  mok  (head (flop path))
      ~|  error-casting-path+[path mok mak]
      =^  cag=cage  nub  (read-file path)
      ?:  =(mok mak)
        [cag nub]
      =^  =tube  nub  (build-tube mok mak)
      ~|  error-running-cast+[path mok mak]
      :_(nub [mak (tube q.cag)])
    ::
    ++  run-pact
      |=  [old=page diff=page]
      ^-  [cage state]
      ?:  ?=(%hoon p.old)
        =/  txt=wain  (to-wain:format ;;(@t q.old))
        =+  ;;(dif=(urge cord) q.diff)
        =/  new=@t  (of-wain:format (lurk:differ txt dif))
        :_(nub [%hoon =>([new ..zuse] !>(-))])
      =^  dys=dais  nub  (build-dais p.old)
      =^  syd=dais  nub  (build-dais p.diff)
      :_(nub [p.old (~(pact dys (vale:dys q.old)) (vale:syd q.diff))])
    ::
    ++  prelude
      |=  =path
      ^-  vase
      =^  cag=cage  nub  (read-file path)
      ?>  =(%hoon p.cag)
      =/  tex=tape  (trip !<(@t q.cag))
      =/  =pile  (parse-pile path tex)
      =.  hoon.pile  !,(*hoon .)
      =^  res=vase  nub  (run-prelude pile)
      res
    ::
    ++  build-dependency
      ~/  %build-dep
      |=  dep=(each [dir=path fil=path] path)
      ^-  [vase state]
      =/  =path
        ?:(?=(%| -.dep) p.dep fil.p.dep)
      ~|  %error-building^path
      %-  soak-vase
      %+  gain-sprig  file+path  |.
      =.  stack.nub  [~ stack.nub]
      %-  (trace 1 |.("make file {(spud path)}"))
      ?:  (~(has in cycle.nub) file+path)
        ~|(cycle+file+path^cycle.nub !!)
      =.  cycle.nub  (~(put in cycle.nub) file+path)
      =^  cag=cage  nub  (read-file path)
      ?>  =(%hoon p.cag)
      =/  tex=tape  (trip !<(@t q.cag))
      =/  =pile  (parse-pile path tex)
      =^  sut=vase  nub  (run-prelude pile)
      %+  gain-leak  file+path
      |=  nob=state
      =.  nub  nob
      =/  res=vase  (slap sut hoon.pile)
      [[%vase res] nub]
    ::
    ++  build-file
      |=  =path
      (build-dependency |+path)
    ::  +build-directory: builds files in top level of a directory
    ::
    ::    this excludes files directly at /path/hoon,
    ::    instead only including files in the unix-style directory at /path,
    ::    such as /path/file/hoon, but not /path/more/file/hoon.
    ::
    ++  build-directory
      |=  =path
      ^-  [(map @ta vase) state]
      %-  soak-arch
      %+  gain-sprig  arch+path  |.
      =.  stack.nub  [~ stack.nub]
      %+  gain-leak  arch+path
      |=  nob=state
      =.  nub  nob
      =/  fiz=(list @ta)
        =/  len  (lent path)
        %+  murn  ~(tap by files)
        |=  [pax=^path *]
        ^-  (unit @ta)
        ?.  =(path (scag len pax))
          ~
        =/  pat  (slag len pax)
        ?:  ?=([@ %hoon ~] pat)
          `i.pat
        ~
      ::
      =|  rez=(map @ta vase)
      |-
      ?~  fiz
        [[%arch rez] nub]
      =*  nom=@ta    i.fiz
      =/  pax=^path  (weld path nom %hoon ~)
      =^  res  nub   (build-dependency &+[path pax])
      $(fiz t.fiz, rez (~(put by rez) nom res))
    ::
    ++  run-prelude
      |=  =pile
      =/  sut=vase  zuse.bud
      =^  sut=vase  nub  (run-tauts sut %sur sur.pile)
      =^  sut=vase  nub  (run-tauts sut %lib lib.pile)
      =^  sut=vase  nub  (run-raw sut raw.pile)
      =^  sut=vase  nub  (run-raz sut raz.pile)
      =^  sut=vase  nub  (run-maz sut maz.pile)
      =^  sut=vase  nub  (run-caz sut caz.pile)
      =^  sut=vase  nub  (run-bar sut bar.pile)
      [sut nub]
    ::
    ++  parse-pile
      ~/  %parse-pile
      |=  [pax=path tex=tape]
      ^-  pile
      =/  [=hair res=(unit [=pile =nail])]
        %-  road  |.
        ((pile-rule pax) [1 1] tex)
      ?^  res  pile.u.res
      %-  mean
      =/  lyn  p.hair
      =/  col  q.hair
      ^-  (list tank)
      :~  leaf+"syntax error at [{<lyn>} {<col>}] in {<pax>}"
        ::
          =/  =wain  (to-wain:format (crip tex))
          ?:  (gth lyn (lent wain))
            '<<end of file>>'
          (snag (dec lyn) wain)
        ::
          leaf+(runt [(dec col) '-'] "^")
      ==
    ::
    ++  pile-rule
      |=  pax=path
      %-  full
      %+  ifix
        :_  gay
        ::  parse optional /? and ignore
        ::
        ;~(plug gay (punt ;~(plug fas wut gap dem gap)))
      |^
      ;~  plug
        %+  cook  (bake zing (list (list taut)))
        %+  rune  hep
        (most ;~(plug com gaw) taut-rule)
      ::
        %+  cook  (bake zing (list (list taut)))
        %+  rune  lus
        (most ;~(plug com gaw) taut-rule)
      ::
        %+  rune  tis
        ;~(plug sym ;~(pfix gap stap))
      ::
        %+  rune  sig
        ;~((glue gap) sym wyde:vast stap)
      ::
        %+  rune  cen
        ;~(plug sym ;~(pfix gap ;~(pfix cen sym)))
      ::
        %+  rune  buc
        ;~  (glue gap)
          sym
          ;~(pfix cen sym)
          ;~(pfix cen sym)
        ==
      ::
        %+  rune  tar
        ;~  (glue gap)
          sym
          ;~(pfix cen sym)
          ;~(pfix stap)
        ==
      ::
        %+  stag  %tssg
        (most gap tall:(vang & pax))
      ==
      ::
      ++  pant
        |*  fel=^rule
        ;~(pose fel (easy ~))
      ::
      ++  mast
        |*  [bus=^rule fel=^rule]
        ;~(sfix (more bus fel) bus)
      ::
      ++  rune
        |*  [bus=^rule fel=^rule]
        %-  pant
        %+  mast  gap
        ;~(pfix fas bus gap fel)
      --
    ::
    ++  taut-rule
      %+  cook  |=(taut +<)
      ;~  pose
        (stag ~ ;~(pfix tar sym))
        ;~(plug (stag ~ sym) ;~(pfix tis sym))
        (cook |=(a=term [`a a]) sym)
      ==
    ::
    ++  run-tauts
      |=  [sut=vase wer=?(%lib %sur) taz=(list taut)]
      ^-  [vase state]
      ?~  taz  [sut nub]
      =^  pin=vase  nub  (build-fit wer pax.i.taz)
      =?  p.pin  ?=(^ face.i.taz)  [%face u.face.i.taz p.pin]
      $(sut (slop pin sut), taz t.taz)
    ::
    ++  run-raw
      |=  [sut=vase raw=(list [face=term =path])]
      ^-  [vase state]
      ?~  raw  [sut nub]
      =^  pin=vase  nub  (build-file (snoc path.i.raw %hoon))
      =.  p.pin  [%face face.i.raw p.pin]
      $(sut (slop pin sut), raw t.raw)
    ::
    ++  run-raz
      |=  [sut=vase raz=(list [face=term =spec =path])]
      ^-  [vase state]
      ?~  raz  [sut nub]
      =^  res=(map @ta vase)  nub
        (build-directory path.i.raz)
      =;  pin=vase
        =.  p.pin  [%face face.i.raz p.pin]
        $(sut (slop pin sut), raz t.raz)
      ::
      =/  =type  (~(play ut p.sut) [%kttr spec.i.raz])
      ::  ensure results nest in the specified type,
      ::  and produce a homogenous map containing that type.
      ::
      :-  %-  ~(play ut p.sut)
          [%kttr %make [%wing ~[%map]] ~[[%base %atom %ta] spec.i.raz]]
      |-
      ?~  res  ~
      ?.  (~(nest ut type) | p.q.n.res)
        ~|  [%nest-fail path.i.raz p.n.res]
        !!
      :-  [p.n.res q.q.n.res]
      [$(res l.res) $(res r.res)]
    ::
    ++  run-maz
      |=  [sut=vase maz=(list [face=term =mark])]
      ^-  [vase state]
      ?~  maz  [sut nub]
      =^  pin=vase  nub  (build-nave mark.i.maz)
      =.  p.pin  [%face face.i.maz p.pin]
      $(sut (slop pin sut), maz t.maz)
    ::
    ++  run-caz
      |=  [sut=vase caz=(list [face=term =mars])]
      ^-  [vase state]
      ?~  caz  [sut nub]
      =^  pin=vase  nub  (build-cast mars.i.caz)
      =.  p.pin  [%face face.i.caz p.pin]
      $(sut (slop pin sut), caz t.caz)
    ::
    ++  run-bar
      |=  [sut=vase bar=(list [face=term =mark =path])]
      ^-  [vase state]
      ?~  bar  [sut nub]
      =^  =cage  nub  (cast-path [path mark]:i.bar)
      =.  p.q.cage  [%face face.i.bar p.q.cage]
      $(sut (slop q.cage sut), bar t.bar)
    ::
    ::  +build-fit: build file at path, maybe converting '-'s to '/'s in path
    ::
    ++  build-fit
      |=  [pre=@tas pax=@tas]
      ^-  [vase state]
      (build-file (fit-path pre pax))
    ::
    ::  +fit-path: find path, maybe converting '-'s to '/'s
    ::
    ::    Try '-' before '/', applied left-to-right through the path,
    ::    e.g. 'a-foo/bar' takes precedence over 'a/foo-bar'.
    ::
    ++  fit-path
      |=  [pre=@tas pax=@tas]
      ^-  path
      =/  paz  (segments pax)
      |-  ^-  path
      ?~  paz
        ~_(leaf/"clay: no files match /{(trip pre)}/{(trip pax)}/hoon" !!)
      =/  pux=path  pre^(snoc i.paz %hoon)
      ?:  (~(has by files) pux)
        pux
      $(paz t.paz)
    ::
    ++  all-fits
      |=  [=term suf=term]
      ^-  (list path)
      %+  turn  (segments suf)
      |=  seg=path
      [term (snoc seg %hoon)]
    ::
    ::  Gets a map of the data at the given path and all children of it.
    ::
    ::    i.e. +dip:of for a map, except doesn't shorten paths
    ::
    ++  dip-hat
      |=  pax=path
      ^-  (map path (each page lobe))
      %-  malt
      %+  skim  ~(tap by files)
      |=  [p=path *]
      ?|  ?=(~ pax)
          ?&  !?=(~ p)
              =(-.pax -.p)
              $(p +.p, pax +.pax)
      ==  ==
    ::
    ++  trace
      |=  [pri=@ print=(trap tape)]
      (^trace verb pri print)
    ::
    ++  mist-to-pour
      |=  =mist
      ^-  pour
      ?+    -.mist  mist
          %vale
        :+  %vale  path.mist
        ~|  %file-not-found-mist^path.mist
        =/  lob  (~(got by files) path.mist)
        ?-  -.lob
          %&  (page-to-lobe p.lob)
          %|  p.lob
        ==
      ::
          %arch
        =/  dip  (dip-hat path.mist)
        :+  %arch  path.mist
        %-  ~(run by dip)
        |=  file=(each page lobe)
        ?-  -.file
          %&  (page-to-lobe p.file)
          %|  p.file
        ==
      ==
    ::
    ++  soak-cage  |=([s=soak n=state] ?>(?=(%cage -.s) [cage.s n]))
    ++  soak-vase  |=([s=soak n=state] ?>(?=(%vase -.s) [vase.s n]))
    ++  soak-dais  |=([s=soak n=state] ?>(?=(%dais -.s) [dais.s n]))
    ++  soak-tube  |=([s=soak n=state] ?>(?=(%tube -.s) [tube.s n]))
    ++  soak-arch  |=([s=soak n=state] ?>(?=(%arch -.s) [dir.s n]))
    ::
    ++  gain-sprig
      |=  [=mist next=(trap [soak state])]
      ^-  [soak state]
      ?~  got=(~(get by sprig.nub) mist)
        $:next
      =?  stack.nub  ?=(^ stack.nub)
        stack.nub(i (~(put in i.stack.nub) leak.u.got))
      [soak.u.got nub]
    ::
    ++  gain-leak
      |=  [=mist next=$-(state [soak state])]
      ^-  [soak state]
      =^  top=(set leak)  stack.nub  stack.nub
      =/  =leak  [(mist-to-pour mist) top]
      =.  cycle.nub  (~(del in cycle.nub) mist)
      =?  stack.nub  ?=(^ stack.nub)
        stack.nub(i (~(put in i.stack.nub) leak))
      =/  spilt  (~(has in spill.nub) leak)
      =^  =soak  nub
        ?^  got=(~(get by cache.nub) leak)
          %-  %+  trace  3  |.
              =/  refs    ?:(spilt 0 1)
              %+  welp  "cache {<pour.leak>}: adding {<refs>}, "
              "giving {<(add refs refs.u.got)>}"
          =?  cache.nub  !spilt
            (~(put by cache.nub) leak [+(refs.u.got) soak.u.got])
          [soak.u.got nub]
        %-  (trace 2 |.("cache {<pour.leak>}: creating"))
        =^  =soak  nub  (next nub)
        =.  cache.nub  (~(put by cache.nub) leak [1 soak])
        ::  If we're creating a cache entry, add refs to our dependencies
        ::
        =/  deps  ~(tap in deps.leak)
        |-
        ?~  deps
          [soak nub]
        =/  got  (~(got by cache.nub) i.deps)
        %-  %+  trace  3  |.
            %+  welp  "cache {<pour.leak>} for {<pour.i.deps>}"
            ": bumping to ref {<refs.got>}"
        =.  cache.nub  (~(put by cache.nub) i.deps got(refs +(refs.got)))
        $(deps t.deps)
      ?:  spilt
        [soak nub]
      %-  (trace 3 |.("spilt {<mist>}"))
      =:  spill.nub  (~(put in spill.nub) leak)
          sprig.nub  (~(put by sprig.nub) mist leak soak)
        ==
      [soak nub]
    --
  ::
  ++  lose-leak
    |=  [verb=@ fad=flow =leak]
    ^-  flow
    ?~  got=(~(get by fad) leak)
      %-  (trace verb 0 |.("lose missing leak {<leak>}"))
      fad
    ?:  (lth 1 refs.u.got)
      %-  (trace verb 3 |.("cache {<pour.leak>}: decrementing from {<refs.u.got>}"))
      =.  fad  (~(put by fad) leak u.got(refs (dec refs.u.got)))
      fad
    =+  ?.  =(0 refs.u.got)  ~
        ((trace verb 0 |.("lose zero leak {<leak>}")) ~)
    %-  (trace verb 2 |.("cache {<pour.leak>}: freeing"))
    =.  fad  (~(del by fad) leak)
    =/  leaks  ~(tap in deps.leak)
    |-  ^-  flow
    ?~  leaks
      fad
    =.  fad  ^$(leak i.leaks)
    $(leaks t.leaks)
  ::
  ++  lose-leaks
    |=  [verb=@ fad=flow leaks=(set leak)]
    ^-  flow
    =/  leaks  ~(tap in leaks)
    |-
    ?~  leaks
      fad
    $(fad (lose-leak verb fad i.leaks), leaks t.leaks)
  ::
  ++  trace
    |=  [verb=@ pri=@ print=(trap tape)]
    ?:  (lth verb pri)
      same
    (slog leaf+"ford: {(print)}" ~)
  --
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::  section 4cA, filesystem logic
::
::  This core contains the main logic of clay.  Besides `++ze`, this directly
::  contains the logic for commiting new revisions (local urbits), managing
::  and notifying subscribers (reactivity), and pulling and validating content
::  (remote urbits).
::
::  The state includes:
::
::  --  local urbit `our`
::  --  current time `now`
::  --  current duct `hen`
::  --  scry handler `ski`
::  --  all vane state `++raft` (rarely used, except for the object store)
::  --  target urbit `her`
::  --  target desk `syd`
::
::  For local desks, `our` == `her` is one of the urbits on our pier.  For
::  foreign desks, `her` is the urbit the desk is on and `our` is the local
::  urbit that's managing the relationship with the foreign urbit.  Don't mix
::  up those two, or there will be wailing and gnashing of teeth.
::
::  While setting up `++de`, we check if `our` == `her`. If so, we get
::  the desk information from `dos.rom`.  Otherwise, we get the rung from
::  `hoy` and get the desk information from `rus` in there.  In either case,
::  we normalize the desk information to a `++rede`, which is all the
::  desk-specific data that we utilize in `++de`.  Because it's effectively
::  a part of the `++de` state, let's look at what we've got:
::
::  --  `lim` is the most recent date we're confident we have all the
::      information for.  For local desks, this is always `now`.  For foreign
::      desks, this is the last time we got a full update from the foreign
::      urbit.
::  --  `ref` is a possible request manager.  For local desks, this is null.
::      For foreign desks, this keeps track of all pending foreign requests
::      plus a cache of the responses to previous requests.
::  --  `qyx` is the set of subscriptions, with listening ducts. These
::      subscriptions exist only until they've been filled.
::  --  `dom` is the actual state of the filetree.  Since this is used almost
::      exclusively in `++ze`, we describe it there.
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
++  de                                                  ::  per desk
  ~%  %de  ..de  ~
  |=  [now=@da rof=roof hen=duct raft]
  ~/  %de-in
  |=  [her=ship syd=desk]
  ::  NB: ruf=raft crashes in the compiler
  ::
  =*  ruf  |3.+6.^$
  =|  [mow=(list move) hun=(unit duct) rede]
  =*  red=rede  ->+
  =<  apex
  ~%  %de-core  ..$  ~
  |%
  ++  abet                                              ::  resolve
    ^-  [(list move) raft]
    :-  (flop mow)
    ?.  =(our her)
      ::  save foreign +rede
      ::
      =/  run  (~(gut by hoy.ruf) her *rung)
      =/  rug  (~(put by rus.run) syd red)
      ruf(hoy (~(put by hoy.ruf) her run(rus rug)))
    ::  save domestic +room
    ::
    %=  ruf
      hun.rom  (need hun)
      dos.rom  (~(put by dos.rom.ruf) syd [qyx dom per pew fiz]:red)
    ==
  ::
  ++  apex
    ^+  ..park
    ?.  =(our her)
      ::  no duct, foreign +rede or default
      ::
      =.  mow
        ?:  (~(has by hoy.ruf) her)
          ~
        [hun.rom.ruf %pass /sinks %j %public-keys (silt her ~)]~
      =.  hun  ~
      =.  |2.+6.park
        =/  rus  rus:(~(gut by hoy.ruf) her *rung)
        %+  ~(gut by rus)  syd
        [lim=~2000.1.1 ref=`*rind qyx=~ dom=*dome per=~ pew=~ fiz=*melt]
      ..park
    ::  administrative duct, domestic +rede
    ::
    =.  mow  ~
    =.  hun  `hun.rom.ruf
    =.  |2.+6.park
      =/  jod  (~(gut by dos.rom.ruf) syd *dojo)
      [lim=now ref=*(unit rind) [qyx dom per pew fiz]:jod]
    ..park
  ::
  ::  Handle `%sing` requests
  ::
  ++  aver
    |=  [for=(unit ship) mun=mood]
    ^-  [(unit (unit cage)) _..park]
    =+  ezy=?~(ref ~ (~(get by haw.u.ref) mun))
    ?^  ezy
      [`u.ezy ..park]
    ?:  ?=([%s [%ud *] %late *] mun)
      :_  ..park
      ^-  (unit (unit cage))
      :+  ~  ~
      ^-  cage
      :-  %cass
      ?~  let.dom
        !>([0 *@da])
      !>([let.dom t:(~(got by hut.ran) (~(got by hit.dom) let.dom))])
    =+  tak=(case-to-tako case.mun)
    ?:  ?=([%s case %case ~] mun)
      ::  case existence check
      [``[%flag !>(!=(~ tak))] ..park]
    ?~(tak [~ ..park] (read-at-tako:ze for u.tak mun))
  ::
  ::  Queue a move.
  ::
  ++  emit
    |=  mof=move
    %_(+> mow [mof mow])
  ::
  ::  Queue a list of moves
  ::
  ++  emil
    |=  mof=(list move)
    %_(+> mow (weld (flop mof) mow))
  ::
  ::  Queue a list of moves, to be emitted before the rest
  ::
  ++  lime
    |=  mof=(list move)
    %_(+> mow (weld mow (flop mof)))
  ::
  ::  Set timer.
  ::
  ++  bait
    |=  [hen=duct tym=@da]
    (emit hen %pass /tyme/(scot %p her)/[syd] %b %wait tym)
  ::
  ::  Cancel timer.
  ::
  ++  best
    |=  [hen=duct tym=@da]
    (emit hen %pass /tyme/(scot %p her)/[syd] %b %rest tym)
  ::
  ::  Give %writ, or slip a drip if foreign desk
  ::
  ++  writ
    |=  res=(unit [=mood =cage])
    ^-  card
    =/  =riot
      ?~  res
        ~
      `[[care.mood case.mood syd] path.mood cage]:[u.res syd=syd]
    ?~  ref
      [%give %writ riot]
    [%pass /drip %b %drip !>([%writ riot])]
  ::
  ++  case-to-date
    |=  =case
    ^-  @da
    ::  if the case is already a date, use it.
    ::
    ?:  ?=([%da *] case)
      p.case
    ::  translate other cases to dates
    ::
    =/  aey  (case-to-aeon-before lim case)
    ?~  aey  `@da`0
    ?:  =(0 u.aey)  `@da`0
    t:(aeon-to-yaki:ze u.aey)
  ::
  ++  case-to-aeon  (cury case-to-aeon-before lim)
  ::
  ::  Reduce a case to an aeon (version number)
  ::
  ::  We produce null if we can't yet reduce the case for whatever
  ::  resaon (usually either the time or aeon hasn't happened yet or
  ::  the label hasn't been created).
  ::
  ++  case-to-aeon-before
    |=  [lim=@da lok=case]
    ^-  (unit aeon)
    ?-    -.lok
        %tas  (~(get by lab.dom) p.lok)
        %ud   ?:((gth p.lok let.dom) ~ [~ p.lok])
        %uv   `(tako-to-aeon:ze p.lok)
        %da
      ?:  (gth p.lok lim)  ~
      |-  ^-  (unit aeon)
      ?:  =(0 let.dom)  [~ 0]                         ::  avoid underflow
      ?:  %+  gte  p.lok
          =<  t
          ~|  [%letdom let=let.dom hit=hit.dom hut=~(key by hut.ran)]
          ~|  [%getdom (~(get by hit.dom) let.dom)]
          %-  aeon-to-yaki:ze
          let.dom
        [~ let.dom]
      $(let.dom (dec let.dom))
    ==
  ::
  ++  case-to-tako
    |=  lok=case
    ^-  (unit tako)
    ?:  ?=(%uv -.lok)
      ?:((~(has by hut.ran) p.lok) `p.lok ~)
    (bind (case-to-aeon-before lim lok) aeon-to-tako:ze)
  ::
  ::  Create a ford appropriate for the aeon
  ::
  ::  Don't forget to call +tako-flow!
  ::
  ++  tako-ford
    |=  tak=tako
    %-  ford:fusion
    :-  (~(run by q:(tako-to-yaki:ze tak)) |=(=lobe |+lobe))
    [lat.ran veb.bug fad ?:(=(tak (aeon-to-tako:ze let.dom)) fod.dom [~ ~])]
  ::  Produce ford cache appropriate for the aeon
  ::
  ++  tako-flow
    |*  [tak=tako res=* fud=flow fod=flue]
    :-  res
    ^+  ..park
    ?:  &(?=(~ ref) =((aeon-to-tako:ze let.dom) tak))
      ..park(fad fud, fod.dom fod)
    :: if in the past, don't update ford cache, since any results have
    :: no roots
    ::
    ..park
  ::
  ++  request-wire
    |=  [kind=@ta =ship =desk index=@ud]
    /[kind]/(scot %p ship)/[desk]/(scot %ud index)
  ::
  ::  Transfer a request to another ship's clay.
  ::
  ++  send-over-ames
    |=  [=duct =ship index=@ud =riff]
    ^+  +>
    ::
    =/  =desk  p.riff
    =/  =wire  (request-wire %warp-index ship desk index)
    =/  =path  [%question desk (scot %ud index) ~]
    (emit duct %pass wire %a %plea ship %c path `riff-any`[%1 riff])
  ::
  ++  send-over-scry
    |=  [kind=@ta =duct =ship index=@ud =desk =mood]
    ^-  [[timeout=@da =path] _..send-over-scry]
    =/  =time  (add now scry-timeout-time)
    =/  =wire  (request-wire kind ship desk index)
    =/  =path
      =,  mood
      [%c care (scot case) desk path]
    :-  [time path]
    %-  emil
    :~  [hen %pass wire %a %keen ship path]
        [hen %pass wire %b %wait time]
    ==
  ::
  ++  cancel-scry-timeout
    |=  inx=@ud
    ~|  [%strange-timeout-cancel-no-scry-request her syd inx]
    ?>  ?=(^ ref)
    =/  sat=update-state  (~(got by bom.u.ref) inx)
    ?>  ?=([~ ^] busy.sat)
    =/  =wire  (request-wire kind.u.busy.sat her syd inx)
    (emit hen %pass wire %b %rest time.u.busy.sat)
  ::
  ++  foreign-capable
    |=  =rave
    |^
    ?-    -.rave
        %many  &
        %sing  (good-care care.mood.rave)
        %next  (good-care care.mood.rave)
        %mult
      %-  ~(all in paths.mool.rave)
      |=  [=care =path]
      (good-care care)
    ==
    ::
    ++  good-care
      |=  =care
      (~(has in ^~((silt `(list ^care)`~[%q %u %w %x %y %z]))) care)
    --
  ::
  ::  Build and send agents to gall
  ::
  ::  Must be called at the end of a commit, but only while Clay is in a
  ::  fully-consistent state (eg not in the middle of a kelvin upgrade).
  ::
  ++  goad
    ^+  ..park
    =^  moves-1  ruf  abet
    =^  moves-2  ruf  abet:goad:(lu now rof hen ruf)
    =.  ..park  apex
    (emil (weld moves-1 moves-2))
  ::
  ::  Notify subscribers of changes to tire
  ::
  ::  Must be called any time tire could have changed, unless you called
  ::  goad (which calls tare internally).
  ::
  ++  tare
    ^+  ..park
    =^  moves-1  ruf  abet
    =^  moves-2  ruf  abet:tare:(lu now rof hen ruf)
    =.  ..park  apex
    (emil (weld moves-1 moves-2))
  ::
  ::  Create a request that cannot be filled immediately.
  ::
  ::  If it's a local request, we just put in in `qyx`, setting a timer if it's
  ::  waiting for a particular time.  If it's a foreign request, we add it to
  ::  our request manager (ref, which is a ++rind) and make the request to the
  ::  foreign ship.
  ::
  ++  duce                                              ::  produce request
    |=  wov=wove
    ^+  +>
    =.  wov  (dedupe wov)
    =.  qyx  (~(put ju qyx) wov hen)
    ?~  ref
      ::  [wake] at @da must check if subscription was fulfilled
      ::
      (run-if-future rove.wov |=(@da (bait hen +<)))
    |-  ^+  +>+.$
    =/  =rave  (rove-to-rave rove.wov)
    =?   rave  ?=([%sing %v *] rave)
      [%many %| [%ud let.dom] case.mood.rave path.mood.rave]
    ::
    ?.  (foreign-capable rave)
      ~|([%clay-bad-foreign-request-care rave] !!)
    ::
    =+  inx=nix.u.ref
    =.  +>+.$
      =<  ?>(?=(^ ref) .)
      (send-over-ames hen her inx syd `rave)
    %=  +>+.$
      nix.u.ref  +(nix.u.ref)
      bom.u.ref  (~(put by bom.u.ref) inx [hen rave ~ ~ ~ ~])
      fod.u.ref  (~(put by fod.u.ref) hen inx)
    ==
  ::
  ::  If a similar request exists, switch to the existing request.
  ::
  ::  "Similar" requests are those %next and %many requests which are the same
  ::  up to starting case, but we're already after the starting case.  This
  ::  stacks later requests for something onto the same request so that they
  ::  all get filled at once.
  ::
  ++  dedupe                                            ::  find existing alias
    |=  wov=wove
    ^-  wove
    =;  won=(unit wove)  (fall won wov)
    =*  rov  rove.wov
    ?-    -.rov
        %sing  ~
        %next
      =+  aey=(case-to-aeon case.mood.rov)
      ?~  aey  ~
      %-  ~(rep in ~(key by qyx))
      |=  [haw=wove res=(unit wove)]
      ?^  res  res
      ?.  =(for.wov for.haw)  ~
      =*  hav  rove.haw
      =-  ?:(- `haw ~)
      ?&  ?=(%next -.hav)
          =(mood.hav mood.rov(case case.mood.hav))
        ::
          ::  only a match if this request is before
          ::  or at our starting case.
          =+  hay=(case-to-aeon case.mood.hav)
          ?~(hay | (lte u.hay u.aey))
      ==
    ::
        %mult
      =+  aey=(case-to-aeon case.mool.rov)
      ?~  aey  ~
      %-  ~(rep in ~(key by qyx))
      |=  [haw=wove res=(unit wove)]
      ?^  res  res
      ?.  =(for.wov for.haw)  ~
      =*  hav  rove.haw
      =-  ?:(- `haw ~)
      ?&  ?=(%mult -.hav)
          =(mool.hav mool.rov(case case.mool.hav))
        ::
          ::  only a match if this request is before
          ::  or at our starting case, and it has been
          ::  tested at least that far.
          =+  hay=(case-to-aeon case.mool.hav)
          ?&  ?=(^ hay)
              (lte u.hay u.aey)
              ?=(^ aeon.hav)
              (gte u.aeon.hav u.aey)
          ==
      ==
    ::
        %many
      =+  aey=(case-to-aeon from.moat.rov)
      ?~  aey  ~
      %-  ~(rep in ~(key by qyx))
      |=  [haw=wove res=(unit wove)]
      ?^  res  res
      ?.  =(for.wov for.haw)  ~
      =*  hav  rove.haw
      =-  ?:(- `haw ~)
      ?&  ?=(%many -.hav)
          =(hav rov(from.moat from.moat.hav))
        ::
          ::  only a match if this request is before
          ::  or at our starting case.
          =+  hay=(case-to-aeon from.moat.hav)
          ?~(hay | (lte u.hay u.aey))
      ==
    ==
  ::
  ++  set-norm
    |=  =norm
    =.  nor.dom  norm
    ..park
  ::
  ++  set-worn
    |=  [=tako =norm]
    ?:  &(=(our her) =(tako (aeon-to-tako:ze let.dom)))
      (mean leaf+"clay: can't set norm for current commit in {<syd>}" ~)
    =.  tom.dom  (~(put by tom.dom) tako norm)
    ..park
  ::
  ::  Attach label to aeon
  ::
  ++  label
    |=  [bel=@tas aey=(unit aeon)]
    ^+  ..park
    =/  yon  ?~(aey let.dom u.aey)
    =/  yen  (~(get by lab.dom) bel)  :: existing aeon?
    ::  no existing aeon is bound to this label
    ::
    ?~  yen
      =.  lab.dom  (~(put by lab.dom) bel yon)          ::  [wake] <>
      wake
    ::  an aeon is bound to this label,
    ::  but it is the same as the existing one, so we no-op
    ::
    ?:  =(u.yen yon)
      ~&  "clay: tried to rebind existing label {<bel>} to equivalent aeon {<yon>}"
      ..park
    ::  an existing aeon bound to the label
    ::  that is distinct from the requested one.
    ::  rewriting would violate referential transparency
    ::
    ~|  %tried-to-rewrite-existing-label
    ~|  "requested aeon: {<yon>}, existing aeon: {<u.yen>}"
    !!
  ::
  ::  Porcelain commit
  ::
  ++  info
    ~/  %info
    |=  [deletes=(set path) changes=(map path cage)]
    ^+  ..park
    ?:  =(0 let.dom)
      ?>  ?=(~ deletes)
      =/  data=(map path (each page lobe))
        (~(run by changes) |=(=cage &+[p q.q]:cage))
      (park | & &+[~ data] *rang)
    ::
    =/  parent-tako=tako  (aeon-to-tako:ze let.dom)
    =/  data=(map path (each page lobe))
      =/  parent-yaki  (tako-to-yaki:ze parent-tako)
      =/  after-deletes
        %-  ~(dif by q.parent-yaki)
        (malt (turn ~(tap in deletes) |=(=path [path *lobe])))
      =/  after=(map path (each page lobe))
        (~(run by after-deletes) |=(=lobe |+lobe))
      %-  ~(uni by after)
      ^-  (map path (each page lobe))
      (~(run by changes) |=(=cage &+[p q.q]:cage))
    ::
    =/  =yuki  [~[parent-tako] data]
    (park | & &+yuki *rang)
  ::
  ::  Unix commit
  ::
  ++  into
    ~/  %into
    |=  [pax=path all=? mod=(list [pax=path mim=(unit mime)])]
    ^+  ..park
    ::  filter out unchanged, cached %mime values
    ::
    =.  mod
      %+  skip  mod
      |=  [pax=path mim=(unit mime)]
      ?~  mim
        |
      ?~  mum=(~(get by mim.dom) pax)
        |
      ::  TODO: check mimetype
      ::
      =(q.u.mim q.u.mum)
    =/  =yaki
      ?:  =(0 let.dom)
        *yaki
      (~(got by hut.ran) (~(got by hit.dom) let.dom))
    (info (mode-to-commit q.yaki pax all mod))
  ::
  ::  Plumbing commit
  ::
  ::    Guaranteed to finish in one event.
  ::
  ::    updated: whether we've already completed sys upgrade
  ::    goat: whether we should call +goad at the end.  Only false
  ::      during kelvin upgrade so that all commits can happen before
  ::      the +goad.
  ::    yoki: new commit
  ::    rang: any additional objects referenced
  ::
  ::    [goad] < if goat is false, then the caller is responsible to
  ::    call +goad.
  ::
  ::    TODO: needs to check tako in rang
  ::
  ++  park
    =/  check-sane  |
    |^
    |=  [updated=? goat=? =yoki =rang]
    ^+  ..park
    =:  hut.ran  (~(uni by hut.rang) hut.ran)
        lat.ran  (~(uni by lat.rang) lat.ran)
      ==
    =/  new-data=(map path (each page lobe))
      ?-  -.yoki
        %&  q.p.yoki
        %|  (~(run by q.p.yoki) |=(=lobe |+lobe))
      ==
    ?.  %-  ~(all in new-data)  ::  use +all:in so we get the key
        |=  [=path tum=(each page lobe)]
        ?:  |(?=(%& -.tum) (~(has by lat.ran) p.tum))
          &
        =-  (mean leaf/- ~)
        "clay: commit failed, file tombstoned: {<path>} {<`@uv`p.tum>}"
      !!
    ::  find desk kelvin
    ::
    =/  kel=(set weft)  (waft-to-wefts (get-kelvin yoki))
    ?.  ?|  (~(has in kel) zuse+zuse)                   ::  kelvin match
            ?&  !=(%base syd)                           ::  best-effort compat
                %-  ~(any in kel)
                |=  =weft
                &(=(%zuse lal.weft) (gth num.weft zuse))
            ==
            ?&  =(%base syd)                            ::  ready to upgrade
                %+  levy  ~(tap by tore:(lu now rof hen ruf))
                |=  [=desk =zest wic=(set weft)]
                ?|  =(%base desk)
                    !?=(%live zest)
                    !=(~ (~(int in wic) kel))
                ==
            ==
        ==
      ?:  (~(all in kel) |=(=weft (gth num.weft zuse)))
        %-  (slog leaf+"clay: old-kelvin, {<[need=zuse/zuse have=kel]>}" ~)
        ..park
      =.  wic.dom                                       ::  [tare] <
        %+  roll  ~(tap in kel)
        |:  [weft=*weft wic=wic.dom]
        (~(put by wic) weft yoki)
      =?  ..park  !?=(%base syd)  wick                  ::  [wick]
      %-  (slog leaf+"clay: wait-for-kelvin, {<[need=zuse/zuse have=kel]>}" ~)
      tare                                              ::  [tare] >
    =.  wic.dom
      %-  ~(gas by *(map weft ^yoki))
      %+  skip  ~(tap by wic.dom)
      |=  [w=weft ^yoki]
      (gte num.w zuse)
    ::
    =/  old-yaki
      ?:  =(0 let.dom)
        *yaki
      (aeon-to-yaki:ze let.dom)
    =/  old-kel=(set weft)
      ?:  =(0 let.dom)
        [zuse+zuse ~ ~]
      (waft-to-wefts (get-kelvin %| old-yaki))
    =/  [deletes=(set path) changes=(map path (each page lobe))]
      (get-changes q.old-yaki new-data)
    ~|  [from=let.dom deletes=deletes changes=~(key by changes)]
    ::
    ::  promote ford cache
    ::  promote and fill in mime cache
    ::
    =/  invalid  (~(uni in deletes) ~(key by changes))
    ::  if /sys updated in %base, defer to arvo and return early
    ::
    ?:  &(=(%base syd) !updated (~(any in invalid) is-kernel-path))
      (sys-update yoki new-data)
    ::  after this point, there must be no early return except if it's a
    ::  complete no-op.  any error conditions must crash.  since we're
    ::  changing state, we may need to call +wake, +goad, etc, which
    ::  happens at the end of the function.
    ::
    ::  [wick] if this commit added compatibility to a future kelvin,
    ::  then we might have unblocked a kelvin upgrade.
    ::
    ::  or, if *this* is a kelvin upgrade, it's possible that another
    ::  kelvin upgrade will immediately be ready.  for example, this
    ::  could be the case if all desks but one are ready for the next
    ::  two kelvins, and then that desk is suspended or receives a
    ::  commit with compatiblity with both kelvins.
    ::
    ::  in any of these cases, we finish the current commit but call
    ::  +wick so that we try to execute the kelvin upgrade afterward.
    ::  we want this commit to persist even if the subsequent kelvin
    ::  upgrade fails.
    ::
    =.  ..park  wick
    =.  wic.dom                                         ::  [tare] <
      %+  roll  ~(tap in kel)
      |:  [weft=*weft wic=wic.dom]
      ?:  (gte num.weft zuse)
        wic
      (~(put by wic) weft yoki)
    ::
    =+  ?.  (did-kernel-update invalid)  ~
        ((slog 'clay: kernel updated' ~) ~)
    =?  updated  updated  (did-kernel-update invalid)
    =>  ?.  updated  .
        ~>(%slog.0^leaf/"clay: rebuilding {<syd>} after kernel update" .)
    ::  clear caches if zuse reloaded
    ::
    =/  old-fod  fod.dom
    =.  fod.dom
      ?:  updated  [~ ~]
      (promote-ford fod.dom invalid)
    =.  fad
      (lose-leaks:fusion veb.bug fad (~(dif in spill.old-fod) spill.fod.dom))
    =?  changes  updated  (changes-for-upgrade q.old-yaki deletes changes)
    ::
    =/  files
      =/  original=(map path (each page lobe))
        (~(run by q.old-yaki) |=(=lobe |+lobe))
      %-  ~(dif by (~(uni by original) changes))
      %-  ~(gas by *(map path (each page lobe)))
      (turn ~(tap in deletes) |=(=path [path |+*lobe]))
    =/  =args:ford:fusion  [files lat.ran veb.bug fad fod.dom]
    ::
    =^  change-cages  args  (checkout-changes args changes)
    =/  sane-continuation  (sane-changes changes change-cages)
    =/  new-pages=(map lobe page)
      %-  malt
      %+  turn  ~(tap by change-cages)
      |=  [=path =lobe =cage]
      [lobe [p q.q]:cage]
    =/  data=(map path lobe)
      %-  ~(urn by new-data)
      |=  [=path value=(each page lobe)]
      ?-  -.value
        %|  p.value
        %&  lobe:(~(got by change-cages) path)
      ==
    ::  if we didn't change the data and it's not a merge commit, abort
    ::
    ?:  &(=([r.old-yaki ~] p.p.yoki) =(data q.old-yaki))
      ::  [tare] > if no changes, then commits-in-waiting could not have
      ::  changed.
      ::
      ..park
    =/  =yaki
      ?-  -.yoki
        %&  (make-yaki p.p.yoki data now)
        %|  ?>  =(data q.p.yoki)
            p.yoki
      ==
    ::  [wake] < [ergo] < [goad] <
    ::
    =:  let.dom  +(let.dom)
        hit.dom  (~(put by hit.dom) +(let.dom) r.yaki)
        hut.ran  (~(put by hut.ran) r.yaki yaki)
        lat.ran  (~(uni by new-pages) lat.ran)
      ==
    =.  file-store.args  lat.ran
    ::
    =/  mem  (want-mime 0)
    =/  res=[mum=(map path (unit mime)) mim=_mim.dom args=_args]
      ?.  mem  [~ ~ args]
      =^  mum  args  (checkout-mime args deletes ~(key by changes))
      [mum (apply-changes-to-mim mim.dom mum) args]
    =.  mim.dom  mim.res
    =.  args     args.res
    ::
    =.  fod.dom  [spill sprig]:args
    =.  fad      cache.args
    =.  ..park   (emil (print q.old-yaki data))
    ::  if upgrading kelvin and there's a commit-in-waiting, use that
    ::
    =?  ..park  &(=(%base syd) !=(old-kel kel))
      =/  desks=(list [=desk =dojo])  ~(tap by dos.rom)
      =^  moves-1  ruf  abet
      =|  moves-2=(list move)
      |-  ^+  ..park
      ?~  desks
        =.  ..park  apex
        (emil (weld moves-1 moves-2))
      ?.  ?=(%live liv.dom.dojo.i.desks)
        $(desks t.desks)
      ?:  ?=(%base desk.i.desks)
        $(desks t.desks)
      ?~  wat=(~(get by wic.dom.dojo.i.desks) zuse+zuse)
        (mean (cat 3 'clay: missing commit-in-waiting on ' desk.i.desks) ~)
      =/  den  ((de now rof hen ruf) our desk.i.desks)
      ::  [goad] < call without goading so that we apply all the commits
      ::  before trying to compile all desks to send to gall.
      ::
      =^  moves-3  ruf  abet:(park:den | | u.wat *^rang)
      =.  moves-2  (weld moves-2 moves-3)
      $(desks t.desks)
    ::  tell gall to try to run agents if %held
    ::
    ::  [goad] > if goat or desk not running.  %held uses park-held to
    ::  defer the goad into a new event, to attempt to revive the desk.
    ::  Note that %base will always be %live.
    ::
    =.  ..park
      ?-  liv.dom
        %held  (emit hen %pass /park-held/[syd] %b %wait now)
        %dead  ..park
        %live  ?:(goat goad ..park)
      ==
    ::  notify unix and subscribers
    ::
    =?  ..park  mem  (ergo 0 mum.res)                   ::  [ergo] >
    wake:tare                                           ::  [wake] > [tare] >
    ::
    ::  +is-kernel-path: should changing .pax cause a kernel or vane reload?
    ::
    ++  is-kernel-path  |=(pax=path ?=([%sys *] pax))
    ::
    ++  did-kernel-update
      |=  invalid=(set path)
      ?.  =(%base syd)
        |
      %-  ~(any in invalid)
      |=(p=path &((is-kernel-path p) !?=([%sys %vane *] p)))
    ::
    ::  +get-kelvin: read the desk's kernel version from /sys/kelvin
    ::
    ++  get-kelvin
      |=  =yoki
      ^-  waft
      |^  ?-    -.yoki
              %|
            %-  lobe-to-waft
            ~>  %mean.(cat 3 'clay: missing /sys/kelvin on ' syd)
            ~|  ~(key by q.p.yoki)
            (~(got by q.p.yoki) /sys/kelvin)
          ::
              %&
            =/  fil=(each page lobe)
              ~>  %mean.(cat 3 'clay: missing /sys/kelvin on ' syd)
              ~|  ~(key by q.p.yoki)
              (~(got by q.p.yoki) /sys/kelvin)
            ?-    -.fil
                %&  (page-to-waft p.fil)
                %|  (lobe-to-waft p.fil)
            ==
          ==
      ::
      ++  lobe-to-waft
        |=  =lobe
        ^-  waft
        =/  peg=(unit page)  (~(get by lat.ran) lobe)
        ?~  peg  ~|([%sys-kelvin-tombstoned syd] !!)
        (page-to-waft u.peg)
      ::
      ++  page-to-waft
        |=  =page
        ^-  waft
        ?+    p.page  ~|(clay-bad-kelvin-mark/p.page !!)
            %kelvin  ;;(waft q.page)
            %mime    (cord-to-waft q.q:;;(mime q.page))
        ==
      --
    ::
    ::  Find which files changed or were deleted
    ::
    ++  get-changes
      |=  [old=(map path lobe) new=(map path (each page lobe))]
      ^-  [deletes=(set path) changes=(map path (each page lobe))]
      =/  old=(map path (each page lobe))
        (~(run by old) |=(=lobe |+lobe))
      :*  %-  silt  ^-  (list path)
          %+  murn  ~(tap by (~(uni by old) new))
          |=  [=path *]
          ^-  (unit ^path)
          =/  a  (~(get by new) path)
          =/  b  (~(get by old) path)
          ?:  |(=(a b) !=(~ a))
            ~
          `path
        ::
          %-  malt  ^-  (list [path (each page lobe)])
          %+  murn  ~(tap by (~(uni by old) new))
          |=  [=path *]
          ^-  (unit [^path (each page lobe)])
          =/  a  (~(get by new) path)
          =/  b  (~(get by old) path)
          ?:  |(=(a b) ?=(~ a))
            ~
          `[path u.a]
      ==
    ::  Find all files for full desk rebuild
    ::
    ++  changes-for-upgrade
      |=  $:  old=(map path lobe)
              deletes=(set path)
              changes=(map path (each page lobe))
          ==
      ^+  changes
      =.  old
        %+  roll  ~(tap in deletes)
        |=  [pax=path old=_old]
        (~(del by old) pax)
      =/  pre=_changes  (~(run by old) |=(lob=lobe |+lob))
      (~(uni by pre) changes)
    ::
    ++  promote-ford
      |=  [fod=flue invalid=(set path)]
      ^-  flue
      =/  old=(list leak)  ~(tap in spill.fod)
      =|  new=flue
      |-  ^-  flue
      ?~  old
        new
      =/  invalid
        |-  ^-  ?
        ?|  ?+    -.pour.i.old  %|
                %vale  (~(has in invalid) path.pour.i.old)
                %arch
              ::  TODO: overly conservative, should be only direct hoon
              ::  children
              ::
              =/  len  (lent path.pour.i.old)
              %-  ~(any in invalid)
              |=  =path
              =(path.pour.i.old (scag len path))
            ==
          ::
            =/  deps  ~(tap in deps.i.old)
            |-  ^-  ?
            ?~  deps
              %|
            ?|  ^$(i.old i.deps)
                $(deps t.deps)
            ==
        ==
      =?  new  !invalid
        :-  (~(put in spill.new) i.old)
        =/  =mist  (pour-to-mist pour.i.old)
        ?~  got=(~(get by sprig.fod) mist)
          sprig.new
        (~(put by sprig.new) mist u.got)
      $(old t.old)
    ::
    ++  page-to-cord
      |=  =page
      ^-  @t
      ?+  p.page  ~|([%sys-bad-mark p.page] !!)
        %hoon  ;;(@t q.page)
        %mime  q.q:;;(mime q.page)
      ==
    ::
    ++  lobe-to-cord
      |=  =lobe
      ^-  @t
      =/  peg=(unit page)  (~(get by lat.ran) lobe)
      ?~  peg
        ~|([%lobe-to-cord-tombstoned syd lobe] !!)
      ;;(@t q.u.peg)
    ::
    ::  Updated q.yaki
    ::
    ++  checkout-changes
      |=  [=ford=args:ford:fusion changes=(map path (each page lobe))]
      ^-  [(map path [=lobe =cage]) args:ford:fusion]
      %+  roll  `(list [path (each page lobe)])`~(tap by changes)
      |=  $:  [=path change=(each page lobe)]
              [built=(map path [lobe cage]) cache=_ford-args]
          ==
      ^+  [built ford-args]
      =.  ford-args  cache
      =/  [=cage fud=flow fod=flue]
        ::  ~>  %slog.[0 leaf/"clay: validating {(spud path)}"]
        %-  wrap:fusion
        (read-file:(ford:fusion ford-args) path)
      =.  cache.ford-args  fud
      =.  spill.ford-args  spill.fod
      =.  sprig.ford-args  sprig.fod
      =/  =lobe
        ?-  -.change
          %|  p.change
          ::  Don't use p.change.i.cans because that's before casting to
          ::  the correct mark.
          ::
          %&  (page-to-lobe [p q.q]:cage)
        ==
      [(~(put by built) path [lobe cage]) ford-args]
    ::
    ::  Print notification to console
    ::
    ++  print
      |=  [old=(map path lobe) new=(map path lobe)]
      ^-  (list move)
      =/  [deletes=(set path) upserts=(map path (each page lobe))]
        (get-changes old (~(run by new) |=(=lobe |+lobe)))
      =/  upsert-set  ~(key by upserts)
      =/  old-set     ~(key by old)
      =/  changes=(set path)    (~(int in upsert-set) old-set)
      =/  additions=(set path)  (~(dif in upsert-set) old-set)
      ?~  hun
        ~
      ?:  (lte let.dom 1)
        ~
      |^
      ;:  weld
        (paths-to-notes '-' deletes)
        (paths-to-notes ':' changes)
        (paths-to-notes '+' additions)
      ==
      ::
      ++  paths-to-notes
        |=  [prefix=@tD paths=(set path)]
        %+  turn  ~(tap in paths)
        |=  =path
        ^-  move
        [u.hun %pass /note %d %text prefix ' ' ~(ram re (path-to-tank path))]
      ::
      ++  path-to-tank
        |=  =path
        =/  pre=^path  ~[(scot %p our) syd (scot %ud let.dom)]
        :+  %rose  ["/" "/" ~]
        %+  turn  (weld pre path)
        |=  a=cord
        ^-  tank
        ?:  ((sane %ta) a)
          [%leaf (trip a)]
        [%leaf (dash:us (trip a) '\'' ~)]
      --
    ::
    ::  Check sanity
    ::
    ++  sane-changes
      |=  $:  changes=(map path (each page lobe))
              change-cages=(map path [lobe cage])
          ==
      ^-  (unit [(map path [lobe cage]) args:ford:fusion])
      ?.  check-sane
        ~
      =/  tak=(unit tako)  (~(get by hit.dom) let.dom)
      ?~  tak
        ~
      =/  =yaki  (~(got by hut.ran) u.tak)
      ::  Assert all pages hash to their lobe
      ::
      =/  foo
        %-  ~(urn by lat.ran)
        |=  [=lobe =page]
        =/  actual-lobe=^lobe  `@uv`(page-to-lobe page)
        ~|  [%bad-lobe have=lobe need=actual-lobe]
        ?>  =(lobe actual-lobe)
        ~
      ::  Assert we calculated the same change-cages w/o cache
      ::
      ::  ? remove deletes
      ::
      =/  all-changes=(map path (each page lobe))
        =/  original=(map path (each page lobe))
          (~(run by q.yaki) |=(=lobe |+lobe))
        (~(uni by original) changes)
      =/  =args:ford:fusion  [all-changes lat.ran veb.bug ~ ~ ~]
      =^  all-change-cages  args  (checkout-changes args all-changes)
      =/  ccs=(list [=path =lobe =cage])  ~(tap by change-cages)
      |-  ^+  *sane-changes
      ?^  ccs
        ?.  =(`[lobe cage]:i.ccs (~(get by all-change-cages) path.i.ccs))
          ~|  not-same-cages+path.i.ccs
          !!
        $(ccs t.ccs)
      `[all-change-cages args]
    ::
    ::  Delay current update until sys update is complete
    ::
    ++  sys-update
      |=  $:  =yoki
              data=(map path (each page lobe))
          ==
      ^+  ..park
      ?>  =(~ pud)
      =.  pud  `[syd yoki]
      |^  %.  [hen %slip %c %pork ~]
          emit:(pass-what files)
      ::
      ++  files
        ^-  (list (pair path (cask)))
        %+  murn
          ~(tap by data)
        |=  [pax=path dat=(each page lobe)]
        ^-  (unit (pair path (cask)))
        =/  xap  (flop pax)
        ?>  ?=(^ xap)
        ?.  ?=(%hoon i.xap)  ~
        :^  ~  (flop t.xap)  %hoon
        ~|  [pax=pax p.dat]
        ?-  -.dat
          %&  (page-to-cord p.dat)
          %|  (lobe-to-cord p.dat)
        ==
      ::
      ++  pass-what
        |=  fil=(list (pair path (cask)))
        ^+  ..park
        (emit hen %pass /what %$ what/fil)
      --
    --
  ::
  ::  [goad] Try to revive desk, but if it fails crash the event.
  ::
  ++  take-park-held
    |=  err=(unit tang)
    ^+  ..park
    ?^  err
      ((slog leaf+"clay: desk {<syd>} failed to unsuspend" u.err) ..park)
    =.  liv.dom  %live
    goad
  ::
  ::  We always say we're merging from 'ali' to 'bob'.  The basic steps,
  ::  not all of which are always needed, are:
  ::
  ::  --  fetch ali's desk, async in case it's remote
  ::  --  diff ali's desk against the mergebase
  ::  --  diff bob's desk against the mergebase
  ::  --  merge the diffs
  ::  --  commit
  ::
  ++  start-merge
    |=  [=ali=ship =ali=desk =case =germ]
    ^+  ..start-merge
    =/  =wire  /merge/[syd]/(scot %p ali-ship)/[ali-desk]/[germ]
    (emit hen %pass wire %c %warp ali-ship ali-desk `[%sing %v case /])
  ::
  ++  make-melt
    |=  [bas=beak con=(list [beak germ])]
    ^-  melt
    :+  bas  con
    %-  ~(gas by *(map beak (unit domo)))
    :-  [bas *(unit domo)]
    (turn con |=(a=[beak germ] [-.a *(unit domo)]))
  ::
  ++  start-fuse
    |=  [bas=beak con=(list [beak germ])]
    ^+  ..start-fuse
    =/  moves=(list move)
      %+  turn
        [[bas *germ] con]
      |=  [bec=beak germ]
      ^-  move
      =/  wir=wire  /fuse/[syd]/(scot %p p.bec)/[q.bec]/(scot r.bec)
      [hen %pass wir %c %warp p.bec q.bec `[%sing %v r.bec /]]
    ::
    ::  We also want to clear the state (fiz) associated with this
    ::  merge and print a warning if it's non trivial i.e. we're
    ::  starting a new fuse before the previous one terminated.
    ::
    =/  err=tang
      ?~  con.fiz
        ~
      =/  discarded=tang
        %+  turn
          ~(tap in sto.fiz)
        |=  [k=beak v=(unit domo)]
        ^-  tank
        =/  received=tape  ?~(v "missing" "received")
        leaf+"{<(en-beam k ~)>} {received}"
      :_  discarded
      leaf+"fusing into {<syd>} from {<bas>} {<con>} - overwriting prior fuse"
    =.  fiz  (make-melt bas con)
    ((slog err) (emil moves))
  ::
  ++  take-fuse
    |^
    ::
    |=  [bec=beak =riot]
    ^+  ..take-fuse
    ?~  riot
      ::
      ::  By setting fiz to *melt the merge is aborted - any further
      ::  responses we get for the merge will cause take-fuse to crash
      ::
      =.  fiz  *melt
      =/  msg=tape  <(en-beam bec ~)>
      ((slog [leaf+"clay: fuse failed, missing {msg}"]~) ..take-fuse)
    ?.  (~(has by sto.fiz) bec)
      =/  msg=tape  <(en-beam bec ~)>
      ((slog [leaf+"clay: got strange fuse response {<msg>}"]~) ..take-fuse)
    =.  fiz
        :+  bas.fiz  con.fiz
        (~(put by sto.fiz) bec `!<(domo q.r.u.riot))
    =/  all-done=flag
      %-  ~(all by sto.fiz)
      |=  res=(unit domo)
      ^-  flag
      !=(res ~)
    ?.  all-done
      ..take-fuse
    =|  rag=rang
    =/  clean-state  ..take-fuse
    =/  initial-dome=domo  (need (~(got by sto.fiz) bas.fiz))
    =/  next-yaki=yaki
      (~(got by hut.ran) (~(got by hit.initial-dome) let.initial-dome))
    =/  parents=(list tako)  ~[(~(got by hit.initial-dome) let.initial-dome)]
    =/  merges  con.fiz
    |-
    ^+  ..take-fuse
    ?~  merges
      =.  ..take-fuse  (done-fuse clean-state %& ~)
      (park | & [%| next-yaki(p (flop parents))] rag)
    =/  [bec=beak g=germ]  i.merges
    =/  ali-dom=domo  (need (~(got by sto.fiz) bec))
    =/  result  (merge-helper p.bec q.bec g ali-dom `next-yaki)
    ?-    -.result
        %|
      =/  failing-merge=tape  "{<bec>} {<g>}"
      (done-fuse clean-state %| %fuse-merge-failed leaf+failing-merge p.result)
    ::
        %&
      =/  merge-result=(unit merge-result)  +.result
      ?~  merge-result
        ::
        ::  This merge was a no-op, just continue
        ::
        $(merges t.merges)
      ?^  conflicts.u.merge-result
        ::
        :: If there are merge conflicts send the error and abort the merge
        ::
        (done-fuse clean-state %& conflicts.u.merge-result)
      =/  merged-yaki=yaki
        ?-    -.new.u.merge-result
            %|  +.new.u.merge-result
            %&
          ::
          ::  Convert the yuki to yaki
          ::
          =/  yuk=yuki  +.new.u.merge-result
          =/  lobes=(map path lobe)
            %-  ~(run by q.yuk)
            |=  val=(each page lobe)
            ^-  lobe
            ?-  -.val
              %&  (page-to-lobe +.val)
              %|  +.val
            ==
          (make-yaki p.yuk lobes now)
        ==
      %=  $
        next-yaki  merged-yaki
        merges     t.merges
        hut.ran    (~(put by hut.ran) r.merged-yaki merged-yaki)
        lat.rag    (~(uni by lat.u.merge-result) lat.rag)
        lat.ran    (~(uni by lat.u.merge-result) lat.ran)
        parents    [(~(got by hit.ali-dom) let.ali-dom) parents]
      ==
    ==
    ::  +done-fuse: restore state after a fuse is attempted, whether it
    ::  succeeds or fails.
    ::
    ++  done-fuse
      |=  [to-restore=_..take-fuse result=(each (set path) (pair term tang))]
      ^+  ..take-fuse
      =.  fiz.to-restore  *melt
      (done:to-restore result)
    --
  ::
  ++  done
    |=  result=(each (set path) (pair term tang))
    ^+  ..merge
    (emit hen %give %mere result)
  ::
  ++  merge
    |=  [=ali=ship =ali=desk =germ =riot]
    ^+  ..merge
    ?~  riot
      (done %| %ali-unavailable ~[>[ali-ship ali-desk germ]<])
    =/  ali-dome=domo
      ?:  &(?=(@ -.q.q.r.u.riot) !=(~ -.q.q.r.u.riot))
        !<(domo q.r.u.riot)
      +:!<([* domo] q.r.u.riot)
    =/  result=(each (unit merge-result) (pair term tang))
      (merge-helper ali-ship ali-desk germ ali-dome ~)
    ?-    -.result
        %|  (done %| +.result)
        %&
      =/  mr=(unit merge-result)  +.result
      ?~  mr
        (done %& ~)
      =.  ..merge  (done %& conflicts.u.mr)
      (park | & new.u.mr ~ lat.u.mr)
    ==
  ::
  +$  merge-result  [conflicts=(set path) new=yoki lat=(map lobe page)]
  ::
  ++  merge-helper
    |=  [=ali=ship =ali=desk =germ ali-dome=domo next-yaki=(unit yaki)]
    ^-  (each (unit merge-result) [term tang])
    |^
    ^-  (each (unit merge-result) [term tang])
    =/  ali-yaki=yaki  (~(got by hut.ran) (~(got by hit.ali-dome) let.ali-dome))
    =/  bob-yaki=(unit yaki)
      ?~  next-yaki
        ?~  let.dom
          ~
        (~(get by hut.ran) (~(got by hit.dom) let.dom))
      next-yaki
    =/  res  (mule |.((merge-by-germ ali-yaki bob-yaki)))
    ?-  -.res
      %&  &+p.res
      %|  |+merge-failed+p.res
    ==
    ::
    ++  merge-by-germ
      |=  [=ali=yaki bob-yaki=(unit yaki)]
      ^-  (unit merge-result)
      ::
      ::  If this is an %init merge, we set the ali's commit to be
      ::  bob's.
      ::
      ?:  ?=(%init germ)
        ?>  ?=(~ bob-yaki)
        `[conflicts=~ new=|+ali-yaki lat=~]
      ::
      =/  bob-yaki  (need bob-yaki)
      |^
      ^-  (unit merge-result)
      ?-    germ
      ::
      ::  If this is a %only-this merge, we check to see if ali's and bob's
      ::  commits are the same, in which case we're done.
      ::  Otherwise, we create a new commit with bob's data plus ali and
      ::  bob as parents.
      ::
          %only-this
        ?:  =(r.ali-yaki r.bob-yaki)
          ~
        :*  ~
            conflicts=~
            new=&+[[r.bob-yaki r.ali-yaki ~] (to-yuki q.bob-yaki)]
            lat=~
        ==
      ::
      ::  If this is a %only-that merge, we check to see if ali's and bob's
      ::  commits are the same, in which case we're done.  Otherwise, we
      ::  create a new commit with ali's data plus ali and bob as
      ::  parents.
      ::
          %only-that
        ?:  =(r.ali-yaki r.bob-yaki)
          ~
        :*  ~
            conflicts=~
            new=&+[[r.bob-yaki r.ali-yaki ~] (to-yuki q.ali-yaki)]
            lat=~
        ==
      ::
      ::  Create a merge commit with exactly the contents of the
      ::  destination desk except take any files from the source commit
      ::  which are not in the destination desk.
      ::
          %take-this
        ?:  =(r.ali-yaki r.bob-yaki)
          ~
        =/  new-data  (~(uni by q.ali-yaki) q.bob-yaki)
        :*  ~
            conflicts=~
            new=&+[[r.bob-yaki r.ali-yaki ~] (to-yuki new-data)]
            lat=~
        ==
      ::
      ::  Create a merge commit with exactly the contents of the source
      ::  commit except preserve any files from the destination desk
      ::  which are not in the source commit.
      ::
          %take-that
        ?:  =(r.ali-yaki r.bob-yaki)
          ~
        =/  new-data  (~(uni by q.bob-yaki) q.ali-yaki)
        :*  ~
            conflicts=~
            new=&+[[r.bob-yaki r.ali-yaki ~] (to-yuki new-data)]
            lat=~
        ==
      ::
      ::  If this is a %fine merge, we check to see if ali's and bob's
      ::  commits are the same, in which case we're done.  Otherwise, we
      ::  check to see if ali's commit is in the ancestry of bob's, in
      ::  which case we're done.  Otherwise, we check to see if bob's
      ::  commit is in the ancestry of ali's.  If not, this is not a
      ::  fast-forward merge, so we error out.  If it is, we add ali's
      ::  commit to bob's desk and checkout.
      ::
          %fine
        ?:  =(r.ali-yaki r.bob-yaki)
          ~
        ?:  (~(has in (reachable-takos:ze r.bob-yaki)) r.ali-yaki)
          ~
        ?.  (~(has in (reachable-takos:ze r.ali-yaki)) r.bob-yaki)
          ~_  %bad-fine-merge
          ~|  "tried fast-forward but is not ancestor or descendant"
          !!
        `[conflicts=~ new=|+ali-yaki lat=~]
      ::
          ?(%meet %mate %meld %meet-this %meet-that)
        ?:  =(r.ali-yaki r.bob-yaki)
          ~
        ?:  (~(has in (reachable-takos:ze r.bob-yaki)) r.ali-yaki)
          ~
        ?:  (~(has in (reachable-takos:ze r.ali-yaki)) r.bob-yaki)
          $(germ %fine)
        =/  merge-points  (find-merge-points ali-yaki bob-yaki)
        ?~  merge-points
          ~_  %merge-no-merge-base
          ~|  "consider a %this or %that merge to get a mergebase"
          !!
        =/  merge-point=yaki  n.merge-points
        ?:  ?=(?(%mate %meld) germ)
          =/  ali-diffs=cane  (diff-base ali-yaki bob-yaki merge-point)
          =/  bob-diffs=cane  (diff-base bob-yaki ali-yaki merge-point)
          =/  bof=(map path (unit cage))
            (merge-conflicts can.ali-diffs can.bob-diffs)
          (build ali-yaki bob-yaki merge-point ali-diffs bob-diffs bof)
        =/  ali-diffs=cane  (calc-diffs ali-yaki merge-point)
        =/  bob-diffs=cane  (calc-diffs bob-yaki merge-point)
        =/  both-diffs=(map path *)
          %-  %~  int  by
              %-  ~(uni by `(map path *)`new.ali-diffs)
              %-  ~(uni by `(map path *)`cal.ali-diffs)
              %-  ~(uni by `(map path *)`can.ali-diffs)
              `(map path *)`old.ali-diffs
          %-  ~(uni by `(map path *)`new.bob-diffs)
          %-  ~(uni by `(map path *)`cal.bob-diffs)
          %-  ~(uni by `(map path *)`can.bob-diffs)
          `(map path *)`old.bob-diffs
        ?:  &(?=(%meet germ) !=(~ both-diffs))
          ~_  %meet-conflict
          ~|  [~(key by both-diffs) "consider a %mate merge"]
          !!
        =/  both-done=(map path lobe)
          |^
          ?-  germ
            %meet       ~
            %meet-this  (resolve (~(uni by new.bob-diffs) cal.bob-diffs))
            %meet-that  (resolve (~(uni by new.ali-diffs) cal.ali-diffs))
          ==
          ++  resolve
            |=  news=(map path lobe)
            %-  malt  ^-  (list [path lobe])
            %+  murn  ~(tap by both-diffs)
            |=  [=path *]
            ^-  (unit [^path lobe])
            =/  new  (~(get by news) path)
            ?~  new
              ~
            `[path u.new]
          --
        ::
        =/  deleted
          %-  ~(dif by (~(uni by old.ali-diffs) old.bob-diffs))
          (~(run by both-done) |=(* ~))
        =/  not-deleted=(map path lobe)
          %+  roll  ~(tap by deleted)
          =<  .(not-deleted q.merge-point)
          |=  [[pax=path ~] not-deleted=(map path lobe)]
          (~(del by not-deleted) pax)
        =/  hat=(map path lobe)
          %-  ~(uni by not-deleted)
          %-  ~(uni by new.ali-diffs)
          %-  ~(uni by new.bob-diffs)
          %-  ~(uni by cal.ali-diffs)
          cal.bob-diffs
        :*  ~
            conflicts=~
            new=&+[[r.bob-yaki r.ali-yaki ~] (to-yuki hat)]
            lat=~
        ==
      ==
      ::
      ++  to-yuki
        |=  m=(map path lobe)
        ^-  (map path (each page lobe))
        (~(run by m) |=(=lobe |+lobe))
      ::
      ::  The set of changes between the mergebase and one of the desks
      ::  being merged
      ::
      ::  --  `new` is the set of files in the new desk and not in the
      ::  mergebase.
      ::  --  `cal` is the set of changes in the new desk from the
      ::  mergebase except for any that are also in the other new desk.
      ::  --  `can` is the set of changes in the new desk from the
      ::  mergebase that are also in the other new desk (potential
      ::  conflicts).
      ::  --  `old` is the set of files in the mergebase and not in the
      ::  new desk.
      ::
      +$  cane
        $:  new=(map path lobe)
            cal=(map path lobe)
            can=(map path cage)
            old=(map path ~)
        ==
      ::
      ::  Calculate cane knowing there are no files changed by both
      ::  desks
      ::
      ++  calc-diffs
        |=  [hed=yaki bas=yaki]
        ^-  cane
        :*  %-  molt
            %+  skip  ~(tap by q.hed)
            |=  [pax=path lob=lobe]
            (~(has by q.bas) pax)
          ::
            %-  molt
            %+  skip  ~(tap by q.hed)
            |=  [pax=path lob=lobe]
            =+  (~(get by q.bas) pax)
            |(=(~ -) =([~ lob] -))
          ::
            ~
          ::
            %-  malt  ^-  (list [path ~])
            %+  murn  ~(tap by q.bas)
            |=  [pax=path lob=lobe]
            ^-  (unit (pair path ~))
            ?.  =(~ (~(get by q.hed) pax))
              ~
            `[pax ~]
        ==
      ::
      ::  Diff yak against bas where different from yuk
      ::
      ++  diff-base
        |=  [yak=yaki yuk=yaki bas=yaki]
        ^-  cane
        =/  new=(map path lobe)
          %-  malt
          %+  skip  ~(tap by q.yak)
          |=  [=path =lobe]
          (~(has by q.bas) path)
        ::
        =/  cal=(map path lobe)
          %-  malt  ^-  (list [path lobe])
          %+  murn  ~(tap by q.bas)
          |=  [pax=path lob=lobe]
          ^-  (unit (pair path lobe))
          =+  a=(~(get by q.yak) pax)
          =+  b=(~(get by q.yuk) pax)
          ?.  ?&  ?=(^ a)
                  !=([~ lob] a)
                  =([~ lob] b)
              ==
            ~
          `[pax +.a]
        ::
        =/  can=(map path cage)
          %-  malt
          %+  murn  ~(tap by q.bas)
          |=  [=path =lobe]
          ^-  (unit [^path cage])
          =/  in-yak  (~(get by q.yak) path)
          ?~  in-yak
            ~
          ?:  =(lobe u.in-yak)
            ~
          =/  in-yuk  (~(get by q.yuk) path)
          ?~  in-yuk
            ~
          ?:  =(lobe u.in-yuk)
            ~
          ?:  =(u.in-yak u.in-yuk)
            ~
          =/  cug=(unit cage)  (diff-lobes lobe u.in-yak)
          ?~  cug
            ~_  %tombstoned-mergebase
            ~|  path
            ~|  "consider a 2-way merge such as %only-this or %only-that"
            !!
          `[path u.cug]
        ::
        =/  old=(map path ~)
            %-  malt  ^-  (list [path ~])
            %+  murn  ~(tap by q.bas)
            |=  [pax=path lob=lobe]
            ?.  =(~ (~(get by q.yak) pax))
              ~
            (some pax ~)
        ::
        [new cal can old]
      ::
      ::  These can/should save their caches
      ::
      ++  lobe-to-cage
        |=  =lobe
        ^-  (unit cage)
        =/  peg=(unit page)  (~(get by lat.ran) lobe)
        ?~  peg
          ~
        =/  [=cage *]
          %-  wrap:fusion
          (page-to-cage:(tako-ford (~(got by hit.dom) let.dom)) u.peg)
        `cage
      ::
      ++  get-dais
        |=  =mark
        ^-  dais
        =/  [=dais *]
          %-  wrap:fusion
          (build-dais:(tako-ford (~(got by hit.dom) let.dom)) mark)
        dais
      ::
      ::  Diff two files on bob-desk
      ::
      ++  diff-lobes
        |=  [=a=lobe =b=lobe]
        ^-  (unit cage)
        =/  a-cage  (lobe-to-cage a-lobe)
        =/  b-cage  (lobe-to-cage b-lobe)
        ?:  |(?=(~ a-cage) ?=(~ b-cage))
          ~
        ?>  =(p.u.a-cage p.u.b-cage)
        =/  =dais  (get-dais p.u.a-cage)
        `[form:dais (~(diff dais q.u.a-cage) q.u.b-cage)]
      ::
      ::  Merge diffs that are on the same file.
      ::
      ++  merge-conflicts
        |=  [ali-conflicts=(map path cage) bob-conflicts=(map path cage)]
        ^-  (map path (unit cage))
        %-  ~(urn by (~(int by ali-conflicts) bob-conflicts))
        |=  [=path *]
        ^-  (unit cage)
        =/  cal=cage  (~(got by ali-conflicts) path)
        =/  cob=cage  (~(got by bob-conflicts) path)
        =/  =mark
          =+  (slag (dec (lent path)) path)
          ?~(- %$ i.-)
        =/  =dais  (get-dais mark)
        =/  res=(unit (unit vase))  (~(join dais *vale:dais) q.cal q.cob)
        ?~  res
          `[form:dais q.cob]
        ?~  u.res
          ~
        `[form:dais u.u.res]
      ::
      ::  Apply the patches in bof to get the new merged content.
      ::
      ::  Gather all the changes between ali's and bob's commits and the
      ::  mergebase.  This is similar to the %meet of ++merge, except
      ::  where they touch the same file, we use the merged versions.
      ::
      ++  build
        |=  $:  ali=yaki
                bob=yaki
                bas=yaki
                dal=cane
                dob=cane
                bof=(map path (unit cage))
            ==
        ^-  (unit merge-result)
        =/  both-patched=(map path cage)
          %-  malt
          %+  murn  ~(tap by bof)
          |=  [=path cay=(unit cage)]
          ^-  (unit [^path cage])
          ?~  cay
            ~
          :+  ~  path
          =+  (~(get by q.bas) path)
          ?~  -
            ~|  %mate-strange-diff-no-base
            !!
          ::  +need ok because we would have crashed in +diff-base
          ::
          =/  =cage  ~|([%build-need path] (need (lobe-to-cage u.-)))
          =/  =dais  (get-dais p.cage)
          ?>  =(p.u.cay form.dais)
          :-  p.cage
          (~(pact dais q.cage) q.u.cay)
        =/  con=(map path *)                            ::  2-change conflict
          %-  molt
          %+  skim  ~(tap by bof)
          |=([pax=path cay=(unit cage)] ?=(~ cay))
        =/  cab=(map path lobe)                         ::  conflict base
          %-  ~(urn by con)
          |=  [pax=path *]
          (~(got by q.bas) pax)
        =.  con                                         ::  change+del conflict
          %-  ~(uni by con)
          %-  malt  ^-  (list [path *])
          %+  skim  ~(tap by old.dal)
          |=  [pax=path ~]
          ?:  (~(has by new.dob) pax)
            ~|  %strange-add-and-del
            !!
          (~(has by can.dob) pax)
        =.  con                                         ::  change+del conflict
          %-  ~(uni by con)
          %-  malt  ^-  (list [path *])
          %+  skim  ~(tap by old.dob)
          |=  [pax=path ~]
          ?:  (~(has by new.dal) pax)
            ~|  %strange-del-and-add
            !!
          (~(has by can.dal) pax)
        =.  con                                         ::  add+add conflict
          %-  ~(uni by con)
          %-  malt  ^-  (list [path *])
          %+  skip  ~(tap by (~(int by new.dal) new.dob))
          |=  [pax=path *]
          =((~(got by new.dal) pax) (~(got by new.dob) pax))
        ?:  &(?=(%mate germ) ?=(^ con))
          =+  (turn ~(tap by `(map path *)`con) |=([path *] >[+<-]<))
          ~_  %mate-conflict
          ~|  (turn ~(tap by `(map path *)`con) |=([path *] +<-))
          !!
        =/  old=(map path lobe)                         ::  oldies but goodies
          %+  roll  ~(tap by (~(uni by old.dal) old.dob))
          =<  .(old q.bob)
          |=  [[pax=path ~] old=(map path lobe)]
          (~(del by old) pax)
        =/  [hot=(map path lobe) lat=(map lobe page)]   ::  new content
          %+  roll  ~(tap by both-patched)
          |=  [[pax=path cay=cage] hat=(map path lobe) lat=(map lobe page)]
          =/  =page  [p q.q]:cay
          =/  =lobe  (page-to-lobe page)
          :-  (~(put by hat) pax lobe)
          ?:  (~(has by lat) lobe)
            lat
          (~(uni by (malt [lobe page] ~)) lat)
        =/  hat=(map path lobe)                         ::  all the content
          %-  ~(uni by old)
          %-  ~(uni by new.dal)
          %-  ~(uni by new.dob)
          %-  ~(uni by cal.dal)
          %-  ~(uni by cal.dob)
          %-  ~(uni by hot)
          cab
        =/  del=(map path ?)
          (~(run by (~(uni by old.dal) old.dob)) |=(~ %|))
        =/  new  &+[[r.bob r.ali ~] (~(run by hat) |=(=lobe |+lobe))]
        :*  ~
            (silt (turn ~(tap by con) head))
            new
            lat
        ==
      --
    --
  ::
  ::  Find the most recent common ancestor(s).
  ::
  ::    For performance, this depends on +reachable-takos being
  ::    memoized.
  ::
  ++  find-merge-points
    |=  [=ali=yaki =bob=yaki]
    ^-  (set yaki)
    ::  Loop through ancestors breadth-first, lazily generating ancestry
    ::
    =/  ali-takos  (reachable-takos:ze r.ali-yaki)
    ::  Tako worklist
    ::
    =/  takos=(qeu tako)  [r.bob-yaki ~ ~]
    ::  Mergebase candidates.  Have proven they're common ancestors, but
    ::  not that they're a most recent
    ::
    =|  bases=(set tako)
    ::  Takos we've already checked or are in our worklist
    ::
    =|  done=(set tako)
    |-  ^-  (set yaki)
    =*  outer-loop  $
    ::  If we've finished our worklist, convert to yakis and return
    ::
    ?:  =(~ takos)
      (silt (turn ~(tap in bases) ~(got by hut.ran)))
    =^  =tako  takos  ~(get to takos)
    =.  done  (~(put in done) tako)
    ::  If this is a common ancestor, stop recursing through our
    ::  parentage.  Check if it's comparable to any existing candidate.
    ::
    ?:  (~(has in ali-takos) tako)
      =/  base-list  ~(tap in bases)
      |-  ^-  (set yaki)
      =*  bases-loop  $
      ?~  base-list
        ::  Proven it's not an ancestor of any previous candidate.
        ::  Remove all ancestors of new candidate and add it to the
        ::  candidate list.
        ::
        =.  bases
          =/  new-reachable  (reachable-takos:ze tako)
          (~(put in (~(dif in bases) new-reachable)) tako)
        outer-loop
      ::  If it's an ancestor of another candidate, this is not most
      ::  recent, so skip and try next in worklist.
      ::
      =/  base-reachable  (reachable-takos:ze i.base-list)
      ?:  (~(has in base-reachable) tako)
        outer-loop
      bases-loop(base-list t.base-list)
    ::  Append parents to list and recurse
    ::
    =/  bob-yaki  (~(got by hut.ran) tako)
    =/  new-candidates  (skip p.bob-yaki ~(has in done))
    %_  outer-loop
      done   (~(gas in done) new-candidates)
      takos  (~(gas to takos) new-candidates)
    ==
  ::
  ++  want-mime
    |=  yon=aeon
    %-  ~(any by mon)
    |=  =beam
    &(=(p.beam her) =(q.beam syd) =(r.beam ud+yon))
  ::
  ::  Update mime cache
  ::
  ++  checkout-mime
    |=  $:  =ford=args:ford:fusion
            deletes=(set path)
            changes=(set path)
        ==
    ^-  [(map path (unit mime)) args:ford:fusion]
    =/  mim=(map path (unit mime))
      =/  dels=(list path)  ~(tap by deletes)
      |-  ^-  (map path (unit mime))
      ?~  dels
        ~
      (~(put by $(dels t.dels)) i.dels ~)
    =/  cans=(list path)  ~(tap by changes)
    |-  ^-  [(map path (unit mime)) args:ford:fusion]
    ?~  cans
      [mim ford-args]
    =/  [=cage fud=flow fod=flue]
      ~|  mime-cast-fail+i.cans
      (wrap:fusion (cast-path:(ford:fusion ford-args) i.cans %mime))
    =.  cache.ford-args  fud
    =.  spill.ford-args  spill.fod
    =.  sprig.ford-args  sprig.fod
    =^  mim  ford-args  $(cans t.cans)
    [(~(put by mim) i.cans `!<(mime q.cage)) ford-args]
  ::
  ::  Add or remove entries to the mime cache
  ::
  ++  apply-changes-to-mim
    |=  [mim=(map path mime) changes=(map path (unit mime))]
    ^-  (map path mime)
    =/  changes-l=(list [pax=path change=(unit mime)])
      ~(tap by changes)
    |-  ^-  (map path mime)
    ?~  changes-l
      mim
    ?~  change.i.changes-l
      $(changes-l t.changes-l, mim (~(del by mim) pax.i.changes-l))
    $(changes-l t.changes-l, mim (~(put by mim) [pax u.change]:i.changes-l))
  ::
  ::  Emit update to unix sync
  ::
  ::  [ergo] Must be called any time the set of files changes that must
  ::  be mirrored to unix.  +want-mime may optionally be used to cheaply
  ::  check if a version of a desk is mirrored to unix (and so +ergo
  ::  must be called).
  ::
  ++  ergo
    |=  [yon=aeon mim=(map path (unit mime))]
    ^+  ..park
    =/  must  (must-ergo yon mon (turn ~(tap by mim) head))
    %-  emil
    %+  turn  ~(tap by must)
    |=  [pot=term len=@ud pak=(set path)]
    :*  (need hez)  %give  %ergo  pot
        %+  turn  ~(tap in pak)
        |=  pax=path
        [(slag len pax) (~(got by mim) pax)]
    ==
  ::
  ::  Output is a map of mount points to {length-of-mounted-path set-of-paths}.
  ::
  ++  must-ergo
    |=  [yon=aeon mon=(map term beam) can=(list path)]
    ^-  (map term (pair @ud (set path)))
    %-  malt  ^-  (list (trel term @ud (set path)))
    %+  murn  ~(tap by mon)
    |=  [nam=term bem=beam]
    ^-  (unit (trel term @ud (set path)))
    =-  ?~(- ~ `[nam (lent s.bem) (silt `(list path)`-)])
    %+  skim  can
    |=  pax=path
    &(=(p.bem her) =(q.bem syd) =(r.bem ud+yon) =(s.bem (scag (lent s.bem) pax)))
  ::
  ::  Mount a beam to unix
  ::
  ++  mount
    |=  [pot=term =case =spur]
    ^+  ..mount
    =/  old-mon  (~(get by mon) pot)
    ?^  old-mon
      %-  (slog >%already-mounted< >u.old-mon< ~)
      ..mount
    =/  yon  (case-to-aeon case)
    ?~  yon
      %-  (slog >%unknown-case< >[her syd case spur]< ~)
      ..mount
    =/  for-yon  ?:(=(let.dom u.yon) 0 u.yon)
    =.  mon                                             ::  [ergo]
      (~(put by mon) pot [her syd ud+for-yon] spur)
    =/  =yaki  (~(got by hut.ran) (~(got by hit.dom) u.yon))
    =/  files  (~(run by q.yaki) |=(=lobe |+lobe))
    =/  =args:ford:fusion
      [files lat.ran veb.bug fad ?:(=(yon let.dom) fod.dom [~ ~])]
    =^  mim  args
      (checkout-mime args ~ ~(key by files))
    =.  mim.dom  (apply-changes-to-mim mim.dom mim)
    (ergo for-yon mim)
  ::
  ::  Unmount a beam
  ::
  ++  unmount
    |=  [pot=term =case =spur]
    ^+  ..unmount
    ?>  ?=(^ hez.ruf)
    =.  mon  (~(del by mon) pot)                        ::  [ergo]
    =?  mim.dom  !(want-mime 0)  ~
    (emit u.hez.ruf %give %ogre pot)
  ::
  ::  Set permissions for a node.
  ::
  ++  perm
    |=  [pax=path rit=rite]
    ^+  +>
    =/  mis=(set @ta)
      %+  roll
        =-  ~(tap in -)
        ?-  -.rit
          %r    who:(fall red.rit *rule)
          %w    who:(fall wit.rit *rule)
          %rw   (~(uni in who:(fall red.rit *rule)) who:(fall wit.rit *rule))
        ==
      |=  [w=whom s=(set @ta)]
      ?:  |(?=(%& -.w) (~(has by cez) p.w))  s
      (~(put in s) p.w)
    ?^  mis
      ::  TODO remove this nasty hack
      ::
      ?.  ?=([[%a *] *] hen)
        +>.$
      =-  (emit hen %give %done `[%perm-fail [%leaf "No such group(s): {-}"]~])
      %+  roll  ~(tap in `(set @ta)`mis)
      |=  [g=@ta t=tape]
      ?~  t  (trip g)
      :(weld t ", " (trip g))
    ::  TODO remove this nasty hack
    ::
    =<  ?.  ?=([[%a *] *] hen)
          .
        (emit hen %give %done ~)
    ::
    ?-  -.rit                                           ::  [wake] <>
      %r    wake(per (put-perm per pax red.rit))
      %w    wake(pew (put-perm pew pax wit.rit))
      %rw   wake(per (put-perm per pax red.rit), pew (put-perm pew pax wit.rit))
    ==
  ::
  ++  put-perm
    |=  [pes=regs pax=path new=(unit rule)]
    ?~  new  (~(del by pes) pax)
    (~(put by pes) pax u.new)
  ::
  ::  Remove a group from all rules.
  ::
  ::  [wake] <
  ::
  ++  forget-crew
    |=  nom=@ta
    %=  +>                                              ::  [wake] < +call
      per  (forget-crew-in nom per)
      pew  (forget-crew-in nom pew)
    ==
  ::
  ++  forget-crew-in
    |=  [nom=@ta pes=regs]
    %-  ~(run by pes)
    |=  r=rule
    r(who (~(del in who.r) |+nom))
  ::
  ++  set-rein                                          ::  [goad] <
    |=  [ren=(map dude:gall ?)]
    ^+  ..park
    ..park(ren.dom ren)
  ::
  ++  set-zest                                          ::  [goad] <
    |=  liv=zest
    =?  liv  =(%base syd)  %live
    ..park(liv.dom liv)
  ::
  ++  rise                                              ::  [goad] <
    |=  [=dude:gall on=(unit ?)]
    ?<  =(%base syd)
    %_    ..park
        ren.dom
      ?~  on
        (~(del by ren.dom) dude)
      (~(put by ren.dom) dude u.on)
    ==
  ::
  ++  stay
    |=  ver=(unit weft)
    ^+  ..park
    =.  wic.dom                                         ::  [tare] <>
      ?~  ver
        ~
      (~(del by wic.dom) u.ver)
    tare
  ::
  ::  Try to apply highest-versioned %base commit-in-waiting
  ::
  ::  [wick] Must be called whenever we might have unblocked a kelvin
  ::  upgrade.  This is move-order agnostic because it defers the
  ::  upgrade into a new event.
  ::
  ++  wick
    ^+  ..park
    (emit hen %pass /wick %b %wait now)
  ::
  ++  take-wick
    |=  err=(unit tang)
    ^+  ..park
    ?^  err
      ((slog leaf+"clay: failed to upgrade kelvin (wick)" u.err) ..park)
    ?>  ?=(%base syd)
    =/  wis=(list [weft =yoki])
      %+  sort  ~(tap by wic.dom)
      |=  [a=[weft yoki] b=[weft yoki]]
      (gth num.a num.b)
    =.  wis  (skip wis |=([[* a=@ud] *] (gte a zuse)))
    ?~  wis  ::  Every commit bottoms out here ?
      ..park
    (park | & yoki.i.wis *rang)
  ::
  ::  Cancel a request.
  ::
  ::  For local requests, we just remove it from `qyx`.  For foreign requests,
  ::  we remove it from `ref` and tell the foreign ship to cancel as well.
  ::
  ++  cancel-request                                    ::  release request
    ^+  ..cancel-request
    =^  wos=(list wove)  qyx
      :_  (~(run by qyx) |=(a=(set duct) (~(del in a) hen)))
      %-  ~(rep by qyx)
      |=  [[a=wove b=(set duct)] c=(list wove)]
      ?:((~(has in b) hen) [a c] c)
    ::
    ?~  ref
      =>  .(ref `(unit rind)`ref)
      ?:  =(~ wos)  ..cancel-request          ::  TODO handle?
      |-  ^+  ..cancel-request
      ?~  wos  ..cancel-request
      =.  ..cancel-request  (run-if-future rove.i.wos |=(@da (best hen +<)))
      $(wos t.wos)
    ::
    ?~  nux=(~(get by fod.u.ref) hen)
      ..cancel-request(ref `(unit rind)`ref)  ::  XX TMI
    =/  sat  (~(got by bom.u.ref) u.nux)
    =:  fod.u.ref  (~(del by fod.u.ref) hen)
        bom.u.ref  (~(del by bom.u.ref) u.nux)
      ==
    ::  cancel the request as appropriate
    ::
    ?.  ?=([~ ^] busy.sat)
      %.  [hen her u.nux [syd ~]]
      send-over-ames(ref `(unit rind)`ref)    ::  XX TMI
    %-  emil
    =*  bus  u.busy.sat
    =/  =wire  (request-wire kind.bus her syd u.nux)
    ~&  %cancel-request-yawn
    :~  [hen %pass wire %a %yawn her path.bus]
        [hen %pass wire %b %rest time.bus]
    ==
  ::
  ::  Handles a request.
  ::
  ::  `%sing` requests are handled by ++aver.  `%next` requests are handled by
  ::  running ++aver at the given case, and then subsequent cases until we find
  ::  a case where the two results aren't equivalent.  If it hasn't happened
  ::  yet, we wait.  `%many` requests are handled by producing as much as we can
  ::  and then waiting if the subscription range extends into the future.
  ::
  ++  start-request
    |=  [for=(unit [ship @ud]) rav=rave]
    ^+  ..start-request
    ?:  &(?=(^ for) !(foreign-capable rav))
      ~&  [%bad-foreign-request-care from=for rav]
      ..start-request
    =^  [new-sub=(unit rove) cards=(list card)]  ..start-request
      (try-fill-sub for (rave-to-rove rav))
    =.  ..start-request  (send-cards cards [hen ~ ~])
    ?~  new-sub
      ..start-request
    (duce for u.new-sub)
  ::
  ::  +retry-with-ames: we tried scrying. now try with ames instead.
  ::
  ++  retry-with-ames
    |=  [kind=@ta inx=@ud]
    ^+  ..retry-with-ames
    ~|  [%retry-with-ames kind]
    ?>  ?=(%back-index kind)
    ~|  [%strange-retry-no-request her syd inx]
    ?>  ?=(^ ref)
    =/  sat=update-state  (~(got by bom.u.ref) inx)
    ::  mark her as having broken scry comms
    ::
    =.  sad  (~(put by sad) her now)
    ::  clean up scry request & timer
    ::
    =.  ..retry-with-ames
      =<  ?>(?=(^ ref) .)
      ~|  [%strange-retry-not-scry her syd inx busy.sat -.rave.sat]
      =/  bus  ?>(?=([~ ^] busy.sat) u.busy.sat)
      =/  =wire  (request-wire kind her syd inx)
      %-  emil
      ~&  %retry-with-ames-yawn
      :~  [hen %pass wire %b %rest time.bus]
          [hen %pass wire %a %yawn her path.bus]
      ==
    ::  re-send over ames
    ::
    =.  bom.u.ref  (~(put by bom.u.ref) inx sat(busy ~))
    abet:work:(foreign-update inx)
  ::
  ::  Called when a foreign ship answers one of our requests.
  ::
  ::  If it's a `%many` request, process in +take-foreign-update
  ::
  ::  After updating ref (our request manager), we handle %x, %w, and %y
  ::  responses.  For %x, we call ++validate-x to validate the type of
  ::  the response.  For %y, we coerce the result to an arch.
  ::
  ++  take-foreign-answer                               ::  external change
    |=  [inx=@ud rut=(unit rand)]
    ^+  +>
    ?>  ?=(^ ref)
    =+  ruv=(~(get by bom.u.ref) inx)
    ?~  ruv
      ~&  %bad-answer
       +>.$
    =/  rav=rave  rave.u.ruv
    ?:  ?=(%many -.rav)
      abet:(apex:(foreign-update inx) rut)
    ?~  rut
      ::  nothing here, so cache that
      ::
      %_    wake                                        ::  [wake] <>
          haw.u.ref
        ?.  ?=(%sing -.rav)  haw.u.ref
        (~(put by haw.u.ref) mood.rav ~)
      ==
    |^
    =/  result=(unit cage)  (validate u.rut)
    =/  =mood  [p.p q.p q]:u.rut
    =:  haw.u.ref  (~(put by haw.u.ref) mood result)    ::  [wake] <>
        bom.u.ref  (~(del by bom.u.ref) inx)
        fod.u.ref  (~(del by fod.u.ref) hen)
      ==
    wake
    ::  something here, so validate
    ::
    ++  validate
      |=  =rand
      ^-  (unit cage)
      ?-    p.p.rand
          %a  ~|  %no-big-ford-builds-across-network-for-now  !!
          %b  ~|  %i-guess-you-ought-to-build-your-own-marks  !!
          %c  ~|  %casts-should-be-compiled-on-your-own-ship  !!
          %d  ~|  %totally-temporary-error-please-replace-me  !!
          %e  ~|  %yes-naves-also-shouldnt-cross-the-network  !!
          %f  ~|  %even-static-casts-should-be-built-locally  !!
          %p  ~|  %requesting-foreign-permissions-is-invalid  !!
          %r  ~|  %no-cages-please-they-are-just-way-too-big  !!
          %s  ~|  %please-dont-get-your-takos-over-a-network  !!
          %t  ~|  %requesting-foreign-directory-is-vaporware  !!
          %v  ~|  %weird-shouldnt-get-v-request-from-network  !!
          %q  `[p %noun q]:r.rand
          %u  `(validate-u r.rand)
          %w  `(validate-w r.rand)
          %x  (validate-x [p.p q.p q r]:rand)
          %y  `[p.r.rand !>(;;(arch q.r.rand))]
          %z  `(validate-z r.rand)
      ==
    ::
    ::  Make sure the incoming data is a %u response
    ::
    ++  validate-u
      |=  =page
      ^-  cage
      ?>  ?=(%flag p.page)
      :-  p.page
      !>  ;;(? q.page)
    ::
    ::  Make sure the incoming data is a %w response
    ::
    ++  validate-w
      |=  =page
      ^-  cage
      :-  p.page
      ?+  p.page  ~|  %strange-w-over-nextwork  !!
        %cass  !>(;;(cass q.page))
        %null  [[%atom %n ~] ~]
        %nako  !>(~|([%molding [&1 &2 &3]:q.page] ;;(nako q.page)))
      ==
    ::
    ::  Make sure that incoming data is of the mark it claims to be.
    ::
    ++  validate-x
      |=  [car=care cas=case pax=path peg=page]
      ^-  (unit cage)
      =/  vale-result
        %-  mule  |.
        %-  wrap:fusion
        ::  Use %base's marks to validate, so we don't have to build the
        ::  foreign marks
        ::
        =/  base-dome  dom:(~(got by dos.rom) %base)
        =/  f
          %-  %*(. tako-ford dom base-dome)
          (~(got by hit.base-dome) let.base-dome)
        (page-to-cage:f peg)
      ?:  ?=(%| -.vale-result)
        %-  (slog >%validate-x-failed< p.vale-result)
        ~
      `-.p.vale-result
    ::
    ::  Make sure the incoming data is a %z response
    ::
    ++  validate-z
      |=  =page
      ^-  cage
      ?>  ?=(%uvi p.page)
      :-  p.page
      !>  ;;(@uvI q.page)
    --
  ::
  ::  Respond to backfill request
  ::
  ::  Maybe should verify the requester is allowed to access this lobe?
  ::
  ++  give-backfill
    |=  [ver=?(%0 %1) =lobe]
    ^+  ..give-backfill
    =/  peg=(unit page)  (~(get by lat.ran) lobe)
    =/  res
      ?-  ver
        %0  ?~(peg [%1 ~] [%direct lobe u.peg])
        %1  [%1 peg]
      ==
    (emit hen %give %boon res)
  ::
  ::  Ingest foreign update, requesting missing lobes if necessary
  ::
  ++  foreign-update
    |=  inx=@ud
    ?>  ?=(^ ref)
    =/  [sat=update-state lost=?]
      =/  ruv  (~(get by bom.u.ref) inx)
      ?~  ruv
        ~&  [%clay-foreign-update-lost her syd inx]
        [*update-state &]
      [u.ruv |]
    =/  done=?  |
    =.  hen  duct.sat
    |%
    ++  abet
      ^+  ..foreign-update
      ?:  lost
        ..foreign-update
      ?:  done
        =:  bom.u.ref  (~(del by bom.u.ref) inx)
            fod.u.ref  (~(del by fod.u.ref) hen)
          ==
        =<(?>(?=(^ ref) .) wake)
      =.  bom.u.ref  (~(put by bom.u.ref) inx sat)
      ..foreign-update
    ::
    ++  apex
      |=  rut=(unit rand)
      ^+  ..abet
      ?:  lost  ..abet
      ?~  rut
        =.  nako.sat  (~(put to nako.sat) ~)
        work
      ?>  ?=(%nako p.r.u.rut)
      =/  nako  ;;(nako q.r.u.rut)
      ::  must be appended because we delete off front
      ::
      =.  need.sat  (welp need.sat (missing-lobes nako))
      =.  nako.sat  (~(put to nako.sat) ~ nako)
      work
    ::
    ++  missing-lobes
      |=  =nako
      ^-  (list [tako path lobe])
      =|  miss=(set lobe)
      =/  let-tako  (~(got by gar.nako) let.nako)
      =/  yakis  ~(tap in lar.nako)
      |-  ^-  (list [tako path lobe])
      =*  yaki-loop  $
      ?~  yakis
        ~
      =/  =norm
        ::  Always try to fetch the entire last commit, because often we
        ::  want to merge from it.
        ::
        ?:  =(let-tako r.i.yakis)
          *norm:clay
        (~(gut by tom.dom) r.i.yakis nor.dom)
      =/  lobes=(list [=path =lobe])  ~(tap by q.i.yakis)
      |-  ^-  (list [tako path lobe])
      =*  blob-loop  $
      ?~  lobes
        yaki-loop(yakis t.yakis)
      =*  lobe  lobe.i.lobes
      ?:  ?|  (~(has by lat.ran) lobe)
              =([~ %|] +:(~(fit of norm) path.i.lobes))
              (~(has in miss) lobe)
          ==
        blob-loop(lobes t.lobes)
      :-  [r.i.yakis i.lobes]
      blob-loop(lobes t.lobes, miss (~(put in miss) lobe))
    ::
    ::  Receive backfill response
    ::
    ++  take-backfill
      |=  =fell
      ^+  ..abet
      ?:  lost  ..abet
      =?  need.sat  ?=(^ need.sat)  t.need.sat
      =.  ..park  =>((take-fell fell) ?>(?=(^ ref) .))
      work(busy.sat ~)
    ::
    ::  Fetch next lobe
    ::
    ++  work
      ^+  ..abet
      ?.  =(~ busy.sat)  ::NOTE  tmi
        ..abet
      |-  ^+  ..abet
      ?~  need.sat
        ::  NB: if you change to release nakos as we get enough lobes
        ::  for them instead of all at the end, you *must* store the
        ::  `lim` that should be applied after the nako is complete and
        ::  not use the one in the rave, since that will apply to the
        ::  end of subscription.
        ::
        |-  ^+  ..abet
        ?:  =(~ nako.sat)
          ..abet
        =^  next=(unit nako)  nako.sat  ~(get to nako.sat)
        ?~  next
          ..abet(done &)
        =.  ..abet  =>((apply-foreign-update u.next) ?>(?=(~ need.sat) .))
        =.  ..foreign-update  =<(?>(?=(^ ref) .) wake)  ::  [wake] >
        $
      ::  This used to be what always removed an item from `need`.  Now,
      ::  we remove in +take-backfill, but in the meantime we could have
      ::  received the next data from elsewhere (such as another desk
      ::  updating).  Additionally, this is needed for backward
      ::  compatibility with old /backfill wires.
      ::
      =/  =lobe
        ?@  i.need.sat  i.need.sat
        lobe.i.need.sat
      ?:  (~(has by lat.ran) lobe)
        $(need.sat t.need.sat)
      ::  otherwise, fetch the next blob (aka fell)
      ::
      =^  scry=(unit [@ta @da path])  ..foreign-update
        =<  ?>(?=(^ ref) .)
        ::  if we know a revision & path for the blob,
        ::  and :ship's remote scry isn't known to be broken,
        ::  or we learned it was broken more than an hour ago,
        ::
        ?:  ?&  ?=(^ i.need.sat)
            ?|  !(~(has by sad) her)
                (gth now (add scry-retry-time (~(got by sad) her)))
            ==  ==
          ::  make the request over remote scry
          ::
          =/  =mood  [%q uv+tako path]:i.need.sat
          =<  [`[%back-index -] +]
          (send-over-scry %back-index hen her inx syd mood)
        ::  otherwise, request over ames
        ::
        :-  ~
        =/  =wire  (request-wire %back-index her syd inx)
        =/  =path  [%backfill syd (scot %ud inx) ~]
        ::  TODO: upgrade to %1 when most ships have upgaded
        =/  =fill  [%0 syd lobe]
        (emit hen %pass wire %a %plea her %c path fill)
      ..abet(busy.sat ?~(scry `%ames scry))
    ::
    ::  When we get a %w foreign update, store this in our state.
    ::
    ::  We get the commits from the nako and add them to our object
    ::  store, then we update the map of aeons to commits and the latest
    ::  aeon.
    ::
    ::  [wake] <
    ::
    ++  apply-foreign-update
      |=  =nako
      ^+  ..abet
      ::  hit: updated commit-hashes by @ud case
      ::  nut: new commit-hash/commit pairs
      ::  hut: updated commits by hash
      ::
      =/  hit  (~(uni by hit.dom) gar.nako)
      =/  nut  (turn ~(tap in lar.nako) |=(=yaki [r.yaki yaki]))
      =/  hut  (~(uni by (malt nut)) hut.ran)
      ::  traverse updated state and sanity check
      ::
      =+  ~|  :*  %bad-foreign-update
                  [gar=gar.nako let=let.nako nut=(turn nut head)]
                  [hitdom=hit.dom letdom=let.dom]
              ==
        ?:  =(0 let.nako)
          ~
        =/  =aeon  1
        |-  ^-  ~
        =/  =tako
          ~|  [%missing-aeon aeon]  (~(got by hit) aeon)
        =/  =yaki
          ~|  [%missing-tako tako]  (~(got by hut) tako)
        ?:  =(let.nako aeon)
          ~
        $(aeon +(aeon))
      ::  produce updated state
      ::
      =/  =rave  rave:(~(got by bom.u.ref) inx)
      ?>  ?=(%many -.rave)
      ::  [ergo] We do not call +ergo here, but if we wanted to support
      ::  keeping a foreign mounted desk up-to-date, this would open
      ::  that invariant.
      ::
      ::  [goad] Same for +goad -- if we supported running agents off
      ::  foreign desks at an up-to-date revision, we would need to call
      ::  +goad here.
      ::
      =:  let.dom   (max let.nako let.dom)              ::  [wake] < +work
          hit.dom   hit
          hut.ran   hut
          ::  Is this correct?  Seeems like it should only go to `to` if
          ::  we've gotten all the way to the end.  Leaving this
          ::  behavior unchanged for now, but I believe it's wrong.
          ::
          lim       ?.(?=(%da -.to.moat.rave) lim p.to.moat.rave)
        ==
      ..abet
    --
  ::
  ++  seek
    |=  =cash
    ^+  ..park
    ?>  ?=(^ ref)
    =/  =tako
      ?:  ?=(%tako -.cash)
        p.cash
      (aeon-to-tako:ze (need (case-to-aeon cash)))
    =/  =yaki  (tako-to-yaki:ze tako)
    =/  lobes=(list lobe)
      %+  murn  ~(tap by q.yaki)
      |=  [=path =lobe]
      ?:  (~(has by lat.ran) lobe)
        ~
      `lobe
    %-  emil
    %+  turn  lobes
    |=  =lobe
    ::  TODO: upgrade to %1 when most ships have upgaded
    ::
    =/  =fill  [%0 syd lobe]
    =/  =wire  /seek/(scot %p her)/[syd]
    =/  =path  [%backfill syd ~]
    [hen %pass wire %a %plea her %c path fill]
  ::
  ++  take-fell
    |=  =fell
    ^+  ..park
    ?>  ?=(^ ref)
    =/  peg=(unit page)  (fell-to-page fell)
    =?  lat.ran  ?=(^ peg)
      (~(uni by (malt [(page-to-lobe u.peg) u.peg] ~)) lat.ran)
    ..park
  ::
  ::  fire function if request is in future
  ::
  ++  run-if-future
    |=  [rov=rove fun=$-(@da _.)]
    ^+  +>.$
    =/  date=(unit @da)
      ?-    -.rov
          %sing
        ?.  ?=(%da -.case.mood.rov)  ~
        `p.case.mood.rov
      ::
          %next  ~
          %mult  ~
          %many
        %^  hunt  lth
          ?.  ?=(%da -.from.moat.rov)    ~
          ?.  (lth now p.from.moat.rov)  ~
          [~ p.from.moat.rov]
        ?.  ?=(%da -.to.moat.rov)  ~
        `(max now p.to.moat.rov)
      ==
    ?~  date
      +>.$
    (fun u.date)
  ::
  ++  send-cards
    |=  [cards=(list card) ducts=(set duct)]
    ^+  ..park
    %-  emil
    %-  zing
    %+  turn  cards
    |=  =card
    %+  turn  ~(tap by ducts)
    |=  =duct
    [duct card]
  ::
  ::  Loop through open subscriptions and check if we can fill any of
  ::  them.
  ::
  ::  [wake] This must be called any time something might have changed
  ::  which fills a subscription or changes the set of subscriptions.
  ::
  ::  It is safe to call this multiple times, because it updates the
  ::  subscription state to reflect that it's responded.  Usually this
  ::  means deleting the subscription, but %many can respond multiple
  ::  times.
  ::
  ::  One way of describing this invariant is that if you called +wake
  ::  on every desk at the end of every +call/+take, it would always
  ::  no-op.
  ::
  ++  wake
    ^+  .
    =/  subs=(list [=wove ducts=(set duct)])  ~(tap by qyx)
    =|  qux=cult
    |-  ^+  ..wake
    ?~  subs
      ..wake(qyx qux)
    ?:  =(~ ducts.i.subs)
      $(subs t.subs)
    =^  [new-sub=(unit rove) cards=(list card)]  ..park
      (try-fill-sub wove.i.subs)
    =.  ..wake  (send-cards cards ducts.i.subs)
    =?  qux  ?=(^ new-sub)
      =/  =wove  [for.wove.i.subs u.new-sub]
      %+  ~(put by qux)  wove
      (~(uni in ducts.i.subs) (~(get ju qux) wove))
    $(subs t.subs)
  ::
  ::  Try to fill a subscription
  ::
  ++  try-fill-sub
    |=  [far=(unit [=ship ver=@ud]) rov=rove]
    ^-  [[(unit rove) (list card)] _..park]
    =/  for=(unit ship)  ?~(far ~ `ship.u.far)
    ?-    -.rov
        %sing
      =/  cache-value=(unit (unit cage))
        ?~(ref ~ (~(get by haw.u.ref) mood.rov))
      ?^  cache-value
        ::  if we have a result in our cache, produce it
        ::
        :_  ..park  :-  ~  :_  ~
        (writ ?~(u.cache-value ~ `[mood.rov u.u.cache-value]))
      ::  else, check to see if rove is for an aeon we know
      ::
      =/  tako=(unit tako)  (case-to-tako case.mood.rov)
      ?~  tako
        [[`rov ~] ..park]
      ::  we have the appropriate tako, so read in the data
      ::
      =^  value=(unit (unit cage))  ..park
        (read-at-tako:ze for u.tako mood.rov)
      ?~  value
        ::  we don't have the data directly.  how can we fetch it?
        ::
        ?:  =(0v0 u.tako)
          ~&  [%clay-sing-indirect-data-0 `path`[syd '0' path.mood.rov]]
          [[~ ~] ..park]
        ~&  [%clay-sing-indirect-data desk=syd mood=mood.rov tako=u.tako]
        [[`rov ~] ..park]
      ::  we have the data, so produce the results
      ::
      :_  ..park  :-  ~  :_  ~
      %-  writ
      ?~  u.value
        ~
      `[mood.rov u.u.value]
    ::
    ::  %next is just %mult with one path, so we pretend %next = %mult here.
    ::
        ?(%next %mult)
      ?.  ?=(~ for)
      ::  reject if foreign (doesn't work over the network)
      ::
        [[~ ~] ..park]
      ::  because %mult requests need to wait on multiple files for each
      ::  revision that needs to be checked for changes, we keep two
      ::  cache maps.  {old} is the revision at {(dec aeon)}, {new} is
      ::  the revision at {aeon}.  if we have no {aeon} yet, that means
      ::  it was still unknown last time we checked.
      ::
      =*  vor  rov
      |^
      =/  rov=rove
        ?:  ?=(%mult -.vor)  vor
        :*  %mult
            [case [[care path] ~ ~]]:mood.vor
            aeon.vor
            [[[care.mood.vor path.mood.vor] cach.vor] ~ ~]
            ~
        ==
      ?>  ?=(%mult -.rov)
      ::  recurse here on next aeon if possible/needed.
      ::
      |-
      ::  if we don't have an aeon yet, see if we have one now.
      ::
      ?~  aeon.rov
        =/  aeon=(unit aeon)  (case-to-aeon case.mool.rov)
        ::  if we still don't, wait.
        ::
        ?~  aeon  [(store rov) ..park]
        ::  if we do, update the request and retry.
        ::
        $(aeon.rov `+(u.aeon), old-cach.rov ~, new-cach.rov ~)
      ::  if old isn't complete, try filling in the gaps.
      ::
      =^  o  ..park
        ?:  (complete old-cach.rov)
          [old-cach.rov ..park]
        (read-unknown mool.rov(case [%ud (dec u.aeon.rov)]) old-cach.rov)
      =.  old-cach.rov  o
      ::  if the next aeon we want to compare is in the future, wait again.
      ::
      =/  next-aeon=(unit aeon)  (case-to-aeon [%ud u.aeon.rov])
      ?~  next-aeon  [(store rov) ..park]
      ::  if new isn't complete, try filling in the gaps.
      ::
      =^  n  ..park
        ?:  (complete new-cach.rov)
          [new-cach.rov ..park]
        (read-unknown mool.rov(case [%ud u.aeon.rov]) new-cach.rov)
      =.  new-cach.rov  n
      ::  if new still isn't complete, wait again.
      ::
      ?.  (complete new-cach.rov)
        [(store rov) ..park]
      ::  if old not complete, give a result (possible false positive).
      ::
      ?:  !(complete old-cach.rov)
        :_  ..park
        %-  respond
        %-  malt
        %+  murn  ~(tap in paths.mool.rov)
        |=  [=care =path]
        ^-  (unit [mood (unit cage)])
        =/  cached  (~(get by new-cach.rov) [care path])
        ?.  ?=([~ ~ *] cached)
          %-  (slog 'clay: strange new-cache' >[care path cached]< ~)
          ~
        `u=[[care [%ud let.dom] path] u.u.cached]
      ::  both complete, so check if anything has changed
      ::
      =/  changes=(map mood (unit cage))
        %+  roll  ~(tap by old-cach.rov)
        |=  $:  [[car=care pax=path] old-cach=cach]
                changes=(map mood (unit cage))
            ==
        =/  new-cach=cach  (~(got by new-cach.rov) car pax)
        ?<  |(?=(~ old-cach) ?=(~ new-cach))
        =/  new-entry=(unit (pair mood (unit cage)))
          =/  =mood  [car [%ud u.aeon.rov] pax]
          ?~  u.new-cach
            ::  if new does not exist, always notify
            ::
            `[mood ~]
          ?~  u.old-cach
            ::  added
            ::
            `[mood `u.u.new-cach]
          ?:  =([p q.q]:u.u.new-cach [p q.q]:u.u.old-cach)
            ::  unchanged
            ::
            ~
          ::  changed
          ::
          `[mood `u.u.new-cach]
        ::  if changed, save the change
        ::
        ?~  new-entry
          changes
        (~(put by changes) u.new-entry)
      ::  if there are any changes, send response. if none, move on to
      ::  next aeon.
      ::
      ?^  changes  [(respond changes) ..park]
      $(u.aeon.rov +(u.aeon.rov), new-cach.rov ~)
      ::
      ::  check again later
      ::
      ++  store
        |=  rov=rove
        ^-  [(unit rove) (list card)]
        =/  new-rove=rove
          ?>  ?=(%mult -.rov)
          ?:  ?=(%mult -.vor)  rov
          ?>  ?=([* ~ ~] old-cach.rov)
          =*  one  n.old-cach.rov
          [%next [care.p.one case.mool.rov path.p.one] aeon.rov q.one]
        [`new-rove ~]
      ::
      ::  send changes
      ::
      ++  respond
        |=  res=(map mood (unit cage))
        ^-  [(unit rove) (list card)]
        :-  ~
        ?:  ?=(%mult -.vor)
          :_  ~
          =/  moods  ~(key by res)
          =/  cas
            ?>  ?=(^ moods)
            [%da (case-to-date case.n.moods)]
          =/  res
            (~(run in moods) |=(m=mood [care.m path.m]))
          =/  gift  [%wris cas res]
          ?:  ?=(^ ref)
            [%pass /drip %b %drip !>(gift)]  :: XX s/b [%behn %wris ...] in $sign?
          [%give gift]
        ?>  ?=([* ~ ~] res)
        :_  ~
        %-  writ
        ?~  q.n.res
          ~
        `[p u.q]:n.res
      ::
      ::  no unknowns
      ::
      ++  complete
        |=  hav=(map (pair care path) cach)
        ?&  !=(~ hav)
            (levy ~(tap by hav) know)
        ==
      ::
      ::  know about file in cach
      ::
      ++  know  |=([(pair care path) c=cach] ?=(^ c))
      ::
      ::  fill in the blanks
      ::
      ++  read-unknown
        |=  [=mool hav=(map (pair care path) cach)]
        ^-  [_hav _..park]
        =?  hav  ?=(~ hav)
          %-  malt  ^-  (list (pair (pair care path) cach))
          %+  turn
            ~(tap in paths.mool)
          |=  [c=care p=path]
          ^-  [[care path] cach]
          [[c p] ~]
        |-  ^+  [hav ..park]
        ?~  hav  [hav ..park]
        =^  lef  ..park  $(hav l.hav)
        =.  l.hav  lef
        =^  rig  ..park  $(hav r.hav)
        =.  r.hav  rig
        =/  [[=care =path] =cach]  n.hav
        ?^  cach
          [hav ..park]
        =^  q  ..park  (aver for care case.mool path)
        =.  q.n.hav  q
        [hav ..park]
      --
    ::
        %many
      :_  ..park
      ?.  |(?=(~ for) (allowed-by:ze u.for path.moat.rov per.red))
        [~ ~]
      =/  from-aeon  (case-to-aeon from.moat.rov)
      ?~  from-aeon
        ::  haven't entered the relevant range, so do nothing
        ::
        [`rov ~]
      =/  to-aeon  (case-to-aeon to.moat.rov)
      ::  TODO: shouldn't skip if tracking
      ::
      =/  up-to  ?~(to-aeon let.dom u.to-aeon)
      =/  ver  ?~(far %1 ver.u.far)
      =.  from.moat.rov  [%ud +(let.dom)]
      =/  =card
        =/  =cage
          ?:  track.rov
            [%null [%atom %n ~] ~]
          [%nako !>((make-nako:ze ver u.from-aeon up-to))]
        (writ ~ [%w ud+let.dom /] cage)
      ?~  to-aeon
        ::  we're in the middle of the range, so produce what we can,
        ::  but don't end the subscription
        ::
        [`rov card ~]
      ::  we're past the end of the range, so end subscription
      ::
      [~ [card (writ ~) ~]]
    ==
  ::
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ::
  ::  This core has no additional state, and the distinction exists purely for
  ::  documentation.  The overarching theme is that `++de` directly contains
  ::  logic for metadata about the desk, while `++ze` is composed primarily
  ::  of helper functions for manipulating the desk state (`++dome`) itself.
  ::  Functions include:
  ::
  ::  --  converting between cases, commit hashes, commits, content hashes,
  ::      and content
  ::  --  creating commits and content and adding them to the tree
  ::  --  finding which data needs to be sent over the network to keep the
  ::      other urbit up-to-date
  ::  --  reading from the file tree through different `++care` options
  ::  --  the `++me` core for merging.
  ::
  ::  The dome is composed of the following:
  ::
  ::  --  `let` is the number of the most recent revision.
  ::  --  `hit` is a map of revision numbers to commit hashes.
  ::  --  `lab` is a map of labels to revision numbers.
  ::
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ::
  ::
  ::  Other utility functions
  ::
  ++  ze
    |%
    ::  These convert between aeon (version number), tako (commit hash),
    ::  and yaki (commit data structure)
    ::
    ++  aeon-to-tako  |=(=aeon ?:(=(0 aeon) 0v0 (~(got by hit.dom) aeon)))
    ++  aeon-to-yaki  |=(=aeon (tako-to-yaki (aeon-to-tako aeon)))
    ++  tako-to-yaki  ~(got by hut.ran)
    ::
    ++  tako-to-aeon
      |=  tak=tako
      ^-  aeon  ~+
      ?:  =(0v0 tak)  0
      =/  a=aeon  1
      |-
      ?:  (gth a let.dom)  ~|([%tako-mia tak] !!)
      ?:  (~(has in (reachable-takos (~(got by hit.dom) a))) tak)  a
      $(a +(a))
    ::
    ::  Creates a nako of all the changes between a and b.
    ::
    ++  make-nako
      |=  [ver=@ud a=aeon b=aeon]
      ^-  nako
      :+  ?>  (lte b let.dom)
          |-
          ?:  =(b let.dom)
            hit.dom
          ::  del everything after b
          $(hit.dom (~(del by hit.dom) let.dom), let.dom (dec let.dom))
        b
      ?:  =(0 b)
        [~ ~]
      =/  excludes=(set tako)
          =|  acc=(set tako)
          =/  lower=@ud  1
          |-
          ::  a should be excluded, so wait until we're past it
          ?:  (gte lower +(a))
            acc
          =/  res=(set tako)  (reachable-takos (~(got by hit.dom) lower))
          $(acc (~(uni in acc) res), lower +(lower))
      =/  includes=(set tako)
          =|  acc=(set tako)
          =/  upper=@ud  b
          |-
          ?:  (lte upper a)
            acc
          =/  res=(set tako)  (reachable-takos (~(got by hit.dom) upper))
          $(acc (~(uni in acc) res), upper (dec upper))
      [(~(run in (~(dif in includes) excludes)) tako-to-yaki) ~]
    ::  Traverse parentage and find all ancestor hashes
    ::
    ++  reachable-takos                                 ::  reachable
      |=  p=tako
      ^-  (set tako)
      ~+
      =|  s=(set tako)
      |-  ^-  (set tako)
      =.  s  (~(put in s) p)
      =+  y=(tako-to-yaki p)
      |-  ^-  (set tako)
      ?~  p.y
        s
      ?:  (~(has in s) i.p.y)
        $(p.y t.p.y)
      =.  s  ^$(p i.p.y)
      $(p.y t.p.y)
    ::
    ++  read-a
      !.
      |=  [=tako =path]
      ^-  [(unit (unit cage)) _..park]
      =^  =vase  ..park
        ~_  leaf/"clay: %a build failed {<[syd tako path]>}"
        %+  tako-flow  tako
        %-  wrap:fusion
        (build-file:(tako-ford tako) path)
      :_(..park [~ ~ %vase !>(vase)])
    ::
    ++  read-b
      !.
      |=  [=tako =path]
      ^-  [(unit (unit cage)) _..park]
      ?.  ?=([@ ~] path)
        [[~ ~] ..park]
      =^  =dais  ..park
        %+  tako-flow  tako
        %-  wrap:fusion
        (build-dais:(tako-ford tako) i.path)
      :_(..park [~ ~ %dais !>(dais)])
    ::
    ++  read-c
      !.
      |=  [=tako =path]
      ^-  [(unit (unit cage)) _..park]
      ?.  ?=([@ @ ~] path)
        [[~ ~] ..park]
      =^  =tube  ..park
        %+  tako-flow  tako
        %-  wrap:fusion
        (build-tube:(tako-ford tako) [i i.t]:path)
      :_(..park [~ ~ %tube !>(tube)])
    ::
    ++  read-e
      !.
      |=  [=tako =path]
      ^-  [(unit (unit cage)) _..park]
      ?.  ?=([@ ~] path)
        [[~ ~] ..park]
      =^  =vase  ..park
        %+  tako-flow  tako
        %-  wrap:fusion
        (build-nave:(tako-ford tako) i.path)
      :_(..park [~ ~ %nave vase])
    ::
    ++  read-f
      !.
      |=  [=tako =path]
      ^-  [(unit (unit cage)) _..park]
      ?.  ?=([@ @ ~] path)
        [[~ ~] ..park]
      =^  =vase  ..park
        %+  tako-flow  tako
        %-  wrap:fusion
        (build-cast:(tako-ford tako) [i i.t]:path)
      :_(..park [~ ~ %cast vase])
    ::
    ::  TODO move to +read-buc
    ::
    ++  read-d
      !.
      |=  [=tako =path]
      ^-  (unit (unit cage))
      ~&  [%clay %d-on-desk-deprecated desk=syd %use-empty-desk]
      ?.  =(our her)
        [~ ~]
      ?^  path
        ~&(%no-cd-path [~ ~])
      [~ ~ %noun !>(~(key by dos.rom.ruf))]
    ::
    ::  Gets the permissions that apply to a particular node.
    ::
    ::  If the node has no permissions of its own, we use its parent's.
    ::  If no permissions have been set for the entire tree above the node,
    ::  we default to fully private (empty whitelist).
    ::
    ++  read-p
      |=  pax=path
      ^-  (unit (unit cage))
      =-  [~ ~ %noun !>(-)]
      :-  (read-p-in pax per.red)
      (read-p-in pax pew.red)
    ::
    ++  read-p-in
      |=  [pax=path pes=regs]
      ^-  dict
      =/  rul=(unit rule)  (~(get by pes) pax)
      ?^  rul
        :+  pax  mod.u.rul
        %-  ~(rep in who.u.rul)
        |=  [w=whom out=(pair (set ship) (map @ta crew))]
        ?:  ?=([%& @p] w)
          [(~(put in p.out) +.w) q.out]
        =/  cru=(unit crew)  (~(get by cez.ruf) +.w)
        ?~  cru  out
        [p.out (~(put by q.out) +.w u.cru)]
      ?~  pax  [/ %white ~ ~]
      $(pax (scag (dec (lent pax)) `path`pax))
    ::
    ++  may-read
      |=  [who=ship car=care tak=tako pax=path]
      ^-  ?
      ?+  car
        (allowed-by who pax per.red)
      ::
          %p
        =(who our)
      ::
          ?(%y %z)
        =+  yak=(tako-to-yaki tak)
        =+  len=(lent pax)
        =-  (levy ~(tap in -) |=(p=path (allowed-by who p per.red)))
        %+  roll  ~(tap in (~(del in ~(key by q.yak)) pax))
        |=  [p=path s=(set path)]
        ?.  =(pax (scag len p))  s
        %-  ~(put in s)
        ?:  ?=(%z car)  p
        (scag +(len) p)
      ==
    ::
    ++  may-write
      |=  [w=ship p=path]
      (allowed-by w p pew.red)
    ::
    ++  allowed-by
      |=  [who=ship pax=path pes=regs]
      ^-  ?
      =/  rul=real  rul:(read-p-in pax pes)
      =/  in-list/?
        ?|  (~(has in p.who.rul) who)
          ::
            %-  ~(rep by q.who.rul)
            |=  [[@ta cru=crew] out=_|]
            ?:  out  &
            (~(has in cru) who)
        ==
      ?:  =(%black mod.rul)
        !in-list
      in-list
    ::  +content-hash: get hash of contents (%cz hash)
    ::
    ++  content-hash
      |=  [=yaki pax=path]
      ^-  @uvI
      =+  len=(lent pax)
      =/  descendants=(list (pair path lobe))
          %+  turn
            %+  skim  ~(tap by (~(del by q.yaki) pax))
            |=  [paf=path lob=lobe]
            =(pax (scag len paf))
          |=  [paf=path lob=lobe]
          [(slag len paf) lob]
      =+  us=(~(get by q.yaki) pax)
      ?:  &(?=(~ descendants) ?=(~ us))
        *@uvI
      %+  roll
        ^-  (list (pair path lobe))
        [[~ ?~(us *lobe u.us)] descendants]
      |=([[path lobe] @uvI] (shax (jam +<)))
    ::  +read-q: typeless %x
    ::
    ::  useful if the marks can't be built (eg for old marks built
    ::  against an incompatible standard library).  also useful if you
    ::  don't need the type (eg for remote scry) because it's faster.
    ::
    ++  read-q
      |=  [tak=tako pax=path]
      ^-  (unit (unit cage))
      ?:  =(0v0 tak)
        [~ ~]
      =+  yak=(tako-to-yaki tak)
      =+  lob=(~(get by q.yak) pax)
      ?~  lob
        [~ ~]
      =/  peg=(unit page)  (~(get by lat.ran) u.lob)
      ::  if tombstoned, nothing to return
      ::
      ?~  peg
        ~
      ``[p.u.peg %noun q.u.peg]
    ::  +read-r: %x wrapped in a vase
    ::
    ++  read-r
      |=  [tak=tako pax=path]
      ^-  [(unit (unit cage)) _..park]
      =^  x  ..park  (read-x tak pax)
      :_  ..park
      ?~  x    ~
      ?~  u.x  [~ ~]
      ``[p.u.u.x !>(q.u.u.x)]
    ::  +read-s: produce miscellaneous
    ::
    ++  read-s
      |=  [tak=tako pax=path =case]
      ^-  (unit (unit cage))
      ?:  ?=([%subs ~] pax)
        ?.  =([%da now] case)  ~
        =|  sus=(set ship)
        =/  doj=(unit dojo)  (~(get by dos.rom) syd)
        ?~  doj
          ``noun+!>(sus)
        =/  wos  ~(tap in ~(key by qyx.u.doj))
        |-
        ?~  wos
          ``noun+!>(sus)
        ?~  for.i.wos
          $(wos t.wos)
        %=  $
          wos  t.wos
          sus  (~(put in sus) ship.u.for.i.wos)
        ==
      ?:  ?=([%bloc ~] pax)
        :^  ~  ~  %noun
        :-  -:!>(*(map lobe page))
        ^-  (map lobe page)
        %-  %~  rep  in
            |-  ^-  (set tako)
            =/  ts=(set tako)
              %-  reachable-takos
              (~(got by hit.dom) let.dom)
            ?:  (lte let.dom 1)  ts
            (~(uni in ts) $(let.dom (dec let.dom)))
        |=  [t=tako o=(map lobe page)]
        %-  ~(gas by o)
        %+  turn
          ~(val by q:(~(got by hut.ran) t))
        |=(l=lobe [l (~(got by lat.ran) l)])
      ?.  ?=([@ * *] pax)
        `~
      ?+    i.pax  `~
          %tako
        ``tako+[-:!>(*tako) tak]
      ::
          %yaki
        =/  yak=(unit yaki)  (~(get by hut.ran) (slav %uv i.t.pax))
        ?~  yak
          ~
        ``yaki+[-:!>(*yaki) u.yak]
      ::
          %blob
        =/  peg=(unit page)  (~(get by lat.ran) (slav %uv i.t.pax))
        ?~  peg
          ~
        ``blob+[-:!>(*page) u.peg]
      ::
          %hash
        =/  yak=(unit yaki)  (~(get by hut.ran) (slav %uv i.t.pax))
        ?~  yak
          ~
        ``uvi+[-:!>(*@uvI) (content-hash u.yak /)]
      ::
          %cage
        ::  should save ford cache
        ::
        =/  =lobe  (slav %uv i.t.pax)
        =/  peg=(unit page)  (~(get by lat.ran) lobe)
        ?~  peg
          ~
        =/  [=cage *]
          %-  wrap:fusion
          (page-to-cage:(tako-ford tak) u.peg)
        ``cage+[-:!>(*^cage) cage]
      ::
          %open  ``open+!>(prelude:(tako-ford tak))
          %late  !!  :: handled in +aver
          %case  !!  :: handled in +aver
          %base-tako
        ::  TODO this ignores the given beak
        ::  maybe move to +aver?
        ?>  ?=(^ t.t.pax)
        :^  ~  ~  %uvs  !>
        ^-  (list @uv)
        =/  tako-a  (slav %uv i.t.pax)
        =/  tako-b  (slav %uv i.t.t.pax)
        =/  yaki-a  (~(got by hut.ran) tako-a)
        =/  yaki-b  (~(got by hut.ran) tako-b)
        %+  turn    ~(tap in (find-merge-points yaki-a yaki-b))
        |=  =yaki
        r.yaki
      ::
          %base
        ?>  ?=(^ t.t.pax)
        :^  ~  ~  %uvs  !>
        ^-  (list @uv)
        =/  him  (slav %p i.t.pax)
        =/  other  dom:((de now rof hen ruf) him i.t.t.pax)
        ?:  =(0 let.other)
          ~
        =/  our-yaki  (~(got by hut.ran) tak)
        =/  other-yaki  (~(got by hut.ran) (~(got by hit.other) let.other))
        %+  turn  ~(tap in (find-merge-points other-yaki our-yaki))
        |=  =yaki
        r.yaki
      ==
    ::  +read-t: produce the list of paths within a yaki with :pax as prefix
    ::
    ++  read-t
      |=  [tak=tako pax=path]
      ^-  (unit (unit [%file-list (hypo (list path))]))
      ::  if asked for version 0, produce an empty list of files
      ::
      ?:  =(0v0 tak)
        ``[%file-list -:!>(*(list path)) *(list path)]
      ::  look up the yaki snapshot based on the version
      ::
      =/  yak=yaki  (tako-to-yaki tak)
      ::  calculate the path length once outside the loop
      ::
      =/  path-length  (lent pax)
      ::
      :^  ~  ~  %file-list
      :-  -:!>(*(list path))
      ^-  (list path)
      ::  sort the matching paths alphabetically
      ::
      =-  (sort - aor)
      ::  traverse the filesystem, filtering for paths with :pax as prefix
      ::
      %+  skim  ~(tap in ~(key by q.yak))
      |=(paf=path =(pax (scag path-length paf)))
    ::
    ::  Checks for existence of a node at an aeon.
    ::
    ::  This checks for existence of content at the node, and does *not* look
    ::  at any of its children.
    ::
    ++  read-u
      |=  [tak=tako pax=path]
      ^-  (unit (unit [%flag (hypo ?)]))
      ::  if asked for version 0, that never exists, so always give false
      ::
      ?:  =(0v0 tak)
        ``[%flag -:!>(*?) |]
      ::  look up the yaki snapshot based on the version
      ::
      =/  yak=yaki  (tako-to-yaki tak)
      ::  produce the result based on whether or not there's a file at :pax
      ::
      ``[%flag -:!>(*?) (~(has by q.yak) pax)]
    ::
    ::  Gets the dome (desk state) at a particular aeon.
    ::
    ++  read-v
      |=  [tak=tako pax=path]
      ^-  (unit (unit [%dome (hypo domo:clay)]))
      =/  yon=aeon  (tako-to-aeon:ze tak)
      ?:  (lth yon let.dom)
        :*  ~  ~  %dome  -:!>(*domo)
            ^-  domo
            :*  let=yon
                hit=(molt (skim ~(tap by hit.dom) |=([p=@ud *] (lte p yon))))
                lab=(molt (skim ~(tap by lab.dom) |=([* p=@ud] (lte p yon))))
        ==  ==
      ?:  (gth yon let.dom)
        ~
      ``[%dome -:!>(*domo) [let hit lab]:dom]
    ::
    ::  Gets all cases refering to the same revision as the given case.
    ::
    ::  For the %da case, we give just the canonical timestamp of the revision.
    ::
    ++  read-w
      |=  tak=tako
      ^-  (unit (unit cage))
      =-  [~ ~ %cass !>(-)]
      ^-  cass  ::TODO  should include %uv case
      :-  (tako-to-aeon tak)
      ?:  =(0v0 tak)  `@da`0
      t:(tako-to-yaki tak)
    ::
    ::  Get the data at a node.
    ::
    ::  Use ford to read the file.  Note this special-cases the hoon
    ::  mark for bootstrapping purposes.
    ::
    ++  read-x
      |=  [tak=tako pax=path]
      ^-  [(unit (unit cage)) _..park]
      =/  q  (read-q tak pax)
      ?~  q    `..park
      ?~  u.q  [[~ ~] ..park]
      ::  should convert any lobe to cage
      ::
      =^  =cage  ..park
        %+  tako-flow  tak
        %-  wrap:fusion
        (page-to-cage:(tako-ford tak) p.u.u.q q.q.u.u.q)
      [``cage ..park]
    ::
    ::  Gets an arch (directory listing) at a node.
    ::
    ++  read-y
      |=  [tak=tako pax=path]
      ^-  (unit (unit [%arch (hypo arch)]))
      ?:  =(0v0 tak)
        ``[%arch -:!>(*arch) *arch]
      =+  yak=(tako-to-yaki tak)
      =+  len=(lent pax)
      :^  ~  ~  %arch
      ::  ~&  cy+pax
      :-  -:!>(*arch)
      ^-  arch
      :-  (~(get by q.yak) pax)
      ^-  (map knot ~)
      %-  molt  ^-  (list (pair knot ~))
      %+  turn
        ^-  (list (pair path lobe))
        %+  skim  ~(tap by (~(del by q.yak) pax))
        |=  [paf=path lob=lobe]
        =(pax (scag len paf))
      |=  [paf=path lob=lobe]
      =+  pat=(slag len paf)
      [?>(?=(^ pat) i.pat) ~]
    ::
    ::  Gets a recursive hash of a node and all its children.
    ::
    ++  read-z
      |=  [tak=tako pax=path]
      ^-  (unit (unit [%uvi (hypo @uvI)]))
      ?:  =(0v0 tak)
        ``uvi+[-:!>(*@uvI) *@uvI]
      [~ ~ %uvi [%atom %'uvI' ~] (content-hash (tako-to-yaki tak) pax)]
    ::
    ::  Get a value at an aeon.
    ::
    ::  Value can be either null, meaning we don't have it yet, [null null],
    ::  meaning we know it doesn't exist, or [null null cage],
    ::  meaning we either have the value directly or a content hash of the
    ::  value.
    ::
    ++  read-at-tako                                    ::    read-at-tako:ze
      |=  [for=(unit ship) tak=tako mun=mood]           ::  seek and read
      ^-  [(unit (unit cage)) _..park]
      ::  non-zero commits must be known, and reachable from within this desk
      ::
      ?.  ?|  =(0v0 tak)
          ?&  (~(has by hut.ran) tak)
              ?|  (~(any by hit.dom) |=(=tako =(tak tako)))  ::  fast-path
                  |-  ^-  ?
                  ?:  (lte let.dom 1)
                    %.n
                  ?|  (~(has in (reachable-takos (aeon-to-tako:ze let.dom))) tak)
                      $(let.dom (dec let.dom))
                  ==
              ==
              |(?=(~ for) (may-read u.for care.mun tak path.mun))
          ==  ==
        [~ ..park]
      ::  virtualize to catch and produce deterministic failures
      ::
      |^  =/  res  (mule |.(read))
          ?:  ?=(%& -.res)  p.res
          %.  [[~ ~] ..park]
          (slog leaf+"clay: read-at-tako fail {<[desk=syd mun]>}" p.res)
      ::
      ++  read
        ^-  [(unit (unit cage)) _..park]
        ?-  care.mun
          %a  (read-a tak path.mun)
          %b  (read-b tak path.mun)
          %c  (read-c tak path.mun)
          %d  [(read-d tak path.mun) ..park]
          %e  (read-e tak path.mun)
          %f  (read-f tak path.mun)
          %p  [(read-p path.mun) ..park]
          %q  [(read-q tak path.mun) ..park]
          %r  (read-r tak path.mun)
          %s  [(read-s tak path.mun case.mun) ..park]
          %t  [(read-t tak path.mun) ..park]
          %u  [(read-u tak path.mun) ..park]
          %v  [(read-v tak path.mun) ..park]
          %w  [(read-w tak) ..park]
          %x  (read-x tak path.mun)
          %y  [(read-y tak path.mun) ..park]
          %z  [(read-z tak path.mun) ..park]
        ==
      --
    --
  --
::  userspace agent management
::
++  lu
  |=  [now=@da rof=roof hen=duct raft]
  =*  ruf  |3.+<.$
  =|  mow=(list move)
  |%
  ++  abet
    ^-  [(list move) raft]
    [(flop mow) ruf]
  ::
  ++  emit
    |=  mof=move
    %_(+> mow [mof mow])
  ::
  ++  emil
    |=  mof=(list move)
    %_(+> mow (weld (flop mof) mow))
  ::  +ford: init ford
  ::
  ++  ford
    |=  [her=ship syd=desk yon=(unit aeon)]
    =/  den  ((de now rof hen ruf) her syd)
    %-  tako-ford:den
    ::TODO  is this +got after +got semantically correct?
    (~(got by hit.dom:(~(got by dos.rom) syd)) ?~(yon let.dom:den u.yon))
  ::  +wrap: save ford cache
  ::
  ++  wrap
    |*  [her=ship syd=desk yon=(unit aeon) res=* =state:ford:fusion]
    =^  moves  ruf
      =/  den  ((de now rof hen ruf) her syd)
      =/  tak  (aeon-to-tako:ze:den ?~(yon let.dom:den u.yon))
      abet:+:(tako-flow:den tak res cache.state &2.state)
    [res (emil moves)]
  ::
  ++  trace
    |=  [pri=@ print=(trap tape)]
    ?:  (lth veb.bug pri)
      same
    (slog leaf+"goad: {(print)}" ~)
  ::  +goad: emit %load move for all desks, applying $rein's
  ::
  ::  [goad] Must be called any time the set of running agents changes.
  ::  This is whenever an agent is started, stopped, or updated.
  ::
  ::  This is not move-order agnostic -- you must be careful of
  ::  reentrancy as long as arvo's move order is depth-first.
  ::
  ::  [tare] >
  ::
  ++  goad
    ^+  ..abet
    =^  sat=(list [=desk =bill])  ..abet
      =/  desks=(list desk)  ~(tap in ~(key by dos.rom))
      |-  ^-  [(list [desk bill]) _..abet]
      ?~  desks
        [~ ..abet]
      =/  den  ((de now rof hen ruf) our i.desks)
      ?.  =(%live liv.dom.den)
        %-  (trace 2 |.("{<i.desks>} is not live"))
        $(desks t.desks)
      =^  res  den  (aver:den ~ %x da+now /desk/bill)
      =.  ruf  +:abet:den
      ?.  ?=([~ ~ *] res)
        $(desks t.desks)
      =/  bill  ~|  [%building-bill i.desks]  !<(bill q.u.u.res)
      =/  rid  (override bill ren.dom.den)
      %-  %+  trace  2  |.
          "{<i.desks>} has bill {<bill>} and rein {<ren.dom.den>}, so {<rid>}"
      =^  sats  ..abet  $(desks t.desks)
      [[[i.desks rid] sats] ..abet]
    ::
    =.  sat  (apply-precedence sat)
    =+  ?:  (lth veb.bug 1)  ~
        %.  ~  %-  slog
        %+  turn  sat
        |=  [=desk =bill]
        leaf+"goad: output: {<desk>}: {<bill>}"
    =^  agents  ..abet  (build-agents sat)
    ::  TODO: enable if we can reduce memory usage
    ::
    ::  =.  ..abet
    ::    (build-marks (turn (skip sat |=([desk =bill] =(bill ~))) head))
    ::
    =.  ..abet  tare                                    ::  [tare] >
    (emit hen %pass /lu/load %g %load agents)
  ::  +override: apply rein to bill
  ::
  ++  override
    |=  [duz=bill ren=(map dude:gall ?)]
    ^-  bill
    =.  duz
      %+  skip  duz
      |=  =dude:gall
      =(`| (~(get by ren) dude))
    ::
    =/  dus  (sy duz)
    =.  duz
      %+  weld  duz
      %+  murn  ~(tap by ren)
      |=  [=dude:gall on=?]
      ?:  &(?=(%& on) !(~(has in dus) dude))
        `u=dude
      ~
    duz
  ::  +apply-precedence: resolve conflicts between $bill's
  ::
  ::    policy is to crash if multiple desks are trying to run the same
  ::    agent.
  ::
  ++  apply-precedence
    |=  sat=(list [=desk =bill])
    ^+  sat
    ::  sort desks in alphabetical order with %base first
    ::
    =.  sat  (sort sat sort-desks)
    ::  for each desk
    ::
    =|  done=(set dude:gall)
    |-  ^+  sat
    ?~  sat
      ~
    ::  for each agent
    ::
    =/  bil  bill.i.sat
    =^  this  done
      |-  ^-  [bill (set dude:gall)]
      ?~  bil
        [~ done]
      ::
      ?:  (~(has in done) i.bil)
        ~>  %mean.(cat 3 'clay: cannot run app from two desks: %' i.bil)
        !!
      =.  done  (~(put in done) i.bil)
      =^  next  done  $(bil t.bil)
      [[i.bil next] done]
    [[desk.i.sat this] $(sat t.sat)]
  ::
  ++  sort-desks
    |=  [a=[=desk *] b=[=desk *]]
    ^-  ?
    ?:  =(%base desk.a)  &
    ?:  =(%base desk.b)  |
    (aor desk.a desk.b)
  ::  build-file for each dude
  ::
  ++  build-agents
    |=  sat=(list [=desk =bill])
    ^-  [load:gall _..abet]
    =|  lad=load:gall
    |-  ^-  [load:gall _..abet]
    ?~  sat
      [lad ..abet]
    =/  f  (ford our desk.i.sat ~)
    =^  new=load:gall  ..abet
      %-  wrap  :^  our  desk.i.sat  ~
      |-  ^-  [load:gall state:ford:fusion]
      ?~  bill.i.sat
        [~ nub.f]
      =^  =vase  nub.f  (build-file:f /app/[i.bill.i.sat]/hoon)
      =/  agent  ~|  [%building-app bill.i.sat]  !<(agent:gall vase)
      =^  lid  nub.f  $(bill.i.sat t.bill.i.sat)
      [[[i.bill.i.sat [our desk.i.sat da+now] agent] lid] nub.f]
    =.  lad  (weld lad new)
    $(sat t.sat)
  ::  build-dais for each mark
  ::
  ++  build-marks
    |=  desks=(list desk)
    ^+  ..abet
    ?~  desks
      ..abet
    =/  f  (ford our i.desks ~)
    =^  null  ..abet
      %-  wrap  :^  our  i.desks  ~
      =^  marks=(list mark)  nub.f
        =/  pax=path  /
        |-  ^-  [(list mark) _nub.f]
        =/  den  ((de now rof hen ruf) our i.desks)
        =^  res  den  (aver:den ~ %y da+now mar+pax)
        ?.  ?=([~ ~ *] res)
          [~ nub.f]
        =/  arch  ~|  [%building-arch i.desks]  !<(arch q.u.u.res)
        =/  m1=(list mark)
          ?.  ?&  ?=(^ fil.arch)
                  ?=(^ pax)
                  =(/hoon (slag (dec (lent pax)) `path`pax))
              ==
            ~
          :_  ~
          ?~  t.pax
            ''
          |-  ^-  mark
          ?~  t.t.pax
            i.pax
          (rap 3 i.pax '-' $(pax t.pax) ~)
        ::
        =^  m2  nub.f
          |-  ^-  [(list mark) _nub.f]
          ?~  dir.arch
            [~ nub.f]
          =^  n1  nub.f  ^$(pax (weld pax /[p.n.dir.arch]))
          =^  n2  nub.f  $(dir.arch l.dir.arch)
          =^  n3  nub.f  $(dir.arch r.dir.arch)
          [:(weld n1 n2 n3) nub.f]
        [(weld m1 m2) nub.f]
      ::
      |-  ^-  [~ state:ford:fusion]
      ?~  marks
        [~ nub.f]
      =^  =dais  nub.f  (build-dais:f i.marks)
      $(marks t.marks)
    $(desks t.desks)
  ::
  ++  tore
    ^-  rock:tire
    %-  ~(run by dos.rom)
    |=  =dojo
    [liv.dom.dojo ~(key by wic.dom.dojo)]
  ::
  ::  [tare] Must be called any time the zest or commits-in-waiting
  ::  might have changed for a desk.  +goad calls this uncondtionally,
  ::  but if you're not calling +goad, you may need to call this.
  ::
  ++  tare
    ?:  =(~ tyr)
      ..abet
    =/  tor  tore
    =/  waves=(list wave:tire)  (walk:tire tur tor)
    ?~  waves
      ..abet
    =.  tur  tor
    %-  emil
    %-  zing
    %+  turn  ~(tap in tyr)
    |=  =duct
    ^-  (list move)
    %+  turn  waves
    |=  =wave:tire
    ^-  move
    [duct %give %tire %| wave]
  --
--
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::              section 4cA, filesystem vane
::
::  This is the arvo interface vane.  Our formal state is a `++raft`, which
::  has five components:
::
::  --  `rom` is the state for all local desks.
::  --  `hoy` is the state for all foreign desks.
::  --  `ran` is the global, hash-addressed object store.
::  --  `mon` is the set of mount points in unix.
::  --  `hez` is the duct to the unix sync.
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
=|                                                    ::  instrument state
    $:  ver=%14                                       ::  vane version
        ruf=raft                                      ::  revision tree
    ==                                                ::
|=  [now=@da eny=@uvJ rof=roof]                       ::  current invocation
~%  %clay-top  ..part  ~
|%                                                    ::
++  call                                              ::  handle request
  ~/  %clay-call
  |=  $:  hen=duct
          dud=(unit goof)
          wrapped-task=(hobo task)
      ==
  ^-  [(list move) _..^$]
  ::
  =/  req=task  ((harden task) wrapped-task)
  ::
  ::  TODO handle error notifications
  ::
  ?^  dud
    [[[hen %slip %d %flog %crud [-.req tang.u.dud]] ~] ..^$]
  ::
  ?-    -.req
      %boat
    :_  ..^$
    [hen %give %hill (turn ~(tap by mon.ruf) head)]~
  ::
      %cred
    =.  cez.ruf
      ?~  cew.req  (~(del by cez.ruf) nom.req)
      (~(put by cez.ruf) nom.req cew.req)
    ::  wake all desks, a request may have been affected.
    =|  mos=(list move)
    =/  des  ~(tap in ~(key by dos.rom.ruf))
    |-
    ?~  des  [[[hen %give %done ~] mos] ..^^$]
    =/  den  ((de now rof hen ruf) our i.des)
    =^  mor  ruf
      =<  abet:wake                                   ::  [wake] >
      ?:  ?=(^ cew.req)  den
      (forget-crew:den nom.req)
    $(des t.des, mos (weld mos mor))
  ::
      %crew
    [[hen %give %cruz cez.ruf]~ ..^$]
  ::
      %crow
    =/  des  ~(tap by dos.rom.ruf)
    =|  rus=(map desk [r=regs w=regs])
    |^
      ?~  des  [[hen %give %croz rus]~ ..^^$]
      =+  per=(filter-rules per.q.i.des)
      =+  pew=(filter-rules pew.q.i.des)
      =?  rus  |(?=(^ per) ?=(^ pew))
        (~(put by rus) p.i.des per pew)
      $(des t.des)
    ::
    ++  filter-rules
      |=  pes=regs
      ^+  pes
      =-  (~(gas in *regs) -)
      %+  skim  ~(tap by pes)
      |=  [p=path r=rule]
      (~(has in who.r) |+nom.req)
    --
  ::
      %drop
    ~&  %clay-idle
    [~ ..^$]
  ::
      %info
    ?:  ?=(%| -.dit.req)
      =/  bel=@tas         p.dit.req
      =/  aey=(unit aeon)  q.dit.req
      =^  mos  ruf
        =/  den  ((de now rof hen ruf) our des.req)
        abet:(label:den bel aey)
      [mos ..^$]
    =/  [deletes=(set path) changes=(map path cage)]
      =/  =soba  p.dit.req
      =|  deletes=(set path)
      =|  changes=(map path cage)
      |-  ^+  [deletes changes]
      ?~  soba
        [deletes changes]
      ?-  -.q.i.soba
        %del  $(soba t.soba, deletes (~(put in deletes) p.i.soba))
        %ins  $(soba t.soba, changes (~(put by changes) [p p.q]:i.soba))
        %mut  $(soba t.soba, changes (~(put by changes) [p p.q]:i.soba))
        %dif  ~|(%dif-not-implemented !!)
      ==
    =^  mos  ruf
      =/  den  ((de now rof hen ruf) our des.req)
      abet:(info:den deletes changes)
    [mos ..^$]
  ::
      %init
    [~ ..^$(hun.rom.ruf hen)]
  ::
      %into
    =.  hez.ruf  `hen
    =+  bem=(~(get by mon.ruf) des.req)
    ?:  &(?=(~ bem) !=(%$ des.req))
      ~|([%bad-mount-point-from-unix des.req] !!)
    =/  bem=beam
        ?^  bem
          u.bem
        [[our %base %ud 1] ~]  ::  TODO: remove this fallback?
    =/  dos  (~(get by dos.rom.ruf) q.bem)
    ?~  dos
      !!  ::  fire next in queue
    =^  mos  ruf
      =/  den  ((de now rof hen ruf) our q.bem)
      abet:(into:den s.bem all.req fis.req)
    [mos ..^$]
  ::
      %merg                                               ::  direct state up
    ?:  =(%$ des.req)
      ~|(%merg-no-desk !!)
    ?.  ((sane %tas) des.req)
      ~|([%merg-bad-desk-name des.req] !!)
    =^  mos  ruf
      =/  den  ((de now rof hen ruf) our des.req)
      abet:(start-merge:den her.req dem.req cas.req how.req)
    [mos ..^$]
  ::
      %fuse
    ?:  =(%$ des.req)
      ~|(%fuse-no-desk !!)
    ?.  ((sane %tas) des.req)
      ~|([%fuse-bad-desk-name des.req] !!)
    =^  mos  ruf
      =/  den  ((de now rof hen ruf) our des.req)
      abet:(start-fuse:den bas.req con.req)
    [mos ..^$]
  ::
      %mont
    =.  hez.ruf  ?^(hez.ruf hez.ruf `[[%$ %sync ~] ~])
    =^  mos  ruf
      =/  den  ((de now rof hen ruf) p.bem.req q.bem.req)
      abet:(mount:den pot.req r.bem.req s.bem.req)
    [mos ..^$]
  ::
      %dirk
    ?~  hez.ruf
      ~&  %no-sync-duct
      [~ ..^$]
    ?.  (~(has by mon.ruf) pot.req)
      ~&  [%not-mounted pot.req]
      [~ ..^$]
    [~[[u.hez.ruf %give %dirk pot.req]] ..^$]
  ::
      %ogre
    ?:  =(~ hez.ruf)
      ~&  %no-sync-duct
      [~ ..^$]
    =*  pot  pot.req
    =/  bem=(list [pot=term beam])
      ?@  pot
        ?~  got=(~(get by mon.ruf) pot)
          ~&  [%not-mounted pot]
          ~
        [pot u.got]~
      %+  skim  ~(tap by mon.ruf)
      |=  [=term =beam]
      =(pot beam)
    |-  ^-  [(list move) _..^^$]
    ?~  bem
      [~ ..^^$]
    =^  moves-1  ruf
      =/  den  ((de now rof hen ruf) p.i.bem q.i.bem)
      abet:(unmount:den pot.i.bem r.i.bem s.i.bem)
    =^  moves-2  ..^^$  $(bem t.bem)
    [(weld moves-1 moves-2) ..^^$]
  ::
      %park
    ?.  ((sane %tas) des.req)
      ~|([%park-bad-desk des.req] !!)
    =^  mos  ruf
      =/  den  ((de now rof hen ruf) our des.req)
      abet:(park:den | & [yok ran]:req)
    [mos ..^$]
  ::
      %pork
    =/  [syd=desk =yoki]  (need pud.ruf)
    =.  pud.ruf  ~
    =^  mos  ruf
      =/  den  ((de now rof hen ruf) our syd)
      abet:(park:den & & yoki *rang)
    [mos ..^$]
  ::
      %prep
    [~ ..^$(lat.ran.ruf (~(uni by lat.req) lat.ran.ruf))]
  ::
      %perm
    =^  mos  ruf
      =/  den  ((de now rof hen ruf) our des.req)
      abet:(perm:den pax.req rit.req)
    [mos ..^$]
  ::
      %rein
    =^  m1  ruf
      =/  den  ((de now rof hen ruf) our des.req)
      abet:(set-rein:den ren.req)
    =^  m2  ruf  abet:goad:(lu now rof hen ruf)         ::  [goad] >
    [(weld m1 m2) ..^$]
  ::
      %stir
    ?+    arg.req  ~|(%strange-stir !!)
        [%verb @]  [~ ..^$(veb.bug.ruf +.arg.req)]
        [%mass @]  [~ ..^$(mas.bug.ruf +.arg.req)]
        [%goad ~]
      =^  mos  ruf  abet:goad:(lu now rof hen ruf)
      [mos ..^$]
    ::
        [%rise =desk =dude:gall on=(unit ?)]
      =^  m1  ruf
        =/  den  ((de now rof hen ruf) our desk.arg.req)
        abet:(rise:den dude.arg.req on.arg.req)
      =^  m2  ruf  abet:goad:(lu now rof hen ruf)       ::  [goad] <
      [(weld m1 m2) ..^$]
    ::
        [%stay =desk ver=(unit weft)]
      =^  moves  ruf
        =/  den  ((de now rof hen ruf) our desk.arg.req)
        abet:(stay:den ver.arg.req)
      [moves ..^$]
    ::
        [%trim ~]
      =:    fad.ruf      *flow
            dos.rom.ruf
          %-  ~(run by dos.rom.ruf)
          |=  =dojo
          dojo(fod.dom *flue)
        ::
            hoy.ruf
          %-  ~(run by hoy.ruf)
          |=  =rung
          %=    rung
              rus
            %-  ~(run by rus.rung)
            |=  =rede
            rede(fod.dom *flue)
          ==
        ==
      [~ ..^$]
    ::
        [%fine ~]
      ~&  "clay: resetting fine state.  old:"
      ~&  sad.ruf
      `..^$(sad.ruf ~)
    ==
  ::
      %tire
    ?~  p.req
      =.  tyr.ruf  (~(del in tyr.ruf) hen)
      `..^$
    =.  tyr.ruf  (~(put in tyr.ruf) hen)
    :_  ..^$
    [hen %give %tire %& tore:(lu now rof hen ruf)]~
  ::
      %tomb  (tomb-clue:tomb hen clue.req)
      %trim  [~ ..^$]
      %vega
    ::  wake all desks, then send pending notifications
    ::
    =^  wake-moves  ..^$
      =/  desks=(list [=ship =desk])
        %+  welp
          (turn ~(tap by dos.rom.ruf) |=([=desk *] [our desk]))
        %-  zing
        %+  turn  ~(tap by hoy.ruf)
        |=  [=ship =rung]
        %+  turn  ~(tap by rus.rung)
        |=  [=desk *]
        [ship desk]
      |-  ^+  [*(list move) ..^^$]
      ?~  desks
        [~ ..^^$]
      =^  moves-1  ..^^$  $(desks t.desks)
      =^  moves-2  ruf  abet:wake:((de now rof hen ruf) [ship desk]:i.desks)
      [(weld moves-1 moves-2) ..^^$]
    [wake-moves ..^$]
  ::
      ?(%warp %werp)
    ::  capture whether this read is on behalf of another ship
    ::  for permissions enforcement
    ::
    =^  for  req
      ?:  ?=(%warp -.req)
        [~ req]
      ::  ?:  =(our who.req)
      ::    [~ [%warp wer.req rif.req]]
      :-  ?:(=(our who.req) ~ `[who.req -.rif.req])
      [%warp wer.req riff.rif.req]
    ::
    ?>  ?=(%warp -.req)
    =*  rif  rif.req
    =^  mos  ruf
      =/  den  ((de now rof hen ruf) wer.req p.rif)
      =<  abet
      ?~  q.rif
        cancel-request:den
      (start-request:den for u.q.rif)
    [mos ..^$]
  ::
      %wick
    =^  mos  ruf
      abet:wick:((de now rof hen ruf) our %base)        ::  [wick]
    [mos ..^$]
  ::
      %zeal
    =^  m1  ruf
      =|  mos=(list move)
      |-  ^+  [mos ruf]
      ?~  lit.req
        [mos ruf]
      =/  den  ((de now rof hen ruf) our desk.i.lit.req)
      =^  mos-new  ruf  abet:(set-zest:den zest.i.lit.req)
      $(mos (weld mos mos-new), lit.req t.lit.req)
    =^  m2  ruf
      abet:wick:((de now rof hen ruf) our %base)
    =^  m3  ruf  abet:goad:(lu now rof hen ruf)
    [:(weld m1 m2 m3) ..^$]
  ::
      %zest
    =^  m1  ruf
      =/  den  ((de now rof hen ruf) our des.req)
      ::  [wick] could be suspending the last blocking desk
      ::
      abet:wick:(set-zest:den liv.req)
    =^  m2  ruf  abet:goad:(lu now rof hen ruf)
    [(weld m1 m2) ..^$]
  ::
      %plea
    =*  her  ship.req
    =*  pax  path.plea.req
    =*  res  payload.plea.req
    ::
    ?:  ?=([%backfill *] pax)
      =+  ;;(=fill res)
      =^  mos  ruf
        =/  den  ((de now rof hen ruf) our desk.fill)
        abet:(give-backfill:den -.fill lobe.fill)
      [[[hen %give %done ~] mos] ..^$]
    ?>  ?=([%question *] pax)
    =+  ryf=;;(riff-any res)
    :_  ..^$
    :~  [hen %give %done ~]
        =/  =wire
          [%foreign-warp (scot %p her) t.pax]
        [hen %pass wire %c %werp her our ryf]
    ==
  ==
::
++  load
  =>  |%
      +$  raft-any
        $%  [%14 raft-14]
            [%13 raft-13]
            [%12 raft-12]
            [%11 raft-11]
            [%10 raft-10]
            [%9 raft-9]
            [%8 raft-8]
            [%7 raft-7]
            [%6 raft-6]
        ==
      ::  We redefine the latest raft with * for the the ford caches.
      ::  +clear-cache upgrades to +raft
      ::
      +$  raft-14
        $+  raft-14
        $:  rom=room-13
            hoy=(map ship rung-14)
            ran=rang
            fad=*
            mon=(map term beam)
            hez=(unit duct)
            cez=(map @ta crew)
            tyr=(set duct)
            tur=rock:tire
            pud=(unit [=desk =yoki])
            sad=(map ship @da)
            bug=[veb=@ mas=@]
        ==
      +$  rung-14
        $:  rus=(map desk rede-14)
        ==
      +$  rede-14
        $:  lim=@da
            ref=(unit rind-14)
            qyx=cult
            dom=dome-13
            per=regs
            pew=regs
            fiz=melt
        ==
      +$  rind-14
        $:  nix=@ud
            bom=(map @ud update-state)
            fod=(map duct @ud)
            haw=(map mood (unit cage))
        ==
      ::
      +$  raft-13
        $+  raft-13
        $:  rom=room-13
            hoy=(map ship rung-13)
            ran=rang
            fad=*
            mon=(map term beam)
            hez=(unit duct)
            cez=(map @ta crew)
            tyr=(set duct)
            tur=rock:tire
            pud=(unit [=desk =yoki])
            bug=[veb=@ mas=@]
        ==
      +$  room-13
        $:  hun=duct
            dos=(map desk dojo-13)
        ==
      +$  dojo-13
        $:  qyx=cult
            dom=dome-13
            per=regs
            pew=regs
            fiz=melt
        ==
      +$  dome-13
        $:  let=aeon
            hit=(map aeon tako)
            lab=(map @tas aeon)
            tom=(map tako norm)
            nor=norm
            mim=(map path mime)
            fod=*
            wic=(map weft yoki)
            liv=zest
            ren=rein
        ==
      +$  rung-13
        $:  rus=(map desk rede-13)
        ==
      +$  rede-13
        $:  lim=@da
            ref=(unit rind-11)
            qyx=cult
            dom=dome-13
            per=regs
            pew=regs
            fiz=melt
        ==
      ::
      +$  raft-12
        $+  raft-12
        $:  rom=room-11
            hoy=(map ship rung-11)
            ran=rang
            fad=*
            mon=(map term beam)
            hez=(unit duct)
            cez=(map @ta crew)
            pud=(unit [=desk =yoki])
            bug=[veb=@ mas=@]
        ==
      +$  raft-11
        $+  raft-11
        $:  rom=room-11
            hoy=(map ship rung-11)
            ran=rang
            fad=*
            mon=(map term beam)
            hez=(unit duct)
            cez=(map @ta crew)
            pud=(unit [=desk =yoki])
        ==
      +$  room-11
        $+  room-11
        $:  hun=duct
            dos=(map desk dojo-11)
        ==
      +$  dojo-11
        $+  dojo-11
        $:  qyx=cult
            dom=dome-11
            per=regs
            pew=regs
            fiz=melt
        ==
      +$  dome-11
        $+  dome-11
        $:  let=aeon
            hit=(map aeon tako)
            lab=(map @tas aeon)
            tom=(map tako norm)
            nor=norm
            mim=(map path mime)
            fod=*
        ==
      +$  rung-11
        $+  rung-11
        $:  rus=(map desk rede-11)
        ==
      +$  rede-11
        $+  rede-11
        $:  lim=@da
            ref=(unit rind-11)
            qyx=cult
            dom=dome-11
            per=regs
            pew=regs
            fiz=melt
        ==
      +$  rind-11
        $+  rind-11
        $:  nix=@ud
            bom=(map @ud update-state-11)
            fod=(map duct @ud)
            haw=(map mood (unit cage))
        ==
      +$  update-state-11
        $+  update-state-11
        $:  =duct
            =rave
            need=(list lobe)
            nako=(qeu (unit nako))
            busy=_|
        ==
      +$  raft-10
        $+  raft-10
        $:  rom=room-10
            hoy=(map ship rung-10)
            ran=rang-10
            mon=(map term beam)
            hez=(unit duct)
            cez=(map @ta crew)
            pud=(unit [=desk =yoki])
            dist-upgraded=_|
        ==
      +$  rang-10
        $:  hut=(map tako yaki)
            lat=(map lobe blob-10)
        ==
      +$  blob-10
        $%  [%delta p=lobe q=[p=mark q=lobe] r=page]
            [%direct p=lobe q=page]
            [%dead p=lobe ~]
        ==
      +$  room-10
        $:  hun=duct
            dos=(map desk dojo-10)
        ==
      +$  dojo-10
        $:  qyx=cult-10
            dom=dome-10
            per=regs
            pew=regs
            fiz=melt-10
        ==
      +$  dome-10
        $:  ank=ankh-10
            let=aeon
            hit=(map aeon tako)
            lab=(map @tas aeon)
            mim=(map path mime)
            fod=*
        ==
      +$  ankh-10  (axal [p=lobe q=cage])
      +$  rung-10
        $:  rus=(map desk rede-10)
        ==
      +$  rede-10
        $:  lim=@da
            ref=(unit rind-10)
            qyx=cult-10
            dom=dome-10
            per=regs
            pew=regs
            fiz=melt-10
        ==
      +$  rind-10
        $:  nix=@ud
            bom=(map @ud update-state-10)
            fod=(map duct @ud)
            haw=(map mood (unit cage))
        ==
      +$  update-state-10
        $:  =duct
            =rave
            have=(map lobe blob-10)
            need=(list lobe)
            nako=(qeu (unit nako-10))
            busy=_|
        ==
      +$  nako-10
        $:  gar=(map aeon tako)
            let=aeon
            lar=(set yaki)
            bar=(set blob-10)
        ==
      +$  melt-10
        [bas=beak con=(list [beak germ]) sto=(map beak (unit dome-clay-10))]
      +$  dome-clay-10
        $:  ank=ankh-10
            let=@ud
            hit=(map @ud tako)
            lab=(map @tas @ud)
        ==
      +$  cult-10  (jug wove-10 duct)
      +$  wove-10  [for=(unit [=ship ver=@ud]) =rove-10]
      +$  rove-10
        $%  [%sing =mood]
            [%next =mood aeon=(unit aeon) =cach-10]
            $:  %mult
                =mool
                aeon=(unit aeon)
                old-cach=(map [=care =path] cach-10)
                new-cach=(map [=care =path] cach-10)
            ==
            [%many track=? =moat lobes=(map path lobe)]
        ==
      +$  cach-10  (unit (unit (each cage lobe)))
      +$  raft-9
        $+  raft-9
        $:  rom=room-10
            hoy=(map ship rung-10)
            ran=rang-10
            mon=(map term beam)
            hez=(unit duct)
            cez=(map @ta crew)
            pud=(unit [=desk =yoki])
        ==
      +$  raft-8
        $+  raft-8
        $:  rom=room-8
            hoy=(map ship rung-8)
            ran=rang-10
            mon=(map term beam)
            hez=(unit duct)
            cez=(map @ta crew)
            pud=(unit [=desk =yoki])
        ==
      +$  room-8
        $:  hun=duct
            dos=(map desk dojo-8)
        ==
      +$  rung-8
        $:  rus=(map desk rede-8)
        ==
      +$  dojo-8
        $:  qyx=cult-10
            dom=dome-8
            per=regs
            pew=regs
            fiz=melt-10
        ==
      +$  dome-8
        $:  ank=ankh-10
            let=aeon
            hit=(map aeon tako)
            lab=(map @tas aeon)
            mim=(map path mime)
            fod=*
            fer=*  ::  reef cache, obsolete
        ==
      +$  rede-8
        $:  lim=@da
            ref=(unit rind-10)
            qyx=cult-10
            dom=dome-8
            per=regs
            pew=regs
            fiz=melt-10
        ==
      +$  raft-7
        $+  raft-7
        $:  rom=room-7
            hoy=(map ship rung-7)
            ran=rang-10
            mon=(map term beam)
            hez=(unit duct)
            cez=(map @ta crew)
            pud=(unit [=desk =yoki])
        ==
      +$  room-7
        $:  hun=duct
            dos=(map desk dojo-7)
        ==
      +$  rung-7
        $:  rus=(map desk rede-7)
        ==
      +$  dojo-7
        $:  qyx=cult-10
            dom=dome-8
            per=regs
            pew=regs
        ==
      +$  rede-7
        $:  lim=@da
            ref=(unit rind-10)
            qyx=cult-10
            dom=dome-8
            per=regs
            pew=regs
        ==
      +$  raft-6
        $+  raft-6
        $:  rom=room-6
            hoy=(map ship rung-6)
            ran=rang-10
            mon=(map term beam)
            hez=(unit duct)
            cez=(map @ta crew)
            pud=(unit [=desk =yoki])
        ==
      +$  room-6  [hun=duct dos=(map desk dojo-6)]
      +$  dojo-6
        $:  qyx=cult-10
            dom=dome-6
            per=regs
            pew=regs
        ==
      +$  dome-6
        $:  ank=ankh-10
            let=aeon
            hit=(map aeon tako)
            lab=(map @tas aeon)
            mim=(map path mime)
            fod=*
            fer=*
        ==
      +$  rung-6
        $:  rus=(map desk rede-6)
        ==
      +$  rede-6
        $:  lim=@da
            ref=(unit rind-10)
            qyx=cult-10
            dom=dome-6
            per=regs
            pew=regs
        ==
      --
  |=  old=raft-any
  |^
  =?  old  ?=(%6 -.old)   7+(raft-6-to-7 +.old)
  =?  old  ?=(%7 -.old)   8+(raft-7-to-8 +.old)
  =?  old  ?=(%8 -.old)   9+(raft-8-to-9 +.old)
  =?  old  ?=(%9 -.old)   10+(raft-9-to-10 +.old)
  =?  old  ?=(%10 -.old)  11+(raft-10-to-11 +.old)
  =?  old  ?=(%11 -.old)  12+(raft-11-to-12 +.old)
  =?  old  ?=(%12 -.old)  13+(raft-12-to-13 +.old)
  =?  old  ?=(%13 -.old)  14+(raft-13-to-14 +.old)
  ?>  ?=(%14 -.old)
  ..^^$(ruf (clear-cache +.old))
  ::
  ::  We clear the ford cache so we don't have to know how to upgrade
  ::  the types, which are complicated and eg contravariant in +hoon.
  ::  Also, many of the results would be different if zuse is different.
  ::
  ++  clear-cache
    |=  raf=raft-14
    ^-  raft
    %=    raf
        fad  *flow
        dos.rom
      %-  ~(run by dos.rom.raf)
      |=  doj=dojo-13
      ^-  dojo
      doj(fod.dom *flue)
    ::
        hoy
      %-  ~(run by hoy.raf)
      |=  =rung-14
      %-  ~(run by rus.rung-14)
      |=  =rede-14
      ^-  rede
      rede-14(dom dom.rede-14(fod *flue))
    ==
  ::  +raft-6-to-7: delete stale ford caches (they could all be invalid)
  ::
  ++  raft-6-to-7
    |=  raf=raft-6
    ^-  raft-7
    %=    raf
        dos.rom
      %-  ~(run by dos.rom.raf)
      |=  doj=dojo-6
      ^-  dojo-7
      doj(fod.dom **)
    ::
        hoy
      %-  ~(run by hoy.raf)
      |=  =rung-6
      %-  ~(run by rus.rung-6)
      |=  =rede-6
      rede-6(dom dom.rede-6(fod **))
    ==
  ::  +raft-7-to-8: create bunted melts in each dojo/rede
  ::
  ++  raft-7-to-8
    |=  raf=raft-7
    ^-  raft-8
    %=    raf
        dos.rom
      %-  ~(run by dos.rom.raf)
      |=  doj=dojo-7
      ^-  dojo-8
      [qyx.doj dom.doj per.doj pew.doj *melt-10]
    ::
        hoy
      %-  ~(run by hoy.raf)
      |=  =rung-7
      %-  ~(run by rus.rung-7)
      |=  r=rede-7
      ^-  rede-8
      [lim.r ref.r qyx.r dom.r per.r pew.r *melt-10]
    ==
  ::  +raft-8-to-9: remove reef cache
  ::
  ++  raft-8-to-9
    |=  raf=raft-8
    ^-  raft-9
    %=    raf
        dos.rom
      %-  ~(run by dos.rom.raf)
      |=  =dojo-8
      ^-  dojo-10
      =/  dom  dom.dojo-8
      dojo-8(dom [ank.dom let.dom hit.dom lab.dom mim.dom *flow])
    ::
        hoy
      %-  ~(run by hoy.raf)
      |=  =rung-8
      %-  ~(run by rus.rung-8)
      |=  =rede-8
      ^-  rede-10
      =/  dom  dom.rede-8
      rede-8(dom [ank.dom let.dom hit.dom lab.dom mim.dom *flow])
    ==
  ::  +raft-9-to-10: add .dist-upgraded
  ::
  ++  raft-9-to-10
    |=  raf=raft-9
    ^-  raft-10
    raf(pud [pud.raf dist-upgraded=|])
  ::
  ::  +raft-10-to-11:
  ::
  ::    add tom and nor to dome
  ::    remove parent-mark from delta blobs
  ::    change blobs to pages
  ::    remove have from update-state
  ::    remove bar from nako
  ::    remove ankh
  ::    set cases in mon to ud+0
  ::    add fad
  ::    change fod type in dom
  ::    change bom type in dom
  ::
  ++  raft-10-to-11
    |=  raf=raft-10
    |^
    ^-  raft-11
    %=    raf
        dos.rom
      %-  ~(run by dos.rom.raf)
      |=  =dojo-10
      ^-  dojo-11
      %=    dojo-10
          fiz  *melt
          qyx  (cult-10-to-cult qyx.dojo-10)
          dom
        :*  let.dom.dojo-10
            hit.dom.dojo-10
            lab.dom.dojo-10
            ~
            *norm
            mim.dom.dojo-10
            [~ ~]
        ==
      ==
    ::
        hoy
      %-  ~(run by hoy.raf)
      |=  =rung-10
      %-  ~(run by rus.rung-10)
      |=  =rede-10
      ^-  rede-11
      %=    rede-10
          fiz     *melt
          qyx     (cult-10-to-cult qyx.rede-10)
          dom
        :*  let.dom.rede-10
            hit.dom.rede-10
            lab.dom.rede-10
            ~
            *norm
            mim.dom.rede-10
            [~ ~]
        ==
      ::
          ref
        ?~  ref.rede-10
          ~
        %=    ref.rede-10
            bom.u
          %-  ~(run by bom.u.ref.rede-10)
          |=  =update-state-10
          ^-  update-state-11
          %=    update-state-10
              |2
            ^-  [(list lobe) (qeu (unit nako)) _|]
            %=    |3.update-state-10
                nako
              %-  ~(gas to *(qeu (unit nako)))
              %+  turn  ~(tap to nako.update-state-10)
              |=  nak=(unit nako-10)
              ?~  nak  ~
              `u.nak(bar ~)
            ==
          ==
        ==
      ==
    ::
        lat.ran
      %-  ~(gas by *(map lobe page))
      %+  murn  ~(tap by lat.ran.raf)
      |=  [=lobe =blob-10]
      ^-  (unit [^lobe page])
      ?-  -.blob-10
        %delta   ((slog 'clay: tombstoning delta!' ~) ~)
        %dead    ~
        %direct  `[lobe q.blob-10]
      ==
    ::
        |3
      ^+  |3:*raft-11
      :-  *flow
      %=  |3.raf
        mon  (~(run by mon.raf) |=(=beam beam(r ud+0)))
        |3   pud.raf
      ==
    ==
    ::
    ++  cult-10-to-cult
      |=  qyx=cult-10
      ^-  cult
      =/  qux=(list [=wove-10 ducts=(set duct)])  ~(tap by qyx)
      %-  malt
      |-  ^-  (list [wove (set duct)])
      ?~  qux
        ~
      :_  $(qux t.qux)
      %=    i.qux
          rove-10.wove-10
        ?-    -.rove-10.wove-10.i.qux
            %sing  rove-10.wove-10.i.qux
            %many  rove-10.wove-10.i.qux
            %next
          %=  rove-10.wove-10.i.qux
            cach-10  (cach-10-to-cach cach-10.rove-10.wove-10.i.qux)
          ==
        ::
            %mult
          %=  rove-10.wove-10.i.qux
            old-cach  (caches-10-to-caches old-cach.rove-10.wove-10.i.qux)
            new-cach  (caches-10-to-caches new-cach.rove-10.wove-10.i.qux)
          ==
        ==
      ==
    ::
    ++  cach-10-to-cach
      |=  =cach-10
      ^-  cach
      ?~  cach-10
        ~
      ?~  u.cach-10
        [~ ~]
      ?-  -.u.u.cach-10
        %&  ``p.u.u.cach-10
        %|  ~
      ==
    ::
    ++  caches-10-to-caches
      |=  caches-10=(map [=care =path] cach-10)
      ^-  (map [=care =path] cach)
      (~(run by caches-10) cach-10-to-cach)
    --
  ::  +raft-11-to-12: add bug
  ::
  ++  raft-11-to-12
    |=  raf=raft-11
    ^-  raft-12
    raf(pud [pud.raf 0 0])
  ::  +raft-12-to-13:
  ::
  ::    add .liv and .ren to $dome's
  ::    add .tyr and .tur to $raft
  ::
  ++  raft-12-to-13
    |=  raf=raft-12
    |^  ^-  raft-13
    ::  turn on %base desk  ::  TODO handle other desks somehow
    ::                      ::  maybe have kiln send one-time list of desks
    ::
    =;  rof
      rof(dos.rom (~(jab by dos.rom.rof) %base |=(d=dojo-13 d(liv.dom %live))))
    ^-  raft-13
    %=  raf
      dos.rom  (~(run by dos.rom.raf) dojo-11-to-13)
      hoy      (~(run by hoy.raf) rung-11-to-13)
      |6       [&7.raf ~ ~ |7.raf]
    ==
    ::
    ++  dojo-11-to-13
      |=  doj=dojo-11
      ^-  dojo-13
      doj(dom (dome-11-to-13 dom.doj))
    ::
    ++  rung-11-to-13
      |=  rug=rung-11
      ^-  rung-13
      rug(rus (~(run by rus.rug) rede-11-to-13))
    ::
    ++  rede-11-to-13
      |=  red=rede-11
      ^-  rede-13
      red(dom (dome-11-to-13 dom.red))
    ::
    ++  dome-11-to-13
      |=  dom=dome-11
      ^-  dome-13
      dom(fod [fod.dom ~ liv=%dead ren=~])
    --
  ::
  ::  +raft-13-to-14: add sad, change busy
  ::
  ++  raft-13-to-14
    |=  raf=raft-13
    ^-  raft-14
    %=    raf
      bug  [~ bug.raf]
    ::
        hoy
      %-  ~(run by hoy.raf)
      |=  =rung-13
      %-  ~(run by rus.rung-13)
      |=  =rede-13
      ^-  rede-14
      %=    rede-13
          ref
        ?~  ref.rede-13
          ~
        %=    ref.rede-13
            bom.u
          %-  ~(run by bom.u.ref.rede-13)
          |=  update-state-11
          ^-  update-state
          =/  busy  ?:(busy `%ames ~)
          [duct rave ~ need nako busy]
        ==
      ==
    ==
  --
::
++  scry                                              ::  inspect
  ~/  %clay-scry
  ^-  roon
  |=  [lyc=gang pov=path car=term bem=beam]
  ^-  (unit (unit cage))
  =*  scry-loop  $
  |^
  =*  ren  car
  =/  why=shop  &/p.bem
  =*  syd  q.bem
  =/  lot=coin  $/r.bem
  =*  tyl  s.bem
  ::
  ?.  ?=(%& -.why)  ~
  =*  his  p.why
  ::
  ?:  &(?=(%x ren) =(tyl //whey))
    ``mass+!>(whey)
  ::
  ::  ~&  scry+[ren `path`[(scot %p his) syd ~(rent co lot) tyl]]
  ::  =-  ~&  %scry-done  -
  =+  luk=?.(?=(%$ -.lot) ~ ((soft case) p.lot))
  ?~  luk  [~ ~]
  ?:  =(%$ ren)
    [~ ~]
  =+  run=((soft care) ren)
  ?~  run  [~ ~]
  ::TODO  if it ever gets filled properly, pass in the full fur.
  ::
  =/  for=(unit ship)  ?~(lyc ~ ?~(u.lyc ~ `n.u.lyc))
  ?:  &(=(our his) ?=(?(%d %x) ren) =(%$ syd) =([%da now] u.luk))
    ?-  ren
      %d  (read-buc-d tyl)
      %x  (read-buc-x tyl)
    ==
  =/  den  ((de now rof [/scryduct ~] ruf) his syd)
  =/  result  (mule |.(-:(aver:den for u.run u.luk tyl)))
  ?:  ?=(%| -.result)
    %-  (slog >%clay-scry-fail< p.result)
    ~
  p.result
  ::
  ++  read-buc-d
    |=  =path
    ^-  (unit (unit cage))
    ?^  path  ~&(%no-cd-path [~ ~])
    [~ ~ %noun !>(~(key by dos.rom.ruf))]
  ::
  ++  read-buc-x
    |=  =path
    ^-  (unit (unit cage))
    ?~  path
      ~
    ?+    i.path  ~
        %sweep  ``[%sweep !>(sweep)]
        %rang   ``[%rang !>(ran.ruf)]
        %tomb   ``[%flag !>((tomb t.path))]
        %cult   ``[%cult !>((cult t.path))]
        %flow   ``[%flow !>(fad.ruf)]
        %domes  domes
        %tire   ``[%tire !>(tore:(lu now rof *duct ruf))]
        %tyre   ``[%tyre !>(tyr.ruf)]
    ==
  ::
  ++  domes
    =/  domes
      %-  ~(gas by *cone)
      %+  turn  ~(tap by dos.rom.ruf)
      |=  [=desk =dojo]
      [[our desk] dom.dojo]
    =.  domes
      %-  ~(uni by domes)
      %-  ~(gas by *cone)
      ^-  (list [[ship desk] dome])
      %-  zing
      ^-  (list (list [[ship desk] dome]))
      %+  turn  ~(tap by hoy.ruf)
      |=  [=ship =rung]
      ^-  (list [[^ship desk] dome])
      %+  turn  ~(tap by rus.rung)
      |=  [=desk =rede]
      [[ship desk] dom.rede]
    ``[%domes !>(`cone`domes)]
  ::
  ++  cult
    |=  =path
    ^-  (set [@p rave])
    %-  %~  run  in
        %~  key  by
        ?~  path  *^cult
        qyx:(~(gut by dos.rom.ruf) i.path *dojo)
    |=  wove
    :-  ship:(fall for [ship=our @ud])
    ?-  -.rove
      %sing  rove
      %next  [%next mood.rove]
      %mult  [%mult mool.rove]
      %many  [%many [track moat]:rove]
    ==
  ::
  ::  True if file is accessible
  ::
  ++  tomb
    |=  =path
    ^-  ?
    =/  bem  (de-beam path)
    ?~  bem       %|
    =/  cay  scry-loop(car %y, bem u.bem)
    ?~  cay       %|
    ?~  u.cay     %|
    =+  !<(=arch q.u.u.cay)
    ?~  fil.arch  %|
    (~(has by lat.ran.ruf) u.fil.arch)
  ::
  ::  Check for refcount errors
  ::
  ++  sweep
    ^-  (list [need=@ud have=@ud leak])
    =/  marked=(map leak [need=@ud have=@ud])
      (~(run by fad.ruf) |=([refs=@ud *] [0 refs]))
    =.  marked
      =/  items=(list [=leak *])  ~(tap by fad.ruf)
      |-  ^+  marked
      ?~  items
        marked
      =/  deps  ~(tap in deps.leak.i.items)
      |-  ^+  marked
      ?~  deps
        ^$(items t.items)
      =.  marked
        %+  ~(put by marked)  i.deps
        =/  gut  (~(gut by marked) i.deps [0 0])
        [+(-.gut) +.gut]
      $(deps t.deps)
    ::
    =/  spills=(list (set leak))
      %+  welp
        %+  turn  ~(tap by dos.rom.ruf)
        |=  [* =dojo]
        spill.fod.dom.dojo
      %-  zing
      %+  turn  ~(tap by hoy.ruf)
      |=  [* =rung]
      %+  turn  ~(tap by rus.rung)
      |=  [* =rede]
      spill.fod.dom.rede
    ::
    =.  marked
      |-
      ?~  spills
        marked
      =/  leaks  ~(tap in i.spills)
      |-
      ?~  leaks
        ^$(spills t.spills)
      =.  marked
        %+  ~(put by marked)  i.leaks
        =/  gut  (~(gut by marked) i.leaks [0 0])
        [+(-.gut) +.gut]
      $(leaks t.leaks)
    ::
    %+  murn  ~(tap by marked)
    |=  [=leak need=@ud have=@ud]
    ?:  =(need have)
      ~
    `u=[need have leak]
  --
::
::  We clear the ford cache by replacing it with its bunt as a literal,
::  with its singleton type.  This nests within +flow and +flue without
::  reference to +type, +hoon, or anything else in the sample of cache
::  objects.  Otherwise we would be contravariant in those types, which
::  makes them harder to change.
::
++  stay
  ^-  raft-any:load
  =/  flu  [~ ~]
  =+  `flue`flu
  =/  flo  ~
  =+  `flow`flo
  :-  ver
  ^-  raft-14:load
  %=    ruf
      fad  flo
      dos.rom
    %-  ~(run by dos.rom.ruf)
    |=  =dojo
    dojo(fod.dom flu)
  ::
      hoy
    %-  ~(run by hoy.ruf)
    |=  =rung
    %=    rung
        rus
      %-  ~(run by rus.rung)
      |=  =rede
      rede(fod.dom flu)
    ==
  ==
::
++  take                                              ::  accept response
  ~/  %clay-take
  |=  [tea=wire hen=duct dud=(unit goof) hin=sign]
  ^+  [*(list move) ..^$]
  ?^  dud
    ?+    tea
      ~|(%clay-take-dud (mean tang.u.dud))
    ::
        [%drip ~]
      %.  [~ ..^$]
      %-  slog
      ^-  tang
      :*  'clay: drip fail'
          [%rose [": " "" ""] 'bail' mote.u.dud ~]
          tang.u.dud
      ==
    ==
  ::
  ::  pseudo %slip on %drip
  ::
  ?:  ?=([%drip ~] tea)
    ?>  ?=([?(%behn %clay) ?(%writ %wris) *] hin)
    [[`move`[hen %give +.hin] ~] ..^$]
  ::
  ?:  ?=([%lu %load *] tea)
    ?>  ?=(%unto +<.hin)
    ?>  ?=(%poke-ack -.p.hin)
    ?~  p.p.hin
      [~ ..^$]
    =+  ((slog 'clay: reloading agents failed' u.p.p.hin) ~)
    !!
  ::
  ?:  ?=([%merge @ @ @ @ ~] tea)
    ?>  ?=(%writ +<.hin)
    =*  syd  i.t.tea
    =/  ali-ship  (slav %p i.t.t.tea)
    =*  ali-desk  i.t.t.t.tea
    =/  germ  (germ i.t.t.t.t.tea)
    =^  mos  ruf
      =/  den  ((de now rof hen ruf) our syd)
      abet:(merge:den ali-ship ali-desk germ p.hin)
    [mos ..^$]
  ::
  ?:  ?=([%fuse @ @ @ @ ~] tea)
    ?>  ?=(%writ +<.hin)
    =*  syd  i.t.tea
    =/  ali-ship=@p  (slav %p i.t.t.tea)
    =*  ali-desk=desk  i.t.t.t.tea
    =/  ali-case  (rash i.t.t.t.t.tea nuck:so)
    ?>  ?=([%$ *] ali-case)
    =^  mos  ruf
      =/  den  ((de now rof hen ruf) our syd)
      abet:(take-fuse:den [ali-ship ali-desk (case +.ali-case)] p.hin)
    [mos ..^$]
  ::
  ?:  ?=([%park-held @ ~] tea)
    ?>  ?=(%wake +<.hin)
    =*  syd  i.t.tea
    =^  mos  ruf
      =/  den  ((de now rof hen ruf) our syd)
      abet:(take-park-held:den error.hin)
    [mos ..^$]
  ::
  ?:  ?=([%wick ~] tea)
    ?>  ?=(%wake +<.hin)
    =^  mos  ruf
      =/  den  ((de now rof hen ruf) our %base)
      abet:(take-wick:den error.hin)
    [mos ..^$]
  ::
  ?:  ?=([%foreign-warp *] tea)
    ?:  ?=(%wris +<.hin)  ~&  %dropping-wris  `..^$
    ?>  ?=(%writ +<.hin)
    :_  ..^$
    [hen %give %boon `(unit rand)`(bind `riot`p.hin rant-to-rand)]~
  ::
  ?:  ?=([%warp-index @ @ @ ~] tea)
    ?+    +<.hin  ~|  %clay-warp-index-strange  !!
        %done
      ?~  error.hin
        [~ ..^$]
      ::  TODO better error handling
      ::
      ~&  %clay-take-warp-index-error^our^tea^tag.u.error.hin
      %-  (slog tang.u.error.hin)
      [~ ..^$]
    ::
        %lost
      %-  (slog leaf+"clay: lost warp from {<tea>}" ~)
      [~ ..^$]
    ::
        %boon
      =/  her=ship   (slav %p i.t.tea)
      =/  =desk      (slav %tas i.t.t.tea)
      =/  index=@ud  (slav %ud i.t.t.t.tea)
      ::
      =^  mos  ruf
        =+  ;;(res=(unit rand) payload.hin)
        =/  den  ((de now rof hen ruf) her desk)
        abet:(take-foreign-answer:den index res)
      [mos ..^$]
    ==
  ::
  ?:  ?=([%back-index @ @ @ *] tea)
    ?+    +<.hin  ~|  %clay-backfill-index-strange  !!
        %done
      ?~  error.hin
        [~ ..^$]
      ::  TODO better error handling
      ::
      ~&  %clay-take-backfill-index-error^our^tea^tag.u.error.hin
      %-  (slog tang.u.error.hin)
      [~ ..^$]
    ::
        %lost
      %-  (slog leaf+"clay: lost backfill from {<tea>}" ~)
      [~ ..^$]
    ::
        ?(%boon %tune)
      =/  her=ship   (slav %p i.t.tea)
      =/  =desk      (slav %tas i.t.t.tea)
      =/  index=@ud  (slav %ud i.t.t.t.tea)
      ::
      =/  fell=(unit fell)
        ?:  ?=(%boon +<.hin)  `;;(fell payload.hin)
        ?~  roar.hin  ~
        ?~  q.dat.u.roar.hin  ~
        `[%1 `u.q.dat.u.roar.hin]
      ::
      =^  mos  ruf
        =/  den  ((de now rof hen ruf) her desk)
        ?~  fell
          ::  We shouldn't get back null on any of the fine requests we
          ::  make unless they're out of date
          ::
          %-  (slog leaf+"clay: got null from {<her>}, falling back to ames" ~)
          abet:(retry-with-ames:den %back-index index)
        =?  den  ?=(%tune +<.hin)
          (cancel-scry-timeout:den index)
        abet:abet:(take-backfill:(foreign-update:den index) u.fell)
      [mos ..^$]
    ::
         %wake
      ?^  error.hin
        [[hen %slip %d %flog %crud %wake u.error.hin]~ ..^$]
      =/  her=ship   (slav %p i.t.tea)
      =/  =desk      (slav %tas i.t.t.tea)
      =/  index=@ud  (slav %ud i.t.t.t.tea)
      =^  mos  ruf
        =/  den  ((de now rof hen ruf) her desk)
        abet:(retry-with-ames:den %back-index index)
      [mos ..^$]
    ==
  ::
  ?:  ?=([%seek @ @ ~] tea)
    ?+    +<.hin  ~|  %clay-seek-strange  !!
        %done
      ?~  error.hin
        [~ ..^$]
      %-  (slog leaf+"clay: seek nack from {<tea>}" u.error.hin)
      [~ ..^$]
    ::
        %lost
      %-  (slog leaf+"clay: lost boon from {<tea>}" ~)
      [~ ..^$]
    ::
        %boon
      =+  ;;  =fell  payload.hin
      ::
      =/  her=ship  (slav %p i.t.tea)
      =/  =desk     (slav %tas i.t.t.tea)
      =^  mos  ruf
        =/  den  ((de now rof hen ruf) her desk)
        abet:(take-fell:den fell)
      [mos ..^$]
    ==
  ::
  ?:  ?=([%sinks ~] tea)
    ?>  ?=(%public-keys +<.hin)
    ?.  ?=(%breach -.public-keys-result.hin)
      [~ ..^$]
    =/  who  who.public-keys-result.hin
    ?:  =(our who)
      [~ ..^$]
    ::  Cancel subscriptions
    ::
    =/  foreign-desk=(unit rung)
      (~(get by hoy.ruf) who)
    ?~  foreign-desk
      [~ ..^$]
    =/  cancel-ducts=(list duct)
      %-  zing  ^-  (list (list duct))
      %+  turn  ~(tap by rus.u.foreign-desk)
      |=  [=desk =rede]
      ^-  (list duct)  %-  zing  ^-  (list (list duct))
      %+  turn  ~(tap by qyx.rede)
      |=  [=wove ducts=(set duct)]
      ::  ~&  [%sunk-wove desk (print-wove wove) ducts]
      ~(tap in ducts)
    =/  cancel-moves=(list move)
      %+  turn  cancel-ducts
      |=(=duct [duct %pass /drip %b %drip !>([%writ ~])])
    ::  delete local state of foreign desk
    ::
    =.  hoy.ruf  (~(del by hoy.ruf) who)
    [cancel-moves ..^$]
  ::
  ?-    -.+.hin
      %public-keys  ~|([%public-keys-raw tea] !!)
  ::
      %mere
    ?:  ?=(%& -.p.+.hin)
      ~&  'initial merge succeeded'
      [~ ..^$]
    ~>  %slog.
        :^  0  %rose  [" " "[" "]"]
        :^    leaf+"initial merge failed"
            leaf+"my most sincere apologies"
          >p.p.p.+.hin<
        q.p.p.+.hin
    [~ ..^$]
  ::
      %wake
    ::  TODO: handle behn errors
    ::
    ?^  error.hin
      [[hen %slip %d %flog %crud %wake u.error.hin]~ ..^$]
    ::
    ?.  ?=([%tyme @ @ ~] tea)
      ~&  [%clay-strange-timer tea]
      [~ ..^$]
    ::  [wake] when requested time passes, call +wake
    ::
    =/  her  (slav %p i.t.tea)
    =/  syd  (slav %tas i.t.t.tea)
    =^  mos  ruf
      =/  den  ((de now rof hen ruf) her syd)
      abet:wake:den
    [mos ..^$]
  ::
      ::  handled in the wire dispatcher
      ::
      %boon  !!
      %tune  !!
      %lost  !!
      %unto  !!
      %wris  ~&  %strange-wris  !!
      %writ
    %-  (slog leaf+"clay: strange writ (expected on upgrade to Fusion)" ~)
    [~ ..^$]
  ::
      %done
    ?~  error=error.hin
      [~ ..^$]
    %-  (slog >%clay-lost< >tag.u.error< tang.u.error)
    [~ ..^$]
  ==
::
++  rant-to-rand
  |=  rant
  ^-  rand
  [p q [p q.q]:r]
::  +whey: produce memory usage report
::
++  whey
  ^-  (list mass)
  ?:  (gth mas.bug.ruf 0)
    =/  domestic
      %+  turn  (sort ~(tap by dos.rom.ruf) aor)
      |=  [=desk =dojo]
      :+  desk  %|
      :~  mime+&+mim.dom.dojo
          flue+&+fod.dom.dojo
          dojo+&+dojo
      ==
    :~  :+  %object-store  %|
        :~  commits+&+hut.ran.ruf
            :+  %pages  %|
            %+  turn  ~(tap by lat.ran.ruf)
            |=  [=lobe =page]
            [(scot %uv lobe) %& page]
        ==
        domestic+|+domestic
        foreign+&+hoy.ruf
        ford-cache+&+fad.ruf
    ==
  =/  domestic
    %+  turn  (sort ~(tap by dos.rom.ruf) aor)
    |=  [=desk =dojo]
    :+  desk  %|
    :~  mime+&+mim.dom.dojo
        flue+&+fod.dom.dojo
        dojo+&+dojo
    ==
  :~  :+  %object-store  %|
      :~  commits+&+hut.ran.ruf
          pages+&+lat.ran.ruf
      ==
      domestic+|+domestic
      foreign+&+hoy.ruf
      ford-cache+&+fad.ruf
  ==
::
++  tomb
  |%
  ::  +tomb-clue: safely remove objects
  ::
  ++  tomb-clue
    |=  [=duct =clue]
    ^-  [(list move) _..^$]
    ?-    -.clue
        %lobe  `(tomb-lobe lobe.clue &)
        %all
      =/  lobes=(list [=lobe =page])  ~(tap by lat.ran.ruf)
      |-
      ?~  lobes
        `..^^$
      =.  ..^^$  (tomb-lobe lobe.i.lobes &)
      $(lobes t.lobes)
    ::
        %pick  pick
        %norm
      =^  mos  ruf
        =/  den  ((de now rof duct ruf) ship.clue desk.clue)
        abet:(set-norm:den norm.clue)
      [mos ..^$]
    ::
        %worn
      =^  mos  ruf
        =/  den  ((de now rof duct ruf) ship.clue desk.clue)
        abet:(set-worn:den tako.clue norm.clue)
      [mos ..^$]
    ::
        %seek
      =^  mos  ruf
        =/  den  ((de now rof duct ruf) ship.clue desk.clue)
        abet:(seek:den cash.clue)
      [mos ..^$]
    ==
  ::  +tomb-lobe: remove specific lobe
  ::
  ++  tomb-lobe
    |=  [lob=lobe veb=?]
    ^+  ..^$
    =/  peg=(unit page)  (~(get by lat.ran.ruf) lob)
    ?~  peg
      (noop veb leaf+"clay: file already tombstoned" ~)
    ::
    =/  used=(unit beam)
      =/  desks=(list [=desk =dojo])  ~(tap by dos.rom.ruf)
      |-
      =*  desk-loop  $
      ?~  desks
        ~
      ?:  =(0 let.dom.dojo.i.desks)
        desk-loop(desks t.desks)
      =/  =yaki
        %-  ~(got by hut.ran.ruf)
        %-  ~(got by hit.dom.dojo.i.desks)
        let.dom.dojo.i.desks
      =/  paths=(list [=path =lobe])  ~(tap by q.yaki)
      |-
      =*  path-loop  $
      ?~  paths
        desk-loop(desks t.desks)
      ?:  =(lob lobe.i.paths)
        `[[our desk.i.desks ud+let.dom.dojo.i.desks] path.i.paths]
      path-loop(paths t.paths)
    ::
    ?^  used
      (noop veb leaf+"clay: file used in {<(en-beam u.used)>}" ~)
    ::
    =.  lat.ran.ruf  (~(del by lat.ran.ruf) lob)
    (noop veb leaf+"clay: file successfully tombstoned" ~)
  ::
  ++  noop
    |=  [veb=? =tang]
    ?.  veb
      ..^$
    ((slog tang) ..^$)
  ::
  ++  draw-raft
    ^-  (set [norm yaki])
    =/  room-yakis
      =/  rooms=(list [=desk =dojo])  ~(tap by dos.rom.ruf)
      |-  ^-  (set [norm yaki])
      ?~  rooms
        ~
      (~(uni in $(rooms t.rooms)) (draw-dome %& dom.dojo.i.rooms))
    =/  rung-yakis
      =/  rungs=(list [=ship =rung])  ~(tap by hoy.ruf)
      |-  ^-  (set [norm yaki])
      ?~  rungs
        ~
      %-  ~(uni in $(rungs t.rungs))
      =/  redes=(list [=desk =rede])  ~(tap by rus.rung.i.rungs)
      |-  ^-  (set [norm yaki])
      ?~  redes
        ~
      (~(uni in $(redes t.redes)) (draw-dome %| dom.rede.i.redes))
    (~(uni in room-yakis) rung-yakis)
  ::
  ++  draw-dome
    |=  [domestic=? =dome]
    ^-  (set [norm yaki])
    =/  =aeon  1
    |-  ^-  (set [norm yaki])
    ?:  (lth let.dome aeon)
      ~
    =/  =tako  (~(got by hit.dome) aeon)
    =/  yakis=(set [norm yaki])
      ?.  &(=(let.dome aeon) domestic)
        ~
      [[*norm (~(got by hut.ran.ruf) tako)] ~ ~]
    %-  ~(uni in yakis)
    %-  ~(uni in (draw-tako tom.dome nor.dome tako))
    $(aeon +(aeon))
  ::
  ++  draw-tako
    |=  [tom=(map tako norm) nor=norm =tako]
    ^-  (set [norm yaki])
    ~+
    =/  =norm  (~(gut by tom) tako nor)
    =/  =yaki  (~(got by hut.ran.ruf) tako)
    =/  takos
      |-  ^-  (set [^norm ^yaki])
      ?~  p.yaki
        ~
      (~(uni in $(p.yaki t.p.yaki)) ^$(tako i.p.yaki))
    (~(put in takos) norm yaki)
  ::
  ::  +pick: copying gc based on norms
  ::
  ++  pick
    =|  lat=(map lobe page)
    =|  sen=(set [norm (map path lobe)])
    |^
    =.  ..pick-raft  pick-raft
    =.  lat.ran.ruf  lat
    `..^$
    ::
    ++  pick-raft
      ^+  ..pick-raft
      =/  yakis=(list [=norm =yaki])  ~(tap in draw-raft)
      |-  ^+  ..pick-raft
      ?~  yakis
        ..pick-raft
      ::  ~&  >  [%picking [norm r.yaki]:i.yakis]
      $(yakis t.yakis, ..pick-raft (pick-yaki i.yakis))
    ::
    ::  NB: recurring tree-wise with the `sen` cache provides
    ::  approximately a 100x speedup on a mainnet moon in 4/2022
    ::
    ++  pick-yaki
      |=  [=norm =yaki]
      ^+  ..pick-raft
      |-  ^+  ..pick-raft
      ?~  q.yaki
        ..pick-raft
      ?:  (~(has in sen) norm q.yaki)
        ..pick-raft
      =.  sen  (~(put in sen) norm q.yaki)
      =/  peg=(unit page)  (~(get by lat.ran.ruf) q.n.q.yaki)
      ::  ~&  >>  [%picking-lobe ?=(^ peg) +:(~(fit of norm) p.n.q.yaki) n.q.yaki]
      =?  lat  &(?=(^ peg) !=([~ %|] +:(~(fit of norm) p.n.q.yaki)))
        (~(uni by `(map lobe page)`[[q.n.q.yaki u.peg] ~ ~]) lat)
      =.  ..pick-raft  $(q.yaki l.q.yaki)
      $(q.yaki r.q.yaki)
    --
  --
--
