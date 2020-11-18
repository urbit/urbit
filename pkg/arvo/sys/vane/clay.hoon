::  clay (4c), revision control
!:
::  This is split in three top-level sections:  structure definitions, main
::  logic, and arvo interface.
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::
::  Here are the structures.  `++raft` is the formal arvo state.  It's also
::  worth noting that many of the clay-related structures are defined in zuse.
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
|=  pit/vase
=,  clay
=>  |%
+$  aeon  @ud                                           ::  version number
::
::  Recursive structure of a desk's data.
::
::  We keep an ankh only for the current version of local desks.  Everywhere
::  else we store it as (map path lobe).
::
+$  ankh                                                ::  expanded node
  $~  [~ ~]
  $:  fil/(unit {p/lobe q/cage})                        ::  file
      dir/(map @ta ankh)                                ::  folders
  ==                                                    ::
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
  $:  new/(map path lobe)
      cal/(map path lobe)
      can/(map path cage)
      old/(map path ~)
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
:: ++  care  ?($d $p $t $u $v $w $x $y $z)
::
::  Keeps track of subscribers.
::
::  A map of requests to a set of all the subscribers who should be notified
::  when the request is filled/updated.
::
+$  cult  (jug wove duct)
::
::  Domestic desk state.
::
::  Includes subscriber list, dome (desk content), possible commit state (for
::  local changes), possible merge state (for incoming merges), and permissions.
::
++  dojo
  $:  qyx/cult                                          ::  subscribers
      dom/dome                                          ::  desk state
      per/regs                                          ::  read perms per path
      pew/regs                                          ::  write perms per path
  ==
::
::  Desk state.
::
::  Includes a checked-out ankh with current content, most recent version, map
::  of all version numbers to commit hashes (commits are in hut.rang), and map
::  of labels to version numbers.
::
::  `mim` is a cache of the content in the directories that are mounted
::  to unix.  Often, we convert to/from mime without anything really
::  having changed; this lets us short-circuit that in some cases.
::  Whenever you give an `%ergo`, you must update this.
::
++  dome
  $:  ank/ankh                                          ::  state
      let/aeon                                          ::  top id
      hit/(map aeon tako)                               ::  versions by id
      lab/(map @tas aeon)                               ::  labels
      mim/(map path mime)                               ::  mime cache
      fod/ford-cache                                    ::  ford cache
      fer/(unit reef-cache)                             ::  reef cache
  ==                                                    ::
::
::  Commit state.
::
::  --  `del` is the paths we're deleting.
::  --  `ink` is the insertions of hoon files (short-circuited for
::      bootstrapping).
::  --  `ins` is all the other insertions.
::  --  `dif` is the diffs in `dig` applied to their files.
::  --  `mut` is the diffs between `muc` and the original files.
::
++  dork                                                ::  diff work
  $:  del/(list path)                                   ::  deletes
      ink/(list (pair path cage))                       ::  hoon inserts
      ins/(list (pair path cage))                       ::  inserts
      dif/(list (trel path lobe cage))                  ::  changes
      mut/(list (trel path lobe cage))                  ::  mutations
  ==                                                    ::
::
::  Over-the-wire backfill request
::
+$  fill  [=desk =lobe]
::
::  Ford cache
::
+$  ford-cache
  $:  vases=(map path [res=vase dez=(set path)])
      marks=(map mark [res=dais dez=(set path)])
      casts=(map mars [res=tube dez=(set path)])
  ==
::  $reef-cache: built system files
::
+$  reef-cache
  $:  hoon=vase
      arvo=vase
      zuse=vase
  ==
::
::  Hash of a blob, for lookup in the object store (lat.ran)
::
++  lobe  @uvI                                          ::  blob ref
::
::  New desk data.
::
::  Sent to other ships to update them about a particular desk.  Includes a map
::  of all new aeons to hashes of their commits, the most recent aeon, and sets
::  of all new commits and data.
::
++  nako                                                ::  subscription state
  $:  gar/(map aeon tako)                               ::  new ids
      let/aeon                                          ::  next id
      lar/(set yaki)                                    ::  new commits
      bar/(set plop)                                    ::  new content
  ==                                                    ::
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
++  raft                                                ::  filesystem
  $:  rom=room                                          ::  domestic
      hoy=(map ship rung)                               ::  foreign
      ran=rang                                          ::  hashes
      mon=(map term beam)                               ::  mount points
      hez=(unit duct)                                   ::  sync duct
      cez=(map @ta crew)                                ::  permission groups
      pud=(unit [=desk =yoki])                          ::  pending update
      pun=(list move)                                   ::  upgrade moves
  ==                                                    ::
::
::  Object store.
::
::  Maps of commit hashes to commits and content hashes to content.
::
++  rang                                                ::
  $:  hut/(map tako yaki)                               ::
      lat/(map lobe blob)                               ::
  ==                                                    ::
::
::  Unvalidated response to a request.
::
::  Like a ++rant, but with a page of data rather than a cage of it.
::
++  rand                                                ::  unvalidated rant
          $:  p/{p/care q/case r/@tas}                  ::  clade release book
              q/path                                    ::  spur
              r/page                                    ::  data
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
++  rede                                                ::  universal project
          $:  lim/@da                                   ::  complete to
              ref/(unit rind)                           ::  outgoing requests
              qyx/cult                                  ::  subscribers
              dom/dome                                  ::  revision state
              per/regs                                  ::  read perms per path
              pew/regs                                  ::  write perms per path
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
::  Active downloads
::
+$  update-state
  $:  =duct
      =rave
      have=(map lobe blob)
      need=(list lobe)
      nako=(qeu (unit nako))
      busy=_|
  ==
::
::  Result of a subscription
::
++  sub-result
  $%  [%blab =mood data=(each cage lobe)]
      [%bleb ver=@ud ins=@ud range=(unit (pair aeon aeon))]
      [%balk cage=(unit (each cage lobe)) =mood]
      [%blas moods=(set mood)]
      [%blub ~]
  ==
::
::  Domestic ship.
::
::  `hun` is the duct to dill, and `dos` is a collection of our desks.
::
++  room                                                ::  fs per ship
          $:  hun/duct                                  ::  terminal duct
              dos/(map desk dojo)                       ::  native desk
          ==                                            ::
::
::  Stored request.
::
::  Like a ++rave but with caches of current versions for %next and %many.
::  Generally used when we store a request in our state somewhere.
::
++  cach  (unit (unit (each cage lobe)))                ::  cached result
+$  wove  [for=(unit [=ship ver=@ud]) =rove]            ::  stored source + req
++  rove                                                ::  stored request
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
++  rung
          $:  rus=(map desk rede)                       ::  neighbor desks
          ==
::
++  move  {p/duct q/(wind note gift:able)}              ::  local move
++  note                                                ::  out request $->
  $~  [%b %wait *@da]                                   ::
  $%  $:  %a                                            ::  to %ames
          $>(%plea task:able:ames)                      ::
      ==                                                ::
      $:  %b                                            ::  to %behn
          $>  $?  %drip                                 ::
                  %rest                                 ::
                  %wait                                 ::
              ==                                        ::
          task:able:behn                                ::
      ==                                                ::
      $:  %c                                            ::  to %clay
          $>  $?  %info                                 ::  internal edit
                  %merg                                 ::  merge desks
                  %pork                                 ::
                  %warp                                 ::
                  %werp                                 ::
              ==                                        ::
          task:able                                     ::
      ==                                                ::
      $:  %d                                            ::  to %dill
          $>(%flog task:able:dill)                      ::
      ==                                                ::
      $:  %g                                            ::  to %gall
          $>(%deal task:able:gall)                      ::
      ==                                                ::
      $:  %j                                            ::  by %jael
          $>(%public-keys task:able:jael)               ::
  ==  ==                                                ::
++  riot  (unit rant)                                   ::  response+complete
++  sign                                                ::  in result $<-
  $~  [%b %wake ~]                                      ::
  $%  $:  %a                                            ::  by %ames
          $>  $?  %boon                                 ::  response
                  %done                                 ::  (n)ack
                  %lost                                 ::  lost boon
              ==                                        ::
          gift:able:ames                                ::
      ==                                                ::
      $:  %b                                            ::  by %behn
          $%  $>(%wake gift:able:behn)                  ::  timer activate
              $>(%writ gift:able)                       ::  XX %slip
      ==  ==                                            ::
      $:  %c                                            ::  by %clay
          $>  $?  %mere                                 ::
                  %note                                 ::
                  %writ                                 ::
              ==                                        ::
          gift:able                                     ::
      ==                                                ::
      $:  %j                                            ::  by %jael
          $>(%public-keys gift:able:jael)               ::
      ==                                                ::
      $:  @tas                                          ::  by any
          $>(%crud vane-task)                           ::  XX strange
  ==  ==                                                ::
--  =>
~%  %clay-utilities  ..is  ~
::  %utilities
::
|%
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
  =>
  |%
  ::  +an: $ankh interface door
  ::
  ++  an
    |_  nak=ankh
    ::  +get: produce file at path
    ::
    ++  get
      |=  =path
      ^-  (unit cage)
      ?~  path
        ?~  fil.nak
          ~
        `q.u.fil.nak
      ?~  kid=(~(get by dir.nak) i.path)
        ~
      $(nak u.kid, path t.path)
    --
  ++  with-face  |=([face=@tas =vase] vase(p [%face face p.vase]))
  ++  with-faces
    =|  res=(unit vase)
    |=  vaz=(list [face=@tas =vase])
    ^-  vase
    ?~  vaz  (need res)
    =/  faz  (with-face i.vaz)
    =.  res  `?~(res faz (slop faz u.res))
    $(vaz t.vaz)
  --
  |%
  ::  +wrap: external wrapper
  ::
  ++  wrap
    |*  [* state:ford]
    [+<- +<+>-]  ::  cache.state
  ::
  ++  ford
    !.
    =>  |%
        +$  build
          $%  [%file =path]
              [%mark =mark]
              [%cast =mars]
              [%vale =path]
          ==
        +$  state
          $:  baked=(map path cage)
              cache=ford-cache
              stack=(list (set path))
              cycle=(set build)
          ==
        +$  args
          $:  bud=vase
              =ankh
              deletes=(set path)
              changes=(map path (each page lobe))
              file-store=(map lobe blob)
              =ford-cache
          ==
        --
    |=  args
    ::  nub: internal mutable state for this computation
    ::
    =|  nub=state
    =.  cache.nub  ford-cache
    |%
    ::  +pop-stack: pop build stack, copying deps downward
    ::
    ++  pop-stack
      ^-  [(set path) _stack.nub]
      =^  top=(set path)  stack.nub  stack.nub
      =?  stack.nub  ?=(^ stack.nub)
        stack.nub(i (~(uni in i.stack.nub) top))
      [top stack.nub]
    ::
    ++  get-value
      |=  =path
      ^-  [cage state]
      ~|  %error-validating^path
      ?^  got=(~(get by baked.nub) path)
        [u.got nub]
      =;  [res=cage bun=state]
        =.  nub  bun
        =.  baked.nub  (~(put by baked.nub) path res)
        [res nub]
      ?:  (~(has in cycle.nub) vale+path)
        ~|(cycle+vale+path^stack.nub !!)
      =.  cycle.nub  (~(put in cycle.nub) vale+path)
      ?^  change=(~(get by changes) path)
        =^  page  nub
          ?:  ?=(%& -.u.change)
            [p.u.change nub]
          ~|  %ugly-lobe^p.u.change^path
          (lobe-to-page p.u.change)
        =^  cage  nub  (validate-page path page)
        [cage nub]
      ?<  (~(has in deletes) path)
      ~|  %file-not-found^path
      :_(nub (need (~(get an ankh) path)))
    ::  +get-mark: build a mark definition
    ::
    ++  get-mark
      |=  mak=mark
      ^-  [dais state]
      ~|  %error-building-mark^mak
      ?^  got=(~(get by marks.cache.nub) mak)
        =?  stack.nub  ?=(^ stack.nub)
          stack.nub(i (~(uni in i.stack.nub) dez.u.got))
        [res.u.got nub]
      ?:  (~(has in cycle.nub) mark+mak)
        ~|(cycle+mark+mak^stack.nub !!)
      =.  cycle.nub  (~(put in cycle.nub) mark+mak)
      =.  stack.nub  [~ stack.nub]
      =;  res=[=dais nub=state]
        =.  nub  nub.res
        =^  top  stack.nub  pop-stack
        =.  marks.cache.nub  (~(put by marks.cache.nub) mak [dais.res top])
        [dais.res nub]
      =^  cor=vase  nub  (build-fit %mar mak)
      =/  gad=vase  (slap cor %limb %grad)
      ?@  q.gad
        =+  !<(mok=mark gad)
        =^  deg=dais  nub  $(mak mok)
        =^  tub=tube  nub  (get-cast mak mok)
        =^  but=tube  nub  (get-cast mok mak)
        :_  nub
        ^-  dais
        |_  sam=vase
        ++  bunt  (slap cor $+6)
        ++  diff
          |=  new=vase
          ^-  vase
          (~(diff deg (tub sam)) (tub new))
        ++  form  form:deg
        ++  join  join:deg
        ++  mash  mash:deg
        ++  pact
          |=  diff=vase
          ^+  sam
          (but (~(pact deg (tub sam)) diff))
        ++  vale
          |=  =noun
          ^+  sam
          (slam (slap cor !,(*hoon noun:grab)) !>(noun))
        ++  volt
          |=  =noun
          ^+  sam
          [p:bunt noun]
        --
      :_  nub
      =+  !<(fom=mark (slap gad %limb %form))
      ^-  dais
      |_  sam=vase
      ++  bunt  (slap cor $+6)
      ++  diff
        |=  new=vase
        ^-  vase
        %+  slap
          (with-faces cor+cor sam+sam new+new ~)
        !,  *hoon
        (diff:~(grad cor sam) new)
      ++  form  fom
      ++  join
        |=  [a=vase b=vase]
        ^-  (unit (unit vase))
        ?:  =(q.a q.b)
          ~
        =;  res  `?~(q.res ~ `(slap res !,(*hoon ?~(. !! u))))
        (slam (slap cor !,(*hoon join:grad)) (slop a b))
      ++  mash
        |=  [a=[=ship =desk diff=vase] b=[=ship =desk diff=vase]]
        ^-  (unit vase)
        ?:  =(q.diff.a q.diff.b)
          ~
        :-  ~
        %+  slam  (slap cor !,(*hoon mash:grad))
        %+  slop
          :(slop !>(ship.a) !>(desk.a) diff.a)
        :(slop !>(ship.b) !>(desk.b) diff.b)
      ++  pact
        |=  diff=vase
        ^+  sam
        %+  slap
          (with-faces cor+cor sam+sam diff+diff ~)
        !,  *hoon
        (pact:~(grad cor sam) diff)
      ++  vale
        |=  =noun
        ^+  sam
        (slam (slap cor !,(*hoon noun:grab)) !>(noun))
      ++  volt
        |=  =noun
        ^+  sam
        [p:bunt noun]
      --
    ::  +get-cast: produce a $tube mark conversion gate from .a to .b
    ::
    ++  get-cast
      |=  [a=mark b=mark]
      ^-  [tube state]
      ~|  error-building-cast+[a b]
      ?:  =([%mime %hoon] [a b])
        :_  nub
        |=  sam=vase
        =+  !<(=mime sam)
        !>(q.q.mime)
      ?^  got=(~(get by casts.cache.nub) [a b])
        =?  stack.nub  ?=(^ stack.nub)
          stack.nub(i (~(uni in i.stack.nub) dez.u.got))
        [res.u.got nub]
      ?:  (~(has in cycle.nub) cast+[a b])
        ~|(cycle+cast+[a b]^stack.nub !!)
      =.  stack.nub  [~ stack.nub]
      =;  res=[=tube nub=state]
        =.  nub  nub.res
        =^  top  stack.nub  pop-stack
        =.  casts.cache.nub  (~(put by casts.cache.nub) [a b] [tube.res top])
        [tube.res nub]
      ::  try +grow; is there a +grow core with a .b arm?
      ::
      =^  old=vase  nub  (build-fit %mar a)
      ?:  =/  ram  (mule |.((slap old !,(*hoon grow))))
          ?:  ?=(%| -.ram)  %.n
          =/  lab  (mule |.((slob b p.p.ram)))
          ?:  ?=(%| -.lab)  %.n
          p.lab
        ::  +grow core has .b arm; use that
        ::
        :_  nub
        ^-  tube
        |=  sam=vase
        ^-  vase
        %+  slap
          (with-faces old+old sam+sam ~)
        :+  %sgzp  !,(*hoon old=old)
        :+  %sgzp  !,(*hoon sam=sam)
        :+  %tsld  [%limb b]
        !,  *hoon
        ~(grow old sam)
      ::  try direct +grab
      ::
      =^  new=vase  nub  (build-fit %mar b)
      =/  rab
        %-  mule  |.
        %+  slap  new
        :+  %tsld  [%limb a]
        [%limb %grab]
      ?:  &(?=(%& -.rab) ?=(^ q.p.rab))
        :_(nub |=(sam=vase ~|([%grab a b] (slam p.rab sam))))
      ::  try +jump
      ::
      =/  jum
        %-  mule  |.
        %+  slap  old
        :+  %tsld  [%limb b]
        [%limb %jump]
      ?:  ?=(%& -.jum)
        (compose-casts a !<(mark p.jum) b)
      ::  try indirect +grab
      ::
      ?:  ?=(%& -.rab)
        (compose-casts a !<(mark p.rab) b)
      ~|(no-cast-from+[a b] !!)
    ::
    ++  compose-casts
      |=  [x=mark y=mark z=mark]
      ^-  [tube state]
      =^  uno=tube  nub  (get-cast x y)
      =^  dos=tube  nub  (get-cast y z)
      :_(nub |=(sam=vase (dos (uno sam))))
    ::
    ++  lobe-to-page
      |=  =lobe
      ^-  [page state]
      =/  =blob  (~(got by file-store) lobe)
      |-  ^-  [page state]
      ?-    -.blob
          %direct  [q.blob nub]
          %delta
        =/  [[=mark =parent=^lobe] diff=page]  [q r]:blob
        =^  parent-page  nub  $(blob (~(got by file-store) parent-lobe))
        =^  =cage  nub  (run-pact parent-page diff)
        [[p q.q]:cage nub]
      ==
    ::
    ++  validate-page
      |=  [=path =page]
      ^-  [cage state]
      ~|  validate-page-fail+path^from+p.page
      =/  mak=mark  (head (flop path))
      ?:  =(mak p.page)
        (page-to-cage page)
      =^  [mark vax=vase]  nub  (page-to-cage page)
      =^  =tube  nub  (get-cast p.page mak)
      :_(nub [mak (tube vax)])
    ::
    ++  page-to-cage
      |=  =page
      ^-  [cage state]
      ?:  =(%hoon p.page)
        :_(nub [%hoon -:!>(*@t) q.page])
      ?:  =(%mime p.page)
        :_(nub [%mime !>(;;(mime q.page))])
      =^  =dais  nub  (get-mark p.page)
      :_(nub [p.page (vale:dais q.page)])
    ::
    ++  cast-path
      |=  [=path mak=mark]
      ^-  [cage state]
      =/  mok  (head (flop path))
      ~|  error-casting-path+[path mok mak]
      =^  cag=cage  nub  (get-value path)
      ?:  =(mok mak)
        [cag nub]
      =^  =tube  nub  (get-cast mok mak)
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
        :_(nub [%hoon !>(new)])
      =^  dys=dais  nub  (get-mark p.old)
      =^  syd=dais  nub  (get-mark p.diff)
      :_(nub [p.old (~(pact dys (vale:dys q.old)) (vale:syd q.diff))])
    ::
    ++  prelude
      |=  =path
      ^-  vase
      =^  cag=cage  nub  (get-value path)
      ?>  =(%hoon p.cag)
      =/  tex=tape  (trip !<(@t q.cag))
      =/  =pile  (parse-pile path tex)
      =.  hoon.pile  !,(*hoon .)
      =^  res=vase  nub  (run-pile pile)
      res
    ::
    ++  build-file
      |=  =path
      ^-  [vase state]
      ~|  %error-building^path
      ?^  got=(~(get by vases.cache.nub) path)
        =?  stack.nub  ?=(^ stack.nub)
          stack.nub(i (~(uni in i.stack.nub) dez.u.got))
        [res.u.got nub]
      ?:  (~(has in cycle.nub) file+path)
        ~|(cycle+file+path^stack.nub !!)
      =.  cycle.nub  (~(put in cycle.nub) file+path)
      =.  stack.nub  [(sy path ~) stack.nub]
      =^  cag=cage  nub  (get-value path)
      ?>  =(%hoon p.cag)
      =/  tex=tape  (trip !<(@t q.cag))
      =/  =pile  (parse-pile path tex)
      =^  res=vase  nub  (run-pile pile)
      =^  top  stack.nub  pop-stack
      =.  vases.cache.nub  (~(put by vases.cache.nub) path [res top])
      [res nub]
    ::
    ++  run-pile
      |=  =pile
      =^  sut=vase  nub  (run-tauts bud %sur sur.pile)
      =^  sut=vase  nub  (run-tauts sut %lib lib.pile)
      =^  sut=vase  nub  (run-raw sut raw.pile)
      =^  sut=vase  nub  (run-bar sut bar.pile)
      =/  res=vase  (road |.((slap sut hoon.pile)))
      [res nub]
    ::
    ++  parse-pile
      |=  [pax=path tex=tape]
      ^-  pile
      =/  [=hair res=(unit [=pile =nail])]  ((pile-rule pax) [1 1] tex)
      ?^  res  pile.u.res
      %-  mean  %-  flop
      =/  lyn  p.hair
      =/  col  q.hair
      :~  leaf+"syntax error at [{<lyn>} {<col>}] in {<pax>}"
          leaf+(trip (snag (dec lyn) (to-wain:format (crip tex))))
          leaf+(runt [(dec col) '-'] "^")
      ==
    ::
    ++  pile-rule
      |=  pax=path
      %-  full
      %+  ifix  [gay gay]
      %+  cook  |=(pile +<)
      ;~  pfix
        ::  parse optional /? and ignore
        ::
        ;~  pose
          (cold ~ ;~(plug fas wut gap dem gap))
          (easy ~)
        ==
      ::
        ;~  plug
          ;~  pose
            ;~  sfix
              %+  cook  |=((list (list taut)) (zing +<))
              %+  more  gap
              ;~  pfix  ;~(plug fas hep gap)
                (most ;~(plug com gaw) taut-rule)
              ==
              gap
            ==
            (easy ~)
          ==
        ::
          ;~  pose
            ;~  sfix
              %+  cook  |=((list (list taut)) (zing +<))
              %+  more  gap
              ;~  pfix  ;~(plug fas lus gap)
                (most ;~(plug com gaw) taut-rule)
              ==
              gap
            ==
            (easy ~)
          ==
        ::
          ;~  pose
            ;~  sfix
              %+  cook  |=((list [face=term =path]) +<)
              %+  more  gap
              ;~  pfix  ;~(plug fas tis gap)
                %+  cook  |=([term path] +<)
                ;~(plug sym ;~(pfix ;~(plug gap fas) (more fas urs:ab)))
              ==
              gap
            ==
            (easy ~)
          ==
        ::
          ;~  pose
            ;~  sfix
              %+  cook  |=((list [face=term =mark =path]) +<)
              %+  more  gap
              ;~  pfix  ;~(plug fas tar gap)
                %+  cook  |=([term mark path] +<)
                ;~  plug
                  sym
                  ;~(pfix ;~(plug gap cen) sym)
                  ;~(pfix ;~(plug gap fas) (more fas urs:ab))
                ==
              ==
              gap
            ==
            (easy ~)
          ==
        ::
          %+  cook  |=(huz=(list hoon) `hoon`tssg+huz)
          (most gap tall:(vang & pax))
        ==
      ==
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
      ?~  paz  ~|(no-file+pre^pax !!)
      =/  pux=path  pre^(snoc i.paz %hoon)
      ?:  (~(has in deletes) pux)
        $(paz t.paz)
      ?:  (~(has by changes) pux)
        pux
      ?^  (~(get an ankh) pux)
        pux
      $(paz t.paz)
    --
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
  |=  [our=ship now=@da ski=sley hen=duct raft]
  |=  [her=ship syd=desk]
  ::  XX ruf=raft crashes in the compiler
  ::
  =*  ruf  |4.+6.^$
  ::
  =/  [mow=(list move) hun=(unit duct) rede]
      ?.  =(our her)
        ::  no duct, foreign +rede or default
        ::
        :+  ?:  (~(has by hoy.ruf) her)
              ~
            [hun.rom.ruf %pass /sinks %j %public-keys (silt her ~)]~
          ~
        =/  rus  rus:(~(gut by hoy.ruf) her *rung)
        %+  ~(gut by rus)  syd
        [lim=~2000.1.1 ref=`*rind qyx=~ dom=*dome per=~ pew=~]
      ::  administrative duct, domestic +rede
      ::
      :+  ~  `hun.rom.ruf
      =/  jod  (~(gut by dos.rom.ruf) syd *dojo)
      [lim=now ref=~ [qyx dom per pew]:jod]
  ::
  =*  red=rede  ->+
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
      dos.rom  (~(put by dos.rom.ruf) syd [qyx dom per pew]:red)
    ==
  ::
  ::  Handle `%sing` requests
  ::
  ++  aver
    |=  {for/(unit ship) mun/mood}
    ^-  [(unit (unit (each cage lobe))) ford-cache]
    =+  ezy=?~(ref ~ (~(get by haw.u.ref) mun))
    ?^  ezy
      :_(fod.dom.red `(bind u.ezy |=(a/cage [%& a])))
    ?:  ?=([%s [%ud *] %late *] mun)
      :_  fod.dom.red
      ^-  (unit (unit (each cage lobe)))
      :^  ~  ~  %&
      ^-  cage
      :-  %cass
      ?~  let.dom
        !>([0 *@da])
      !>([let.dom t:(~(got by hut.ran) (~(got by hit.dom) let.dom))])
    =+  nao=(case-to-aeon case.mun)
    ?~(nao [~ fod.dom.red] (read-at-aeon:ze for u.nao mun))
  ::
  ::  Queue a move.
  ::
  ++  emit
    |=  mof/move
    %_(+> mow [mof mow])
  ::
  ::  Queue a list of moves
  ::
  ++  emil
    |=  mof/(list move)
    %_(+> mow (weld (flop mof) mow))
  ::
  ::  Produce either null or a result along a subscription.
  ::
  ::  Producing null means subscription has been completed or cancelled.
  ::
  ++  balk
    |=  {hen/duct cay/(unit (each cage lobe)) mun/mood}
    ^+  +>
    ?~  cay  (blub hen)
    (blab hen mun u.cay)
  ::
  ::  Set timer.
  ::
  ++  bait
    |=  {hen/duct tym/@da}
    (emit hen %pass /tyme/(scot %p her)/[syd] %b %wait tym)
  ::
  ::  Cancel timer.
  ::
  ++  best
    |=  {hen/duct tym/@da}
    (emit hen %pass /tyme/(scot %p her)/[syd] %b %rest tym)
  ::
  ::  Give subscription result.
  ::
  ::  Result can be either a direct result (cage) or a lobe of a result.  In
  ::  the latter case we fetch the data at the lobe and produce that.
  ::
  ++  blab
    |=  [hen=duct mun=mood dat=(each cage lobe)]
    ^+  +>
    =^  =cage  fod.dom
      ?:  ?=(%& -.dat)
        [p.dat fod.dom]
      =^  =page  fod.dom
        %-  wrap:fusion
        (lobe-to-page:(ford:fusion static-ford-args) p.dat)
      =^  =cage  fod.dom
        %-  wrap:fusion
        (page-to-cage:(ford:fusion static-ford-args) page)
      [cage fod.dom]
    =/  gift  [%writ ~ [care.mun case.mun syd] path.mun cage]
    ?:  ?=(^ ref)
      (emit hen %slip %b %drip !>(gift))
    (emit hen %give gift)
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
        $da
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
    ::
        $tas  (~(get by lab.dom) p.lok)
        $ud   ?:((gth p.lok let.dom) ~ [~ p.lok])
    ==
  ::
  ++  blas
    |=  {hen/duct das/(set mood)}
    ^+  +>
    ?>  ?=(^ das)
    ::  translate the case to a date
    ::
    =/  cas  [%da (case-to-date case.n.das)]
    =/  res
      (~(run in `(set mood)`das) |=(m/mood [care.m path.m]))
    =/  gift  [%wris cas res]
    ?:  ?=(^ ref)
      (emit hen %slip %b %drip !>(gift))
    (emit hen %give gift)
  ::
  ::  Give next step in a subscription.
  ::
  ++  bleb
    |=  [hen=duct ver=@ud ins=@ud hip=(unit (pair aeon aeon))]
    ^+  +>
    %^  blab  hen  [%w [%ud ins] ~]
    :-  %&
    ?~  hip
      [%null [%atom %n ~] ~]
    [%nako !>((make-nako:ze ver u.hip))]
  ::
  ::  Tell subscriber that subscription is done.
  ::
  ++  blub
    |=  hen/duct
    ?:  ?=(^ ref)
      (emit hen %slip %b %drip !>([%writ ~]))
    (emit hen %give %writ ~)
  ::
  ::  Lifts a function so that a single result can be fanned out over a set of
  ::  subscriber ducts.
  ::
  ::  Thus, `((duct-lift func) subs arg)` runs `(func sub arg)` for each `sub`
  ::  in `subs`.
  ::
  ++  duct-lift
    |*  send/_|=({duct *} ..duct-lift)
    |=  {a/(set duct) arg/_+<+.send}  ^+  ..duct-lift
    =+  all=~(tap by a)
    |-  ^+  ..duct-lift
    ?~  all  ..duct-lift
    =.  +>.send  ..duct-lift
    $(all t.all, duct-lift (send i.all arg))
  ::
  ++  blub-all  (duct-lift |=([a=duct ~] (blub a)))
  ++  blab-all  (duct-lift blab)
  ++  blas-all  (duct-lift blas)
  ++  balk-all  (duct-lift balk)
  ++  bleb-all  (duct-lift bleb)
  ::
  ++  static-ford-args
    [zuse:(need fer.dom) ank.dom ~ ~ lat.ran fod.dom]
  ::
  ::  Transfer a request to another ship's clay.
  ::
  ++  send-over-ames
    |=  [=duct =ship index=@ud =riff]
    ^+  +>
    ::
    =/  =desk  p.riff
    =/  =wire  /warp-index/(scot %p ship)/(scot %tas desk)/(scot %ud index)
    =/  =path  [%question desk (scot %ud index) ~]
    (emit duct %pass wire %a %plea ship %c path [[%1 ~] riff])
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
      (run-if-future rove.wov |=(@da (bait hen +<)))
    |-  ^+  +>+.$
    =/  =rave  (rove-to-rave rove.wov)
    =.  rave
      ?.  ?=([%sing %v *] rave)  rave
      [%many %| [%ud let.dom] case.mood.rave path.mood.rave]
    =+  inx=nix.u.ref
    =.  +>+.$
      =<  ?>(?=(^ ref) .)
      (send-over-ames hen her inx syd `rave)
    %=  +>+.$
      nix.u.ref  +(nix.u.ref)
      bom.u.ref  (~(put by bom.u.ref) inx [hen rave ~ ~ ~ |])
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
    |=  wov/wove
    ^-  wove
    =;  won/(unit wove)  (fall won wov)
    =*  rov  rove.wov
    ?-    -.rov
        $sing  ~
        $next
      =+  aey=(case-to-aeon case.mood.rov)
      ?~  aey  ~
      %-  ~(rep in ~(key by qyx))
      |=  {haw/wove res/(unit wove)}
      ?^  res  res
      ?.  =(for.wov for.haw)  ~
      =*  hav  rove.haw
      =-  ?:(- `haw ~)
      ?&  ?=($next -.hav)
          =(mood.hav mood.rov(case case.mood.hav))
        ::
          ::  only a match if this request is before
          ::  or at our starting case.
          =+  hay=(case-to-aeon case.mood.hav)
          ?~(hay | (lte u.hay u.aey))
      ==
    ::
        $mult
      =+  aey=(case-to-aeon case.mool.rov)
      ?~  aey  ~
      %-  ~(rep in ~(key by qyx))
      |=  {haw/wove res/(unit wove)}
      ?^  res  res
      ?.  =(for.wov for.haw)  ~
      =*  hav  rove.haw
      =-  ?:(- `haw ~)
      ?&  ?=($mult -.hav)
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
        $many
      =+  aey=(case-to-aeon from.moat.rov)
      ?~  aey  ~
      %-  ~(rep in ~(key by qyx))
      |=  {haw/wove res/(unit wove)}
      ?^  res  res
      ?.  =(for.wov for.haw)  ~
      =*  hav  rove.haw
      =-  ?:(- `haw ~)
      ?&  ?=($many -.hav)
          =(hav rov(from.moat from.moat.hav))
        ::
          ::  only a match if this request is before
          ::  or at our starting case.
          =+  hay=(case-to-aeon from.moat.hav)
          ?~(hay | (lte u.hay u.aey))
      ==
    ==
  ::
  ::  Porcelain commit
  ::
  ++  info
    |=  [deletes=(set path) changes=(map path cage)]
    ^+  ..park
    ?:  =(0 let.dom)
      ?>  ?=(~ deletes)
      =/  data=(map path (each page lobe))
        (~(run by changes) |=(=cage &+[p q.q]:cage))
      (park | &+[~ data] *rang)
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
    (park | &+yuki *rang)
  ::
  ::  Unix commit
  ::
  ++  into
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
  ::    XX  needs to check that head is ancestor of tako
  ::    XX  needs to check tako in rang
  ::    XX  needs to check that commit doesn't have same date
  ::
  ++  park
    =/  check-sane  |
    |^
    |=  [updated=? =yoki =rang]
    ^+  ..park
    =:  hut.ran  (~(uni by hut.rang) hut.ran)
        lat.ran  (~(uni by lat.rang) lat.ran)
      ==
    =/  new-data=(map path (each page lobe))
      ?-  -.yoki
        %&  q.p.yoki
        %|  (~(run by q.p.yoki) |=(=lobe |+lobe))
      ==
    =/  old-yaki
      ?:  =(0 let.dom)
        *yaki
      (aeon-to-yaki:ze let.dom)
    =/  [deletes=(set path) changes=(map path (each page lobe))]
      (get-changes q.old-yaki new-data)
    ~|  [from=let.dom deletes=deletes changes=~(key by changes)]
    ::
    ::  promote ford cache
    ::  promote and fill in ankh
    ::  promote and fill in mime cache
    ::
    =/  sys-changes  (need-sys-update changes)
    ?:  ?&  =(%home syd)
            !updated
            |(!=(~ sys-changes) !=(~ (need-vane-update changes)))
        ==
      (sys-update yoki new-data changes)
    ::  clear caches if zuse reloaded
    ::
    =/  is-zuse-new=?  !=(~ sys-changes)
    =.  fod.dom
      ?:  is-zuse-new
        *ford-cache
      (promote-ford fod.dom deletes ~(key by changes))
    =.  fer.dom  `(build-reef fer.dom ~(key by changes) new-data)
    =?  ank.dom  is-zuse-new  *ankh
    =?  changes  is-zuse-new
      (changes-for-upgrade q.old-yaki deletes changes)
    ::
    =/  =args:ford:fusion
      [zuse:(need fer.dom) ank.dom deletes changes lat.ran fod.dom]
    ::
    =^  change-cages  ford-cache.args
      (checkout-changes args changes)
    =/  sane-continuation  (sane-changes changes change-cages)
    =/  new-blobs=(map lobe blob)
      %-  malt
      %+  turn  ~(tap by change-cages)
      |=  [=path =lobe =cage]
      [lobe %direct lobe [p q.q]:cage]
    =/  data=(map path lobe)
      %-  ~(urn by new-data)
      |=  [=path value=(each page lobe)]
      ?-  -.value
        %|  p.value
        %&  lobe:(~(got by change-cages) path)
      ==
    ::  if we didn't change the data and it's not a merge commit, abort
    ::
    ::  very important to keep all permanent changes below this point
    ::
    ?:  &(=([r.old-yaki ~] p.p.yoki) =(data q.old-yaki))
      ..park
    =/  =yaki
      ?-  -.yoki
        %&  (make-yaki p.p.yoki data now)
        %|  ?>  =(data q.p.yoki)
            p.yoki
      ==
    =:  let.dom  +(let.dom)
        hit.dom  (~(put by hit.dom) +(let.dom) r.yaki)
        hut.ran  (~(put by hut.ran) r.yaki yaki)
        lat.ran  (~(uni by new-blobs) lat.ran)
      ==
    =.  file-store.args  lat.ran
    ::
    =^  ankh  ford-cache.args
      (checkout-ankh args deletes change-cages ank.dom)
    =/  null  (sane-ankh sane-continuation ankh)
    =.  ankh.args  ankh
    =.  ank.dom  ankh
    =^  mim  ford-cache.args
      (checkout-mime args deletes ~(key by changes))
    =.  mim.dom  (apply-changes-to-mim mim.dom mim)
    =.  fod.dom  ford-cache.args
    =.  ..park  (emil (print q.old-yaki data))
    ::
    wake:(ergo mim)
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
    ::  Keep any parts of the ford cache whose dependencies didn't change
    ::
    ::    Make sure to invalidate any paths whose '-'s or '/'s could be
    ::    converted in an import; i.e. /mar, /lib, and /sur hoon files.
    ::
    ++  promote-ford
      |=  [=ford-cache deletes=(set path) changes=(set path)]
      ^+  ford-cache
      =/  invalid=(set path)  (~(uni in deletes) changes)
      =.  invalid
        %-  ~(gas in invalid)
        %-  zing
        %+  turn  ~(tap in invalid)
        |=  pax=path
        ^-  (list path)
        =/  xap=path  (flop pax)
        ?.  &(=(%hoon (head xap)) ?=([?(%mar %sur %lib) @ @ *] pax))
          ~
        =-  (turn - |=(suf=path [i.pax (snoc suf %hoon)]))
        %-  segments
        %-  crip
        =/  xup  (tail xap)                ::  lose %hoon extension
        =/  pux  (tail (flop xup))         ::  lose static prefix
        %+  turn  (tail (spud pux))        ::  lose leading '/'
        |=(c=@tD `@tD`?:(=('/' c) '-' c))  ::  convert '/' to '-'
      ::
      :*  ((invalidate path vase) vases.ford-cache invalid)
          ((invalidate mark dais) marks.ford-cache invalid)
          ((invalidate mars tube) casts.ford-cache invalid)
      ==
    ::
    ++  invalidate
      |*  [key=mold value=mold]
      |=  [cache=(map key [value dez=(set path)]) invalid=(set path)]
      =/  builds=(list [key value dez=(set path)])  ~(tap by cache)
      |-  ^+  cache
      ?~  builds
        ~
      ?:  ?=(^ (~(int in dez.i.builds) invalid))
        $(builds t.builds)
      (~(put by $(builds t.builds)) i.builds)
    ::
    ++  build-reef
      |=  $:  fer=(unit reef-cache)
              invalid=(set path)
              data=(map path (each page lobe))
          ==
      ^-  reef-cache
      ?:  =(%home syd)
        [!>(..ride) !>(..is) !>(..zuse)]
      |^
      ?:  |(?=(~ fer) (~(has in invalid) /sys/hoon/hoon))
        =/  [home=? hoon=vase]
          ?:  (same-as-home /sys/hoon/hoon)
            &+!>(..ride)
          |+build-hoon
        :-  hoon
        =/  [home=? arvo=vase]
          ?:  &(home (same-as-home /sys/arvo/hoon))
            &+!>(..is)
          |+(build-arvo hoon)
        :-  arvo
        ?:  &(home (same-as-home /sys/zuse/hoon))
          !>(..zuse)
        (build-zuse arvo)
      :-  hoon.u.fer
      ?:  (~(has in invalid) /sys/arvo/hoon)
        =/  [home=? arvo=vase]
          ?:  &((same-as-home /sys/hoon/hoon) (same-as-home /sys/arvo/hoon))
            &+!>(..is)
          |+(build-arvo hoon.u.fer)
        :-  arvo
        ?:  &(home (same-as-home /sys/zuse/hoon))
          !>(..zuse)
        (build-zuse arvo)
      :-  arvo.u.fer
      ?:  (~(has in invalid) /sys/zuse/hoon)
        ?:  ?&  (same-as-home /sys/hoon/hoon)
                (same-as-home /sys/arvo/hoon)
                (same-as-home /sys/zuse/hoon)
            ==
          !>(..zuse)
        (build-zuse arvo.u.fer)
      zuse.u.fer
      ::
      ++  build-hoon
        %-  road  |.
        ~>  %slog.0^leaf+"clay: building hoon on {<syd>}"
        =/  gen
          ~>  %mean.%hoon-parse-fail
          (path-to-hoon data /sys/hoon/hoon)
        ~>  %mean.%hoon-compile-fail
        (slot 7 (slap !>(0) gen))
      ::
      ++  build-arvo
        |=  hoon=vase
        %-  road  |.
        ~>  %slog.0^leaf+"clay: building arvo on {<syd>}"
        =/  gen
          ~>  %mean.%arvo-parse-fail
          (path-to-hoon data /sys/arvo/hoon)
        ~>  %mean.%arvo-compile-fail
        (slap (slap hoon gen) !,(*^hoon ..is))
      ::
      ++  build-zuse
        |=  arvo=vase
        %-  road  |.
        ~>  %slog.0^leaf+"clay: building zuse on {<syd>}"
        =/  gen
          ~>  %mean.%zuse-parse-fail
          (path-to-hoon data /sys/zuse/hoon)
        ~>  %mean.%zuse-compile-fail
        (slap arvo gen)
      ::
      ++  same-as-home
        |=  =path
        ^-  ?
        =/  our-lobe=lobe
          =/  datum  (~(got by data) path)
          ?-  -.datum
            %&  (page-to-lobe %hoon (page-to-cord p.datum))
            %|  p.datum
          ==
        =/  =dome  dom:(~(got by dos.rom) %home)
        =/  =yaki  (~(got by hut.ran) (~(got by hit.dome) let.dome))
        =(`our-lobe (~(get by q.yaki) path))
      --
    ::
    ++  page-to-cord
      |=  =page
      ^-  @t
      ?+  p.page  ~|([%sys-bad-mark p.page] !!)
        %hoon  ;;(@t q.page)
        %mime  q.q:;;(mime q.page)
      ==
    ::
    ++  path-to-hoon
      |=  [data=(map path (each page lobe)) =path]
      (rain path (path-to-cord data path))
    ::
    ++  path-to-cord
      |=  [data=(map path (each page lobe)) =path]
      ^-  @t
      =/  datum  (~(got by data) path)
      ?-  -.datum
        %&  (page-to-cord p.datum)
        %|  (lobe-to-cord p.datum)
      ==
    ::
    ++  lobe-to-cord
      |=  =lobe
      ^-  @t
      =-  ?:(?=(%& -<) p.- (of-wain:format p.-))
      |-  ^-  (each @t wain)
      =/  =blob  (~(got by lat.ran) lobe)
      ?-    -.blob
          %direct  [%& ;;(@t q.q.blob)]
          %delta
        :-  %|
        %+  lurk:differ
          =-  ?:(?=(%| -<) p.- (to-wain:format p.-))
          $(lobe q.q.blob)
        ;;((urge cord) q.r.blob)
      ==
    ::
    ::  Updated q.yaki
    ::
    ++  checkout-changes
      |=  [=ford=args:ford:fusion changes=(map path (each page lobe))]
      =/  cans=(list [=path change=(each page lobe)])  ~(tap by changes)
      |-  ^-  [(map path [=lobe =cage]) ford-cache]
      ?~  cans
        [~ ford-cache.ford-args]
      =^  cage  ford-cache.ford-args
        ::  ~>  %slog.[0 leaf+"clay: validating {(spud path.i.cans)}"]
        %-  wrap:fusion
        (get-value:(ford:fusion ford-args) path.i.cans)
      =/  =lobe
        ?-  -.change.i.cans
          %|  p.change.i.cans
          ::  Don't use p.change.i.cans because that's before casting to
          ::  the correct mark.
          ::
          %&  (page-to-lobe [p q.q]:cage)
        ==
      =^  so-far  ford-cache.ford-args  $(cans t.cans)
      [(~(put by so-far) path.i.cans lobe cage) ford-cache.ford-args]
    ::
    ::  Update ankh
    ::
    ++  checkout-ankh
      |=  $:  =ford=args:ford:fusion
              deletes=(set path)
              changes=(map path [lobe cage])
              =ankh
          ==
      ^+  [ankh ford-cache.ford-args]
      ::  Delete
      ::
      =.  ankh
        =/  dels  ~(tap in deletes)
        |-  ^-  ^ankh
        =*  outer-loop  $
        ?~  dels
          ankh
        =.  ankh
          |-  ^-  ^ankh
          =*  inner-loop  $
          ?~  i.dels
            ankh(fil ~)
          %=    ankh
              dir
            %+  ~(put by dir.ankh)  i.i.dels
            %=  inner-loop
              i.dels  t.i.dels
              ankh    (~(gut by dir.ankh) i.i.dels *^ankh)
            ==
          ==
        outer-loop(dels t.dels)
      ::  Add/change
      ::
      =/  cans=(list [=path =lobe =cage])  ~(tap by changes)
      |-  ^+  [ankh ford-cache.ford-args]
      =*  outer-loop  $
      ?~  cans
        [ankh ford-cache.ford-args]
      =^  new-ankh  ford-cache.ford-args
        |-  ^+  [ankh ford-cache.ford-args]
        =*  inner-loop  $
        ?^  path.i.cans
          =^  child-ankh  ford-cache.ford-args
            %=  inner-loop
              path.i.cans  t.path.i.cans
              ankh         (~(gut by dir.ankh) i.path.i.cans *^ankh)
            ==
          :-  ankh(dir (~(put by dir.ankh) i.path.i.cans child-ankh))
          ford-cache.ford-args
        [ankh(fil `[lobe.i.cans cage.i.cans]) ford-cache.ford-args]
      =.  ankh  new-ankh
      outer-loop(cans t.cans)
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
      ?:  =(0 let.dom)
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
        [u.hun %give %note prefix (path-to-tank path)]
      ::
      ++  path-to-tank
        |=  =path
        =/  pre=^path  ~[(scot %p our) syd (scot %ud +(let.dom))]
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
      ::  Assert all blobs hash to their lobe
      ::
      =/  foo
        %-  ~(urn by lat.ran)
        |=  [=lobe =blob]
        ?:  ?=(%delta -.blob)
          ~
        =/  actual-lobe=^lobe  `@uv`(page-to-lobe q.blob)
        ~|  [lobe p.blob actual-lobe]
        ?>  &(=(lobe p.blob) =(lobe actual-lobe))
        ~
      ::  Assert we calculated the same change-cages w/o cache
      ::
      ::  XX remove deletes
      ::
      =/  all-changes=(map path (each page lobe))
        =/  original=(map path (each page lobe))
          (~(run by q.yaki) |=(=lobe |+lobe))
        (~(uni by original) changes)
      =/  =args:ford:fusion
        [zuse:(need fer.dom) *ankh ~ all-changes lat.ran *ford-cache]
      =^  all-change-cages  ford-cache.args
        (checkout-changes args all-changes)
      =/  ccs=(list [=path =lobe =cage])  ~(tap by change-cages)
      |-  ^+  *sane-changes
      ?^  ccs
        ?.  =(`[lobe cage]:i.ccs (~(get by all-change-cages) path.i.ccs))
          ~|  not-same-cages+path.i.ccs
          !!
        $(ccs t.ccs)
      `[all-change-cages args]
    ::
    ++  sane-ankh
      |=  $:  $=  cont
              (unit [all-changes=(map path [lobe cage]) =ford=args:ford:fusion])
              =test=ankh
          ==
      ?.  check-sane
        ~
      ::  Assert all new lobes are reachable.
      ::
      ::  Needs to run after dome is updated
      ::
      =/  tak=(unit tako)  (~(get by hit.dom) let.dom)
      ?~  tak
        ~
      =/  =yaki  (~(got by hut.ran) u.tak)
      =/  files=(list [=path =lobe])  ~(tap by q.yaki)
      |-  ^+  *sane-ankh
      ?^  files
        ?.  (~(has by lat.ran) lobe.i.files)
          ~|  missing-lobe=[path lobe]
          !!
        $(files t.files)
      ::
      ::  Assert we can rebuild the ankh
      ::
      ?~  cont
        ~
      =+  u.cont
      =^  ankh  ford-cache.ford-args
        (checkout-ankh ford-args ~ all-changes *ankh)
      =|  =path
      |-  ^-  ~
      =*  loop  $
      =/  fil   (bind fil.ankh |=([=lobe =cage] [lobe p.cage q.q.cage]))
      =/  test  (bind fil.ankh |=([=lobe =cage] [lobe p.cage q.q.cage]))
      ?.  =(fil test)
        ~|  [%not-same-file path ?=(~ fil.ankh) ?=(~ fil.test-ankh)]
        ~|  ?~(fil.ankh ~ [[p p.q]:u.fil.ankh `@uv`(page-to-lobe [p q.q]:q.u.fil.ankh)])
        ~|  ?~(fil.test-ankh ~ [[p p.q]:u.fil.test-ankh `@uv`(page-to-lobe [p q.q]:q.u.fil.test-ankh)])
        !!
      ?.  =(~(key by dir.ankh) ~(key by dir.test-ankh))
        ~|  [%not-same-children path ~(key by dir.ankh) ~(key by dir.test-ankh)]
        !!
      =<  ~
      %+  turn  ~(tap by dir.ankh)
      |=  [=@ta =child=^ankh]
      ~|  sane-ankh=[path ta]
      %=  loop
        path       (snoc path ta)
        ankh       child-ankh
        test-ankh  (~(got by dir.test-ankh) ta)
      ==
    ::
    ::  Find /sys changes; does not reload on first commit
    ::
    ++  need-sys-update
      |=  changes=(map path (each page lobe))
      ^-  (map path (each page lobe))
      ~+
      ?:  =(0 let.dom)
        ~
      %-  malt
      %+  skim  ~(tap by changes)
      |=  [=path *]
      ?|  =(/sys/hoon/hoon path)
          =(/sys/arvo/hoon path)
          =(/sys/zuse/hoon path)
      ==
    ::
    ++  need-vane-update
      |=  changes=(map path (each page lobe))
      ^-  (map path (each page lobe))
      ~+
      ?:  =(0 let.dom)
        ~
      %-  malt
      %+  skim  ~(tap by changes)
      |=  [=path *]
      =(/sys/vane (scag 2 path))
    ::
    ::  Delay current update until sys update is complete
    ::
    ++  sys-update
      |=  $:  =yoki
              data=(map path (each page lobe))
              changes=(map path (each page lobe))
          ==
      ^+  ..park
      =/  updates
        %-  ~(uni by (need-sys-update changes))
        (need-vane-update changes)
      ?>  =(~ pud)
      =.  pud  `[syd yoki]
      |^  %.  [hen %slip %c %pork ~]
          =<  emit
          ?:  (~(has by updates) /sys/hoon/hoon)
            (reset &)
          ?:  (~(has by updates) /sys/arvo/hoon)
            (reset |)
          ?:  (~(has by updates) /sys/zuse/hoon)
            reboot
          =/  vanes=(list [=path *])  ~(tap by updates)
          |-  ^+  ..park
          ?~  vanes
            ..park
          ?.  ?=([%sys %vane * %hoon ~] path.i.vanes)
            ~&  [%strange-sys-update path.i.vanes]
            $(vanes t.vanes)
          =.  ..park  (reload i.t.t.path.i.vanes)
          $(vanes t.vanes)
      ::
      ++  reset
        |=  new-hoon=?
        ^+  ..park
        ?.  new-hoon
          =/  arvo=@t  (path-to-cord data /sys/arvo/hoon)
          =.  ..park  (pass-lyra hoon=~ arvo)
          reboot
        =/  hoon=@t  (path-to-cord data /sys/hoon/hoon)
        =/  arvo=@t  (path-to-cord data /sys/arvo/hoon)
        =.  ..park  (pass-lyra `hoon arvo)
        reboot
      ::
      ++  pass-lyra
        |=  [hoon=(unit @t) arvo=@t]
        ^+  ..park
        (emit hen %pass /reset %d %flog %lyra hoon arvo)
      ::
      ++  reboot
        =/  zuse=@t  (path-to-cord data /sys/zuse/hoon)
        =.  ..park
          %-  emit
          [hen %pass /reboot %d %flog %veer %$ /sys/zuse/hoon zuse]
        reload-all
      ::
      ++  reload-all
        =/  vanes=(list term)
          ~[%ames %behn %clay %dill %eyre %gall %iris %jael]
        |-  ^+  ..park
        ?~  vanes
          ..park
        =.  ..park  (reload i.vanes)
        $(vanes t.vanes)
      ::
      ++  reload
        |=  =term
        =/  vane=@t  (path-to-cord data /sys/vane/[term]/hoon)
        %-  emit
        =/  tip  (end 3 1 term)
        =/  =path  /sys/vane/[term]/hoon
        [hen %pass /reload %d %flog %veer tip path vane]
      --
    --
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
  ++  merge
    |=  [=ali=ship =ali=desk =germ =riot]
    ^+  ..merge
    |^
    ?~  riot
      (done %| %ali-unavailable >[ali-ship ali-desk germ]< ~)
    =/  ali-dome=dome:clay  !<(dome:clay q.r.u.riot)
    =/  ali-yaki=yaki  (~(got by hut.ran) (~(got by hit.ali-dome) let.ali-dome))
    =/  bob-yaki=(unit yaki)
      ?~  let.dom
        ~
      (~(get by hut.ran) (~(got by hit.dom) let.dom))
    =/  merge-result  (merge-by-germ ali-yaki bob-yaki)
    ?:  ?=(%| -.merge-result)
      (done %| p.merge-result)
    ?~  p.merge-result
      (done %& ~)
    =.  ..merge  (done %& conflicts.u.p.merge-result)
    (park | new.u.p.merge-result ~ lat.u.p.merge-result)
    ::
    ++  done
      |=  result=(each (set path) (pair term tang))
      ^+  ..merge
      (emit hen %give %mere result)
    ::
    +$  merge-result  [conflicts=(set path) new=yoki lat=(map lobe blob)]
    ++  merge-by-germ
      |=  [=ali=yaki bob-yaki=(unit yaki)]
      ^-  (each (unit merge-result) [term tang])
      ::
      ::  If this is an %init merge, we set the ali's commit to be
      ::  bob's.
      ::
      ?:  ?=(%init germ)
        ?>  ?=(~ bob-yaki)
        &+`[conflicts=~ new=|+ali-yaki lat=~]
      ::
      =/  bob-yaki  (need bob-yaki)
      |^
      ^-  (each (unit merge-result) [term tang])
      ?-    germ
      ::
      ::  If this is a %only-this merge, we check to see if ali's and bob's
      ::  commits are the same, in which case we're done.  Otherwise, we
      ::  check to see if ali's commit is in the ancestry of bob's, in
      ::  which case we're done.  Otherwise, we create a new commit with
      ::  bob's data plus ali and bob as parents.
      ::
          %only-this
        ?:  =(r.ali-yaki r.bob-yaki)
          &+~
        ?:  (~(has in (reachable-takos:ze r.bob-yaki)) r.ali-yaki)
          &+~
        :*  %&  ~
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
          &+~
        :*  %&  ~
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
          &+~
        ?:  (~(has in (reachable-takos:ze r.bob-yaki)) r.ali-yaki)
          &+~
        =/  new-data  (~(uni by q.ali-yaki) q.bob-yaki)
        :*  %&  ~
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
          &+~
        =/  new-data  (~(uni by q.bob-yaki) q.ali-yaki)
        :*  %&  ~
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
          &+~
        ?:  (~(has in (reachable-takos:ze r.bob-yaki)) r.ali-yaki)
          &+~
        ?.  (~(has in (reachable-takos:ze r.ali-yaki)) r.bob-yaki)
          :~  %|  %bad-fine-merge
              leaf+"tried fast-forward but is not ancestor or descendant"
          ==
        &+`[conflicts=~ new=|+ali-yaki lat=~]
      ::
          ?(%meet %mate %meld %meet-this %meet-that)
        ?:  =(r.ali-yaki r.bob-yaki)
          &+~
        ?:  (~(has in (reachable-takos:ze r.bob-yaki)) r.ali-yaki)
          &+~
        ?:  (~(has in (reachable-takos:ze r.ali-yaki)) r.bob-yaki)
          $(germ %fine)
        =/  merge-points  (find-merge-points ali-yaki bob-yaki)
        ?~  merge-points
          :~  %|  %merge-no-merge-base
              leaf+"consider a %this or %that merge to get a mergebase"
          ==
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
          :~  %|  %meet-conflict
            >~(key by both-diffs)<
            leaf+"consider a %mate merge"
          ==
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
        :*  %&  ~
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
          `[path (diff-lobes lobe u.in-yak)]
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
        ^-  cage
        =^  =page  fod.dom
          %-  wrap:fusion
          (lobe-to-page:(ford:fusion static-ford-args) lobe)
        =^  =cage  fod.dom
          %-  wrap:fusion
          (page-to-cage:(ford:fusion static-ford-args) page)
        cage
      ::
      ++  get-dais
        |=  =mark
        ^-  dais
        =^  =dais  fod.dom
          %-  wrap:fusion
          (get-mark:(ford:fusion static-ford-args) mark)
        dais
      ::
      ::  Diff two files on bob-desk
      ::
      ++  diff-lobes
        |=  [=a=lobe =b=lobe]
        ^-  cage
        =/  a-cage  (lobe-to-cage a-lobe)
        =/  b-cage  (lobe-to-cage b-lobe)
        ?>  =(p.a-cage p.b-cage)
        =/  =dais  (get-dais p.a-cage)
        [form:dais (~(diff dais q.a-cage) q.b-cage)]
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
        =/  res=(unit (unit vase))  (~(join dais bunt:dais) q.cal q.cob)
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
        ^-  (each (unit merge-result) [term tang])
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
          =/  =cage  (lobe-to-cage u.-)
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
          [%| %mate-conflict -]
        =/  old=(map path lobe)                         ::  oldies but goodies
          %+  roll  ~(tap by (~(uni by old.dal) old.dob))
          =<  .(old q.bob)
          |=  [[pax=path ~] old=(map path lobe)]
          (~(del by old) pax)
        =/  [hot=(map path lobe) lat=(map lobe blob)]   ::  new content
          %+  roll  ~(tap by both-patched)
          |=  [[pax=path cay=cage] hat=(map path lobe) lat=(map lobe blob)]
          =/  =blob  [%direct (page-to-lobe [p q.q]:cay) [p q.q]:cay]
          :-  (~(put by hat) pax p.blob)
          ?:  (~(has by lat) p.blob)
            lat
          (~(put by lat) p.blob blob)
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
        :*  %&  ~
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
  ::  Update mime cache
  ::
  ++  checkout-mime
    |=  $:  =ford=args:ford:fusion
            deletes=(set path)
            changes=(set path)
        ==
    ^-  [(map path (unit mime)) ford-cache]
    =/  mim=(map path (unit mime))
      =/  dels=(list path)  ~(tap by deletes)
      |-  ^-  (map path (unit mime))
      ?~  dels
        ~
      (~(put by $(dels t.dels)) i.dels ~)
    =/  cans=(list path)  ~(tap by changes)
    |-  ^-  [(map path (unit mime)) ford-cache]
    ?~  cans
      [mim ford-cache.ford-args]
    =^  cage  ford-cache.ford-args
      ~|  mime-cast-fail+i.cans
      (wrap:fusion (cast-path:(ford:fusion ford-args) i.cans %mime))
    =^  mim  ford-cache.ford-args  $(cans t.cans)
    [(~(put by mim) i.cans `!<(mime q.cage)) ford-cache.ford-args]
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
  ++  ergo
    |=  mim=(map path (unit mime))
    ^+  ..park
    =/  must  (must-ergo her syd mon (turn ~(tap by mim) head))
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
    |=  [our=ship syd=desk mon=(map term beam) can/(list path)]
    ^-  (map term (pair @ud (set path)))
    %-  malt  ^-  (list (trel term @ud (set path)))
    %+  murn  ~(tap by mon)
    |=  {nam/term bem/beam}
    ^-  (unit (trel term @ud (set path)))
    =-  ?~(- ~ `[nam (lent s.bem) (silt `(list path)`-)])
    %+  skim  can
    |=  pax/path
    &(=(p.bem our) =(q.bem syd) =((flop s.bem) (scag (lent s.bem) pax)))
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
    =.  mon  (~(put by mon) pot [her syd case] spur)
    =/  =yaki  (~(got by hut.ran) (~(got by hit.dom) let.dom))
    =/  changes  (~(run by q.yaki) |=(=lobe |+lobe))
    =/  =args:ford:fusion
      [zuse:(need fer.dom) ank.dom ~ changes lat.ran fod.dom]
    =^  mim  ford-cache.args
      (checkout-mime args ~ ~(key by changes))
    =.  mim.dom  (apply-changes-to-mim mim.dom mim)
    =.  fod.dom  ford-cache.args
    (ergo mim)
  ::
  ::  Set permissions for a node.
  ::
  ++  perm
    |=  {pax/path rit/rite}
    ^+  +>
    =/  mis/(set @ta)
      %+  roll
        =-  ~(tap in -)
        ?-  -.rit
          $r    who:(fall red.rit *rule)
          $w    who:(fall wit.rit *rule)
          $rw   (~(uni in who:(fall red.rit *rule)) who:(fall wit.rit *rule))
        ==
      |=  {w/whom s/(set @ta)}
      ?:  |(?=(%& -.w) (~(has by cez) p.w))  s
      (~(put in s) p.w)
    ?^  mis
      ::  TODO remove this nasty hack
      ::
      ?.  ?=([[%a *] *] hen)
        +>.$
      =-  (emit hen %give %done `[%perm-fail [%leaf "No such group(s): {-}"]~])
      %+  roll  ~(tap in `(set @ta)`mis)
      |=  {g/@ta t/tape}
      ?~  t  (trip g)
      :(weld t ", " (trip g))
    ::  TODO remove this nasty hack
    ::
    =<  ?.  ?=([[%a *] *] hen)
          .
        (emit hen %give %done ~)
    ::
    ?-  -.rit
      $r    wake(per (put-perm per pax red.rit))
      $w    wake(pew (put-perm pew pax wit.rit))
      $rw   wake(per (put-perm per pax red.rit), pew (put-perm pew pax wit.rit))
    ==
  ::
  ++  put-perm
    |=  {pes/regs pax/path new/(unit rule)}
    ?~  new  (~(del by pes) pax)
    (~(put by pes) pax u.new)
  ::
  ::  Remove a group from all rules.
  ::
  ++  forget-crew
    |=  nom/@ta
    %=  +>
      per  (forget-crew-in nom per)
      pew  (forget-crew-in nom pew)
    ==
  ::
  ++  forget-crew-in
    |=  {nom/@ta pes/regs}
    %-  ~(run by pes)
    |=  r/rule
    r(who (~(del in who.r) |+nom))
  ::
  ::  Cancel a request.
  ::
  ::  For local requests, we just remove it from `qyx`.  For foreign requests,
  ::  we remove it from `ref` and tell the foreign ship to cancel as well.
  ::
  ++  cancel-request                                    ::  release request
    ^+  ..cancel-request
    =^  wos/(list wove)  qyx
      :_  (~(run by qyx) |=(a/(set duct) (~(del in a) hen)))
      %-  ~(rep by qyx)
      |=  {{a/wove b/(set duct)} c/(list wove)}
      ?.((~(has in b) hen) c [a c])
    ::
    ?~  ref
      =>  .(ref `(unit rind)`ref)             ::  XX TMI
      ?:  =(~ wos)  ..cancel-request                    ::  XX handle?
      |-  ^+  ..cancel-request
      ?~  wos  ..cancel-request
      =.  ..cancel-request  (run-if-future rove.i.wos |=(@da (best hen +<)))
      $(wos t.wos)
    ::
    ?~  nux=(~(get by fod.u.ref) hen)
      ..cancel-request(ref `(unit rind)`ref)  ::  XX TMI
    =:  fod.u.ref  (~(del by fod.u.ref) hen)
        bom.u.ref  (~(del by bom.u.ref) u.nux)
      ==
    %.  [hen her u.nux [syd ~]]
    send-over-ames(ref `(unit rind)`ref)      ::  XX TMI
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
    =^  [new-sub=(unit rove) sub-results=(list sub-result)]  fod.dom
      (try-fill-sub for (rave-to-rove rav))
    =.  ..start-request  (send-sub-results sub-results [hen ~ ~])
    ?~  new-sub
      ..start-request
    (duce for u.new-sub)
  ::
  ::  Called when a foreign ship answers one of our requests.
  ::
  ::  If it's a `%many` request, process in +take-foreign-update
  ::
  ::  After updating ref (our request manager), we handle %x, %w, and %y
  ::  responses.  For %x, we call ++validate-x to validate the type of
  ::  the response.  For %y, we coerce the result to an arch.
  ::
  ++  take-foreign-answer                              ::  external change
    |=  [inx=@ud rut=(unit rand)]
    ^+  +>
    ?>  ?=(^ ref)
    =+  ruv=(~(get by bom.u.ref) inx)
    ?~  ruv  +>.$
    =/  rav=rave  rave.u.ruv
    ?:  ?=(%many -.rav)
      abet:(apex:(foreign-update inx) rut)
    ?~  rut
      ::  nothing here, so cache that
      ::
      %_    wake
          haw.u.ref
        ?.  ?=($sing -.rav)  haw.u.ref
        (~(put by haw.u.ref) mood.rav ~)
      ==
    |^
    =/  result=(unit cage)  (validate u.rut)
    =/  =mood  [p.p q.p q]:u.rut
    =:  haw.u.ref  (~(put by haw.u.ref) mood result)
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
          $a  ~|  %no-big-ford-builds-across-network-for-now  !!
          $b  ~|  %i-guess-you-ought-to-build-your-own-marks  !!
          $c  ~|  %casts-should-be-compiled-on-your-own-ship  !!
          $d  ~|  %totally-temporary-error-please-replace-me  !!
          $p  ~|  %requesting-foreign-permissions-is-invalid  !!
          $r  ~|  %no-cages-please-they-are-just-way-too-big  !!
          $s  ~|  %please-dont-get-your-takos-over-a-network  !!
          $t  ~|  %requesting-foreign-directory-is-vaporware  !!
          $u  ~|  %prolly-poor-idea-to-get-rang-over-network  !!
          $v  ~|  %weird-shouldnt-get-v-request-from-network  !!
          $z  `(validate-z r.rand)
          $w  `(validate-w r.rand)
          $x  (validate-x [p.p q.p q r]:rand)
          $y  `[p.r.rand !>(;;(arch q.r.rand))]
      ==
    ::
    ::  Make sure the incoming data is a %w response
    ::
    ++  validate-w
      |=  =page
      ^-  cage
      :-  p.page
      ?+  p.page  ~|  %strange-w-over-nextwork  !!
        $cass  !>(;;(cass q.page))
        $null  [[%atom %n ~] ~]
        $nako  !>(~|([%molding [&1 &2 &3]:q.page] ;;(nako q.page)))
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
        (page-to-cage:(ford:fusion static-ford-args) peg)
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
      !>(;;(@uvI q.page))
    --
  ::
  ::  Respond to backfill request
  ::
  ::  Maybe should verify the requester is allowed to access this blob?
  ::
  ++  give-backfill
    |=  =lobe
    ^+  ..give-backfill
    (emit hen %give %boon (~(got by lat.ran) lobe))
  ::
  ::  Ingest foreign update, requesting missing blobs if necessary
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
      =/  missing  (missing-blobs nako)
      =.  need.sat  `(list lobe)`(welp need.sat ~(tap in missing))
      =.  nako.sat  (~(put to nako.sat) ~ nako)
      work
    ::
    ++  missing-blobs
      |=  =nako
      ^-  (set lobe)
      =/  yakis  ~(tap in lar.nako)
      |-  ^-  (set lobe)
      =*  yaki-loop  $
      ?~  yakis
        ~
      =/  lobes=(list [=path =lobe])  ~(tap by q.i.yakis)
      |-  ^-  (set lobe)
      =*  blob-loop  $
      ?~  lobes
        yaki-loop(yakis t.yakis)
      ?:  (~(has by lat.ran) lobe.i.lobes)
        blob-loop(lobes t.lobes)
      (~(put in blob-loop(lobes t.lobes)) lobe.i.lobes)
    ::
    ::  Receive backfill response
    ::
    ++  take-backfill
      |=  =blob
      ^+  ..abet
      ?:  lost  ..abet
      =?    need.sat
          ?&  ?=(%delta -.blob)
              !(~(has by lat.ran) q.q.blob)
              !(~(has by have.sat) q.q.blob)
          ==
        [q.q.blob need.sat]
      ::  We can't put a blob in lat.ran if its parent isn't already
      ::  there.  Unions are in reverse order so we don't overwrite
      ::  existing blobs.
      ::
      =.  ..abet
        ?:  &(?=(%delta -.blob) !(~(has by lat.ran) q.q.blob))
          ..abet(have.sat (~(uni by (malt [p.blob `^blob`blob] ~)) have.sat))
        ..abet(lat.ran (~(uni by (malt [p.blob blob] ~)) lat.ran))
      work(busy.sat |)
    ::
    ::  Fetch next blob
    ::
    ++  work
      ^+  ..abet
      ?:  busy.sat
        ..abet
      |-  ^+  ..abet
      ?:  =(~ need.sat)
        ::  NB: if you change to release nakos as we get enough blobs
        ::  for them instead of all at the end, you *must* store the
        ::  `lim` that should be applied after the nako is complete and
        ::  not use the one in the rave, since that will apply to the
        ::  end of subscription.
        ::
        =.  lat.ran  (~(uni by have.sat) lat.ran)
        |-  ^+  ..abet
        ?:  =(~ nako.sat)
          ..abet
        =^  next=(unit nako)  nako.sat  ~(get to nako.sat)
        ?~  next
          ..abet(done &)
        =.  ..abet  (apply-foreign-update u.next)
        =.  ..foreign-update  =<(?>(?=(^ ref) .) wake)
        $
      ?>  ?=(^ need.sat)
      ::  This is what removes an item from `need`.  This happens every
      ::  time we take a backfill response, but it could happen more than
      ::  once if we somehow got this data in the meantime (maybe from
      ::  another desk updating concurrently, or a previous update on this
      ::  same desk).
      ::
      ?:  ?|  (~(has by lat.ran) i.need.sat)
              (~(has by have.sat) i.need.sat)
          ==
        $(need.sat t.need.sat)
      ::  Otherwise, fetch the next blob
      ::
      =/  =fill  [syd i.need.sat]
      =/  =wire  /back-index/(scot %p her)/[syd]/(scot %ud inx)
      =/  =path  [%backfill syd (scot %ud inx) ~]
      =.  ..foreign-update
        =<  ?>(?=(^ ref) .)
        (emit hen %pass wire %a %plea her %c path fill)
      ..abet(busy.sat &)
    ::
    ::  When we get a %w foreign update, store this in our state.
    ::
    ::  We get the commits and blobs from the nako and add them to our
    ::  object store, then we update the map of aeons to commits and the
    ::  latest aeon.
    ::
    ++  apply-foreign-update
      |=  =nako
      ^+  ..abet
      ::  hit: updated commit-hashes by @ud case
      ::  nut: new commit-hash/commit pairs
      ::  hut: updated commits by hash
      ::  nat: new blob-hash/blob pairs
      ::  lat: updated blobs by hash
      ::
      =/  hit  (~(uni by hit.dom) gar.nako)
      =/  nut  (turn ~(tap in lar.nako) |=(=yaki [r.yaki yaki]))
      =/  hut  (~(uni by (malt nut)) hut.ran)
      =/  nat  (turn ~(tap in bar.nako) |=(=blob [p.blob blob]))
      =/  lat  (~(uni by (malt nat)) lat.ran)
      ::  traverse updated state and sanity check
      ::
      =+  ~|  :*  %bad-foreign-update
                  [gar=gar.nako let=let.nako nut=(turn nut head) nat=(turn nat head)]
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
        =+  %+  turn
              ~(tap by q.yaki)
            |=  [=path =lobe]
            ~|  [%missing-blob path lobe]
            ?>  (~(has by lat) lobe)
            ~
        ?:  =(let.nako aeon)
          ~
        $(aeon +(aeon))
      ::  produce updated state
      ::
      =/  =rave  rave:(~(got by bom.u.ref) inx)
      ?>  ?=(%many -.rave)
      =:  let.dom   (max let.nako let.dom)
          hit.dom   hit
          hut.ran   hut
          lat.ran   lat
          ::  Is this correct?  Seeems like it should only go to `to` if
          ::  we've gotten all the way to the end.  Leaving this
          ::  behavior unchanged for now, but I believe it's wrong.
          ::
          lim       ?.(?=(%da -.to.moat.rave) lim p.to.moat.rave)
        ==
      ..abet
    --
  ::
  ::  fire function if request is in future
  ::
  ++  run-if-future
    |=  [rov=rove fun=$-(@da _.)]
    ^+  +>.$
    %+  fall
      %+  bind
        ^-  (unit @da)
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
      fun
    +>.$
  ::
  ++  rave-to-rove
    |=  rav/rave
    ^-  rove
    ?-  -.rav
      %sing  rav
      %next  [- mood ~ ~]:rav
      %mult  [- mool ~ ~ ~]:rav
      %many  [- track moat ~]:rav
    ==
  ::
  ++  rove-to-rave
    |=  rov/rove
    ^-  rave
    ?-  -.rov
      %sing  rov
      %next  [- mood]:rov
      %mult  [- mool]:rov
      %many  [- track moat]:rov
    ==
  ::
  ++  send-sub-results
    |=  [sub-results=(list sub-result) ducts=(set duct)]
    ^+  ..wake
    ?~  sub-results
      ..wake
    =.  ..wake
      ?-  -.i.sub-results
        %blab  (blab-all ducts +.i.sub-results)
        %bleb  (bleb-all ducts +.i.sub-results)
        %balk  (balk-all ducts +.i.sub-results)
        %blas  (blas-all ducts +.i.sub-results)
        %blub  (blub-all ducts +.i.sub-results)
      ==
    $(sub-results t.sub-results)
  ::
  ::  Loop through open subscriptions and check if we can fill any of
  ::  them.
  ::
  ++  wake
    ^+  .
    =/  old-subs=(list [=wove ducts=(set duct)])  ~(tap by qyx)
    =|  new-subs=(list [=wove ducts=(set duct)])
    |-  ^+  ..wake
    ?~  old-subs
      ::  install new subs
      ::
      ..wake(qyx (~(gas by *cult) new-subs))
    ?:  =(~ ducts.i.old-subs)
      ::  drop forgotten roves
      ::
      $(old-subs t.old-subs)
    =^  [new-sub=(unit rove) sub-results=(list sub-result)]  fod.dom
      (try-fill-sub wove.i.old-subs)
    =.  ..wake  (send-sub-results sub-results ducts.i.old-subs)
    =.  new-subs
      ?~  new-sub
        new-subs
      [[[for.wove.i.old-subs u.new-sub] ducts.i.old-subs] new-subs]
    $(old-subs t.old-subs)
  ::
  ::  Try to fill a subscription
  ::
  ++  try-fill-sub
    |=  [far=(unit [=ship ver=@ud]) rov=rove]
    ^-  [[new-sub=(unit rove) (list sub-result)] ford-cache]
    =/  for=(unit ship)  ?~(far ~ `ship.u.far)
    ?-    -.rov
        %sing
      =/  cache-value=(unit (unit cage))
        ?~(ref ~ (~(get by haw.u.ref) mood.rov))
      ?^  cache-value
        ::  if we have a result in our cache, produce it
        ::
        :_  fod.dom
        :-  ~
        ?~  u.cache-value
          [%blub ~]~
        [%blab mood.rov %& u.u.cache-value]~
      ::  else, check to see if rove is for an aeon we know
      ::
      =/  aeon=(unit aeon)  (case-to-aeon case.mood.rov)
      ?~  aeon
        [[`rov ~] fod.dom]
      ::  we have the appropriate aeon, so read in the data
      ::
      =^  value=(unit (unit (each cage lobe)))  fod.dom
        (read-at-aeon:ze for u.aeon mood.rov)
      ?~  value
        ::  We don't have the data directly, which is potentially
        ::  problematical.  How can we fetch the data?
        ::
        ?:  =(0 u.aeon)
          ~&  [%clay-sing-indirect-data-0 `path`[syd '0' path.mood.rov]]
          [[~ ~] fod.dom]
        ~&  [%clay-sing-indirect-data desk=syd mood=mood.rov aeon=u.aeon]
        [[`rov ~] fod.dom]
      ::  we have the data, so we produce the results
      ::
      [[~ [%balk u.value mood.rov]~] fod.dom]
    ::
    ::  %next is just %mult with one path, so we pretend %next = %mult here.
    ::
        ?(%next %mult)
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
      ::  we will either respond or store the maybe updated request.
      ::
      =;  [res=(each (map mood (unit (each cage lobe))) rove) fod=ford-cache]
          :_  fod
          ?:  ?=(%& -.res)
            (respond p.res)
          (store p.res)
      ::  recurse here on next aeon if possible/needed.
      ::
      |-  ^-  [(each (map mood (unit (each cage lobe))) rove) ford-cache]
      ::  if we don't have an aeon yet, see if we have one now.
      ::
      ?~  aeon.rov
        =/  aeon=(unit aeon)  (case-to-aeon case.mool.rov)
        ::  if we still don't, wait.
        ::
        ?~  aeon  [|+rov fod.dom]
        ::  if we do, update the request and retry.
        ::
        $(aeon.rov `+(u.aeon), old-cach.rov ~, new-cach.rov ~)
      ::  if old isn't complete, try filling in the gaps.
      ::
      =^  o  fod.dom
        ?:  (complete old-cach.rov)
          [old-cach.rov fod.dom]
        (read-unknown mool.rov(case [%ud (dec u.aeon.rov)]) old-cach.rov)
      =.  old-cach.rov  o
      ::  if the next aeon we want to compare is in the future, wait again.
      ::
      =/  next-aeon=(unit aeon)  (case-to-aeon [%ud u.aeon.rov])
      ?~  next-aeon  [|+rov fod.dom]
      ::  if new isn't complete, try filling in the gaps.
      ::
      =^  n  fod.dom
        ?:  (complete new-cach.rov)
          [new-cach.rov fod.dom]
        (read-unknown mool.rov(case [%ud u.aeon.rov]) new-cach.rov)
      =.  new-cach.rov  n
      ::  if they're still not both complete, wait again.
      ::
      ?.  ?&  (complete old-cach.rov)
              (complete new-cach.rov)
          ==
        [|+rov fod.dom]
      ::  both complete, so check if anything has changed
      ::
      =/  changes=(map mood (unit (each cage lobe)))
        %+  roll  ~(tap by old-cach.rov)
        |=  $:  [[car=care pax=path] old-cach-value=cach]
                changes=(map mood (unit (each cage lobe)))
            ==
        =/  new-cach-value=cach  (~(got by new-cach.rov) car pax)
        ?<  |(?=(~ old-cach-value) ?=(~ new-cach-value))
        =/  new-entry=(unit (pair mood (unit (each cage lobe))))
          =/  =mood  [car [%ud u.aeon.rov] pax]
          ?~  u.old-cach-value
            ?~  u.new-cach-value
              ::  not added
              ::
              ~
            ::  added
            ::
            `[mood `u.u.new-cach-value]
          ?~  u.new-cach-value
            ::  deleted
            ::
            `[mood ~]
          ?:  (equivalent-data:ze u.u.new-cach-value u.u.old-cach-value)
            ::  unchanged
            ::
            ~
          ::  changed
          ::
          `[mood `u.u.new-cach-value]
        ::  if changed, save the change
        ::
        ?~  new-entry
          changes
        (~(put by changes) u.new-entry)
      ::  if there are any changes, send response. if none, move on to
      ::  next aeon.
      ::
      ?^  changes  [&+changes fod.dom]
      $(u.aeon.rov +(u.aeon.rov), new-cach.rov ~)
      ::
      ::  check again later
      ::
      ++  store
        |=  rov=rove
        ^-  [new-sub=(unit rove) (list sub-result)]
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
        |=  res=(map mood (unit (each cage lobe)))
        ^-  [new-sub=(unit rove) (list sub-result)]
        :-  ~
        ?:  ?=(%mult -.vor)
          [%blas ~(key by res)]~
        ?>  ?=([* ~ ~] res)
        ?~  q.n.res
          [%blub ~]~
        [%blab [p u.q]:n.res]~
      ::
      ::  no unknowns
      ::
      ++  complete
        |=  hav=(map (pair care path) cach)
        ?&  ?=(^ hav)
            (levy ~(tap by `(map (pair care path) cach)`hav) know)
        ==
      ::
      ::  know about file in cach
      ::
      ++  know  |=({(pair care path) c/cach} ?=(^ c))
      ::
      ::  fill in the blanks
      ::
      ++  read-unknown
        |=  [=mool hav=(map (pair care path) cach)]
        ^-  [_hav ford-cache]
        =?  hav  ?=(~ hav)
          %-  malt  ^-  (list (pair (pair care path) cach))
          %+  turn
            ~(tap in paths.mool)
          |=  [c=care p=path]
          ^-  [[care path] cach]
          [[c p] ~]
        |-  ^+  [hav fod.dom]
        ?~  hav  [hav fod.dom]
        =^  lef  fod.dom  $(hav l.hav)
        =.  l.hav  lef
        =^  rig  fod.dom  $(hav r.hav)
        =.  r.hav  rig
        =/  [[=care =path] =cach]  n.hav
        ?^  cach
          [hav fod.dom]
        =^  q  fod.dom  (aver for care case.mool path)
        =.  q.n.hav  q
        [hav fod.dom]
      --
    ::
        %many
      :_  fod.dom
      =/  from-aeon  (case-to-aeon from.moat.rov)
      ?~  from-aeon
        ::  haven't entered the relevant range, so do nothing
        ::
        [`rov ~]
      =/  to-aeon  (case-to-aeon to.moat.rov)
      =/  ver  ?~(far %1 ver.u.far)
      ?~  to-aeon
        ::  we're in the middle of the range, so produce what we can,
        ::  but don't end the subscription
        ::
        ::  update "from" case to the aeon after now
        ::
        =.  from.moat.rov
          [%ud +(let.dom)]
        :-  `rov
        =/  new-lobes=(map path lobe)
          (lobes-at-path:ze for let.dom path.moat.rov)
        ?:  =(lobes.rov new-lobes)
          ::  if no changes, don't produce results
          ::
          ~
        ::  else changes, so produce them
        ::
        [%bleb ver let.dom ?:(track.rov ~ `[u.from-aeon let.dom])]~
      ::  we're past the end of the range, so end subscription
      ::
      :-  ~
      =/  new-lobes=(map path lobe)
        (lobes-at-path:ze for u.to-aeon path.moat.rov)
      ::  if changed, give subscription result
      ::
      =/  bleb=(list sub-result)
        ?:  =(lobes.rov new-lobes)
          ~
        [%bleb ver +(u.from-aeon) ?:(track.rov ~ `[u.from-aeon u.to-aeon])]~
      ::  end subscription
      ::
      =/  blub=(list sub-result)
        [%blub ~]~
      (weld bleb blub)
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
  ::  --  `ank` is the ankh, which is the file data itself.  An ankh is both
  ::      a possible file and a possible directory.  An ankh has both:
  ::      --  `fil`, a possible file, stored as both a cage and its hash
  ::      --  `dir`, a map of @ta to more ankhs.
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
    ::  These convert between aeon (version number), tako (commit hash), yaki
    ::  (commit data structure), lobe (content hash), and blob (content).
    ::
    ::    XX the following are duplicated from the +state core
    ::
    ++  aeon-to-tako  ~(got by hit.dom)
    ++  aeon-to-yaki  |=(=aeon (tako-to-yaki (aeon-to-tako aeon)))
    ++  lobe-to-blob  ~(got by lat.ran)
    ++  tako-to-yaki  ~(got by hut.ran)
    ++  lobe-to-mark
      |=  a/lobe
      =>  (lobe-to-blob a)
      ?-  -
        $delta      p.q
        $direct     p.q
      ==
    ::
    ::  Checks whether two pieces of data (either cages or lobes) are the same.
    ::
    ++  equivalent-data
      |=  {one/(each cage lobe) two/(each cage lobe)}
      ^-  ?
      ?:  ?=(%& -.one)
        ?:  ?=(%& -.two)
          =([p q.q]:p.one [p q.q]:p.two)
        =(p.two (page-to-lobe [p q.q]:p.one))
      ?:  ?=(%& -.two)
        =(p.one (page-to-lobe [p q.q]:p.two))
      =(p.one p.two)
    ::
    ::  Gets a map of the data at the given path and all children of it.
    ::
    ++  lobes-at-path
      |=  {for/(unit ship) yon/aeon pax/path}
      ^-  (map path lobe)
      ?:  =(0 yon)  ~
      ::  we use %z for the check because it looks at all child paths.
      ?.  |(?=(~ for) (may-read u.for %z yon pax))  ~
      %-  malt
      %+  skim
        %~  tap  by
        =<  q
        %-  aeon-to-yaki
        yon
      |=  {p/path q/lobe}
      ?|  ?=(~ pax)
          ?&  !?=(~ p)
              =(-.pax -.p)
              $(p +.p, pax +.pax)
      ==  ==
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
          $(hit.dom (~(del by hit.dom) let.dom), let.dom (dec let.dom))
        b
      ?:  =(0 b)
        [~ ~]
      (data-twixt-takos =(0 ver) (~(get by hit.dom) a) (aeon-to-tako b))
    ::
    ::  Traverse parentage and find all ancestor hashes
    ::
    ++  reachable-takos                                 ::  reachable
      |=  p/tako
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
    ::  Gets the data between two commit hashes, assuming the first is an
    ::  ancestor of the second.
    ::
    ::  Get all the takos before `a`, then get all takos before `b` except the
    ::  ones we found before `a`.  Then convert the takos to yakis and also get
    ::  all the data in all the yakis.
    ::
    ::  What happens if you run an %init merge on a desk that already
    ::  had a commit?
    ::
    ++  data-twixt-takos
      |=  [plops=? a=(unit tako) b=tako]
      ^-  [(set yaki) (set plop)]
      =+  old=?~(a ~ (reachable-takos u.a))
      =/  yal=(set tako)
          %-  silt
          %+  skip
            ~(tap in (reachable-takos b))
          |=(tak=tako (~(has in old) tak))
      :-  (silt (turn ~(tap in yal) tako-to-yaki))
      ?.  plops
        ~
      (silt (turn ~(tap in (new-lobes (new-lobes ~ old) yal)) lobe-to-blob))
    ::
    ::  Get all the lobes that are referenced in `a` except those that are
    ::  already in `b`.
    ::
    ++  new-lobes                                       ::  object hash set
      |=  {b/(set lobe) a/(set tako)}                   ::  that aren't in b
      ^-  (set lobe)
      %+  roll  ~(tap in a)
      |=  {tak/tako bar/(set lobe)}
      ^-  (set lobe)
      =+  yak=(tako-to-yaki tak)
      %+  roll  ~(tap by q.yak)
      =<  .(far bar)
      |=  {{path lob/lobe} far/(set lobe)}
      ^-  (set lobe)
      ?~  (~(has in b) lob)                             ::  don't need
        far
      =+  gar=(lobe-to-blob lob)
      ?-  -.gar
        $direct    (~(put in far) lob)
        $delta     (~(put in $(lob q.q.gar)) lob)
      ==
    ::
    ::  Probably can get rid of the cache checks because they happen in
    ::  ford
    ::
    ++  read-a
      !.
      |=  [=aeon =path]
      ^-  [(unit (unit (each cage lobe))) ford-cache]
      ?.  =(aeon let.dom)
        [~ fod.dom]
      =/  cached=(unit [=vase *])  (~(get by vases.fod.dom) path)
      ?^  cached
        :_(fod.dom [~ ~ %& %vase !>(vase.u.cached)])
      =/  x  (read-x aeon path)
      ?~  x
        [~ fod.dom]
      ?~  u.x
        [[~ ~] fod.dom]
      ::  should never happen at current aeon
      ?:  ?=(%| -.u.u.x)
        [~ fod.dom]
      =^  =vase  fod.dom
        %-  wrap:fusion
        (build-file:(ford:fusion static-ford-args) path)
      :_(fod.dom [~ ~ %& %vase !>(vase)])
    ::
    ++  read-b
      !.
      |=  [=aeon =path]
      ^-  [(unit (unit (each cage lobe))) ford-cache]
      ?.  =(aeon let.dom)
        [~ fod.dom]
      ?.  ?=([@ ~] path)
        [[~ ~] fod.dom]
      =/  cached=(unit [=dais *])  (~(get by marks.fod.dom) i.path)
      ?^  cached
        :_(fod.dom [~ ~ %& %dais !>(dais.u.cached)])
      =^  =dais  fod.dom
        %-  wrap:fusion
        (get-mark:(ford:fusion static-ford-args) i.path)
      :_(fod.dom [~ ~ %& %dais !>(dais)])
    ::
    ++  read-c
      !.
      |=  [=aeon =path]
      ^-  [(unit (unit (each cage lobe))) ford-cache]
      ?.  =(aeon let.dom)
        [~ fod.dom]
      ?.  ?=([@ @ ~] path)
        [[~ ~] fod.dom]
      =/  cached=(unit [=tube *])  (~(get by casts.fod.dom) [i i.t]:path)
      ?^  cached
        :_(fod.dom [~ ~ %& %tube !>(tube.u.cached)])
      =^  =tube  fod.dom
        %-  wrap:fusion
        (get-cast:(ford:fusion static-ford-args) [i i.t]:path)
      :_(fod.dom [~ ~ %& %tube !>(tube)])
    ::
    ::  Gets the permissions that apply to a particular node.
    ::
    ::  If the node has no permissions of its own, we use its parent's.
    ::  If no permissions have been set for the entire tree above the node,
    ::  we default to fully private (empty whitelist).
    ::
    ++  read-p
      |=  pax/path
      ^-  (unit (unit (each cage lobe)))
      =-  [~ ~ %& %noun !>(-)]
      :-  (read-p-in pax per.red)
      (read-p-in pax pew.red)
    ::
    ++  read-p-in
      |=  {pax/path pes/regs}
      ^-  dict
      =/  rul/(unit rule)  (~(get by pes) pax)
      ?^  rul
        :+  pax  mod.u.rul
        %-  ~(rep in who.u.rul)
        |=  {w/whom out/(pair (set ship) (map @ta crew))}
        ?:  ?=({%& @p} w)
          [(~(put in p.out) +.w) q.out]
        =/  cru/(unit crew)  (~(get by cez.ruf) +.w)
        ?~  cru  out
        [p.out (~(put by q.out) +.w u.cru)]
      ?~  pax  [/ %white ~ ~]
      $(pax (scag (dec (lent pax)) `path`pax))
    ::
    ++  may-read
      |=  {who/ship car/care yon/aeon pax/path}
      ^-  ?
      ?+  car
        (allowed-by who pax per.red)
      ::
          $p
        =(who our)
      ::
          ?($y $z)
        =+  tak=(~(get by hit.dom) yon)
        ?~  tak  |
        =+  yak=(tako-to-yaki u.tak)
        =+  len=(lent pax)
        =-  (levy ~(tap in -) |=(p/path (allowed-by who p per.red)))
        %+  roll  ~(tap in (~(del in ~(key by q.yak)) pax))
        |=  {p/path s/(set path)}
        ?.  =(pax (scag len p))  s
        %-  ~(put in s)
        ?:  ?=($z car)  p
        (scag +(len) p)
      ==
    ::
    ++  may-write
      |=  {w/ship p/path}
      (allowed-by w p pew.red)
    ::
    ++  allowed-by
      |=  {who/ship pax/path pes/regs}
      ^-  ?
      =/  rul/real  rul:(read-p-in pax pes)
      =/  in-list/?
        ?|  (~(has in p.who.rul) who)
          ::
            %-  ~(rep by q.who.rul)
            |=  {{@ta cru/crew} out/_|}
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
      =/  descendants/(list (pair path lobe))
          %+  turn
            %+  skim  ~(tap by (~(del by q.yaki) pax))
            |=  {paf/path lob/lobe}
            =(pax (scag len paf))
          |=  {paf/path lob/lobe}
          [(slag len paf) lob]
      =+  us=(~(get by q.yaki) pax)
      ?:  &(?=(~ descendants) ?=(~ us))
        *@uvI
      %+  roll
        ^-  (list (pair path lobe))
        [[~ ?~(us *lobe u.us)] descendants]
      |=({{path lobe} @uvI} (shax (jam +<)))
    ::  +read-r: %x wrapped in a vase
    ::
    ++  read-r
      |=  [yon=aeon pax=path]
      ^-  (unit (unit cage))
      =/  x  (read-x yon pax)
      ?~  x    ~
      ?~  u.x  [~ ~]
      ?>  ?=(%& -.u.u.x)
      ``[p.p.u.u.x !>(q.p.u.u.x)]
    ::  +read-s: produce yaki or blob for given tako or lobe
    ::
    ++  read-s
      |=  [yon=aeon pax=path]
      ^-  (unit (unit cage))
      ?.  ?=([?(%yaki %blob %hash %cage %open %late %base) * *] pax)
        `~
      ?-    i.pax
          %yaki
        =/  yak=(unit yaki)  (~(get by hut.ran) (slav %uv i.t.pax))
        ?~  yak
          ~
        ``yaki+[-:!>(*yaki) u.yak]
      ::
          %blob
        =/  bol=(unit blob)  (~(get by lat.ran) (slav %uv i.t.pax))
        ?~  bol
          ~
        ``blob+[-:!>(*blob) u.bol]
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
        =^  =page  fod.dom
          %-  wrap:fusion
          (lobe-to-page:(ford:fusion static-ford-args) lobe)
        =^  =cage  fod.dom
          %-  wrap:fusion
          (page-to-cage:(ford:fusion static-ford-args) page)
        ``cage+[-:!>(*^cage) cage]
      ::
          %open
        ``open+!>(prelude:(ford:fusion static-ford-args))
      ::
          %late  !!  :: handled in +aver
          %base
        ?>  ?=(^ t.t.pax)
        :^  ~  ~  %uvs  !>
        ^-  (list @uv)
        =/  him  (slav %p i.t.pax)
        =/  other  dom:((de our now ski hen ruf) him i.t.t.pax)
        ?:  =(0 let.other)
          ~
        =/  our-yaki  (~(got by hut.ran) (~(got by hit.dom) yon))
        =/  other-yaki  (~(got by hut.ran) (~(got by hit.other) let.other))
        %+  turn  ~(tap in (find-merge-points other-yaki our-yaki))
        |=  =yaki
        r.yaki
      ==
    ::  +read-t: produce the list of paths within a yaki with :pax as prefix
    ::
    ++  read-t
      |=  [yon=aeon pax=path]
      ^-  (unit (unit [%file-list (hypo (list path))]))
      ::  if asked for version 0, produce an empty list of files
      ::
      ?:  =(0 yon)
        ``[%file-list -:!>(*(list path)) *(list path)]
      ::  if asked for a future version, we don't have an answer
      ::
      ?~  tak=(~(get by hit.dom) yon)
        ~
      ::  look up the yaki snapshot based on the version
      ::
      =/  yak=yaki  (tako-to-yaki u.tak)
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
      |=  [yon=aeon pax=path]
      ^-  (unit (unit (each [%flag (hypo ?)] lobe)))
      ::  if asked for a future version, we don't have an answer
      ::
      ?~  tak=(~(get by hit.dom) yon)
        ~
      ::  look up the yaki snapshot based on the version
      ::
      =/  yak=yaki  (tako-to-yaki u.tak)
      ::  produce the result based on whether or not there's a file at :pax
      ::
      ``[%& %flag -:!>(*?) (~(has by q.yak) pax)]
    ::
    ::  Gets the dome (desk state) at a particular aeon.
    ::
    ::  For past aeons, we don't give an actual ankh in the dome, but the rest
    ::  of the data is legit. We also never send the mime cache over the wire.
    ::
    ++  read-v
      |=  {yon/aeon pax/path}
      ^-  (unit (unit {$dome (hypo dome:clay)}))
      ?:  (lth yon let.dom)
        :*  ~  ~  %dome  -:!>(*dome:clay)
            ^-  dome:clay
            :*  ank=`[[%ank-in-old-v-not-implemented *ankh] ~ ~]
                let=yon
                hit=(molt (skim ~(tap by hit.dom) |=({p/@ud *} (lte p yon))))
                lab=(molt (skim ~(tap by lab.dom) |=({* p/@ud} (lte p yon))))
        ==  ==
      ?:  (gth yon let.dom)
        ~
      ``[%dome -:!>(*dome:clay) [ank let hit lab]:dom]
    ::
    ::  Gets all cases refering to the same revision as the given case.
    ::
    ::  For the %da case, we give just the canonical timestamp of the revision.
    ::
    ++  read-w
      |=  cas/case
      ^-  (unit (unit (each cage lobe)))
      =+  aey=(case-to-aeon cas)
      ?~  aey  ~
      =-  [~ ~ %& %cass !>(-)]
      ^-  cass
      :-  u.aey
      ?:  =(0 u.aey)  `@da`0
      t:(aeon-to-yaki u.aey)
    ::
    ::  Get the data at a node.
    ::
    ::  If it's in our ankh (current state cache), we can just produce
    ::  the result.  Otherwise, we've got to look up the node at the
    ::  aeon to get the content hash, use that to find the blob, and use
    ::  the blob to get the data.  We also special-case the hoon mark
    ::  for bootstrapping purposes.
    ::
    ++  read-x
      |=  [yon=aeon pax=path]
      ^-  (unit (unit (each cage lobe)))
      ?:  =(0 yon)
        [~ ~]
      =+  tak=(~(get by hit.dom) yon)
      ?~  tak
        ~
      ?:  &(?=(~ ref) =(yon let.dom))
        :-  ~
        %+  bind
          fil.ank:(descend-path:(zu ank.dom) pax)
        |=(a/{p/lobe q/cage} [%& q.a])
      =+  yak=(tako-to-yaki u.tak)
      =+  lob=(~(get by q.yak) pax)
      ?~  lob
        [~ ~]
      =+  mar=(lobe-to-mark u.lob)
      ::  should convert any lobe to cage
      ::
      ?.  ?=($hoon mar)
        [~ ~ %| u.lob]
      :^  ~  ~  %&
      :+  mar  [%atom %t ~]
      |-  ^-  @t                      ::  (urge cord) would be faster
      =+  bol=(lobe-to-blob u.lob)
      ?:  ?=($direct -.bol)
        ;;(@t q.q.bol)
      ?>  ?=($delta -.bol)
      =+  txt=$(u.lob q.q.bol)
      ?>  ?=($txt-diff p.r.bol)
      =+  dif=;;((urge cord) q.r.bol)
      =,  format
      =+  pac=(of-wain (lurk:differ (to-wain (cat 3 txt '\0a')) dif))
      ?~  pac
        ''
      (end 3 (dec (met 3 pac)) pac)
    ::
    ::  Gets an arch (directory listing) at a node.
    ::
    ++  read-y
      |=  {yon/aeon pax/path}
      ^-  (unit (unit {$arch (hypo arch)}))
      ?:  =(0 yon)
        ``[%arch -:!>(*arch) *arch]
      =+  tak=(~(get by hit.dom) yon)
      ?~  tak
        ~
      =+  yak=(tako-to-yaki u.tak)
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
        |=  {paf/path lob/lobe}
        =(pax (scag len paf))
      |=  {paf/path lob/lobe}
      =+  pat=(slag len paf)
      [?>(?=(^ pat) i.pat) ~]
    ::
    ::  Gets a recursive hash of a node and all its children.
    ::
    ++  read-z
      |=  {yon/aeon pax/path}
      ^-  (unit (unit {$uvi (hypo @uvI)}))
      ?:  =(0 yon)
        ``uvi+[-:!>(*@uvI) *@uvI]
      =+  tak=(~(get by hit.dom) yon)
      ?~  tak
        ~
      [~ ~ %uvi -:!>(*@uvI) (content-hash (tako-to-yaki u.tak) pax)]
    ::
    ::  Get a value at an aeon.
    ::
    ::  Value can be either null, meaning we don't have it yet, [null null],
    ::  meaning we know it doesn't exist, or [null null (each cage lobe)],
    ::  meaning we either have the value directly or a content hash of the
    ::  value.
    ::
    ++  read-at-aeon                                    ::    read-at-aeon:ze
      |=  [for=(unit ship) yon=aeon mun=mood]           ::  seek and read
      ^-  [(unit (unit (each cage lobe))) ford-cache]
      =*  fod  fod.dom
      ?.  |(?=(~ for) (may-read u.for care.mun yon path.mun))
        [~ fod]
      ::  virtualize to catch and produce deterministic failures
      ::
      !.
      =-  ?:(?=(%& -<) p.- ((slog p.-) [[~ ~] fod]))
      %-  mule  |.
      ?-  care.mun
          %d
        :_  fod
        ::  XX this should only allow reads at the current date
        ::
        ?:  !=(our her)
          [~ ~]
        ?^  path.mun
          ~&(%no-cd-path [~ ~])
        [~ ~ %& %noun !>(~(key by dos.rom.ruf))]
      ::
        %a  (read-a yon path.mun)
        %b  (read-b yon path.mun)
        %c  (read-c yon path.mun)
        %p  :_(fod (read-p path.mun))
        %r  :_(fod (bind (read-r yon path.mun) (lift |=(a=cage [%& a]))))
        %s  :_(fod (bind (read-s yon path.mun) (lift |=(a=cage [%& a]))))
        %t  :_(fod (bind (read-t yon path.mun) (lift |=(a=cage [%& a]))))
        %u  :_(fod (read-u yon path.mun))
        %v  :_(fod (bind (read-v yon path.mun) (lift |=(a/cage [%& a]))))
        %w  :_(fod (read-w case.mun))
        %x  :_(fod (read-x yon path.mun))
        %y  :_(fod (bind (read-y yon path.mun) (lift |=(a/cage [%& a]))))
        %z  :_(fod (bind (read-z yon path.mun) (lift |=(a/cage [%& a]))))
      ==
    ::  Traverse an ankh.
    ::
    ++  zu                                              ::  filesystem
      |=  ank/ankh                                      ::  filesystem state
      =|  ram/path                                      ::  reverse path into
      |%
      ++  descend                                       ::  descend
        |=  lol/@ta
        ^+  +>
        =+  you=(~(get by dir.ank) lol)
        +>.$(ram [lol ram], ank ?~(you [~ ~] u.you))
      ::
      ++  descend-path                                  ::  descend recursively
        |=  way/path
        ^+  +>
        ?~(way +> $(way t.way, +> (descend i.way)))
      --
    --
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
    $:  ver=%5                                        ::  vane version
        ruf=raft                                      ::  revision tree
    ==                                                ::
|=  [our=ship now=@da eny=@uvJ ski=sley]              ::  current invocation
|%                                                    ::
++  call                                              ::  handle request
  |=  $:  hen=duct
          dud=(unit goof)
          type=*
          wrapped-task=(hobo task:able)
      ==
  ^-  [(list move) _..^$]
  ::
  =/  req=task:able  ((harden task:able) wrapped-task)
  ::
  ::  error notifications "downcast" to %crud
  ::
  =?  req  ?=(^ dud)
    ~|  %crud-in-crud
    ?<  ?=(%crud -.req)
    [%crud -.req tang.u.dud]
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
    =|  mos/(list move)
    =/  des  ~(tap in ~(key by dos.rom.ruf))
    |-
    ?~  des  [[[hen %give %done ~] mos] ..^^$]
    =/  den  ((de our now ski hen ruf) our i.des)
    =^  mor  ruf
      =<  abet:wake
      ?:  ?=(^ cew.req)  den
      (forget-crew:den nom.req)
    $(des t.des, mos (weld mos mor))
  ::
      %crew
    [[hen %give %cruz cez.ruf]~ ..^$]
  ::
      %crow
    =/  des  ~(tap by dos.rom.ruf)
    =|  rus/(map desk {r/regs w/regs})
    |^
      ?~  des  [[hen %give %croz rus]~ ..^^$]
      =+  per=(filter-rules per.q.i.des)
      =+  pew=(filter-rules pew.q.i.des)
      =?  rus  |(?=(^ per) ?=(^ pew))
        (~(put by rus) p.i.des per pew)
      $(des t.des)
    ::
    ++  filter-rules
      |=  pes/regs
      ^+  pes
      =-  (~(gas in *regs) -)
      %+  skim  ~(tap by pes)
      |=  {p/path r/rule}
      (~(has in who.r) |+nom.req)
    --
  ::
      %crud
    [[[hen %slip %d %flog req] ~] ..^$]
  ::
      %drop
    ~&  %clay-idle
    [~ ..^$]
  ::
      %info
    ?:  ?=(%| -.dit.req)
      ~|  %labelling-not-implemented
      !!
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
      =/  den  ((de our now ski hen ruf) our des.req)
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
    =/  bem/beam
        ?^  bem
          u.bem
        [[our %home %ud 1] ~]
    =/  dos  (~(get by dos.rom.ruf) q.bem)
    ?~  dos
      !!  ::  fire next in queue
    =^  mos  ruf
      =/  den  ((de our now ski hen ruf) our q.bem)
      abet:(into:den (flop s.bem) all.req fis.req)
    [mos ..^$]
  ::
      %merg                                               ::  direct state up
    ?:  =(%$ des.req)
      ~&(%merg-no-desk !!)
    =^  mos  ruf
      =/  den  ((de our now ski hen ruf) our des.req)
      abet:(start-merge:den her.req dem.req cas.req how.req)
    [mos ..^$]
  ::
      %mont
    =.  hez.ruf  ?^(hez.ruf hez.ruf `[[%$ %sync ~] ~])
    =^  mos  ruf
      =/  den  ((de our now ski hen ruf) p.bem.req q.bem.req)
      abet:(mount:den pot.req r.bem.req s.bem.req)
    [mos ..^$]
  ::
      %dirk
    ?~  hez.ruf
      ~&  %no-sync-duct
      [~ ..^$]
    ?.  (~(has by mon.ruf) des.req)
      ~&  [%not-mounted des.req]
      [~ ..^$]
    [~[[u.hez.ruf %give %dirk des.req]] ..^$]
  ::
      %ogre
    ?~  hez.ruf
      ~&  %no-sync-duct
      [~ ..^$]
    =*  pot  pot.req
    ?@  pot
      ?.  (~(has by mon.ruf) pot)
        ~&  [%not-mounted pot]
        [~ ..^$]
      :_  ..^$(mon.ruf (~(del by mon.ruf) pot))
      [u.hez.ruf %give %ogre pot]~
    :_  %_    ..^$
            mon.ruf
          %-  molt
          %+  skip  ~(tap by mon.ruf)
          (corl (cury test pot) tail)
        ==
    %+  turn
      (skim ~(tap by mon.ruf) (corl (cury test pot) tail))
    |=  {pon/term bem/beam}
    [u.hez.ruf %give %ogre pon]
  ::
      %park
    =^  mos  ruf
      =/  den  ((de our now ski hen ruf) our des.req)
      abet:(park:den | [yok ran]:req)
    [mos ..^$]
  ::
      %pork
    =/  [syd=desk =yoki]  (need pud.ruf)
    =.  pud.ruf  ~
    =^  mos  ruf
      =/  den  ((de our now ski hen ruf) our syd)
      abet:(park:den & yoki *rang)
    [mos ..^$]
  ::
      %perm
    =^  mos  ruf
      =/  den  ((de our now ski hen ruf) our des.req)
      abet:(perm:den pax.req rit.req)
    [mos ..^$]
  ::
      %trim  [~ ..^$]
  ::
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
      =^  moves-2  ruf  abet:wake:((de our now ski hen ruf) [ship desk]:i.desks)
      [(weld moves-1 moves-2) ..^^$]
    [(welp wake-moves pun.ruf) ..^$(pun.ruf ~)]
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
      =^  ver  rif.req
        ?@  -.rif.req
          [%0 rif.req]
        [-<.rif.req +.rif.req]
      ?>  ?=(@ -.rif.req)
      :-  ?:(=(our who.req) ~ `[who.req ver])
      [%warp wer.req rif.req]
    ::
    ?>  ?=(%warp -.req)
    =*  rif  rif.req
    =^  mos  ruf
      =/  den  ((de our now ski hen ruf) wer.req p.rif)
      =<  abet
      ?~  q.rif
        cancel-request:den
      (start-request:den for u.q.rif)
    [mos ..^$]
  ::
      %plea
    =*  her  ship.req
    =*  pax  path.plea.req
    =*  res  payload.plea.req
    ::
    ?:  ?=([%backfill *] pax)
      =+  ;;(=fill res)
      =^  mos  ruf
        =/  den  ((de our now ski hen ruf) our desk.fill)
        abet:(give-backfill:den +.fill)
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
  !:
  |^
  |=  old=any-state
  ~!  [old=old new=*state-4]
  =?  old  ?=(%2 -.old)  (load-2-to-3 old)
  =?  old  ?=(%3 -.old)  (load-3-to-4 old)
  =?  old  ?=(%4 -.old)  (load-4-to-5 old)
  ?>  ?=(%5 -.old)
  ..^^$(ruf +.old)
  ::
  ++  load-4-to-5
    |=  =state-4
    ^-  state-5
    state-4(- %5, pun ~)
  ::
  ++  load-3-to-4
    |=  =state-3
    ^-  state-4
    |^
    =-  state-3(- %4, hoy hoy.-, rom (room-3-to-4 rom.state-3))
    ^-  hoy=(map ship rung)
    %-  ~(run by hoy.state-3)
    |=  =rung-3
    ^-  rung
    %-  ~(run by rus.rung-3)
    |=  =rede-3
    ^-  rede
    =-  rede-3(ref ref.-, qyx (cult-3-to-4 qyx.rede-3))
    ^-  ref=(unit rind)
    ?~  ref.rede-3
      ~
    =-  `u.ref.rede-3(bom bom.-)
    ^-  bom=(map @ud update-state)
    %-  ~(run by bom.u.ref.rede-3)
    |=  [=duct =rave]
    ^-  update-state
    [duct rave ~ ~ ~ |]
    ::
    ++  room-3-to-4
      |=  =room-3
      ^-  room
      =-  room-3(dos dos.-)
      ^-  dos=(map desk dojo)
      %-  ~(run by dos.room-3)
      |=  =dojo-3
      ^-  dojo
      dojo-3(qyx (cult-3-to-4 qyx.dojo-3))
    ::
    ++  cult-3-to-4
      |=  =cult-3
      ^-  cult
      %-  malt
      %+  turn  ~(tap by cult-3)
      |=  [=wove-3 ducts=(set duct)]
      ^-  [wove (set duct)]
      :_  ducts  :_  rove.wove-3
      ?~  for.wove-3
        ~
      `[u.for.wove-3 %0]
    --
  ::
  ++  load-2-to-3
    |=  =state-2
    ^-  state-3
    |^
    =-  state-2(- %3, rom rom.-, hoy hoy.-, |7 [pud=~ pun.-])
    :+  ^-  pun=(list move)
        %+  welp
          ?~  act.state-2
            ~
          ?.  =(%merge -.eval-data.u.act.state-2)
            ~
          =/  err
            :-  %ford-fusion
            [leaf+"active merge canceled due to upgrade to ford fusion" ~]
          [hen.u.act.state-2 %slip %b %drip !>([%mere %| err])]~
        ^-  (list move)
        %+  murn  ~(tap to cue.state-2)
        ::  use ^ so we don't have to track definition of +task
        ::
        |=  [=duct task=^]
        ^-  (unit move)
        ?.  =(%merg -.task)
          ~&  "queued clay write canceled due to upgrade to ford fusion:"
          ~&  [duct [- +<]:task]
          ~
        =/  err
          :-  %ford-fusion
          [leaf+"queued merge canceled due to upgrade to ford fusion" ~]
        `[duct %slip %b %drip !>([%mere %| err])]
      ^-  rom=room-3
      :-  hun.rom.state-2
      %-  ~(urn by dos.rom.state-2)
      |=  [=desk =dojo-2]
      ^-  dojo-3
      =-  dojo-2(dom -)
      ^-  dome
      =/  fer=(unit reef-cache)
        ?~  let.dom.dojo-2
          ~
        =/  =yaki
          (~(got by hut.ran.state-2) (~(got by hit.dom.dojo-2) let.dom.dojo-2))
        `(build-reef desk q.yaki)
      [ank let hit lab mim fod=*ford-cache fer=fer]:[dom.dojo-2 .]
    ^-  hoy=(map ship rung-3)
    %-  ~(run by hoy.state-2)
    |=  =rung-2
    ^-  rung-3
    %-  ~(run by rus.rung-2)
    |=  =rede-2
    ^-  rede-3
    =-  rede-2(ref ref.-, dom dom.-)
    :-  ^-  dom=dome
        [ank let hit lab mim fod=*ford-cache fer=~]:[dom.rede-2 .]
    ^-  ref=(unit rind-3)
    ?~  ref.rede-2
      ~
    :-  ~
    ^-  rind-3
    =/  rin=rind-3  [nix bom fod haw]:u.ref.rede-2
    =.  rin
      =/  pur=(list [inx=@ud =rand *])  ~(tap by pur.u.ref.rede-2)
      |-  ^+  rin
      ?~  pur  rin
      =/  =mood  [p.p q.p q]:rand.i.pur
      =:  haw.rin  (~(put by haw.rin) mood ~)
          bom.rin  (~(del by bom.rin) inx.i.pur)
          fod.rin  ?~  got=(~(get by bom.rin) inx.i.pur)
                     fod.rin
                   (~(del by fod.rin) p.u.got)
        ==
      $(pur t.pur)
    =/  pud  ~(tap to waiting.pud.u.ref.rede-2)
    |-  ^+  rin
    ?~  pud  rin
    =:  bom.rin  (~(del by bom.rin) inx.i.pud)
        fod.rin  ?~  got=(~(get by bom.rin) inx.i.pud)
                   fod.rin
                 (~(del by fod.rin) p.u.got)
      ==
    $(pud t.pud)
    ::
    ++  build-reef
      |=  [=desk data=(map path lobe)]
      ^-  reef-cache
      ~>  %slog.0^leaf+"clay: building reef on {<desk>}"
      ?:  =(%home desk)
        [!>(..ride) !>(..is) !>(..zuse)]
      |^
      =/  [home=? hoon=vase]
        ?:  (same-as-home /sys/hoon/hoon)
          &+!>(..ride)
        |+build-hoon
      :-  hoon
      =/  [home=? arvo=vase]
        ?:  &(home (same-as-home /sys/arvo/hoon))
          &+!>(..is)
        |+(build-arvo hoon)
      :-  arvo
      ?:  &(home (same-as-home /sys/zuse/hoon))
        !>(..zuse)
      (build-zuse arvo)
      ::
      ++  build-hoon
        %-  road  |.
        ~>  %slog.0^leaf+"clay: building hoon on {<desk>}"
        =/  gen
          ~>  %mean.%hoon-parse-fail
          %+  rain  /sys/hoon/hoon
          (lobe-to-cord (~(got by data) /sys/hoon/hoon))
        ~>  %mean.%hoon-compile-fail
        (slot 7 (slap !>(0) gen))
      ::
      ++  build-arvo
        |=  hoon=vase
        %-  road  |.
        ~>  %slog.0^leaf+"clay: building arvo on {<desk>}"
        =/  gen
          ~>  %mean.%arvo-parse-fail
          %+  rain  /sys/arvo/hoon
          (lobe-to-cord (~(got by data) /sys/arvo/hoon))
        ~>  %mean.%arvo-compile-fail
        (slap (slap hoon gen) !,(*^hoon ..is))
      ::
      ++  build-zuse
        |=  arvo=vase
        %-  road  |.
        ~>  %slog.0^leaf+"clay: building zuse on {<desk>}"
        =/  gen
          ~>  %mean.%zuse-parse-fail
          %+  rain  /sys/zuse/hoon
          (lobe-to-cord (~(got by data) /sys/zuse/hoon))
        ~>  %mean.%zuse-compile-fail
        (slap arvo gen)
      ::
      ++  same-as-home
        |=  =path
        ^-  ?
        =/  our-lobe=lobe  (~(got by data) path)
        =/  =dome-2  dom:(~(got by dos.rom.state-2) %home)
        =/  =yaki  (~(got by hut.ran.state-2) (~(got by hit.dome-2) let.dome-2))
        =(`our-lobe (~(get by q.yaki) path))
      ::
      ++  lobe-to-cord
        |=  =lobe
        ^-  @t
        =-  ?:(?=(%& -<) p.- (of-wain:format p.-))
        |-  ^-  (each @t wain)
        =/  =blob  (~(got by lat.ran.state-2) lobe)
        ?-    -.blob
            %direct  [%& ;;(@t q.q.blob)]
            %delta
          :-  %|
          %+  lurk:differ
            =-  ?:(?=(%| -<) p.- (to-wain:format p.-))
            $(lobe q.q.blob)
          ~|  diff=r.blob
          ;;((urge cord) q.r.blob)
        ==
      --
    --
  ::
  +$  any-state  $%(state-5 state-4 state-3 state-2)
  +$  state-5  [%5 raft]
  +$  state-4
    $:  %4
        rom=room
        hoy=(map ship rung)
        ran=rang
        mon=(map term beam)
        hez=(unit duct)
        cez=(map @ta crew)
        pud=(unit [=desk =yoki])
        pun=(list *)
    ==
  +$  state-3
    $:  %3
        rom=room-3
        hoy=(map ship rung-3)
        ran=rang
        mon=(map term beam)
        hez=(unit duct)
        cez=(map @ta crew)
        pud=(unit [=desk =yoki])
        pun=(list *)
    ==
  +$  rung-3  rus=(map desk rede-3)
  +$  rede-3
    $:  lim/@da
        ref/(unit rind-3)
        qyx/cult-3
        dom/dome
        per/regs
        pew/regs
    ==
  +$  rind-3
    $:  nix/@ud
        bom/(map @ud {p/duct q/rave})
        fod/(map duct @ud)
        haw/(map mood (unit cage))
    ==
  +$  room-3
    $:  hun/duct
        dos/(map desk dojo-3)
    ==
  ++  dojo-3
    $:  qyx/cult-3
        dom/dome
        per/regs
        pew/regs
    ==
  +$  cult-3  (jug wove-3 duct)
  +$  wove-3  [for=(unit ship) =rove]
  +$  state-2
    $:  %2
        rom=room-2                                      ::  domestic
        hoy=(map ship rung-2)                           ::  foreign
        ran=rang                                        ::  hashes
        mon=(map term beam)                             ::  mount points
        hez=(unit duct)                                 ::  sync duct
        cez=(map @ta crew)                              ::  permission groups
        cue=(qeu [=duct task=^])                        ::  queued requests
        act=active-write-2                              ::  active write
    ==                                                  ::
  +$  room-2
    $:  hun/duct                                        ::  terminal duct
        dos/(map desk dojo-2)                           ::  native desk
    ==                                                  ::
  +$  dojo-2
    $:  qyx/cult-3                                      ::  subscribers
        dom/dome-2                                      ::  desk state
        per/regs                                        ::  read perms per path
        pew/regs                                        ::  write perms per path
    ==
  +$  dome-2
    $:  ank/ankh                                        ::  state
        let/aeon                                        ::  top id
        hit/(map aeon tako)                             ::  versions by id
        lab/(map @tas aeon)                             ::  labels
        mim/(map path mime)                             ::  mime cache
    ==                                                  ::
  +$  rung-2  rus=(map desk rede-2)
  +$  rede-2
    $:  lim/@da                                         ::  complete to
        ref/(unit rind-2)                               ::  outgoing requests
        qyx/cult-3                                      ::  subscribers
        dom/dome-2                                      ::  revision state
        per/regs                                        ::  read perms per path
        pew/regs                                        ::  write perms per path
    ==                                                  ::
  +$  rind-2
    $:  nix/@ud                                         ::  request index
        bom/(map @ud {p/duct q/rave})                   ::  outstanding
        fod/(map duct @ud)                              ::  current requests
        haw/(map mood (unit cage))                      ::  simple cache
        pud/update-qeu-2                                ::  active updates
        pur/request-map-2                               ::  active requests
    ==                                                  ::
  +$  request-map-2  (map inx=@ud [=rand eval-form=*])
  +$  update-qeu-2
    $:  waiting=(qeu [inx=@ud rut=(unit rand)])
        eval-data=(unit [inx=@ud rut=(unit rand) eval-form=*])
    ==
  +$  active-write-2  (unit [hen=duct req=* eval-data=^])
  --
::
++  scry                                              ::  inspect
  |=  {fur/(unit (set monk)) ren/@tas why/shop syd/desk lot/coin tyl/path}
  ^-  (unit (unit cage))
  ?.  ?=(%& -.why)  ~
  =*  his  p.why
  ?:  &(=(ren %$) =(tyl /whey))
    ``mass+!>(whey)
  ::  ~&  scry+[ren `path`[(scot %p his) syd ~(rent co lot) tyl]]
  ::  =-  ~&  %scry-done  -
  =+  luk=?.(?=(%$ -.lot) ~ ((soft case) p.lot))
  ?~  luk  [~ ~]
  ?:  =(%$ ren)
    [~ ~]
  =+  run=((soft care) ren)
  ?~  run  [~ ~]
  ::TODO  if it ever gets filled properly, pass in the full fur.
  =/  for/(unit ship)
    %-  ~(rep in (fall fur ~))
    |=  {m/monk s/(unit ship)}
    ?^  s  s
    ?:  ?=(%| -.m)  ~
    ?:  =(p.m his)  ~
    `p.m
  =/  den  ((de our now ski [/scryduct ~] ruf) his syd)
  =/  result  (mule |.(-:(aver:den for u.run u.luk tyl)))
  ?:  ?=(%| -.result)
    %-  (slog >%clay-scry-fail< p.result)
    ~
  ?~  p.result               ~
  ?~  u.p.result             [~ ~]
  ::  should convert %| case to cage
  ::
  ?:  ?=(%& -.u.u.p.result)  ``p.u.u.p.result
  ~
::
::  We clear the ford cache by replacing it with its bunt as a literal.
::  This nests within +ford-cache without reference to +type, +hoon, or
::  anything else in the sample of cache objects.  Otherwise we would be
::  contravariant in the those types, which makes them harder to change.
::
++  stay
  :-  ver
  %=    ruf
      dos.rom
    %-  ~(run by dos.rom.ruf)
    |=  =dojo
    dojo(fod.dom [~ ~ ~])
  ::
      hoy
    %-  ~(run by hoy.ruf)
    |=  =rung
    %=    rung
        rus
      %-  ~(run by rus.rung)
      |=  =rede
      rede(fod.dom [~ ~ ~])
    ==
  ==
::
++  take                                              ::  accept response
  |=  [tea=wire hen=duct dud=(unit goof) hin=(hypo sign)]
  ^+  [*(list move) ..^$]
  ?^  dud
    ~|(%clay-take-dud (mean tang.u.dud))
  ::
  ?:  ?=([%merge @ @ @ @ ~] tea)
    ?>  ?=(%writ +<.q.hin)
    =*  syd  i.t.tea
    =/  ali-ship  (slav %p i.t.t.tea)
    =*  ali-desk  i.t.t.t.tea
    =/  germ  (germ i.t.t.t.t.tea)
    =^  mos  ruf
      =/  den  ((de our now ski hen ruf) our i.t.tea)
      abet:(merge:den ali-ship ali-desk germ p.q.hin)
    [mos ..^$]
  ::
  ?:  ?=([%foreign-warp *] tea)
    ?>  ?=(%writ +<.q.hin)
    :_  ..^$
    [hen %give %boon `(unit rand)`(bind `riot`p.q.hin rant-to-rand)]~
  ::
  ?:  ?=([%warp-index @ @ @ ~] tea)
    ?+    +<.q.hin  ~|  %clay-warp-index-strange  !!
        %done
      ?~  error.q.hin
        [~ ..^$]
      ::  TODO better error handling
      ::
      ~&  %clay-take-warp-index-error^our^tea^tag.u.error.q.hin
      %-  (slog tang.u.error.q.hin)
      [~ ..^$]
    ::
        %lost
      ~|  %clay-take-lost^our
      ::  TODO better error handling
      !!
    ::
        %boon
      =+  ;;  res=(unit rand)  payload.q.hin
      ::
      =/  her=ship   (slav %p i.t.tea)
      =/  =desk      (slav %tas i.t.t.tea)
      =/  index=@ud  (slav %ud i.t.t.t.tea)
      ::
      =^  mos  ruf
        =/  den  ((de our now ski hen ruf) her desk)
        abet:(take-foreign-answer:den index res)
      [mos ..^$]
    ==
  ::
  ?:  ?=([%back-index @ @ @ ~] tea)
    ?+    +<.q.hin  ~|  %clay-backfill-index-strange  !!
        %done
      ?~  error.q.hin
        [~ ..^$]
      ::  TODO better error handling
      ::
      ~&  %clay-take-backfill-index-error^our^tea^tag.u.error.q.hin
      %-  (slog tang.u.error.q.hin)
      [~ ..^$]
    ::
        %lost
      ~|  %clay-take-backfill-lost^our
      ::  TODO better error handling
      !!
    ::
        %boon
      =+  ;;  =blob  payload.q.hin
      ::
      =/  her=ship   (slav %p i.t.tea)
      =/  =desk      (slav %tas i.t.t.tea)
      =/  index=@ud  (slav %ud i.t.t.t.tea)
      ::
      =^  mos  ruf
        =/  den  ((de our now ski hen ruf) her desk)
        abet:abet:(take-backfill:(foreign-update:den index) blob)
      [mos ..^$]
    ==
  ::
  ?:  ?=([%sinks ~] tea)
    ?>  ?=(%public-keys +<.q.hin)
    ?.  ?=(%breach -.public-keys-result.q.hin)
      [~ ..^$]
    =/  who  who.public-keys-result.q.hin
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
      |=(=duct [duct %slip %b %drip !>([%writ ~])])
    ::  delete local state of foreign desk
    ::
    =.  hoy.ruf  (~(del by hoy.ruf) who)
    [cancel-moves ..^$]
  ::
  ?-    -.+.q.hin
      %public-keys  ~|([%public-keys-raw tea] !!)
      %crud
    [[[hen %slip %d %flog +.q.hin] ~] ..^$]
  ::
      %mere
    ?:  ?=(%& -.p.+.q.hin)
      ~&  'initial merge succeeded'
      [~ ..^$]
    ~>  %slog.
        :^  0  %rose  [" " "[" "]"]
        :^    leaf+"initial merge failed"
            leaf+"my most sincere apologies"
          >p.p.p.+.q.hin<
        q.p.p.+.q.hin
    [~ ..^$]
  ::
      %note  [[hen %give +.q.hin]~ ..^$]
      %wake
    ::  TODO: handle behn errors
    ::
    ?^  error.q.hin
      [[hen %slip %d %flog %crud %wake u.error.q.hin]~ ..^$]
    ::
    ?.  ?=([%tyme @ @ ~] tea)
      ~&  [%clay-strange-timer tea]
      [~ ..^$]
    =/  her  (slav %p i.t.tea)
    =/  syd  (slav %tas i.t.t.tea)
    =^  mos  ruf
      =/  den  ((de our now ski hen ruf) her syd)
      abet:wake:den
    [mos ..^$]
  ::
      ::  handled in the wire dispatcher
      ::
      %boon  !!
      %lost  !!
      %writ
    %-  (slog leaf+"clay: strange writ (expected on upgrade to Fusion)" ~)
    [~ ..^$]
  ::
      %done
    ?~  error=error.q.hin
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
  =/  domestic
    %+  turn  (sort ~(tap by dos.rom.ruf) aor)
    |=  [=desk =dojo]
    :+  desk  %|
    :~  ankh+&+ank.dom.dojo
        mime+&+mim.dom.dojo
        ford-vases+&+vases.fod.dom.dojo
        ford-marks+&+marks.fod.dom.dojo
        ford-casts+&+casts.fod.dom.dojo
    ==
  :~  domestic+|+domestic
      foreign+&+hoy.ruf
      :+  %object-store  %|
      :~  commits+&+hut.ran.ruf
          blobs+&+lat.ran.ruf
      ==
  ==
--
