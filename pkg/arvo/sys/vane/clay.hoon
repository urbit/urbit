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
::  Ford cache
::
+$  ford-cache
  $:  vases=(map path [res=vase dez=(set path)])
      marks=(map mark [res=dais dez=(set path)])
      casts=(map mars [res=tube dez=(set path)])
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
::  --  `cue` is a queue of requests to perform in later events.
::  --  `tip` is the date of the last write; if now, enqueue incoming requests.
::
++  raft                                                ::  filesystem
  $:  rom=room                                          ::  domestic
      hoy=(map ship rung)                               ::  foreign
      ran=rang                                          ::  hashes
      mon=(map term beam)                               ::  mount points
      hez=(unit duct)                                   ::  sync duct
      cez=(map @ta crew)                                ::  permission groups
      cue=(qeu [=duct =task:able])                      ::  queued requests
      act=active-write                                  ::  active write
      pud=(unit [=desk =yoki])                          ::  pending update
  ==                                                    ::
::
::  Currently active write
::
++  active-write
  %-  unit
  $:  hen=duct
      req=task:able
      $=  eval-data
      $%  [%merge merge=eval-form:eval:merge-clad]
          [%mount mount=eval-form:eval:mount-clad]
      ==
  ==
::
::  The clad monad for merges.
::
::  Same as +commit-clad, except includes a set of paths documenting the
::  conflicts encountered in the merge.
::
++  merge-clad  (clad ,[(set path) dome rang])
::
::  The clad monad for mounts.
::
::  Just a new mount point and mime cache.
::
++  mount-clad  (clad ,[new-mon=(pair term beam) mim=(map path mime)])
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
++  rind                                                ::  request manager
          $:  nix/@ud                                   ::  request index
              bom/(map @ud {p/duct q/rave})             ::  outstanding
              fod/(map duct @ud)                        ::  current requests
              haw/(map mood (unit cage))                ::  simple cache
              pud/update-qeu                            ::  active updates
              pur/request-map                           ::  active requests
          ==                                            ::
::
::  Result of a subscription
::
++  sub-result
  $%  [%blab =mood data=(each cage lobe)]
      [%bleb ins=@ud range=(unit (pair aeon aeon))]
      [%balk cage=(unit (each cage lobe)) =mood]
      [%blas moods=(set mood)]
      [%blub ~]
  ==
::
::  The clad monad for foreign updates.
::
::  Same as +commit-clad, except includes `lim`, as in +rede.  Null if
::  subscription ended.
::
++  update-clad  (clad ,(unit [lim=@da dome rang]))
++  update-qeu
  $:  waiting=(qeu [inx=@ud rut=(unit rand)])
      eval-data=(unit [inx=@ud rut=(unit rand) =eval-form:eval:update-clad])
  ==
::
::  The clad monad for foreign simple requests
::
++  request-clad  (clad ,cage)
++  request-map   ,(map inx=@ud [=rand =eval-form:eval:request-clad])
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
+$  wove  [for=(unit ship) =rove]                       ::  stored source + req
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
::  Hash of a commit, for lookup in the object store (hut.ran)
::
++  tako  @                                             ::  yaki ref
::
::  Commit.
::
::  List of parents, content, hash of self, and time commited.
::
++  yaki                                                ::  snapshot
          $:  p/(list tako)                             ::  parents
              q/(map path lobe)                         ::  fileset
              r/tako                                    ::
          ::                                            ::  XX s?
              t/@da                                     ::  date
          ==                                            ::
::
::  Unvalidated blob
::
++  plop  blob                                          ::  unvalidated blob
::
::  The clay monad, for easier-to-follow state machines.
::
::  The best way to think about a clad is that it's a transaction that
::  may take multiple arvo events, and may send notes to other vanes to
::  get information.
::
+$  clad-input  [now=@da new-rang=rang =sign]
::
::  notes:   notes to send immediately.  These will go out even if a
::           later stage of the process fails, so they shouldn't have any
::           semantic effect on the rest of the system.  Path is
::           included exclusively for documentation and |verb.
::  effects: moves to send after the process ends.
::  wait:    don't move on, stay here.  The next sign should come back
::           to this same callback.
::  cont:    continue process with new callback.
::  fail:    abort process; don't send effects
::  done:    finish process; send effects
::
++  clad-output-raw
  |*  a=mold
  $~  [~ ~ %done *a]
  $:  notes=(list [path note])
      effects=(list move)
      $=  next
      $%  [%wait ~]
          [%cont self=(clad-form-raw a)]
          [%fail err=(pair term tang)]
          [%done value=a]
      ==
  ==
::
++  clad-form-raw
  |*  a=mold
  $-(clad-input (clad-output-raw a))
::
++  clad-fail
  |=  err=(pair term tang)
  |=  clad-input
  [~ ~ %fail err]
::
++  clad-init-sign  `sign`[%y %init-clad ~]
::
++  clad
  |*  a=mold
  |%
  ++  output  (clad-output-raw a)
  ++  form  (clad-form-raw a)
  ++  pure
    |=  arg=a
    ^-  form
    |=  clad-input
    [~ ~ %done arg]
  ::
  ++  bind
    |*  b=mold
    |=  [m-b=(clad-form-raw b) fun=$-(b form)]
    ^-  form
    |=  input=clad-input
    =/  b-res=(clad-output-raw b)
      (m-b input)
    ^-  output
    :+  notes.b-res  effects.b-res
    ?-    -.next.b-res
      %wait  [%wait ~]
      %cont  [%cont ..$(m-b self.next.b-res)]
      %fail  [%fail err.next.b-res]
      %done  [%cont (fun value.next.b-res)]
    ==
  ::
  ::  The clad monad must be evaluted in a particular way to maintain
  ::  its monadic character.  +take:eval implements this.
  ::
  ++  eval
    |%
    ::  Indelible state of a clad
    ::
    +$  eval-form
      $:  effects=(list move)
          =form
      ==
    ::
    ::  Convert initial form to eval-form
    ::
    ++  from-form
      |=  =form
      ^-  eval-form
      [~ form]
    ::
    ::  The cases of results of +take
    ::
    +$  eval-result
      $%  [%next ~]
          [%fail err=(pair term tang)]
          [%done value=a]
      ==
    ::
    ::  Take a new sign and run the clad against it
    ::
    ++  take
      ::  moves: accumulate throughout recursion the moves to be
      ::         produced now
      =|  moves=(list move)
      |=  [=eval-form =duct =our=wire =clad-input]
      ^-  [[(list move) =eval-result] _eval-form]
      ::  run the clad callback
      ::
      =/  =output  (form.eval-form clad-input)
      ::  add notes to moves
      ::
      =.  moves
        %+  welp
          moves
        %+  turn  notes.output
        |=  [=path =note]
        [duct %pass (weld our-wire path) note]
      ::  add effects to list to be produced when done
      ::
      =.  effects.eval-form
        (weld effects.eval-form effects.output)
      ::  if done, produce effects
      ::
      =?  moves  ?=(%done -.next.output)
        %+  welp
          moves
        effects.eval-form
      ::  case-wise handle next steps
      ::
      ?-  -.next.output
          %wait  [[moves %next ~] eval-form]
          %fail  [[moves %fail err.next.output] eval-form]
          %done  [[moves %done value.next.output] eval-form]
          %cont
        ::  recurse to run continuation with initialization move
        ::
        %_  $
          form.eval-form   self.next.output
          sign.clad-input  clad-init-sign
        ==
      ==
    --
  --
::
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
      $:  %f                                            ::  to %ford
          $>  $?  %build                                ::
                  %keep                                 ::
                  %wipe                                 ::
              ==                                        ::
          task:able:ford                                ::
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
  $%  $:  %y                                            ::
          $%  [%init-clad ~]                            ::
      ==  ==                                            ::
      $:  %a                                            ::  by %ames
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
      $:  %f                                            ::  by %ford
          $>(%made gift:able:ford)                      ::
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
::  Just send a note.
::
++  just-do
  |=  [=path =note]
  =/  m  (clad ,~)
  ^-  form:m
  |=  clad-input
  [[path note]~ ~ %done ~]
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
::  Wait for ford to respond
::
++  expect-ford
  =/  m  (clad ,made-result:ford)
  ^-  form:m
  |=  clad-input
  ?:  ?=(%init-clad +<.sign)
    [~ ~ %wait ~]
  ?:  ?=(%made +<.sign)
    [~ ~ %done result.sign]
  ~|  [%expected-made got=+<.sign]
  !!
::
::  Wait for clay to respond
::
::    This setup where we take in a new-rang in +clad-input but only
::    apply it when calling +expect-clay is suspicious.  I'm not sure
::    what's the best approach to reading in potentially new state that
::    we also may have changed but haven't committed.
::
++  expect-clay
  |=  ran=rang
  =/  m  (clad ,[riot rang])
  ^-  form:m
  |=  clad-input
  ?:  ?=(%init-clad +<.sign)
    [~ ~ %wait ~]
  ?:  ?=(%writ +<.sign)
    =/  uni-rang=rang
      :-  (~(uni by hut.new-rang) hut.ran)
      (~(uni by lat.new-rang) lat.ran)
    [~ ~ %done p.sign uni-rang]
  ~|  [%expected-writ got=+<.sign]
  !!
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
::  This transaction respresents a currently running merge.  We always
::  say we're merging from 'ali' to 'bob'.  The basic steps, not all of
::  which are always needed, are:
::
::  --  fetch ali's desk
::  --  diff ali's desk against the mergebase
::  --  diff bob's desk against the mergebase
::  --  merge the diffs
::  --  build the new state
::  --  "checkout" (apply to actual `++dome`) the new state
::  --  "ergo" (tell unix about) any changes
::
++  merge
  ::  Global constants.  These do not change during a merge.
  ::
  |=  $:  our=ship
          wen=@da
          ali-disc=(pair ship desk)
          bob-disc=(pair ship desk)
          cas=case
          mon=(map term beam)
          hez=(unit duct)
      ==
  ::  Run ford operations on ali unless it's a foreign desk
  ::
  =/  ford-disc=disc:ford
    ?:  =(p.ali-disc p.bob-disc)
      ali-disc
    bob-disc
  |^
  ::  Initial arguments
  ::
  |=  [gem=germ dom=dome ran=rang]
  =/  m  merge-clad
  ^-  form:m
  =/  e  (cor dom ran)
  ;<  [bob=(unit yaki) gem=germ]  bind:m  (get-bob:e gem)
  ;<  [ali=yaki e=_*cor]          bind:m  fetch-ali:e
  ;<    $=  res
        %-  unit
        $:  conflicts=(set path)
            bop=(map path cage)
            new=yaki
            erg=(map path ?)
            e=_*cor
        ==
      bind:m
    (merge:e gem cas ali bob)
  ?~  res
    ::  if no changes, we're done
    ::
    (pure:m ~ dom:e ran:e)
  =.  e  e.u.res
  ;<  e=_*cor   bind:m     (checkout:e gem cas bob new.u.res bop.u.res)
  ;<  mim=(map path mime)  bind:m  (ergo:e gem cas mon erg.u.res new.u.res)
  =.  mim.dom.e  mim
  (pure:m conflicts.u.res dom:e ran:e)
  ::
  ::  A stateful core, where the global state is a dome and a rang.
  ::
  ::    These are the global state variables that a merge may change.
  ::
  ++  cor
    |=  [dom=dome ran=rang]
    =/  original-dome  dom
    |%
    ++  this-cor  .
    ++  sutil  (state:util original-dome dom ran)
    ::
    ::  Fetch the local disk, if it's there.
    ::
    ++  get-bob
      |=  gem=germ
      =/  m  (clad ,[bob=(unit yaki) gem=germ])
      ^-  form:m
      ?:  &(=(0 let.dom) !?=(?(%init %that) gem))
        (error:he cas %no-bob-disc ~)
      ?:  =(0 let.dom)
        (pure:m ~ %init)
      =/  tak  (~(get by hit.dom) let.dom)
      ?~  tak
        (error:he cas %no-bob-version ~)
      =/  bob  (~(get by hut.ran) u.tak)
      ?~  bob
        (error:he cas %no-bob-commit ~)
      (pure:m `u.bob gem)
    ::
    ::  Tell clay to get the state at the requested case for ali's desk.
    ::
    ++  fetch-ali
      =/  m  (clad ,[ali=yaki e=_this-cor])
      ^-  form:m
      ;<  ~  bind:m
        %+  just-do  /fetch-ali
        [%c %warp p.ali-disc q.ali-disc `[%sing %v cas /]]
      ;<  [rot=riot r=rang]  bind:m  (expect-clay ran)
      =.  ran  r
      ?~  rot
        (error:he cas %bad-fetch-ali ~)
      =/  ali-dome
          ;;  $:  ank=*
                  let=@ud
                  hit=(map @ud tako)
                  lab=(map @tas @ud)
              ==
          q.q.r.u.rot
      ?:  =(0 let.ali-dome)
        (error:he cas %no-ali-disc ~)
      =/  tak  (~(get by hit.ali-dome) let.ali-dome)
      ?~  tak
        (error:he cas %no-ali-version ~)
      =/  ali  (~(get by hut.ran) u.tak)
      ?~  ali
        (error:he cas %no-ali-commit ~)
      (pure:m u.ali this-cor)
    ::
    ::  Produce null if nothing to do; else perform merge
    ::
    ++  merge
      |=  [gem=germ cas=case ali=yaki bob=(unit yaki)]
      =/  m
        %-  clad
        %-  unit
        $:  conflicts=(set path)
            bop=(map path cage)
            new=yaki
            erg=(map path ?)
            e=_this-cor
        ==
      ^-  form:m
      ?-    gem
      ::
      ::  If this is an %init merge, we set the ali's commit to be bob's, and
      ::  we checkout the new state.
      ::
          $init
        %^  pure:m  ~  ~
        :^    ~
            ali
          (~(run by q.ali) |=(lobe %&))
        this-cor(hut.ran (~(put by hut.ran) r.ali ali))
      ::
      ::  If this is a %this merge, we check to see if ali's and bob's commits
      ::  are the same, in which case we're done.  Otherwise, we check to see
      ::  if ali's commit is in the ancestry of bob's, in which case we're
      ::  done.  Otherwise, we create a new commit with bob's data plus ali
      ::  and bob as parents.
      ::
          $this
        =/  bob  (need bob)
        ?:  =(r.ali r.bob)
          (pure:m ~)
        ?:  (~(has in (reachable-takos:sutil r.bob)) r.ali)
          (pure:m ~)
        =/  new  (make-yaki:sutil [r.ali r.bob ~] q.bob wen)
        %^  pure:m  ~  ~
        :^    ~
            new
          ~
        this-cor(hut.ran (~(put by hut.ran) r.new new))
      ::
      ::  If this is a %that merge, we check to see if ali's and bob's commits
      ::  are the same, in which case we're done.  Otherwise, we create a new
      ::  commit with ali's data plus ali and bob as parents.
      ::
          $that
        =/  bob  (need bob)
        ?:  =(r.ali r.bob)
          (pure:m ~)
        =/  new  (make-yaki:sutil [r.ali r.bob ~] q.ali wen)
        %^  pure:m  ~  ~
        :^    ~
            new
          %-  malt  ^-  (list {path ?})
          %+  murn  ~(tap by (~(uni by q.bob) q.ali))
          |=  {pax/path lob/lobe}
          ^-  (unit {path ?})
          =+  a=(~(get by q.ali) pax)
          =+  b=(~(get by q.bob) pax)
          ?:  =(a b)
            ~
          `[pax !=(~ a)]
        this-cor(hut.ran (~(put by hut.ran) r.new new))
      ::
      ::  If this is a %fine merge, we check to see if ali's and bob's commits
      ::  are the same, in which case we're done.  Otherwise, we check to see
      ::  if ali's commit is in the ancestry of bob's, in which case we're
      ::  done.  Otherwise, we check to see if bob's commit is in the ancestry
      ::  of ali's.  If not, this is not a fast-forward merge, so we error
      ::  out.  If it is, we add ali's commit to bob's desk and checkout.
      ::
          $fine
        =/  bob  (need bob)
        ?:  =(r.ali r.bob)
          (pure:m ~)
        ?:  (~(has in (reachable-takos:sutil r.bob)) r.ali)
          (pure:m ~)
        ?.  (~(has in (reachable-takos:sutil r.ali)) r.bob)
          (error:he cas %bad-fine-merge ~)
        %^  pure:m  ~  ~
        :^    ~
            ali
          %-  malt  ^-  (list {path ?})
          %+  murn  ~(tap by (~(uni by q.bob) q.ali))
          |=  {pax/path lob/lobe}
          ^-  (unit {path ?})
          =+  a=(~(get by q.ali) pax)
          =+  b=(~(get by q.bob) pax)
          ?:  =(a b)
            ~
          `[pax !=(~ a)]
        this-cor
      ::
      ::  If this is a %meet, %mate, or %meld merge, we may need to
      ::  fetch more data.  If this merge is either trivial or a
      ::  fast-forward, we short-circuit to either ++done or the %fine
      ::  case.
      ::
      ::  Otherwise, we find the best common ancestor(s) with
      ::  ++find-merge-points.  If there's no common ancestor, we error
      ::  out.  Additionally, if there's more than one common ancestor
      ::  (i.e. this is a criss-cross merge), we error out.  Something
      ::  akin to git's recursive merge should probably be used here,
      ::  but it isn't.
      ::
      ::  Once we have our single best common ancestor (merge base), we
      ::  store it in bas.  If this is a %mate or %meld merge, we diff
      ::  both against the mergebase, merge the conflicts, and build the
      ::  new commit.
      ::
      ::  Otherwise (i.e. this is a %meet merge), we create a list of
      ::  all the changes between the mergebase and ali's commit and
      ::  store it in ali-diffs, and we put a similar list for bob's
      ::  commit in bob-diffs.  Then we create bof, which is the a set
      ::  of changes in both ali and bob's commits.  If this has any
      ::  members, we have conflicts, which is an error in a %meet
      ::  merge, so we error out.
      ::
      ::  Otherwise, we merge the merge base data with ali's data and
      ::  bob's data, which produces the data for the new commit.
      ::
          ?($meet $mate $meld)
        =/  bob  (need bob)
        ?:  =(r.ali r.bob)
          (pure:m ~)
        ?.  (~(has by hut.ran) r.bob)
          (error:he cas %bad-bob-tako >r.bob< ~)
        ?:  (~(has in (reachable-takos:sutil r.bob)) r.ali)
          (pure:m ~)
        ?:  (~(has in (reachable-takos:sutil r.ali)) r.bob)
          $(gem %fine)
        =+  r=(find-merge-points:he ali bob)
        ?~  r
          (error:he cas %merge-no-merge-base ~)
        ?.  ?=({* ~ ~} r)
          =+  (lent ~(tap in `(set yaki)`r))
          (error:he cas %merge-criss-cross >[-]< ~)
        =/  bas  n.r
        ?:  ?=(?($mate $meld) gem)
          ;<  ali-diffs=cane              bind:m  (diff-bas ali bob bas)
          ;<  bob-diffs=cane              bind:m  (diff-bas bob ali bas)
          ;<  bof=(map path (unit cage))  bind:m
            (merge-conflicts can.ali-diffs can.bob-diffs)
          ;<    $:  conflicts=(set path)
                    bop=(map path cage)
                    new=yaki
                    erg=(map path ?)
                    e=_this-cor
                ==
              bind:m
            (build gem ali bob bas ali-diffs bob-diffs bof)
          (pure:m `[conflicts bop new erg e])
        =/  ali-diffs=cane  (calc-diffs:he ali bas)
        =/  bob-diffs=cane  (calc-diffs:he bob bas)
        =/  bof=(map path *)
          %-  %~  int  by
              %-  ~(uni by `(map path *)`new.ali-diffs)
              %-  ~(uni by `(map path *)`cal.ali-diffs)
              %-  ~(uni by `(map path *)`can.ali-diffs)
              `(map path *)`old.ali-diffs
          %-  ~(uni by `(map path *)`new.bob-diffs)
          %-  ~(uni by `(map path *)`cal.bob-diffs)
          %-  ~(uni by `(map path *)`can.bob-diffs)
          `(map path *)`old.bob-diffs
        ?.  =(~ bof)
          (error:he cas %meet-conflict >~(key by bof)< ~)
        =/  old=(map path lobe)
          %+  roll  ~(tap by (~(uni by old.ali-diffs) old.bob-diffs))
          =<  .(old q.bas)
          |=  {{pax/path ~} old/(map path lobe)}
          (~(del by old) pax)
        =/  hat=(map path lobe)
          %-  ~(uni by old)
          %-  ~(uni by new.ali-diffs)
          %-  ~(uni by new.bob-diffs)
          %-  ~(uni by cal.ali-diffs)
          cal.bob-diffs
        =/  del=(map path ?)
          (~(run by (~(uni by old.ali-diffs) old.bob-diffs)) |=(~ %|))
        =/  new  (make-yaki:sutil [r.ali r.bob ~] hat wen)
        %^  pure:m  ~  ~
        :^    ~
            new
          %-  ~(uni by del)
          ^-  (map path ?)
          %.  |=(lobe %&)
          ~(run by (~(uni by new.ali-diffs) cal.ali-diffs))
        this-cor(hut.ran (~(put by hut.ran) r.new new))
      ==
    ::
    ::  Diff a commit against the mergebase.
    ::
    ++  diff-bas
      |=  [yak=yaki yuk=yaki bas=yaki]
      =/  m  (clad ,cane)
      ^-  form:m
      ;<  ~  bind:m
        %+  just-do  /diff-bas
        :*  %f  %build  live=%.n  %pin  wen
            %list
            ^-  (list schematic:ford)
            %+  murn  ~(tap by q.bas)
            |=  {pax/path lob/lobe}
            ^-  (unit schematic:ford)
            =+  a=(~(get by q.yak) pax)
            ?~  a
              ~
            ?:  =(lob u.a)
              ~
            =+  (~(get by q.yuk) pax)
            ?~  -
              ~
            ?:  =(u.a u.-)
              ~
            :-  ~
            =/  disc  ford-disc
            :-  [%$ %path !>(pax)]
            :^  %diff  ford-disc
              (lobe-to-schematic:sutil disc pax lob)
            (lobe-to-schematic:sutil disc pax u.a)
        ==
      ;<  res=made-result:ford  bind:m  expect-ford
      =+  tay=(made-result-to-cages-or-error:util res)
      ?:  ?=(%| -.tay)
        (error:he cas %diff-ali-bad-made leaf+"merge diff ali failed" p.tay)
      =+  can=(cages-to-map:util p.tay)
      ?:  ?=(%| -.can)
        (error:he cas %diff-ali p.can)
      %-  pure:m
      :*  %-  molt
          %+  skip  ~(tap by q.yak)
          |=  {pax/path lob/lobe}
          (~(has by q.bas) pax)
        ::
          %-  molt  ^-  (list (pair path lobe))
          %+  murn  ~(tap by q.bas)
          |=  {pax/path lob/lobe}
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
          p.can
        ::
          %-  malt  ^-  (list {path ~})
          %+  murn  ~(tap by q.bas)
          |=  {pax/path lob/lobe}
          ?.  =(~ (~(get by q.yak) pax))
            ~
          (some pax ~)
      ==
    ::
    ::  Merge diffs that are on the same file.
    ::
    ++  merge-conflicts
      |=  [conflicts-ali=(map path cage) conflicts-bob=(map path cage)]
      =/  m  (clad ,bof=(map path (unit cage)))
      ^-  form:m
      ;<  ~  bind:m
        %+  just-do  /merge-conflicts
        :*  %f  %build  live=%.n  %list
            ^-  (list schematic:ford)
            %+  turn
              ~(tap by (~(int by conflicts-ali) conflicts-bob))
            |=  {pax/path *}
            ^-  schematic:ford
            =+  cal=(~(got by conflicts-ali) pax)
            =+  cob=(~(got by conflicts-bob) pax)
            =/  her
                =+  (slag (dec (lent pax)) pax)
                ?~(- %$ i.-)
            :-  [%$ %path !>(pax)]
            [%join [p.bob-disc q.bob-disc] her [%$ cal] [%$ cob]]
        ==
      ;<  res=made-result:ford  bind:m  expect-ford
      =+  tay=(made-result-to-cages-or-error:util res)
      ?:  ?=(%| -.tay)
        (error:he cas %merge-bad-made leaf+"merging failed" p.tay)
      =+  can=(cages-to-map:util p.tay)
      ?:  ?=(%| -.can)
        (error:he cas %merge p.can)
      %-  pure:m
      (~(run by p.can) (flit |=({a/mark ^} !?=($null a))))
    ::
    ::  Apply the patches in bof to get the new merged content.
    ::
    ::  Gather all the changes between ali's and bob's commits and the
    ::  mergebase.  This is similar to the %meet of ++merge, except
    ::  where they touch the same file, we use the merged versions.
    ::
    ++  build
      |=  $:  gem=germ
              ali=yaki
              bob=yaki
              bas=yaki
              dal=cane
              dob=cane
              bof=(map path (unit cage))
          ==
      =/  m
        %-  clad
        $:  conflicts=(set path)
            bop=(map path cage)
            new=yaki
            erg=(map path ?)
            e=_this-cor
        ==
      ^-  form:m
      ;<  ~  bind:m
        %+  just-do  /build
        :*  %f  %build  live=%.n  %list
            ^-  (list schematic:ford)
            %+  murn  ~(tap by bof)
            |=  {pax/path cay/(unit cage)}
            ^-  (unit schematic:ford)
            ?~  cay
              ~
            :-  ~
            :-  [%$ %path !>(pax)]
            =+  (~(get by q.bas) pax)
            ?~  -
              ~|  %mate-strange-diff-no-base
              !!
            :*  %pact
                [p.bob-disc q.bob-disc]
                (lobe-to-schematic:sutil ford-disc pax u.-)
                [%$ u.cay]
            ==
        ==
      ;<  res=made-result:ford  bind:m  expect-ford
      =+  tay=(made-result-to-cages-or-error:util res)
      ?:  ?=(%| -.tay)
        (error:he cas %build-bad-made leaf+"delta building failed" p.tay)
      =/  bop  (cages-to-map:util p.tay)
      ?:  ?=(%| -.bop)
        (error:he cas %built p.bop)
      =/  both-patched  p.bop
      =/  con=(map path *)                            ::  2-change conflict
        %-  molt
        %+  skim  ~(tap by bof)
        |=({pax/path cay/(unit cage)} ?=(~ cay))
      =/  cab=(map path lobe)                         ::  conflict base
        %-  ~(urn by con)
        |=  {pax/path *}
        (~(got by q.bas) pax)
      =.  con                                         ::  change+del conflict
        %-  ~(uni by con)
        %-  malt  ^-  (list {path *})
        %+  skim  ~(tap by old.dal)
        |=  {pax/path ~}
        ?:  (~(has by new.dob) pax)
          ~|  %strange-add-and-del
          !!
        (~(has by can.dob) pax)
      =.  con                                         ::  change+del conflict
        %-  ~(uni by con)
        %-  malt  ^-  (list {path *})
        %+  skim  ~(tap by old.dob)
        |=  {pax/path ~}
        ?:  (~(has by new.dal) pax)
          ~|  %strange-del-and-add
          !!
        (~(has by can.dal) pax)
      =.  con                                         ::  add+add conflict
        %-  ~(uni by con)
        %-  malt  ^-  (list {path *})
        %+  skip  ~(tap by (~(int by new.dal) new.dob))
        |=  {pax/path *}
        =((~(got by new.dal) pax) (~(got by new.dob) pax))
      ?:  &(?=($mate gem) ?=(^ con))
        =+  (turn ~(tap by `(map path *)`con) |=({path *} >[+<-]<))
        (error:he cas %mate-conflict -)
      =/  old=(map path lobe)                         ::  oldies but goodies
        %+  roll  ~(tap by (~(uni by old.dal) old.dob))
        =<  .(old q.bas)
        |=  {{pax/path ~} old/(map path lobe)}
        (~(del by old) pax)
      =/  can=(map path cage)                         ::  content changes
        %-  molt
        ^-  (list (pair path cage))
        %+  murn  ~(tap by bof)
        |=  {pax/path cay/(unit cage)}
        ^-  (unit (pair path cage))
        ?~  cay
          ~
        `[pax u.cay]
      =^  hot  lat.ran                                ::  new content
        ^-  {(map path lobe) (map lobe blob)}
        %+  roll  ~(tap by can)
        =<  .(lat lat.ran)
        |=  {{pax/path cay/cage} hat/(map path lobe) lat/(map lobe blob)}
        =+  ^=  bol
            =+  (~(get by q.bas) pax)
            ?~  -
              ~|  %mate-strange-diff-no-base
              !!
            %^    make-delta-blob:sutil
                (page-to-lobe:sutil [p q.q]:(~(got by both-patched) pax))
              [(lobe-to-mark:sutil u.-) u.-]
            [p q.q]:cay
        :-  (~(put by hat) pax p.bol)
        ?:  (~(has by lat) p.bol)
          lat
        (~(put by lat) p.bol bol)
      ::  ~&  old=(~(run by old) mug)
      ::  ~&  newdal=(~(run by new.dal) mug)
      ::  ~&  newdob=(~(run by new.dob) mug)
      ::  ~&  caldal=(~(run by cal.dal) mug)
      ::  ~&  caldob=(~(run by cal.dob) mug)
      ::  ~&  hot=(~(run by hot) mug)
      ::  ~&  cas=(~(run by cas) mug)
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
      =/  new  (make-yaki:sutil [r.ali r.bob ~] hat wen)
      %-  pure:m
      :*  (silt (turn ~(tap by con) head))
          both-patched
          new
        ::
          %-  ~(uni by del)
          ^-  (map path ?)
          %.  |=(lobe %&)
          %~  run  by
          %-  ~(uni by new.dal)
          %-  ~(uni by cal.dal)
          %-  ~(uni by cab)
          hot
        ::
          this-cor(hut.ran (~(put by hut.ran) r.new new))
      ==
    ::
    ::  Convert new commit into actual data (i.e. blobs rather than
    ::  lobes).  Apply the new commit to our state
    ::
    ++  checkout
      |=  [gem=germ cas=case bob=(unit yaki) new=yaki bop=(map path cage)]
      =/  m  (clad ,_this-cor)
      ^-  form:m
      ;<  ~  bind:m
        =/  val=beak
            ?:  ?=($init gem)
              [p.ali-disc q.ali-disc cas]
            [p.bob-disc q.bob-disc da+wen]
        %+  just-do  /checkout
        :*  %f  %build  live=%.n  %pin  wen  %list
            ^-  (list schematic:ford)
            %+  murn  ~(tap by q.new)
            |=  {pax/path lob/lobe}
            ^-  (unit schematic:ford)
            ?:  (~(has by bop) pax)
              ~
            :+  ~
              [%$ %path !>(pax)]
            (merge-lobe-to-schematic:he (fall bob *yaki) ford-disc pax lob)
        ==
      ;<  res=made-result:ford  bind:m  expect-ford
      =+  tay=(made-result-to-cages-or-error:util res)
      ?:  ?=(%| -.tay)
        (error:he cas %checkout-bad-made leaf+"merge checkout failed" p.tay)
      =+  can=(cages-to-map:util p.tay)
      ?:  ?=(%| -.can)
        (error:he cas %checkout p.can)
      =.  let.dom  +(let.dom)
      =.  hit.dom  (~(put by hit.dom) let.dom r.new)
      =.  ank.dom
        %-  map-to-ankh:sutil
        %-  ~(run by (~(uni by bop) p.can))
        |=(cage [(page-to-lobe:sutil p q.q) +<])
      (pure:m this-cor)
    ::
    ::  Cast all the content that we're going to tell unix about to
    ::  %mime, then tell unix.
    ::
    ++  ergo
      |=  [gem=germ cas=case mon=(map term beam) erg=(map path ?) new=yaki]
      =/  m  (clad ,mim=(map path mime))
      ^-  form:m
      =+  must=(must-ergo:util our q.bob-disc mon (turn ~(tap by erg) head))
      ?:  =(~ must)
        (pure:m mim.dom)
      =/  sum=(set path)
        =+  (turn ~(tap by must) (corl tail tail))
        %+  roll  -
        |=  {pak/(set path) acc/(set path)}
        (~(uni in acc) pak)
      =/  val=beak
        ?:  ?=($init gem)
          [p.ali-disc q.ali-disc cas]
        [p.bob-disc q.bob-disc da+wen]
      ;<  ~  bind:m
        %+  just-do  /ergo
        :*  %f  %build  live=%.n  %pin  wen  %list
            ^-  (list schematic:ford)
            %+  turn  ~(tap in sum)
            |=  a/path
            ^-  schematic:ford
            :-  [%$ %path !>(a)]
            =+  b=(~(got by erg) a)
            ?.  b
              [%$ %null !>(~)]
            =/  disc  ford-disc  ::  [p q]:val
            :^  %cast  ford-disc  %mime
            (lobe-to-schematic:sutil disc a (~(got by q.new) a))
        ==
      ;<  res=made-result:ford  bind:m  expect-ford
      =+  tay=(made-result-to-cages-or-error:util res)
      ?:  ?=(%| -.tay)
        (error:he cas %ergo-bad-made leaf+"merge ergo failed" p.tay)
      =+  =|  nac=mode
          |-  ^-  tan=$^(mode {p/term q/tang})
          ?~  p.tay  nac
          =*  pax  p.i.p.tay
          ?.  ?=($path p.pax)
            [%ergo >[%expected-path got=p.pax]< ~]
          =*  mim  q.i.p.tay
          =+  mit=?.(?=($mime p.mim) ~ `;;(mime q.q.mim))
          $(p.tay t.p.tay, nac :_(nac [;;(path q.q.pax) mit]))
      ?:  ?=([@ *] tan)  (error:he cas tan)
      =/  changes=(map path (unit mime))  (malt tan)
      =/  mim  (apply-changes-to-mim:util mim.dom changes)
      ?~  hez
        (error:he cas %ergo-no-hez ~)
      ;<  ~  bind:m  (give-ergo:util u.hez our q.bob-disc mon changes)
      (pure:m mim)
    ::
    ::  A small set of helper functions to assist in merging.
    ::
    ++  he
      |%
      ::
      ::  Cancel the merge gracefully and produce an error.
      ::
      ++  error
        |=  [cas=case err=term tan=(list tank)]
        (clad-fail err >ali-disc< >bob-disc< >cas< tan)
      ::
      ++  calc-diffs
        |=  [hed=yaki bas=yaki]
        ^-  cane
        :*  %-  molt
            %+  skip  ~(tap by q.hed)
            |=  {pax/path lob/lobe}
            (~(has by q.bas) pax)
          ::
            %-  molt
            %+  skip  ~(tap by q.hed)
            |=  {pax/path lob/lobe}
            =+  (~(get by q.bas) pax)
            |(=(~ -) =([~ lob] -))
          ::
            ~
          ::
            %-  malt  ^-  (list {path ~})
            %+  murn  ~(tap by q.bas)
            |=  {pax/path lob/lobe}
            ^-  (unit (pair path ~))
            ?.  =(~ (~(get by q.hed) pax))
              ~
            `[pax ~]
        ==
      ::
      ::  Create a schematic to turn a lobe into a blob.
      ::
      ::  We short-circuit if we already have the content somewhere.
      ::
      ++  merge-lobe-to-schematic
        |=  [bob=yaki disc=disc:ford pax=path lob=lobe]
        ^-  schematic:ford
        =+  lol=(~(get by q.bob) pax)
        |-  ^-  schematic:ford
        ?:  =([~ lob] lol)
          =+  (need (need (read-x:sutil & let.dom pax)))
          ?>  ?=(%& -<)
          [%$ p.-]
        ::  ?:  =([~ lob] lal)
        ::    [%$ +:(need fil.ank:(descend-path:(zu:sutil ank:(need alh)) pax))]
        =+  bol=(~(got by lat.ran) lob)
        ?-  -.bol
            $direct  (page-to-schematic:sutil disc q.bol)
            $delta
          [%pact disc $(lob q.q.bol) (page-to-schematic:sutil disc r.bol)]
        ==
      ::
      ::  Find the most recent common ancestor(s).
      ::
      ++  find-merge-points
        |=  {p/yaki q/yaki}                           ::  maybe need jet
        ^-  (set yaki)
        %-  reduce-merge-points
        =+  r=(reachable-takos:sutil r.p)
        |-  ^-  (set yaki)
        ?:  (~(has in r) r.q)  (~(put in *(set yaki)) q)
        %+  roll  p.q
        |=  {t/tako s/(set yaki)}
        ?:  (~(has in r) t)
          (~(put in s) (tako-to-yaki:sutil t))        ::  found
        (~(uni in s) ^$(q (tako-to-yaki:sutil t)))    ::  traverse
      ::
      ::  Helper for ++find-merge-points.
      ::
      ++  reduce-merge-points
        |=  unk/(set yaki)                            ::  maybe need jet
        =|  gud/(set yaki)
        =+  ^=  zar
            ^-  (map tako (set tako))
            %+  roll  ~(tap in unk)
            |=  {yak/yaki qar/(map tako (set tako))}
            (~(put by qar) r.yak (reachable-takos:sutil r.yak))
        |-
        ^-  (set yaki)
        ?~  unk  gud
        =+  bun=(~(del in `(set yaki)`unk) n.unk)
        ?:  %+  levy  ~(tap by (~(uni in gud) bun))
            |=  yak/yaki
            !(~(has in (~(got by zar) r.yak)) r.n.unk)
          $(gud (~(put in gud) n.unk), unk bun)
        $(unk bun)
      --
    --
  --
::
::  Mount a beam to unix
::
++  mount
  |=  $:  our=ship
          syd=desk
          wen=@da
          hez=duct
          dom=dome
          ran=rang
      ==
  |^
  |=  [pot=term bem=beam mon=(map term beam)]
  =/  m  mount-clad
  ^-  form:m
  =/  old-mon  (~(get by mon) pot)
  ?^  old-mon
    (clad-fail %already-mounted >u.old-mon< ~)
  =.  mon  (~(put by mon) pot bem)
  ;<  changes=(map path (unit mime))  bind:m  (cast-to-mime bem)
  ;<  ~                               bind:m  (ergo changes mon)
  =/  mim  (apply-changes-to-mim:util mim.dom changes)
  (pure:m [pot bem] mim)
  ::
  ++  sutil  (state:util dom dom ran)
  ::  Initializes a new mount point.
  ::
  ++  cast-to-mime
    |=  bem=beam
    =/  m  (clad ,(map path (unit mime)))
    ^-  form:m
    =*  pax  s.bem
    =/  =aeon  (need (case-to-aeon-before:sutil wen r.bem))
    =/  must
      =/  all  (turn ~(tap by q:(aeon-to-yaki:sutil aeon)) head)
      (skim all |=(paf/path =(pax (scag (lent pax) paf))))
    ?~  must
      (pure:m ~)
    ;<  ~  bind:m
      %+  just-do  /ergoing
      :*  %f  %build  live=%.n  %list
          ^-  (list schematic:ford)
          %+  turn  `(list path)`must
          |=  a/path
          :-  [%$ %path !>(a)]
          :^  %cast  [our %home]  %mime
          =+  (need (need (read-x:sutil & aeon a)))
          ?:  ?=(%& -<)
            [%$ p.-]
          (lobe-to-schematic:sutil [our %home] a p.-)
      ==
    ;<  res=made-result:ford  bind:m  expect-ford
    ?:  ?=([%incomplete *] res)
      (clad-fail %ergo-fail-incomplete leaf+"clay ergo incomplete" tang.res)
    ?.  ?=([%complete %success *] res)
      (clad-fail %ergo-fail leaf+"clay ergo failed" message.build-result.res)
    %-  pure:m
    %-  malt  ^-  mode
    %+  turn  (made-result-to-cages:util res)
    |=  [pax=cage mim=cage]
    ?.  ?=($path p.pax)
      ~|(%ergo-bad-path-mark !!)
    :-  ;;(path q.q.pax)
    ?.  ?=($mime p.mim)
      ~
    `;;(mime q.q.mim)
  ::
  ::  Send changes to unix
  ::
  ++  ergo
    |=  [changes=(map path (unit mime)) mon=(map term beam)]
    (give-ergo:util hez our syd mon changes)
  --
::
::  A simple foreign request.
::
++  foreign-request
  |=  $:  our=ship
          her=ship
          syd=desk
          wen=@da
      ==
  |^
  |=  [=rave =rand]
  =/  m  request-clad
  ^-  form:m
  ?-    p.p.rand
      $a  ~|  %no-big-ford-builds-across-network-for-now  !!
      $b  ~|  %i-guess-you-ought-to-build-your-own-marks  !!
      $c  ~|  %casts-should-be-compiled-on-your-own-ship  !!
      $d  ~|  %totally-temporary-error-please-replace-me  !!
      $p  ~|  %requesting-foreign-permissions-is-invalid  !!
      $s  ~|  %please-dont-get-your-takos-over-a-network  !!
      $t  ~|  %requesting-foreign-directory-is-vaporware  !!
      $u  ~|  %prolly-poor-idea-to-get-rang-over-network  !!
      $v  ~|  %weird-shouldnt-get-v-request-from-network  !!
      $z  ~|  %its-prolly-not-reasonable-to-request-ankh  !!
      $x  (validate-x [p.p q.p q r]:rand)
      $y  (pure:m [p.r.rand !>(;;(arch q.r.rand))])
  ::
      $w
    %-  pure:m
    :-  p.r.rand
    ?+  p.r.rand  ~|  %strange-w-over-nextwork  !!
      $cass  !>(;;(cass q.r.rand))
      $null  [[%atom %n ~] ~]
      $nako  !>(~|([%molding [&1 &2 &3]:q.r.rand] ;;(nako q.r.rand)))
    ==
  ==
  ::
  ::  Make sure that incoming data is of the mark it claims to be.
  ::
  ++  validate-x
    |=  [car=care cas=case pax=path peg=page]
    =/  m  (clad ,cage)
    ;<  ~  bind:m
      %+  just-do  /foreign-x
      [%f %build live=%.n %pin wen (vale-page:util [our %home] peg)]
    ;<  res=made-result:ford  bind:m  expect-ford
    ^-  form:m
    ?.  ?=([%complete %success *] res)
      =/  message  (made-result-as-error:ford res)
      (clad-fail %validate-foreign-x-failed message)
    (pure:m (result-to-cage:ford build-result.res))
  --
::
::  A full foreign update.  Validate and apply to our local cache of
::  their state.
::
++  foreign-update
  |=  $:  our=ship
          her=ship
          syd=desk
          wen=@da
      ==
  |^
  |=  [=moat rand=(unit rand) lim=@da dom=dome ran=rang]
  =/  m  update-clad
  ^-  form:m
  ?~  rand
    (pure:m ~)
  =/  lem  ?.(?=(%da -.to.moat) lim p.to.moat)
  ?>  ?=(%nako p.r.u.rand)
  =/  nako  ;;(nako q.r.u.rand)
  ?:  =(0 let.dom)
    ;<  [dom=dome ran=rang]  bind:m  (apply-foreign-update nako dom ran)
    (pure:m ~ lem dom ran)
  ;<  blobs=(set blob)     bind:m  (validate-plops bar.nako)
  ;<  [dom=dome ran=rang]  bind:m
    (apply-foreign-update nako(bar blobs) dom ran)
  (pure:m ~ lem dom ran)
  ::
  ::  Make sure that incoming data is of the mark it claims to be.
  ::
  ++  validate-plops
    |=  plops=(set plop)
    =/  m  (clad ,(set blob))
    ^-  form:m
    ;<  ~  bind:m
      %+  just-do  /validate-plops
      :*  %f  %build  live=%.n  %pin  wen
          %list
          ^-  (list schematic:ford)
          %+  turn  ~(tap in plops)
          |=  a/plop
          ?-  -.a
              $direct
            :-  [%$ %blob !>([%direct p.a *page])]
            (vale-page:util [our %home] p.q.a q.q.a)
          ::
              $delta
            :-  [%$ %blob !>([%delta p.a q.a *page])]
            (vale-page:util [our %home] p.r.a q.r.a)
          ==
      ==
    ;<  res=made-result:ford  bind:m  expect-ford
    =/  cages  (made-result-to-cages-or-error:util res)
    ?:  ?=(%| -.cages)
      (clad-fail %validate-plops-failed p.cages)
    =|  blobs=(list blob)
    |-  ^-  form:m
    ?~  p.cages
      (pure:m (silt blobs))
    =*  bob  p.i.p.cages
    =*  cay  q.i.p.cages
    ?.  ?=(%blob p.bob)
      (clad-fail %validate-plops-not-blob >p.bob< ~)
    =/  new-blob=blob
      =/  blob  ;;(blob q.q.bob)
      ?-  -.blob
        %delta   [-.blob p.blob q.blob p.cay q.q.cay]
        %direct  [-.blob p.blob p.cay q.q.cay]
      ==
    $(p.cages t.p.cages, blobs [new-blob blobs])
  ::
  ::  When we get a %w foreign update, store this in our state.
  ::
  ::  We get the commits and blobs from the nako and add them to our object
  ::  store, then we update the map of aeons to commits and the latest aeon.
  ::
  ++  apply-foreign-update
    |=  [=nako dom=dome ran=rang]
    =/  m  (clad ,[dome rang])
    ^-  form:m
    ::  hit: updated commit-hashes by @ud case
    ::
    =/  hit  (~(uni by hit.dom) gar.nako)
    ::  nut: new commit-hash/commit pairs
    ::
    =/  nut
      (turn ~(tap in lar.nako) |=(=yaki [r.yaki yaki]))
    ::  hut: updated commits by hash
    ::
    =/  hut  (~(uni by (malt nut)) hut.ran)
    ::  nat: new blob-hash/blob pairs
    ::
    =/  nat
      (turn ~(tap in bar.nako) |=(=blob [p.blob blob]))
    ::  lat: updated blobs by hash
    ::
    =/  lat  (~(uni by (malt nat)) lat.ran)
    ::  traverse updated state and sanity check
    ::
    =+  ~|  :*  %bad-foreign-update
                [gar=gar let=let.nako nut=(turn nut head) nat=(turn nat head)]
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
    =:  let.dom   (max let.nako let.dom)
        hit.dom   hit
        hut.ran   hut
        lat.ran   lat
      ==
    (pure:m dom ran)
  --
::
::  An assortment of useful functions, used in +commit, +merge, and +de
::
++  util
  |%
  ::  Hash a page to get a lobe.
  ::
  ++  page-to-lobe  |=(page (shax (jam +<)))
  ::
  ::  Takes a list of changed paths and finds those paths that are inside a
  ::  mount point (listed in `mon`).
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
  ::  Send changes to unix
  ::
  ++  give-ergo
    |=  $:  hez=duct
            our=ship
            syd=desk
            mon=(map term beam)
            changes=(map path (unit mime))
        ==
    =/  m  (clad ,~)
    ^-  form:m
    =/  must  (must-ergo our syd mon (turn ~(tap by changes) head))
    |=  clad-input
    :-  ~  :_  [%done ~]
    %+  turn  ~(tap by must)
    |=  [pot=term len=@ud pak=(set path)]
    :*  hez  %give  %ergo  pot
        %+  turn  ~(tap in pak)
        |=  pax=path
        [(slag len pax) (~(got by changes) pax)]
    ==
  ::
  ::  Add or remove entries to the mime cache
  ::
  ++  apply-changes-to-mim
    |=  [mim=(map path mime) changes=(map path (unit mime))]
    ^-  (map path mime)
    =/  caz=(list [pax=path change=(unit mime)])  ~(tap by changes)
    |-  ^-  (map path mime)
    ?~  caz
      mim
    ?~  change.i.caz
      $(caz t.caz, mim (~(del by mim) pax.i.caz))
    $(caz t.caz, mim (~(put by mim) [pax u.change]:i.caz))
  ::
  ::  Create a schematic to validate a page.
  ::
  ::  If the mark is %hoon, we short-circuit the validation for bootstrapping
  ::  purposes.
  ::
  ++  vale-page
    |=  [=disc:ford a=page]
    ^-  schematic:ford
    ?.  ?=($hoon p.a)  [%vale disc a]
    ?.  ?=(@t q.a)  [%dude >%weird-hoon< %ride [%zpzp ~] %$ *cage]
    [%$ p.a [%atom %t ~] q.a]
  ::
  ::  Crashes on ford failure
  ::
  ++  ford-fail  |=(tan/tang ~|(%ford-fail (mean tan)))
  ::
  ::  Takes either a result or a stack trace.  If it's a stack trace, we crash;
  ::  else, we produce the result.
  ::
  ++  unwrap-tang
    |*  res/(each * tang)
    ?:(?=(%& -.res) p.res (mean p.res))
  ::
  ::  Parse a gage to a list of pairs of cages, crashing on error.
  ::
  ::  Composition of ++gage-to-cages-or-error and ++unwrap-tang.  Maybe same as
  ::  ++gage-to-success-cages?
  ::
  ++  made-result-to-cages
    |=  result=made-result:ford
    ^-  (list (pair cage cage))
    (unwrap-tang (made-result-to-cages-or-error result))
  ::
  ::  Same as ++gage-to-cages-or-error except crashes on error.  Maybe same as
  ::  ++gage-to-cages?
  ::
  ++  made-result-to-success-cages
    |=  result=made-result:ford
    ^-  (list (pair cage cage))
    ?.  ?=([%complete %success %list *] result)
      (ford-fail >%strange-ford-result< ~)
    ::  process each row in the list, filtering out errors
    ::
    %+  murn  results.build-result.result
    |=  row=build-result:ford
    ^-  (unit [cage cage])
    ::
    ?:  ?=([%error *] row)
      ~&  [%clay-whole-build-failed message.row]
      ~
    ?:  ?=([%success [%error *] *] row)
      ~&  [%clay-first-failure message.head.row]
      ~
    ?:  ?=([%success [%success *] [%error *]] row)
      ~&  %clay-second-failure
      %-  (slog message.tail.row)
      ~
    ?.  ?=([%success [%success *] [%success *]] row)
      ~
    `[(result-to-cage:ford head.row) (result-to-cage:ford tail.row)]
  ::
  ::  Expects a single-level gage (i.e. a list of pairs of cages).  If the
  ::  result is of a different form, or if some of the computations in the gage
  ::  failed, we produce a stack trace.  Otherwise, we produce the list of pairs
  ::  of cages.
  ::
  ++  made-result-to-cages-or-error
    |=  result=made-result:ford
    ^-  (each (list (pair cage cage)) tang)
    ::
    ?:  ?=([%incomplete *] result)
      (mule |.(`~`(ford-fail tang.result)))
    ?.  ?=([%complete %success %list *] result)
      (mule |.(`~`(ford-fail >%strange-ford-result -.build-result.result< ~)))
    =/  results=(list build-result:ford)
      results.build-result.result
    =<  ?+(. [%& .] {@ *} .)
    |-
    ^-  ?((list [cage cage]) (each ~ tang))
    ?~  results  ~
    ::
    ?.  ?=([%success ^ *] i.results)
      (mule |.(`~`(ford-fail >%strange-ford-result< ~)))
    ?:  ?=([%error *] head.i.results)
      (mule |.(`~`(ford-fail message.head.i.results)))
    ?:  ?=([%error *] tail.i.results)
      (mule |.(`~`(ford-fail message.tail.i.results)))
    ::
    =+  $(results t.results)
    ?:  ?=([@ *] -)  -
    :_  -
    [(result-to-cage:ford head.i.results) (result-to-cage:ford tail.i.results)]
  ::
  ::  Assumes the list of pairs of cages is actually a listified map of paths
  ::  to cages, and converts it to (map path cage) or a stack trace on error.
  ::
  ++  cages-to-map
    |=  tay/(list (pair cage cage))
    =|  can/(map path cage)
    |-  ^-  (each (map path cage) tang)
    ?~  tay   [%& can]
    =*  pax  p.i.tay
    ?.  ?=($path p.pax)
      (mule |.(`~`~|([%expected-path got=p.pax] !!)))
    $(tay t.tay, can (~(put by can) ;;(path q.q.pax) q.i.tay))
  ::
  ::  Useful functions which operate on a dome and a rang.
  ::
  ::  `original-dome` is the dome which we had when the transaction
  ::  started.  This is used as a lobe-to-blob cache in
  ::  +lobe-to-schematic so we don't have to recalculate the blobs for
  ::  files which haven't changed.
  ::
  ++  state
    |=  [original-dome=dome dom=dome ran=rang]
    |%
    ::  These convert between aeon (version number), tako (commit hash), yaki
    ::  (commit data structure), lobe (content hash), and blob (content).
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
    ::  Create a schematic out of a page (which is a [mark noun]).
    ::
    ++  page-to-schematic
      |=  [disc=disc:ford a=page]
      ^-  schematic:ford
      ?.  ?=($hoon p.a)  [%volt disc a]
      ::  %hoon bootstrapping
      [%$ p.a [%atom %t ~] q.a]
    ::
    ::  Create a schematic out of a lobe (content hash).
    ::
    ++  lobe-to-schematic  (cury lobe-to-schematic-p &)
    ++  lobe-to-schematic-p
      =.  dom  original-dome
      |=  [local=? disc=disc:ford pax=path lob=lobe]
      ^-  schematic:ford
      ::
      =/  hat/(map path lobe)
          ?:  =(let.dom 0)
            ~
          q:(aeon-to-yaki let.dom)
      =+  lol=`(unit lobe)`?.(local `0vsen.tinel (~(get by hat) pax))
      |-  ^-  schematic:ford
      ?:  =([~ lob] lol)
        =+  (need (need (read-x & let.dom pax)))
        ?>  ?=(%& -<)
        [%$ p.-]
      =+  bol=(~(got by lat.ran) lob)
      ?-  -.bol
        $direct  (page-to-schematic disc q.bol)
        $delta   ~|  delta+q.q.bol
                 [%pact disc $(lob q.q.bol) (page-to-schematic disc r.bol)]
      ==
    ::
    ::  Hash a page to get a lobe.
    ::
    ++  page-to-lobe  |=(page (shax (jam +<)))
    ::
    ::  Make a direct blob out of a page.
    ::
    ++  make-direct-blob
      |=  p/page
      ^-  blob
      [%direct (page-to-lobe p) p]
    ::
    ::  Make a delta blob out of a lobe, mark, lobe of parent, and page of diff.
    ::
    ++  make-delta-blob
      |=  {p/lobe q/{p/mark q/lobe} r/page}
      ^-  blob
      [%delta p q r]
    ::
    ::  Make a commit out of a list of parents, content, and date.
    ::
    ++  make-yaki
      |=  {p/(list tako) q/(map path lobe) t/@da}
      ^-  yaki
      =+  ^=  has
          %^  cat  7  (sham [%yaki (roll p add) q t])
          (sham [%tako (roll p add) q t])
      [p q has t]
    ::
    ++  case-to-date
      |=  [now=@da =case]
      ^-  @da
      ::  if the case is already a date, use it.
      ::
      ?:  ?=([%da *] case)
        p.case
      ::  translate other cases to dates
      ::
      =/  aey  (case-to-aeon-before now case)
      ?~  aey  `@da`0
      ?:  =(0 u.aey)  `@da`0
      t:(aeon-to-yaki u.aey)
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
            %-  aeon-to-yaki
            let.dom
          [~ let.dom]
        $(let.dom (dec let.dom))
      ::
          $tas  (~(get by lab.dom) p.lok)
          $ud   ?:((gth p.lok let.dom) ~ [~ p.lok])
      ==
    ::
    ::  Convert a map of paths to data into an ankh.
    ::
    ++  map-to-ankh
      |=  hat/(map path (pair lobe cage))
      ^-  ankh
      %+  roll  ~(tap by hat)
      |=  {{pat/path lob/lobe zar/cage} ank/ankh}
      ^-  ankh
      ?~  pat
        ank(fil [~ lob zar])
      =+  nak=(~(get by dir.ank) i.pat)
      %=  ank
        dir  %+  ~(put by dir.ank)  i.pat
             $(pat t.pat, ank (fall nak *ankh))
      ==
    ::
    ::  Update the object store with new blobs.
    ::
    ::    Must uni the old-lat into the new-lat so that if we recreate
    ::    the same blob hash, we use the old blob not the new one.  Else
    ::    you get mutually recurring %delta blobs.
    ++  add-blobs
      |=  [new-blobs=(map path blob) old-lat=(map lobe blob)]
      ^-  (map lobe blob)
      =/  new-lat=(map lobe blob)
        %-  malt
        %+  turn
          ~(tap by new-blobs)
        |=  [=path =blob]
        [p.blob blob]
      (~(uni by new-lat) old-lat)
    ::
    ::  Apply a change list, creating the commit and applying it to
    ::  the current state.
    ::
    ++  execute-changes
      |=  [wen=@da lem=suba]
      ^-  (unit [dome rang])
      =/  parent
        ?:  =(0 let.dom)
          ~
        [(aeon-to-tako let.dom)]~
      =/  new-blobs  (apply-changes lem)
      =.  lat.ran  (add-blobs new-blobs lat.ran)
      =/  new-lobes  (~(run by new-blobs) |=(=blob p.blob))
      =/  new-yaki  (make-yaki parent new-lobes wen)
      ::  if no changes and not first commit or merge, abort
      ?.  ?|  =(0 let.dom)
              !=((lent p.new-yaki) 1)
              !=(q.new-yaki q:(aeon-to-yaki let.dom))
          ==
          ~
      =:  let.dom  +(let.dom)
          hit.dom  (~(put by hit.dom) +(let.dom) r.new-yaki)
          hut.ran  (~(put by hut.ran) r.new-yaki new-yaki)
      ==
      `[dom ran]
    ::
    ::  Apply label to current revision
    ::
    ++  execute-label
      |=  lab=@tas
      ?<  (~(has by lab.dom) lab)
      dom(lab (~(put by lab.dom) lab let.dom))
    ::
    ::  Apply a list of changes against the current state and produce
    ::  the new state.
    ::
    ++  apply-changes                                   ::   apply-changes
      |=  [change-files=(list [p=path q=misu])]
      ^-  (map path blob)
      =+  ^=  old-files                                 ::  current state
          ?:  =(let.dom 0)                              ::  initial commit
            ~                                           ::  has nothing
          =<  q
          %-  aeon-to-yaki
          let.dom
      =;  new-files=(map path blob)
          =+  sar=(silt (turn change-files head))       ::  changed paths
          %+  roll  ~(tap by old-files)                 ::  find unchanged
          =<  .(bat new-files)
          |=  [[pax=path gar=lobe] bat=(map path blob)]
          ?:  (~(has in sar) pax)                       ::  has update
            bat
          %+  ~(put by bat)  pax
          ~|  [pax gar (lent ~(tap by lat.ran))]
          (lobe-to-blob gar)                            ::  use original
      %+  roll  change-files
      |=  {{pax/path mys/misu} new-files/(map path blob)}
      ^+  new-files
      ?-    -.mys
          $ins                                          ::  insert if not exist
        ?:  (~(has by new-files) pax)
          ~|([%ins-new-files pax] !!)
        ?:  (~(has by old-files) pax)
          ~|([%ins-old-files pax] !!)
        %+  ~(put by new-files)  pax
        %-  make-direct-blob
        ?:  &(?=($mime -.p.mys) =([%hoon ~] (slag (dec (lent pax)) pax)))
          `page`[%hoon +.+.q.q.p.mys]
        [p q.q]:p.mys
      ::
          $del                                          ::  delete if exists
        ?>  |((~(has by old-files) pax) (~(has by new-files) pax))
        (~(del by new-files) pax)
      ::
          $dif                                          ::  mutate, must exist
        =+  ber=(~(get by new-files) pax)               ::  XX  typed
        =+  her==>((flop pax) ?~(. %$ i))
        ?~  ber
          =+  har=(~(get by old-files) pax)
          ?~  har  !!
          %+  ~(put by new-files)  pax
          (make-delta-blob p.mys [(lobe-to-mark u.har) u.har] [p q.q]:q.mys)
                                                        :: XX check vase !evil
        ::  XX of course that's a problem, p.u.ber isn't in rang since it
        ::     was just created.  We shouldn't be sending multiple
        ::     diffs
        ::  %+  ~(put by bar)  pax
        ::  %^  make-delta-blob  p.mys
        ::    [(lobe-to-mark p.u.ber) p.u.ber]
        ::  [p q.q]:q.mys
        ::                                              :: XX check vase !evil
        ~|([%two-diffs-for-same-file pax] !!)
      ==
    ::
    ::  Traverse parentage and find all ancestor hashes
    ::
    ++  reachable-takos                                 ::  reachable
      |=  p/tako
      ^-  (set tako)
      =+  y=(tako-to-yaki p)
      %+  roll  p.y
      =<  .(s (~(put in *(set tako)) p))
      |=  {q/tako s/(set tako)}
      ?:  (~(has in s) q)                               ::  already done
        s                                               ::  hence skip
      (~(uni in s) ^$(p q))                             ::  otherwise traverse
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
      |=  [local=? yon=aeon pax=path]
      ^-  (unit (unit (each cage lobe)))
      ?:  =(0 yon)
        [~ ~]
      =+  tak=(~(get by hit.dom) yon)
      ?~  tak
        ~
      ?:  &(local =(yon let.dom))
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
      (end 3 (dec (met 3 pac)) pac)
    ::
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
::
++  fusion
  =>
  |%
  ::  $pile: preprocessed hoon source file
  ::
  ::    /-  sur-file            ::  surface imports from /sur
  ::    /+  lib-file            ::  library imports from /lib
  ::    /=  face  /path         ::  imports built hoon file at path
  ::    /*  face  %mark  /path  ::  unbuilt file imports, as mark
  ::
  +$  pile
    $:  sur=(list taut)
        lib=(list taut)
        raw=(list [face=term =path])
        bar=(list [face=term =mark =path])
        =hoon
    ==
  ::  $taut: file import from /lib or /sur
  ::
  +$  taut  [face=(unit term) pax=term]
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
          $:  =ankh
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
          (slam (slap cor ^~((ream 'noun:grab'))) !>(noun))
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
        ^~((ream '(diff:~(grad cor sam) new)'))
      ++  form  fom
      ++  join
        |=  [a=vase b=vase]
        ^-  (unit (unit vase))
        ?:  =(q.a q.b)
          ~
        =;  res  `?~(q.res ~ `(slap res ^~((ream '?~(. !! u)'))))
        (slam (slap cor ^~((ream 'join:grad'))) (slop a b))
      ++  mash
        |=  [a=[=ship =desk diff=vase] b=[=ship =desk diff=vase]]
        ^-  (unit vase)
        ?:  =(q.diff.a q.diff.b)
          ~
        :-  ~
        %+  slam  (slap cor ^~((ream 'mash:grad')))
        %+  slop
          :(slop !>(ship.a) !>(desk.a) diff.a)
        :(slop !>(ship.b) !>(desk.b) diff.b)
      ++  pact
        |=  diff=vase
        ^+  sam
        %+  slap
          (with-faces cor+cor sam+sam diff+diff ~)
        ^~((ream '(pact:~(grad cor sam) diff)'))
      ++  vale
        |=  =noun
        ^+  sam
        (slam (slap cor ^~((ream 'noun:grab'))) !>(noun))
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
      ?:  =/  ram  (mule |.((slap old ^~((ream 'grow')))))
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
        %-  ream
        ;:  (cury cat 3)
          '!:  '
          '~!  old=old  '
          '~!  sam=sam  '
          b  ':~(grow old sam)'
        ==
      ::  try direct +grab
      ::
      =^  new=vase  nub  (build-fit %mar b)
      =/  rab  (mule |.((slap new (ream (cat 3 a ':grab')))))
      ?:  &(?=(%& -.rab) ?=(^ q.p.rab))
        :_(nub |=(sam=vase ~|([%grab a b] (slam p.rab sam))))
      ::  try +jump
      ::
      =/  jum  (mule |.((slap old (ream (cat 3 b ':jump')))))
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
      =^  dys=dais  nub  (get-mark p.old)
      =^  syd=dais  nub  (get-mark p.diff)
      :_(nub [p.old (~(pact dys (vale:dys q.old)) (vale:syd q.diff))])
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
      =^  sut=vase  nub  run-reef
      =^  sut=vase  nub  (run-tauts sut %sur sur.pile)
      =^  sut=vase  nub  (run-tauts sut %lib lib.pile)
      =^  sut=vase  nub  (run-raw sut raw.pile)
      =^  sut=vase  nub  (run-bar sut bar.pile)
      =/  res=vase  (slap sut hoon.pile)
      =^  top  stack.nub  pop-stack
      =.  vases.cache.nub  (~(put by vases.cache.nub) path [res top])
      [res nub]
    ::
    ++  parse-pile
      |=  [pax=path tex=tape]
      ^-  pile
      =/  [=hair res=(unit [=pile =nail])]  ((pile-rule pax) [1 1] tex)
      ?^  res  pile.u.res
      %-  mean
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
          (cold ~ ;~(plug net wut gap dem gap))
          (easy ~)
        ==
      ::
        ;~  plug
          ;~  pose
            ;~  sfix
              %+  cook  |=((list (list taut)) (zing +<))
              %+  more  gap
              ;~  pfix  ;~(plug net hep gap)
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
              ;~  pfix  ;~(plug net lus gap)
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
              ;~  pfix  ;~(plug net tis gap)
                %+  cook  |=([term path] +<)
                ;~(plug sym ;~(pfix ;~(plug gap net) (more net urs:ab)))
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
              ;~  pfix  ;~(plug net tar gap)
                %+  cook  |=([term mark path] +<)
                ;~  plug
                  sym
                  ;~(pfix ;~(plug gap cen) sym)
                  ;~(pfix ;~(plug gap net) (more net urs:ab))
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
    ++  run-reef
      ^-  [vase state]
      [!>(..zuse) nub]  ::  TODO implement
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
    |=  {hen/duct mun/mood dat/(each cage lobe)}
    ^+  +>
    ?:  ?=(%& -.dat)
      %-  emit
      :*  hen  %slip  %b  %drip
          !>([%writ ~ [care.mun case.mun syd] path.mun p.dat])
      ==
    %-  emit
    :*  hen  %pass  [%blab care.mun (scot case.mun) syd path.mun]
        %f  %build  live=%.n  %pin
        (case-to-date case.mun)
        (lobe-to-schematic [her syd] path.mun p.dat)
    ==
  ::
  ++  case-to-date  (cury case-to-date:util lim)
  ++  case-to-aeon  (cury case-to-aeon-before:util lim)
  ++  lobe-to-schematic  (cury lobe-to-schematic-p:util ?=(~ ref))
  ::
  ++  blas
    |=  {hen/duct das/(set mood)}
    ^+  +>
    ?>  ?=(^ das)
    ::  translate the case to a date
    ::
    =/  cas  [%da (case-to-date case.n.das)]
    =-  (emit hen %slip %b %drip !>([%wris cas -]))
    (~(run in `(set mood)`das) |=(m/mood [care.m path.m]))
  ::
  ::  Give next step in a subscription.
  ::
  ++  bleb
    |=  {hen/duct ins/@ud hip/(unit (pair aeon aeon))}
    ^+  +>
    %^  blab  hen  [%w [%ud ins] ~]
    :-  %&
    ?~  hip
      [%null [%atom %n ~] ~]
    [%nako !>((make-nako:ze u.hip))]
  ::
  ::  Tell subscriber that subscription is done.
  ::
  ++  blub
    |=  hen/duct
    (emit hen %slip %b %drip !>([%writ ~]))
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
  ::  Transfer a request to another ship's clay.
  ::
  ++  send-over-ames
    |=  [=duct =ship index=@ud =riff]
    ^+  +>
    ::
    =/  =desk  p.riff
    =/  =wire  /warp-index/(scot %p ship)/(scot %tas desk)/(scot %ud index)
    =/  =path  [%question desk (scot %ud index) ~]
    (emit duct %pass wire %a %plea ship %c path riff)
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
      bom.u.ref  (~(put by bom.u.ref) inx [hen rave])
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
    =/  [deletes=(set path) changes=(map path (each page lobe))]
      =/  previous-yaki
        ?:  =(0 let.dom)
          *yaki
        (aeon-to-yaki:ze let.dom)
      (get-changes q.previous-yaki new-data)
    ~|  [from=let.dom deletes=deletes changes=~(key by changes)]
    ::
    ::  promote ford cache
    ::  promote and fill in ankh
    ::  promote and fill in mime cache
    ::
    =.  fod.dom
      (promote-ford fod.dom deletes ~(key by changes))
    =/  =args:ford:fusion
      [ank.dom deletes changes lat.ran fod.dom]
    ::
    ::  ?:  &(!updated !=(~ (need-sys-update changes)))
    ::    (sys-update args yuki changes)
    ::
    =.  ..park  (emil (print deletes ~(key by changes)))
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
    ::
    wake:(ergo mim)
    ::
    ::  Find which files changed or were deleted
    ::
    ++  get-changes
      |=  [old=(map path lobe) new=(map path (each page lobe))]
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
    ::  Updated q.yaki
    ::
    ++  checkout-changes
      |=  [=ford=args:ford:fusion changes=(map path (each page lobe))]
      =/  cans=(list [=path change=(each page lobe)])  ~(tap by changes)
      |-  ^-  [(map path [=lobe =cage]) ford-cache]
      ?~  cans
        [~ ford-cache.ford-args]
      =^  cage  ford-cache.ford-args
        %-  wrap:fusion
        (get-value:(ford:fusion ford-args) path.i.cans)
      =/  =lobe
        ?-  -.change.i.cans
          %|  p.change.i.cans
          ::  Don't use p.change.i.cans because that's before casting to
          ::  the correct mark.
          ::
          %&  (page-to-lobe:util [p q.q]:cage)
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
      =/  must  (must-ergo:util her syd mon (turn ~(tap by mim) head))
      %-  emil
      %+  turn  ~(tap by must)
      |=  [pot=term len=@ud pak=(set path)]
      :*  (need hez)  %give  %ergo  pot
          %+  turn  ~(tap in pak)
          |=  pax=path
          [(slag len pax) (~(got by mim) pax)]
      ==
    ::
    ::  Print notification to console
    ::
    ++  print
      |=  [deletes=(set path) changes=(set path)]
      ^-  (list move)
      |^
      ?~  hun
        ~
      ?:  =(0 let.dom)
        ~
      %+  weld
        %+  turn  ~(tap in deletes)
        |=  =path
        [u.hun %give %note '-' (path-to-tank path)]
      %+  turn  ~(tap in changes)
      |=  =path
      [u.hun %give %note '+' (path-to-tank path)]
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
        [*ankh ~ all-changes lat.ran *ford-cache]
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
    ::  Find /sys changes
    ::
    ++  need-sys-update
      |=  changes=(map path (each page lobe))
      ^-  (map path (each page lobe))
      ~+
      %-  malt
      %+  skim  ~(tap by changes)
      |=  [=path *]
      ?|  =(/sys/hoon/hoon path)
          =(/sys/arvo/hoon path)
          =(/sys/zuse/hoon path)
          =(/sys/vane (scag 2 path))
      ==
    ::
    ::  Delay current update until sys update is complete
    ::
    ++  sys-update
      |=  [=ford=args:ford:fusion =yoki changes=(map path (each page lobe))]
      ^+  ..park
      =/  updates  (need-sys-update changes)
      ::  Don't save ford cache so it gets properly handled when we
      ::  restart the commit
      ::
      =^  updates-cages=(map path [lobe cage])  ford-cache.ford-args
        (checkout-changes ford-args updates)
      ?>  =(~ pud)
      =.  pud  `[syd yoki]
      |^  ?:  (~(has by updates) /sys/hoon/hoon)
            reset
          ?:  (~(has by updates) /sys/arvo/hoon)
            reset
          ?:  (~(has by updates) /sys/zuse/hoon)
            reboot
          reload-all
      ::
      ++  reset
        =^  hoon=cage  ford-cache.ford-args
          %-  wrap:fusion
          (get-value:(ford:fusion ford-args) /sys/hoon/hoon)
        =^  arvo=cage  ford-cache.ford-args
          %-  wrap:fusion
          (get-value:(ford:fusion ford-args) /sys/arvo/hoon)
        =.  ..park
          %-  emit
          [hen %pass /reset %d %flog %lyra !<(@t q.hoon) !<(@t q.arvo)]
        reboot
      ::
      ++  reboot
        =^  zuse=cage  ford-cache.ford-args
          %-  wrap:fusion
          (get-value:(ford:fusion ford-args) /sys/zuse/hoon)
        =.  ..park
          %-  emit
          [hen %pass /reboot %d %flog %veer %$ /sys/zuse/hoon !<(@t q.zuse)]
        reload-all
      ::
      ++  reload-all
        =/  vanes=(list term)
          ~[%ames %behn %clay %dill %eyre %ford %gall %iris %jael]
        |-  ^+  ..park
        ?~  vanes
          (emit hen %slip %c %pork ~)
        =.  ..park  (reload i.vanes)
        $(vanes t.vanes)
      ::
      ++  reload
        |=  =term
        =^  vane=cage  ford-cache.ford-args
          %-  wrap:fusion
          (get-value:(ford:fusion ford-args) /sys/vane/[term]/hoon)
        %-  emit
        =/  tip  (end 3 1 term)
        =/  =path  /sys/vane/[term]/hoon
        [hen %pass /reload %d %flog %veer tip path !<(@t q.vane)]
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
        &+`[conflicts=~ new=|+ali-yaki lat=~]
      ::
      =/  bob-yaki  (need bob-yaki)
      |^
      ^-  (each (unit merge-result) [term tang])
      ?-    germ
      ::
      ::  If this is a %this merge, we check to see if ali's and bob's
      ::  commits are the same, in which case we're done.  Otherwise, we
      ::  check to see if ali's commit is in the ancestry of bob's, in
      ::  which case we're done.  Otherwise, we create a new commit with
      ::  bob's data plus ali and bob as parents.
      ::
          %this
        ?:  =(r.ali-yaki r.bob-yaki)
          &+~
        ?:  (~(has in (reachable-takos r.bob-yaki)) r.ali-yaki)
          &+~
        :*  %&  ~
            conflicts=~
            new=&+[[r.bob-yaki r.ali-yaki ~] (to-yuki q.bob-yaki)]
            lat=~
        ==
      ::
      ::  If this is a %that merge, we check to see if ali's and bob's
      ::  commits are the same, in which case we're done.  Otherwise, we
      ::  create a new commit with ali's data plus ali and bob as
      ::  parents.
      ::
          %that
        ?:  =(r.ali-yaki r.bob-yaki)
          &+~
        :*  %&  ~
            conflicts=~
            new=&+[[r.bob-yaki r.ali-yaki ~] (to-yuki q.ali-yaki)]
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
        ?:  (~(has in (reachable-takos r.bob-yaki)) r.ali-yaki)
          &+~
        ?.  (~(has in (reachable-takos r.ali-yaki)) r.bob-yaki)
          :~  %|  %bad-fine-merge
              leaf+"tried fast-forward but is not ancestor or descendant"
          ==
        &+`[conflicts=~ new=|+ali-yaki lat=~]
      ::
          ?(%meet %mate %meld)
        ?:  =(r.ali-yaki r.bob-yaki)
          &+~
        ?:  (~(has in (reachable-takos r.bob-yaki)) r.ali-yaki)
          &+~
        ?:  (~(has in (reachable-takos r.ali-yaki)) r.bob-yaki)
          $(germ %fine)
        =/  merge-points  find-merge-points
        ?~  merge-points
          :~  %|  %merge-no-merge-base
              leaf+"consider a %this or %that merge to get a mergebase"
          ==
        =/  merge-point=yaki  n.merge-points
        ?.  ?=(%meet germ)
          =/  ali-diffs=cane  (diff-base ali-yaki bob-yaki merge-point)
          =/  bob-diffs=cane  (diff-base bob-yaki ali-yaki merge-point)
          =/  bof=(map path (unit cage))
            (merge-conflicts can.ali-diffs can.bob-diffs)
          &+`(build ali-yaki bob-yaki merge-point ali-diffs bob-diffs bof)
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
        ?.  =(~ both-diffs)
          :~  %|  %meet-conflict
            >~(key by both-diffs)<
            leaf+"consider a %mate merge"
          ==
        =/  not-deleted=(map path lobe)
          %+  roll  ~(tap by (~(uni by old.ali-diffs) old.bob-diffs))
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
      ++  reachable-takos
        |=  tak=tako
        ^-  (set tako)
        |-  ^-  (set tako)
        =/  =yaki  (~(got by hut.ran) tak)
        %+  roll  p.yaki
        =<  .(takos (~(put in *(set tako)) tak))
        |=  [q=tako takos=(set tako)]
        ?:  (~(has in takos) q)                         ::  already done
          takos                                         ::  hence skip
        (~(uni in takos) ^$(tak q))                     ::  otherwise traverse
      ::
      ::  Find the most recent common ancestor(s).
      ::
      ::    Pretty sure this could be a lot more efficient.
      ::
      ++  find-merge-points
        ^-  (set yaki)
        %-  reduce-merge-points
        =+  r=(reachable-takos r.ali-yaki)
        |-  ^-  (set yaki)
        ~!  bob-yaki
        ?:  (~(has in r) r.bob-yaki)  (~(put in *(set yaki)) bob-yaki)
        %+  roll  p.bob-yaki
        |=  [t=tako s=(set yaki)]
        ?:  (~(has in r) t)
          (~(put in s) (~(got by hut.ran) t))
        (~(uni in s) ^$(bob-yaki (~(got by hut.ran) t)))
      ::
      ::  Eliminate redundant merge-point candidates
      ::
      ++  reduce-merge-points
        |=  unk=(set yaki)
        =|  gud=(set yaki)
        =/  zar=(map tako (set tako))
          %+  roll  ~(tap in unk)
          |=  [yak=yaki qar=(map tako (set tako))]
          (~(put by qar) r.yak (reachable-takos r.yak))
        |-
        ^-  (set yaki)
        ?~  unk  gud
        =+  bun=(~(del in `(set yaki)`unk) n.unk)
        ?:  %+  levy  ~(tap by (~(uni in gud) bun))
            |=  yak=yaki
            !(~(has in (~(got by zar) r.yak)) r.n.unk)
          $(gud (~(put in gud) n.unk), unk bun)
        $(unk bun)
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
          (lobe-to-page:(ford:fusion ank.dom ~ ~ lat.ran fod.dom) lobe)
        =^  =cage  fod.dom
          %-  wrap:fusion
          (page-to-cage:(ford:fusion ank.dom ~ ~ lat.ran fod.dom) page)
        cage
      ::
      ++  get-dais
        |=  =mark
        ^-  dais
        =^  =dais  fod.dom
          %-  wrap:fusion
          (get-mark:(ford:fusion ank.dom ~ ~ lat.ran fod.dom) mark)
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
          ~
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
        ^-  merge-result
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
        =/  old=(map path lobe)                         ::  oldies but goodies
          %+  roll  ~(tap by (~(uni by old.dal) old.dob))
          =<  .(old q.bas)
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
        :*  (silt (turn ~(tap by con) head))
            new
            lat
        ==
      --
    --
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
    ^+  .
    =^  wos/(list wove)  qyx
      :_  (~(run by qyx) |=(a/(set duct) (~(del in a) hen)))
      %-  ~(rep by qyx)
      |=  {{a/wove b/(set duct)} c/(list wove)}
      ?.((~(has in b) hen) c [a c])
    ?~  ref
      =>  .(ref `(unit rind)`ref)     ::  XX TMI
      ?:  =(~ wos)  +                                   ::  XX handle?
      |-  ^+  +>
      ?~  wos  +>
      $(wos t.wos, +> (run-if-future rove.i.wos |=(@da (best hen +<))))
    ^+  ..cancel-request
    =+  nux=(~(get by fod.u.ref) hen)
    ?~  nux  ..cancel-request
    =:  fod.u.ref  (~(del by fod.u.ref) hen)
        bom.u.ref  (~(del by bom.u.ref) u.nux)
      ==
    (send-over-ames hen her u.nux syd ~)
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
    |=  [for=(unit ship) rav=rave]
    ^+  ..start-request
    =^  [new-sub=(unit rove) sub-results=(list sub-result)]  fod.dom
      (try-fill-sub for (rave-to-rove rav))
    =.  ..start-request  (send-sub-results sub-results [hen ~ ~])
    ?~  new-sub
      ..start-request
    (duce for u.new-sub)
  ::
  ::  Continue merging
  ::
  ++  take-merge
    |=  =sign
    ^+  +>
    =/  m  merge-clad
    ?~  act
      ~|(%no-active-write !!)
    ?.  ?=(%merge -.eval-data.u.act)
      ~|(%active-not-merge !!)
    =^  r=[moves=(list move) =eval-result:eval:m]  merge.eval-data.u.act
      (take:eval:m merge.eval-data.u.act hen /merge/[syd] now ran sign)
    =>  .(+>.$ (emil moves.r))  :: TMI
    ?-  -.eval-result.r
      %next  +>.$
      %fail  (fail-merge err.eval-result.r)
      %done  (done-merge value.eval-result.r)
    ==
  ::
  ::  Don't release effects or apply state changes; print error
  ::
  ++  fail-merge
    |=  err=(pair term tang)
    ^+  +>
    =.  +>.$
      (emit [hen %slip %b %drip !>([%mere %| err])])
    finish-write
  ::
  ::  Release effects and apply state changes
  ::
  ++  done-merge
    |=  [conflicts=(set path) =dome =rang]
    ^+  +>
    =.  +>.$  (emit [hen %slip %b %drip !>([%mere %& conflicts])])
    =:  dom      dome
        hut.ran  (~(uni by hut.rang) hut.ran)
        lat.ran  (~(uni by lat.rang) lat.ran)
      ==
    =.  +>.$  wake
    finish-write
  ::
  ::  Continue mounting
  ::
  ++  take-mount
    |=  =sign
    ^+  +>
    =/  m  mount-clad
    ?~  act
      ~|(%no-active-write !!)
    ?.  ?=(%mount -.eval-data.u.act)
      ~|(%active-not-mount !!)
    =^  r=[moves=(list move) =eval-result:eval:m]  mount.eval-data.u.act
      (take:eval:m mount.eval-data.u.act hen /mount/[syd] now ran sign)
    =>  .(+>.$ (emil moves.r))  :: TMI
    ?-  -.eval-result.r
      %next  +>.$
      %fail  (fail-mount err.eval-result.r)
      %done  (done-mount value.eval-result.r)
    ==
  ::
  ::  Don't release effects or apply state changes; print error
  ::
  ++  fail-mount
    |=  err=(pair term tang)
    ^+  +>
    %-  (slog leaf+"mount failed" leaf+(trip p.err) q.err)
    finish-write
  ::
  ::  Release effects and apply state changes
  ::
  ++  done-mount
    |=  [new-mon=(pair term beam) mim=(map path mime)]
    ^+  +>
    =:  mon      (~(put by mon) new-mon)
        mim.dom  mim
      ==
    finish-write
  ::
  ::  Start next item in write queue
  ::
  ++  finish-write
    ^+  .
    =.  act  ~
    ?~  cue
      .
    =/  =duct  duct:(need ~(top to cue))
    (emit [duct %pass /queued-request %b %wait now])
  ::
  ::  Continue foreign request
  ::
  ++  take-foreign-request
    |=  [inx=@ud =sign]
    ^+  +>
    =/  m  request-clad
    ?>  ?=(^ ref)
    ?~  request=(~(get by pur.u.ref) inx)
      ~|(%no-active-foreign-request !!)
    =^  r=[moves=(list move) =eval-result:eval:m]  eval-form.u.request
      %-  take:eval:m
      :*  eval-form.u.request
          hen
          /foreign-request/(scot %p her)/[syd]/(scot %ud inx)
          now
          ran
          sign
      ==
    =>  .(+>.$ (emil moves.r))  :: TMI
    ?-  -.eval-result.r
      %next  +>.$
      %fail  (fail-foreign-request inx rand.u.request err.eval-result.r)
      %done  (done-foreign-request inx rand.u.request value.eval-result.r)
    ==
  ::
  ::  Fail foreign request
  ::
  ++  fail-foreign-request
    |=  [inx=@ud =rand err=(pair term tang)]
    ^+  +>
    %-  (slog leaf+"foreign request failed" leaf+(trip p.err) q.err)
    ?>  ?=(^ ref)
    =/  =mood  [p.p q.p q]:rand
    =:  haw.u.ref  (~(put by haw.u.ref) mood ~)
        bom.u.ref  (~(del by bom.u.ref) inx)
        fod.u.ref  (~(del by fod.u.ref) hen)
      ==
    wake
  ::
  ::  Finish foreign request
  ::
  ++  done-foreign-request
    |=  [inx=@ud =rand =cage]
    ^+  +>
    ?>  ?=(^ ref)
    =/  =mood  [p.p q.p q]:rand
    =:  haw.u.ref  (~(put by haw.u.ref) mood `cage)
        bom.u.ref  (~(del by bom.u.ref) inx)
        fod.u.ref  (~(del by fod.u.ref) hen)
      ==
    wake
  ::
  ::  Called when a foreign ship answers one of our requests.
  ::
  ::  If it's a `%many` request, start a `+foreign-update`.  Else start
  ::  a `+foreign-request`.
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
    =/  rav=rave  q.u.ruv
    ?:  ?=(%many -.rav)
      ::  add to update queue
      ::
      =.  waiting.pud.u.ref
        (~(put to waiting.pud.u.ref) inx rut)
      ::  start update if nothing active
      ::
      start-next-foreign-update
    ?~  rut
      ::  nothing here, so cache that
      ::
      %_    wake
          haw.u.ref
        ?.  ?=($sing -.rav)  haw.u.ref
        (~(put by haw.u.ref) mood.rav ~)
      ==
    ::  something here, so kick off a validator
    ::
    =.  pur.u.ref
      %+  ~(put by pur.u.ref)
        inx
      :-  u.rut
      %-  from-form:eval:request-clad
      ((foreign-request our her syd now) rav u.rut)
    (take-foreign-request inx clad-init-sign)
  ::
  ::  Continue foreign update
  ::
  ++  take-foreign-update
    |=  =sign
    ^+  +>
    =/  m  update-clad
    ?>  ?=(^ ref)
    ?~  eval-data.pud.u.ref
      ~|(%no-active-foreign-update !!)
    =*  ed  u.eval-data.pud.u.ref
    =/  inx  inx.ed
    =^    r=[moves=(list move) =eval-result:eval:m]
        eval-form.u.eval-data.pud.u.ref
      %-  take:eval:m
      :*  eval-form.ed
          hen
          /foreign-update/(scot %p her)/[syd]
          now
          ran
          sign
      ==
    =>  .(+>.$ (emil moves.r))  :: TMI
    ?-  -.eval-result.r
      %next  +>.$
      %fail  (fail-foreign-update inx err.eval-result.r)
      %done  (done-foreign-update inx value.eval-result.r)
    ==
  ::
  ::  Fail foreign update
  ::
  ++  fail-foreign-update
    |=  [inx=@ud err=(pair term tang)]
    ^+  +>
    %-  (slog leaf+"foreign update failed" leaf+(trip p.err) q.err)
    ?>  ?=(^ ref)
    =:  bom.u.ref  (~(del by bom.u.ref) inx)
        fod.u.ref  (~(del by fod.u.ref) hen)
      ==
    =.  +>.$  =<(?>(?=(^ ref) .) wake)
    =.  eval-data.pud.u.ref  ~
    start-next-foreign-update
  ::
  ::  Finish foreign update
  ::
  ++  done-foreign-update
    |=  [inx=@ud res=(unit [new-lim=@da =new=dome =new=rang])]
    ^+  +>
    ?>  ?=(^ ref)
    =:  bom.u.ref  (~(del by bom.u.ref) inx)
        fod.u.ref  (~(del by fod.u.ref) hen)
      ==
    ?~  res
      wake
    =:  lim  new-lim.u.res
        dom  new-dome.u.res
        ran  new-rang.u.res
      ==
    =.  +>.$  =<(?>(?=(^ ref) .) wake)
    =.  eval-data.pud.u.ref  ~
    start-next-foreign-update
  ::
  ::  Kick off the the next foreign update in the queue
  ::
  ++  start-next-foreign-update
    ^+  .
    ?>  ?=(^ ref)
    ?.  =(~ eval-data.pud.u.ref)
      .
    ?:  =(~ waiting.pud.u.ref)
      .
    =^  next=[inx=@ud rut=(unit rand)]  waiting.pud.u.ref
      ~(get to waiting.pud.u.ref)
    =/  ruv  (~(get by bom.u.ref) inx.next)
    ?~  ruv
      ~&  [%clay-foreign-update-lost her syd inx.next]
      start-next-foreign-update
    =.  hen  p.u.ruv
    =/  =rave  q.u.ruv
    ?>  ?=(%many -.rave)
    =.  eval-data.pud.u.ref
      :-  ~
      :+  inx.next
        rut.next
      %-  from-form:eval:update-clad
      ((foreign-update our her syd now) moat.rave rut.next lim dom ran)
    (take-foreign-update clad-init-sign)
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
    |=  [for=(unit ship) rov=rove]
    ^-  [[new-sub=(unit rove) (list sub-result)] ford-cache]
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
        [%bleb let.dom ?:(track.rov ~ `[u.from-aeon let.dom])]~
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
        [%bleb +(u.from-aeon) ?:(track.rov ~ `[u.from-aeon u.to-aeon])]~
      ::  end subscription
      ::
      =/  blub=(list sub-result)
        [%blub ~]~
      (weld bleb blub)
    ==
  ::
  ++  drop-me
    ^+  .
    ~|  %clay-drop-me-not-implemented
    !!
    ::  ?~  mer
    ::    .
    ::  %-  emit(mer ~)  ^-  move  :*
    ::    hen.u.mer  %give  %mere  %|  %user-interrupt
    ::    >sor.u.mer<  >our<  >cas.u.mer<  >gem.u.mer<  ~
    ::  ==
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
  ::  The useful utility functions that are common to several cores
  ::
  ++  util  (state:[^util] dom dom ran)
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
    ++  page-to-lobe  page-to-lobe:util
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
      |=  {a/aeon b/aeon}
      ^-  nako
      :+  ?>  (lte b let.dom)
          |-
          ?:  =(b let.dom)
            hit.dom
          $(hit.dom (~(del by hit.dom) let.dom), let.dom (dec let.dom))
        b
      ?:  =(0 b)
        [~ ~]
      (data-twixt-takos (~(get by hit.dom) a) (aeon-to-tako b))
    ::
    ::  Gets the data between two commit hashes, assuming the first is an
    ::  ancestor of the second.
    ::
    ::  Get all the takos before `a`, then get all takos before `b` except the
    ::  ones we found before `a`.  Then convert the takos to yakis and also get
    ::  all the data in all the yakis.
    ::
    ++  data-twixt-takos
      |=  {a/(unit tako) b/tako}
      ^-  {(set yaki) (set plop)}
      =+  old=?~(a ~ (reachable-takos:util u.a))
      =/  yal/(set tako)
          %-  silt
          %+  skip
            ~(tap in (reachable-takos:util b))
          |=(tak/tako (~(has in old) tak))
      :-  (silt (turn ~(tap in yal) tako-to-yaki))
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
        (build-file:(ford:fusion ank.dom ~ ~ lat.ran fod.dom) path)
      :_(fod.dom [~ ~ %& %vase !>(vase)])
    ::
    ++  read-b
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
        (get-mark:(ford:fusion ank.dom ~ ~ lat.ran fod.dom) i.path)
      :_(fod.dom [~ ~ %& %dais !>(dais)])
    ::
    ++  read-c
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
        (get-cast:(ford:fusion ank.dom ~ ~ lat.ran fod.dom) [i i.t]:path)
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
      :: ~&  read-z+[yon=yon qyt=~(wyt by q.yaki) pax=pax]
      =/  descendants/(list (pair path lobe))
          ::  ~&  %turning
          ::  =-  ~&  %turned  -
          %+  turn
            ::  ~&  %skimming
            ::  =-  ~&  %skimmed  -
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
    ::  +read-s: produce yaki or blob for given tako or lobe
    ::
    ++  read-s
      |=  [yon=aeon pax=path]
      ^-  (unit (unit cage))
      ?.  ?=([?(%yaki %blob %hash %cage) * ~] pax)
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
          (lobe-to-page:(ford:fusion ank.dom ~ ~ lat.ran fod.dom) lobe)
        =^  =cage  fod.dom
          %-  wrap:fusion
          (page-to-cage:(ford:fusion ank.dom ~ ~ lat.ran fod.dom) page)
        ``cage+[-:!>(*^cage) cage]
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
        :*  ~  ~  %dome  -:!>(%dome)
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
    ::  Gets the data at a node.
    ::
    ++  read-x  (cury read-x:util ?=(~ ref))
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
        %s  :_(fod (bind (read-s yon path.mun) (lift |=(a=cage [%& a]))))
        %t  :_(fod (bind (read-t yon path.mun) (lift |=(a=cage [%& a]))))
        %u  :_(fod (read-u yon path.mun))
        %v  :_(fod (bind (read-v yon path.mun) (lift |=(a/cage [%& a]))))
        %w  :_(fod (read-w case.mun))
        %x  :_(fod (read-x yon path.mun))
        %y  :_(fod (bind (read-y yon path.mun) (lift |=(a/cage [%& a]))))
        %z  :_(fod (bind (read-z yon path.mun) (lift |=(a/cage [%& a]))))
      ==
    ++  zu  zu:util
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
    $:  ver=%2                                        ::  vane version
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
  ::  only one of these should be going at once, so queue
  ::
  ?:  ?=(?(%info %merg %mont) -.req)
    ::  If there's an active write or a queue, enqueue
    ::
    ::    We only want one active write so each can be a clean
    ::    transaction.  We don't intercept `%into` because it
    ::    immediately translates itself into one or two `%info` calls.
    ::
    ?:  |(!=(~ act.ruf) !=(~ cue.ruf))
      =.  cue.ruf  (~(put to cue.ruf) [hen req])
      ::  ~&  :*  %clall-enqueing
      ::          cue=(turn ~(tap to cue.ruf) |=([=duct =task:able] [duct -.task]))
      ::          ^=  act
      ::          ?~  act.ruf
      ::            ~
      ::          [hen req -.eval-data]:u.act.ruf
      ::      ==
      [~ ..^$]
    ::  If the last commit happened in this event, enqueue
    ::
    ::    Without this, two commits could have the same date, which
    ::    would make clay violate referential transparency.
    ::
    =/  =desk  des.req
    =/  =dojo  (~(gut by dos.rom.ruf) desk *dojo)
    ?:  =(0 let.dom.dojo)
      (handle-task hen req)
    =/  sutil  (state:util dom.dojo dom.dojo ran.ruf)
    =/  last-write=@da  t:(aeon-to-yaki:sutil let.dom.dojo)
    ?:  !=(last-write now)
      (handle-task hen req)
    =.  cue.ruf  (~(put to cue.ruf) [hen req])
    =/  wait-behn  [hen %pass /queued-request %b %wait now]
    [[wait-behn ~] ..^$]
  (handle-task hen req)
::
::  Handle a task, without worrying about write queueing
::
++  handle-task
  |=  [hen=duct req=task:able]
  ^-  [(list move) _..^$]
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
    ~?  =(~ act.ruf)
      [%clay-idle cue-length=~(wyt in cue.ruf)]
    ~?  ?=(^ act.ruf)
      [%clay-cancelling hen -.req -.eval-data]:u.act.ruf
    =.  act.ruf  ~
    ?~  cue.ruf
      [~ ..^$]
    =/  =duct  duct:(need ~(top to cue.ruf))
    [[duct %pass /queued-request %b %wait now]~ ..^$]
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
        [[our %base %ud 1] ~]
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
    =.  act.ruf
      =/  =dojo  (~(gut by dos.rom.ruf) q.bem.req *dojo)
      =/  writer=form:mount-clad
        %-  %-  mount
            :*  our
                q.bem.req
                now
                (need hez.ruf)
                dom.dojo
                ran.ruf
            ==
        :*  des.req
            bem.req
            mon.ruf
        ==
      `[hen req %mount (from-form:eval:mount-clad writer)]
    =^  mos  ruf
      =/  den  ((de our now ski hen ruf) p.bem.req q.bem.req)
      abet:(take-mount:den clad-init-sign)
    [mos ..^$]
  ::
      %dirk
    ?~  hez.ruf
      ~&  %no-sync-duct
      [~ ..^$]
    ?.  (~(has by mon.ruf) des.req)
      ~&  [%not-mounted des.req]
      [~ ..^$]
    :-  ~[[u.hez.ruf %give %dirk des.req]]
        ..^$
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
      %vega  [~ ..^$]
  ::
      ?(%warp %werp)
    ::  capture whether this read is on behalf of another ship
    ::  for permissions enforcement
    ::
    =^  for  req
      ?:  ?=(%warp -.req)
        [~ req]
      :-  ?:(=(our who.req) ~ `who.req)
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
    ?>  ?=({%question *} pax)
    =+  ryf=;;(riff res)
    :_  ..^$
    :~  [hen %give %done ~]
        =/  =wire
          [%foreign-warp (scot %p her) t.pax]
        [hen %pass wire %c %werp her our ryf]
    ==
  ::
      %wegh
    :_  ..^$  :_  ~
    :^  hen  %give  %mass
    :+  %clay  %|
    =/  domestic
      %+  turn  (sort ~(tap by dos.rom.ruf) aor)
      |=  [=desk =dojo]
      :+  desk  %|
      :~  ankh+&+ank.dom.dojo
          mime+&+mim.dom.dojo
          ford+&+fod.dom.dojo
      ==
    :~  domestic+|+domestic
        foreign+&+hoy.ruf
        :+  %object-store  %|
        :~  commits+&+hut.ran.ruf
            blobs+&+lat.ran.ruf
        ==
        dot+&+ruf
    ==
  ==
::
++  load
  !:
  |=  [%2 =raft]
  ..^$(ruf raft)
::
++  scry                                              ::  inspect
  |=  {fur/(unit (set monk)) ren/@tas why/shop syd/desk lot/coin tyl/path}
  ^-  (unit (unit cage))
  ?.  ?=(%& -.why)  ~
  =*  his  p.why
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
++  stay  [ver ruf]
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
  ?:  ?=([%mount @ *] tea)
    =*  syd  i.t.tea
    =^  mos  ruf
      =/  den  ((de our now ski hen ruf) our syd)
      abet:(take-mount:den q.hin)
    [mos ..^$]
  ::
  ?:  ?=([%foreign-warp *] tea)
    ?>  ?=(%writ +<.q.hin)
    :_  ..^$
    [hen %give %boon `(unit rand)`(bind `riot`p.q.hin rant-to-rand)]~
  ::
  ?:  ?=([%foreign-request @ @ @ *] tea)
    =/  her  (slav %p i.t.tea)
    =/  syd  (slav %tas i.t.t.tea)
    =/  inx  (slav %ud i.t.t.t.tea)
    =^  mos  ruf
      =/  den  ((de our now ski hen ruf) her syd)
      abet:(take-foreign-request:den inx q.hin)
    [mos ..^$]
  ::
  ?:  ?=([%foreign-update @ @ *] tea)
    =/  her  (slav %p i.t.tea)
    =/  syd  (slav %tas i.t.t.tea)
    =^  mos  ruf
      =/  den  ((de our now ski hen ruf) her syd)
      abet:(take-foreign-update:den q.hin)
    [mos ..^$]
  ::
  ?:  ?=([%blab care @ @ *] tea)
    ?>  ?=(%made +<.q.hin)
    ?.  ?=([%complete %success *] result.q.hin)
      ~|  %blab-fail
      ~>  %mean.|.((made-result-as-error:ford result.q.hin))
      !!                              ::  interpolate ford fail into stack trace
    :_  ..^$  :_  ~
    :*  hen  %slip  %b  %drip  !>
    :*  %writ  ~
        ^-  [care case @tas]
        [i.t.tea ;;(case +>:(slay i.t.t.tea)) i.t.t.t.tea]
    ::
        `path`t.t.t.t.tea
        `cage`(result-to-cage:ford build-result.result.q.hin)
    ==  ==
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
      |=  =duct
      [duct %slip %b %drip !>([%writ ~])]
    ::  Clear ford cache
    ::
    =/  clear-ford-cache-moves=(list move)
      :~  [hen %pass /clear/keep %f %keep 0 1]
          [hen %pass /clear/wipe %f %wipe 100]
          [hen %pass /clear/kep %f %keep 2.048 64]
      ==
    ::  delete local state of foreign desk
    ::
    =.  hoy.ruf  (~(del by hoy.ruf) who)
    [(weld clear-ford-cache-moves cancel-moves) ..^$]
  ::
  ?-    -.+.q.hin
      %public-keys  ~|([%public-keys-raw tea] !!)
      %init-clad
    ~|(%clad-not-real !!)
  ::
      %crud
    [[[hen %slip %d %flog +.q.hin] ~] ..^$]
  ::
      %made  ~&  %clay-raw-ford  [~ ..^$]
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
    ?:  ?=([%tyme @ @ ~] tea)
      =/  her  (slav %p i.t.tea)
      =/  syd  (slav %tas i.t.t.tea)
      =^  mos  ruf
        =/  den  ((de our now ski hen ruf) her syd)
        abet:wake:den
      [mos ..^$]
    ::
    =^  queued  cue.ruf  ~(get to cue.ruf)
    ::
    =/  queued-duct=duct       -.queued
    =/  queued-task=task:able  +.queued
    ::
    ::  ~&  :*  %clay-waking
    ::          queued-duct
    ::          hen
    ::          ?~(cue.ruf /empty -:(need ~(top to cue.ruf)))
    ::      ==
    ~|  [%mismatched-ducts %queued queued-duct %timer hen]
    ?>  =(hen queued-duct)
    ::
    (handle-task hen queued-task)
  ::
      ::  handled in the wire dispatcher
      ::
      %boon  !!
      %lost  !!
      %writ  !!
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
::
::
++  mode-to-soba
  |=  {hat/(map path lobe) pax/path all/? mod/mode}
  ^-  soba
  %+  weld
    ^-  (list (pair path miso))
    ?.  all
      ~
    =+  mad=(malt mod)
    =+  len=(lent pax)
    =/  descendants/(list path)
        %+  turn
          %+  skim  ~(tap by hat)
          |=  {paf/path lob/lobe}
          =(pax (scag len paf))
        |=  {paf/path lob/lobe}
        (slag len paf)
    %+  murn
      descendants
    |=  pat/path
    ^-  (unit (pair path {$del ~}))
    ?:  (~(has by mad) pat)
      ~
    `[(weld pax pat) %del ~]
  ^-  (list (pair path miso))
  %+  murn  mod
  |=  {pat/path mim/(unit mime)}
  ^-  (unit (pair path miso))
  =+  paf=(weld pax pat)
  ?~  mim
    =+  (~(get by hat) paf)
    ?~  -
      ~&  [%deleting-already-gone pax pat]
      ~
    `[paf %del ~]
  =+  (~(get by hat) paf)
  ?~  -
    `[paf %ins %mime -:!>(*mime) u.mim]
  `[paf %mut %mime -:!>(*mime) u.mim]
--
