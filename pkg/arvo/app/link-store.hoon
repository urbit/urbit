::  link: social bookmarking
::
::    the paths under which links are submitted are generally expected to
::    correspond to existing group paths. for strictly-local collections of
::    links, arbitrary paths are probably fair game, but could trip up
::    primitive ui implementations.
::
::    urls in paths are expected to be encoded using +wood, for @ta sanity.
::    generally, use /lib/link's +build-discussion-path.
::
::    see link-listen-hook to see what's synced in, and similarly
::    see link-proxy-hook to see what's exposed.
::
::  scry and subscription paths:
::
::      (map path pages)                      %local-pages
::    /local-pages                          our saved pages
::    /local-pages/some-path                our saved pages on path
::
::      (map path submissions)                %submissions
::    /submissions                          all submissions we've seen
::    /submissions/some-path                all submissions we've seen on path
::
::      (map path (map url notes))            %annotations
::    /annotations                          our comments
::    /annotations/wood-url                 our comments on url
::    /annotations/wood-url/some-path       our comments on url on path
::    /annotations//some-path               our comments on path
::
::      (map path (map url comments))         %discussions
::    /discussions                          all comments
::    /discussions/wood-url                 all comments on url
::    /discussions/wood-url/some-path       all comments on url on path
::    /discussions//some-path               all comments on path
::
::  subscription-only paths:
::
::      [path url]                            %observation
::    /seen                                 updates whenever an item is seen
::
::  scry-only paths:
::
::
::      (map path (set url))
::    /unseen                               the ones we haven't seen yet
::
::      (set url)
::    /unseen/some-path                     the ones we haven't seen here yet
::
::      ?
::    /seen/wood-url/some-path              have we seen this here
::
/-  *link, gra=graph-store, *resource
/+  store=link-store, graph-store, default-agent, verb, dbug
::
|%
+$  state-any  $%(state-1 state-0)
+$  state-1  [%1 ~]
+$  state-0
  $:  %0
      by-group=(map path links)
      by-site=(map site (list [path submission]))
      discussions=(per-path-url discussion)
  ==
::
+$  links
  $:  ::NOTE  all lists by recency
      =submissions
      ours=pages
      seen=(set url)
  ==
::
+$  discussion
  $:  =comments
      ours=notes
  ==
::
+$  card  card:agent:gall
--
::
=|  state-1
=*  state  -
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
|_  =bowl:gall
+*  this  .
    do    ~(. +> bowl)
    def   ~(. (default-agent this %|) bowl)
::
++  on-init  on-init:def
++  on-save  !>(state)
++  on-load
  |=  old=vase
  ^-  (quip card _this)
  =/  s  !<(state-any old)
  ?:  ?=(%1 -.s)
    [~ this(state s)]
  ::
  :_  this(state *state-1)
  =/  orm  orm:graph-store
  |^  ^-  (list card)
  %-  zing
  %+  turn  ~(tap by by-group.s)
  |=  [=path =links]
  ^-  (list card)
  ?.  ?=([@ @ *] path)
    (on-bad-path path links)
  =/  =resource  [(slav %p i.path) i.t.path]
  :_  [(archive-graph resource)]~
  %+  add-graph  resource
  ^-  graph:gra
  %+  gas:orm  ~
  =/  comments  (~(gut by discussions.s) path *(map url discussion))
  %+  turn  submissions.links
  |=  sub=submission
  ^-  [atom node:gra]
  :-  time.sub
  =/  contents  ~[text+title.sub url+url.sub]
  =/  parent-hash  `@ux`(sham ~ ship.sub time.sub contents)
  :-  ^-  post:gra
      :*  author=ship.sub
          index=~[time.sub]
          time-sent=time.sub
          contents
          hash=`parent-hash
          signatures=~
      ==
  ^-  internal-graph:gra
  =/  dis  (~(get by comments) url.sub)
  ?~  dis
    [%empty ~]
  :-  %graph
  ^-  graph:gra
  %+  gas:orm  ~
  %+  turn  comments.u.dis
  |=  [=ship =time udon=@t]
  ^-  [atom node:gra]
  :-  time
  :_  `internal-graph:gra`[%empty ~]
  =/  contents  ~[text+udon]
  :*  author=ship
      index=~[time.sub time]
      time-sent=time
      contents
      hash=``@ux`(sham `parent-hash ship time contents)
      signatures=~
  ==
  ::
  ++  on-bad-path
    |=  [=path =links]
    ^-  (list card)
    ~|  discarding-malformed-links+[path links]
    ~
  ::
  ++  add-graph
    |=  [=resource =graph:gra]
    ^-  card
    %-  poke-graph-store
    [%0 now.bowl %add-graph resource graph `%graph-validator-link]
  ::
  ++  archive-graph
    |=  =resource
    ^-  card
    %-  poke-graph-store
    [%0 now.bowl %archive-graph resource]
  ::
  ++  poke-graph-store
    |=  =update:gra
    ^-  card
    :*  %pass  /migrate-link  %agent  [our.bowl %graph-store]
        %poke  %graph-update  !>(update)
    ==
  --
::
++  on-poke   on-poke:def
++  on-peek   on-peek:def
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-agent  on-agent:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
