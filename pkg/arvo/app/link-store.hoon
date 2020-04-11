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
/+  *link, default-agent, verb, dbug
::
|%
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
=|  state-0
=*  state  -
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
=<
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
    [~ this(state !<(state-0 old))]
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?>  (team:title [our src]:bowl)  ::TODO  /lib/store
    =^  cards  state
      ?+  mark  (on-poke:def mark vase)
        ::TODO  move json conversion into mark once mark performance improves
        %json         (do-action:do (action:de-json !<(json vase)))
        %link-action  (do-action:do !<(action vase))
      ==
    [cards this]
  ::
  ++  on-peek
    |=  =path
    ^-  (unit (unit cage))
    ?+  path  (on-peek:def path)
        [%y ?(%local-pages %submissions) ~]
      ``noun+!>(~(key by by-group))
    ::
        [%x %local-pages *]
      ``noun+!>((get-local-pages:do t.t.path))
    ::
        [%x %submissions *]
      ``noun+!>((get-submissions:do t.t.path))
    ::
        [%y ?(%annotations %discussions) *]
      =/  [spath=^path surl=url]
        (break-discussion-path t.t.path)
      =-  ``noun+!>(-)
      ::
      ?:  =(~ surl)
        ::  no url, provide urls that have comments
        ::
        ^-  (set url)
        ?~  spath
          ::  no path, find urls accross all paths
          ::
          %-  ~(rep by discussions)
          |=  [[* discussions=(map url discussion)] urls=(set url)]
          %-  ~(uni in urls)
          ~(key by discussions)
        ::  specified path, find urls for that specific path
        ::
        %~  key  by
        (~(gut by discussions) spath *(map url *))
      ::  specified url and path, nothing to list here
      ::
      ?^  spath  !!
      ::  no path, find paths with comments for this url
      ::
      ^-  (set ^path)
      %-  ~(rep by discussions)
      |=  [[=^path urls=(map url discussion)] paths=(set ^path)]
      ?.  (~(has by urls) surl)  paths
      (~(put in paths) path)
    ::
        [%x %annotations *]
      ``noun+!>((get-annotations:do t.t.path))
    ::
        [%x %discussions *]
      ``noun+!>((get-discussions:do t.t.path))
    ::
        [%x %seen @ ^]
      ``noun+!>((is-seen:do t.t.path))
    ::
        [%x %unseen ~]
      ``noun+!>(get-all-unseen:do)
    ::
        [%x %unseen ^]
      ``noun+!>((get-unseen:do t.t.path))
    ==
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    ?>  (team:title [our src]:bowl)  ::TODO  /lib/store
    :_  this
    |^  ?+  path  (on-watch:def path)
            [%local-pages *]
          %+  give  %link-initial
          ^-  initial
          [%local-pages (get-local-pages:do t.path)]
        ::
            [%submissions *]
          %+  give  %link-initial
          ^-  initial
          [%submissions (get-submissions:do t.path)]
        ::
            [%annotations *]
          %+  give  %link-initial
          ^-  initial
          [%annotations (get-annotations:do t.path)]
        ::
            [%discussions *]
          %+  give  %link-initial
          ^-  initial
          [%discussions (get-discussions:do t.path)]
        ::
            [%seen ~]
          ~
        ==
    ::
    ++  give
      |*  [=mark =noun]
      ^-  (list card)
      [%give %fact ~ mark !>(noun)]~
    ::
    ++  give-single
      |*  [=mark =noun]
      ^-  card
      [%give %fact ~ mark !>(noun)]
    --
  ::
  ++  on-leave  on-leave:def
  ++  on-agent  on-agent:def
  ++  on-arvo   on-arvo:def
  ++  on-fail   on-fail:def
  --
::
|_  =bowl:gall
::
::  writing
::
++  do-action
  |=  =action
  ^-  (quip card _state)
  ?-  -.action
    %save  (save-page +.action)
    %note  (note-note +.action)
    %seen  (seen-submission +.action)
  ::
    %hear  (hear-submission +.action)
    %read  (read-comment +.action)
  ==
::  +save-page: save a page ourselves
::
++  save-page
  |=  [=path title=@t =url]
  ^-  (quip card _state)
  ?<  |(=(~ path) =(~ title) =(~ url))
  ::  add page to group ours
  ::
  =/  =links  (~(gut by by-group) path *links)
  =/  =page  [title url now.bowl]
  =.  ours.links  [page ours.links]
  =.  by-group  (~(put by by-group) path links)
  ::  do generic submission logic
  ::
  =^  submission-cards  state
    (hear-submission path [our.bowl page])
  ::  mark page as seen (because we submitted it ourselves)
  ::
  =^  seen-cards  state
    (seen-submission path `url)
  ::  send updates to subscribers
  ::
  :_  state
  :_  (weld submission-cards seen-cards)
  :+  %give  %fact
  :+  :~  /local-pages
          [%local-pages path]
      ==
    %link-update
  !>([%local-pages path [page]~])
::  +note-note: save a note for a url
::
++  note-note
  |=  [=path =url udon=@t]
  ^-  (quip card _state)
  ?<  |(=(~ path) =(~ url) =(~ udon))
  ::  add note to discussion ours
  ::
  =/  urls  (~(gut by discussions) path *(map ^url discussion))
  =/  =discussion  (~(gut by urls) url *discussion)
  =/  =note  [now.bowl udon]
  =.  ours.discussion  [note ours.discussion]
  =.  urls  (~(put by urls) url discussion)
  =.  discussions  (~(put by discussions) path urls)
  ::  do generic comment logic
  ::
  =^  cards  state
    (read-comment path url [our.bowl note])
  ::  send updates to subscribers
  ::
  :_  state
  ^-  (list card)
  :_  cards
  :+  %give  %fact
  :+  :~  /annotations
          [%annotations %$ path]
          [%annotations (build-discussion-path url)]
          [%annotations (build-discussion-path path url)]
      ==
    %link-update
  !>([%annotations path url [note]~])
::  +seen-submission: mark url as seen/read
::
::    if no url specified, all under path are marked as read
::
++  seen-submission
  |=  [=path murl=(unit url)]
  ^-  (quip card _state)
  =/  =links  (~(gut by by-group) path *links)
  ::  new: urls we want to, but haven't yet, marked as seen
  ::
  =/  new=(set url)
    %.  seen.links
    %~  dif  in
    ^-  (set url)
    ?^  murl  (sy ~[u.murl])
    %-  ~(gas in *(set url))
    %+  turn  submissions.links
    |=(submission url)
  ?:  =(~ new)  [~ state]
  =.  seen.links  (~(uni in seen.links) new)
  :_  state(by-group (~(put by by-group) path links))
  [%give %fact ~[/seen] %link-update !>([%observation path new])]~
::  +hear-submission: record page someone else saved
::
++  hear-submission
  |=  [=path =submission]
  ^-  (quip card _state)
  ?<  =(~ path)
  ::  add link to group submissions
  ::
  =/  =links  (~(gut by by-group) path *links)
  =^  added  submissions.links
    ?:  ?=(^ (find ~[submission] submissions.links))
      [| submissions.links]
    :-  &
    (submissions:merge submissions.links ~[submission])
  =.  by-group  (~(put by by-group) path links)
  ::  add submission to global sites
  ::
  =/  =site  (site-from-url url.submission)
  =.  by-site  (~(add ja by-site) site [path submission])
  ::  send updates to subscribers
  ::
  :_  state
  ?.  added  ~
  :_  ~
  :+  %give  %fact
  :+  :~  /submissions
          [%submissions path]
      ==
    %link-update
  !>([%submissions path [submission]~])
::  +read-comment: record a comment someone else made
::
++  read-comment
  |=  [=path =url =comment]
  ^-  (quip card _state)
  ::  add comment to url's discussion
  ::
  =/  urls  (~(gut by discussions) path *(map ^url discussion))
  =/  =discussion  (~(gut by urls) url *discussion)
  =^  added  comments.discussion
    ?:  ?=(^ (find ~[comment] comments.discussion))
      [| comments.discussion]
    :-  &
    (comments:merge comments.discussion ~[comment])
  =.  urls  (~(put by urls) url discussion)
  =.  discussions  (~(put by discussions) path urls)
  ::  send updates to subscribers
  ::
  :_  state
  ?.  added  ~
  :_  ~
  :+  %give  %fact
  :+  :~  /discussions
          [%discussions '' path]
          [%discussions (build-discussion-path url)]
          [%discussions (build-discussion-path path url)]
      ==
    %link-update
  !>([%discussions path url [comment]~])
::
::  reading
::
++  get-local-pages
  |=  =path
  ^-  (map ^path pages)
  ?~  path
    ::  all paths
    ::
    %-  ~(run by by-group)
    |=(links ours)
  ::  specific path
  ::
  %+  ~(put by *(map ^path pages))  path
  ours:(~(gut by by-group) path *links)
::
++  get-submissions
  |=  =path
  ^-  (map ^path submissions)
  ?~  path
    ::  all paths
    ::
    %-  ~(run by by-group)
    |=(links submissions)
  ::  specific path
  ::
  %+  ~(put by *(map ^path submissions))  path
  submissions:(~(gut by by-group) path *links)
::
++  get-all-unseen
  ^-  (jug path url)
  %-  ~(rut by by-group)
  |=  [=path *]
  (get-unseen path)
::
++  get-unseen
  |=  =path
  ^-  (set url)
  =/  =links
    (~(gut by by-group) path *links)
  %-  ~(gas in *(set url))
  %+  murn  submissions.links
  |=  submission
  ?:  (~(has in seen.links) url)  ~
  (some url)
::
++  is-seen
  |=  =path
  ^-  ?
  =/  [=^path =url]
    (break-discussion-path path)
  %.  url
  %~  has  in
  seen:(~(gut by by-group) path *links)
::
::
++  get-annotations
  |=  =path
  ^-  (per-path-url notes)
  =/  args=[=^path =url]
    (break-discussion-path path)
  |^  ?~  path
        ::  all paths
        ::
        (~(run by discussions) get-ours)
      ::  specific path
      ::
      %+  ~(put by *(per-path-url notes))  path.args
      %-  get-ours
      %+  ~(gut by discussions)  path.args
      *(map url discussion)
  ::
  ++  get-ours
    |=  m=(map url discussion)
    ^-  (map url notes)
    ?:  =(~ url.args)
      ::  all urls
      ::
      %-  ~(run by m)
      |=(discussion ours)
    ::  specific url
    ::
    %+  ~(put by *(map url notes))  url.args
    ours:(~(gut by m) url.args *discussion)
  --
::
++  get-discussions
  |=  =path
  ^-  (per-path-url comments)
  =/  args=[=^path =url]
    (break-discussion-path path)
  |^  ?~  path
        ::  all paths
        ::
        (~(run by discussions) get-comments)
      ::  specific path
      ::
      %+  ~(put by *(per-path-url comments))  path.args
      %-  get-comments
      %+  ~(gut by discussions)  path.args
      *(map url discussion)
  ::
  ++  get-comments
    |=  m=(map url discussion)
    ^-  (map url comments)
    ?:  =(~ url.args)
      ::  all urls
      ::
      %-  ~(run by m)
      |=(discussion comments)
    ::  specific url
    ::
    %+  ~(put by *(map url comments))  url.args
    comments:(~(gut by m) url.args *discussion)
  --
--
