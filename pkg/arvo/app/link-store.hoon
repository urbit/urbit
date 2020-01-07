::  link: social bookmarking
::
::    the paths under which links are submitted are generally expected to
::    correspond to existing group paths. for strictly-local collections of
::    links, arbitrary paths are probably fair game, but could trip up
::    primitive ui implementations.
::
::    see link-listen-hook to see what's synced in, and similarly
::    see link-proxy-hook to see what's exposed.
::
::  scry and subscription paths:
::
::      urls
::    /local-pages/[some-group]        all pages we saved by recency
::    /submissions/[some-group]        all submissions by recency
::      comments
::    /annotations/[some-group]/[url]  all our comments on url by recency
::    /discussions/[some-group]/[url]  all known comments on url by recency
::
/+  *link, default-agent, verb
::
|%
+$  state-0
  $:  %0
      by-group=(map path links)
      by-site=(map site (list [path submission]))
      discussions=(map path (map url discussion))
  ==
::
+$  links
  $:  ::NOTE  all lists by recency
      =submissions
      ours=pages
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
        [%x %local-pages ^]
      ``noun+!>((get-local-pages:do t.t.path))
      ::
        [%x %submissions ^]
      ``noun+!>((get-submissions:do t.t.path))
      ::
        [%y ?(%annotations %discussions) ~]
      ``noun+!>(~(key by discussions))
      ::
        [%y ?(%annotations %discussions) ^]
      =/  urls  (~(get by discussions) t.t.path)
      ?~  urls  ~
      ``noun+!>(~(key by u.urls))
      ::
        [%x %annotations @ ^]
      ``noun+!>((get-annotations:do t.t.path))
      ::
        [%x %discussions @ ^]
      ``noun+!>((get-discussions:do t.t.path))
    ==
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    ?>  (team:title [our src]:bowl)  ::TODO  /lib/store
    :_  this
    |^  ?+  path  (on-watch:def path)
            [%local-pages ^]
          %+  give  %link-update
          [%local-pages t.path (get-local-pages:do t.path)]
        ::
            [%submissions ^]
          %+  give  %link-update
          [%submissions t.path (get-submissions:do t.path)]
        ::
            [%annotations @ ^]
          %+  give  %link-update
          [%annotations t.path (get-annotations:do t.path)]
        ::
            [%discussions @ ^]
          %+  give  %link-update
          [%discussions t.path (get-discussions:do t.path)]
        ==
    ::
    ++  give
      |*  [=mark =noun]
      ^-  (list card)
      [%give %fact ~ mark !>(noun)]~
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
  ::
    %hear  (hear-submission +.action)
    %read  (read-comment +.action)
  ==
::  +save-page: save a page ourselves
::
++  save-page
  |=  [=path title=@t =url]
  ^-  (quip card _state)
  ?<  =(~ path)
  ::  add page to group ours
  ::
  =/  =links  (~(gut by by-group) path *links)
  =/  =page  [title url now.bowl]
  =.  ours.links  [page ours.links]
  =.  by-group  (~(put by by-group) path links)
  ::  do generic submission logic
  ::
  =^  cards  state
    (hear-submission path [our.bowl page])
  ::  send updates to subscribers
  ::
  :_  state
  :_  cards
  :+  %give  %fact
  :+  [%local-pages path]~
    %link-update
  !>([%local-pages path [page]~])
::  +note-note: save a note for a url
::
++  note-note
  |=  [=path =url udon=@t]
  ^-  (quip card _state)
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
  =/  fact
    :-  %link-update
    !>([%annotations path url [note]~])
  :*  [%give %fact [%annotations (snoc path url)]~ fact]
      [%give %fact [%annotations path]~ fact]
      cards
  ==
::  +hear-submission: record page someone else saved
::
++  hear-submission
  |=  [=path =submission]
  ^-  (quip card _state)
  ?<  =(~ path)
  ::  add link to group submissions
  ::
  =/  =links  (~(gut by by-group) path *links)
  =.  submissions.links  [submission submissions.links]
  =.  by-group  (~(put by by-group) path links)
  ::  add submission to global sites
  ::
  =/  =site  (site-from-url url.submission)
  =.  by-site  (~(add ja by-site) site [path submission])
  ::  send updates to subscribers
  ::
  :_  state
  :_  ~
  :+  %give  %fact
  :+  [%submissions path]~
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
  =.  comments.discussion  [comment comments.discussion]
  =.  urls  (~(put by urls) url discussion)
  =.  discussions  (~(put by discussions) path urls)
  ::  send updates to subscribers
  ::
  :_  state
  ^-  (list card)
  =/  fact
    :-  %link-update
    !>([%discussions path url [comment]~])
  :~  [%give %fact [%discussions (snoc path url)]~ fact]
      [%give %fact [%discussions path]~ fact]
  ==
::
::  reading
::
++  get-local-pages
  |=  =path
  ^-  pages
  ours:(~(gut by by-group) path *links)
::
++  get-submissions
  |=  =path
  ^-  submissions
  submissions:(~(gut by by-group) path *links)
::
++  get-annotations
  |=  =path
  ^-  notes
  ours:(get-discussion path)
::
++  get-discussions
  |=  =path
  ^-  comments
  comments:(get-discussion path)
::
++  get-discussion
  |=  =path
  ^-  discussion
  =+  (split-discussion-path path)  ::TODO  =/  [=path =url]
  =-  (~(gut by -) url *discussion)
  %+  ~(gut by discussions)  path
  *(map ^url discussion)
--
