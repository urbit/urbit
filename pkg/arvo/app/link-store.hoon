::  link: social bookmarking
::
::    the paths under which links are submitted are generally expected to
::    correspond to existing group paths. for strictly-local collections of
::    links, arbitrary paths are probably fair game, but could trip up
::    primitive ui implementations.
::
::  scry and subscription paths:
::
::    /local-pages/[some-group]    all pages we saved by recency
::    /submissions/[some-group]    all submissions by recency
::
/+  *link, default-agent, verb
::
|%
+$  state-0
  $:  %0
      by-group=(map path links)
      by-site=(map site (list [path submission]))
  ==
::
+$  links
  $:  ::NOTE  all lists by recency
      =submissions
      ours=pages
  ==
::
+$  card  card:agent:gall
--
::
=|  state-0
=*  state  -
::
%+  verb  &
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
    %add   (add-page +.action)
    %hear  (hear-submission +.action)
  ==
::  +add-page: save a page ourselves
::
++  add-page
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
  :+  `[%local-pages path]
    %link-update
  !>([%local-pages path [page]~])
::  +hear-submission: record page someone else saved
::
++  hear-submission
  |=  [=path =submission]
  ^-  (quip card _state)
  ~&  [%hear-submission submission]
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
  :+  `[%submissions path]
    %link-update
  !>([%submissions path [submission]~])
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
--
