::  peerlist: (manual) userspace peer discovery
::
::    a drop-in solution to peer discovery for peer-to-peer applications.
::    for usage, see +local-pokes.
::
|%
+$  state
  $:  ::  peers: all relations
      ::
      =peers
      ::  files: locally defined clusters
      ::
      =files
      ::  settings: behavior configuration
      ::
      =settings
  ==
::
+$  peers  (map ship relation)
+$  files  (map tag file)
+$  tag    @ta
+$  file   (set ship)
::
+$  relation
  $:  ::  we-since: when we added them as a peer
      ::
      we-since=(unit @da)
      ::  they-since: when they added us as a peer
      ::
      they-since=(unit @da)
      ::  public: visible to others
      ::
      public=?
  ==
::
+$  settings
  $:  ::  default-public: public.relation is set to this on-creation
      ::
      default-public=_|
  ==
::
+$  local-pokes
  $%  ::  %add: add `who` as a peer, and/or to `file`
      ::
      ::    if the file doesn't exist, it is created
      ::
      [%add who=ship file=(unit tag)]
      ::  %remove: remove `who` as a peer (& all files), or from `file`
      ::
      ::    if a file becomes empty, it's removed
      ::
      [%remove who=ship file=(unit tag)]
      ::  %fill: add all peers to this file
      ::
      [%fill file=tag]
      ::  %export: write export-file to path
      ::
      [%export =path]
      ::  %import: replace state with export-file at path
      ::
      [%import =path]
      ::  %debug: helper
      ::
      [%debug ~]
  ==
::
+$  foreign-pokes
  $%  ::  %peer: `src.bowl` added you as a peer
      ::
      [%peer since=@da]
      ::  %drop: `src.bowl` removed you as a peer
      ::
      [%drop ~]
  ==
::
+$  filter
  $%  ::  %all: all relations
      ::
      %all
      ::  %ours: relations where ?=(^ we-since)
      ::
      %ours
      ::  %theirs: relations where ?=(^ they-since)
      ::
      %theirs
      ::  %mutuals: only mutual relations
      ::
      %mutuals
      ::  %none: no relations
      ::
      %none
  ==
::
+$  move  [bone card]
+$  card
  $%  [%poke wire dock %noun foreign-pokes]
  ==
--
::
::
|_  [=bowl:gall state]
++  this  .
::
++  prep
  |=  old=(unit state)
  ^-  [(list move) _this]
  ?~  old
    [~ this]
  [~ this(+<+ u.old)]
::
::  utilities
::
++  is-mutual
  |=  relation
  &(?=(^ we-since) ?=(^ they-since))
::
++  files-with
  |=  who=ship
  %-  ~(gas in *(set tag))
  %+  murn  ~(tap by files)
  |=  [=tag =file]
  ^-  (unit ^tag)
  ?.  (~(has in file) who)  ~
  `tag
::
++  fall-relation
  |=  f=(unit relation)
  %+  fall  f
  %*(. *relation public default-public.settings)
::
::  management
::
++  do-add
  |=  [who=ship file=(unit tag)]
  =/  old=relation
    %-  fall-relation
    (~(get by peers) who)
  =/  new=?  ?=(~ we-since.old)
  =?  peers  new
    %+  ~(put by peers)  who
    old(we-since `now.bowl)
  =?  files  ?=(^ file)
    (~(put ju files) u.file who)
  :_  this
  ?.  new  ~
  [(peer who) ~]
::
++  do-remove
  |=  [who=ship from=(unit tag)]
  =/  have=(unit [who=ship =relation files=(set tag)])
    =+  old=(~(get by peers) who)
    ?~  old  ~
    `[who u.old (files-with who)]
  ?^  from
    ::  remove from one file
    ::
    ~&  "removing {(scow %p who)} from %{(trip u.from)}"
    =*  from-file=tag  u.from
    =-  [~ this(files -)]
    (~(del ju files) from-file who)
  ::  remove as peer
  ::
  ~&  "droping {(scow %p who)}"
  =?  peers  ?=(^ have)
    ?~  they-since.relation.u.have
      ::  if they don't care, remove relation
      ::
      (~(del by peers) who)
    ::  if they still care, keep them registered
    ::
    %+  ~(put by peers)  who
    relation.u.have(we-since ~)
  ::  & remove from all files
  ::
  =.  files
    %-  ~(gas by *^files)
    %+  murn  ~(tap by files)
    |=  [=tag =file]
    ^-  (unit _+<)
    =+  (~(del in file) who)
    ?:(=(~ -) ~ `[tag -])
  :_  this
  ::  only send drop poke if we used to care
  ::
  ?.  ?&  ?=(^ have)
          ?=(^ we-since.relation.u.have)
      ==
    ~
  [(drop who) ~]
::
++  do-fill
  |=  file=tag
  =-  [~ this(files -)]
  %+  ~(put by files)  file
  %-  ~(uni in ~(key by peers))
  (fall (~(get by files) file) *^file)
::
::  move construction
::
++  peer
  |=  who=ship
  ^-  move
  [ost.bowl %poke / [who dap.bowl] %noun %peer now.bowl]
::
++  drop
  |=  who=ship
  ^-  move
  [ost.bowl %poke / [who dap.bowl] %noun %drop ~]
::
::  interaction interface
::
++  poke-noun
  |=  poke=*
  ^-  [(list move) _this]
  ~|  %weird-poke-data
  =/  poke  ;;($%(local-pokes foreign-pokes) poke)
  ?-  -.poke
      ?(%peer %drop)
    ?:  =(src.bowl our.bowl)  [~ this]
    (poke-foreign-poke poke)
  ::
      ?(%add %remove %fill %export %import %debug)
    ?.  =(src.bowl our.bowl)  [~ this]
    (poke-local-poke poke)
  ==
::
++  poke-local-poke
  |=  poke=local-pokes
  ?.  =(our.bowl src.bowl)  [~ this]
  ~&  [%poked -.poke]
  ^-  [(list move) _this]
  ?-  -.poke
    %add  (do-add +.poke)
    %remove  (do-remove +.poke)
    %fill  (do-fill +.poke)
    %export  !!
    %import  !!
  ::
      %debug
    ~&  peers+peers
    ~&  files+files
    [~ this]
  ==
::
++  poke-foreign-poke
  |=  poke=foreign-pokes
  ~&  [%foreign-poke src.bowl -.poke]
  ^-  [(list move) _this]
  =*  who=@p  src.bowl
  =/  =relation
    %-  fall-relation
    (~(get by peers) who)
  ?-  -.poke
      %peer
    ~&  "{(scow %p who)} added you as a peer"
    =-  [~ this(peers -)]
    %+  ~(put by peers)  who
    relation(they-since `since.poke)
  ::
      %drop
    ~&  "{(scow %p who)} removed you as a peer"
    =-  [~ this(peers -)]
    ?~  we-since.relation
      (~(del by peers) who)
    %+  ~(put by peers)  who
    relation(they-since ~)
  ==
::
::TODO  data interface
::
--
