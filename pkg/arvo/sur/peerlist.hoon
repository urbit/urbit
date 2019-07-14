|%
::
::  primitives
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
+$  peers   (map ship relation)
+$  tag     @ta
+$  group   (set ship)
+$  groups  (map tag group)
::
+$  settings
  $:  ::  default-public: public.relation is set to this on-creation
      ::
      default-public=_|
  ==
::
::  interface
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
+$  local-poke
  $%  ::  %add: add `who` as a peer, and/or to `group`
      ::
      ::    if the group doesn't exist, it is created
      ::
      [%add who=ship group=(unit tag)]
      ::  %remove: remove `who` as a peer (& all groups), or from `group`
      ::
      ::    if a group becomes empty, it's removed
      ::
      [%remove who=ship group=(unit tag)]
      ::  %fill: add all peers to this group
      ::
      [%fill group=tag]
      ::  %clear: clear all peers from this group
      ::
      [%clear group=tag]
      ::  %reset: clear all state
      ::
      [%reset ~]
      ::  %export: write export-file to spur (without mark)
      ::
      [%export =spur which=(set tag)]
      ::  %import: combine state with export-file at path (without mark)
      ::
      [%import =path]
      ::  %settings: configure new settings
      ::
      [%settings =settings]
      ::  %debug: helper
      ::
      [%debug ~]
  ==
::
+$  foreign-poke
  $%  ::  %peer: `src.bowl` added you as a peer
      ::
      [%peer since=@da]
      ::  %drop: `src.bowl` removed you as a peer
      ::
      [%drop ~]
  ==
--
