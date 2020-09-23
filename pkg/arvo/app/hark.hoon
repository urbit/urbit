::  hark: notifications [landscape]
::
/-  post
/+  default-agent, dbug
=*  resource  resource:post
::
~%  %hark-top  ..is  ~
|%
+$  card  card:agent:gall
+$  action
  $%  [?(%listen %ignore) ?(%graph %group) =resource]
  ==
+$  versioned-state
  $%  state-0
  ==
+$  state-0
  $:  %0
      unreads=((mop @ unread) lte)
      $=  listens
      $:  groups=(set resource)
          graphs=(set resource)
  ==  ==
+$  unread
  $:  date=@da
      module=@tas
      =group=resource
      =app=resource
      =body
  ==
+$  body
  $%  [%group =group-body]
      [%metadata =metadata-body]
      [%publish =index:post =publish-body]
      [%link =index:post =link-body]
      ::  [%chat ...] not included until it's in :graph-store
  ==
+$  group-body
  $%  [%ship-joined =ship]
      [%ship-left =ship]
  ==
+$  metadata-body
  $%  [%new title=@t description=@t]
      [%changed-title title=@t]
      [%changed-description description=@t]
      [%changed-color color=@ux]
      [%deleted title=@t]
  ==
+$  publish-body
  $%  [%post-new author=ship title=@t snippet=@t]
      [%post-deleted author=ship title=@t]
      [%post-edit author=ship title=@t]
      [%comment author=ship title=@t snippet=@t]
  ==
+$  link-body
  $%  [%new author=ship title=@t url=@t]
      [%deleted author=ship title=@t url=@t]
      [%comment author=ship title=@t snippet=@t]
  ==
--
::
=|  state-0
=*  state  -
::
%-  agent:dbug
^-  agent:gall
~%  %hark-agent  ..card  ~
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init
  :_  this
  :~  [%pass /metadata %agent [our.bowl %metadata-store] %watch /updates]
      [%pass /group %agent [our.bowl %group-store] %watch /groups]
  ==
::
++  on-save  !>(state)
++  on-load
  |=  old=vase
  ^-  (quip card _this)
  [~ this(state !<(state-0 old))]
::
++  on-watch
  ~/  %hark-watch
  |=  =path
  ^-  (quip card _this)
  !!
::
++  on-poke
  ~/  %hark-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  !!
::
++  on-peek
  ~/  %hark-peek
  |=  =path
  ^-  (unit (unit cage))
  !!
::
++  on-arvo
  ~/  %hark-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card _this)
  !!
::
++  on-agent  on-agent:def  ::  TODO resubscribe on kick
++  on-leave  on-leave:def
++  on-fail   on-fail:def
--
