/-  *resource, post
^?
|%
+$  unread-mop  ((mop index:post unread) compare-indexes:post)
::
+$  read-type
  $%  [%group =group=resource]
      [%app =app=resource]
      [%app-at-index =app=resource =index:post]
  ==
::
::  NOTE: resource must be a graph within %graph-store
+$  action
  $%  [?(%listen %ignore) =app=resource]
      [%read =read-type]
  ==
::
+$  update
  $%  [%0 update-0]
  ==
::
+$  update-0
  $%  [%keys keys=(map =group=resource (set =app=resource))]
      [%add =unread]
      [%unreads =group=resource =app=resource unreads=unread-mop]
      action
  ==
::
+$  unread
  $:  date=@da
      module=@tas
      =group=resource
      =app=resource
      =body
  ==
::
+$  body
  $%  [%group =group-body]
      [%metadata =metadata-body]
      [%publish =index:post =publish-body]
      [%link =index:post =link-body]
      ::  [%chat ...] not included until it's in :graph-store
  ==
::
+$  group-body
  $%  [%add-members ships=(set ship)]
      [%remove-members ships=(set ship)]
  ==
::
+$  metadata-body
  $%  [%new title=@t description=@t]
      [%changed-title title=@t]
      [%changed-description description=@t]
      [%changed-color color=@ux]
      [%deleted title=@t]
  ==
::
+$  publish-body
  $%  [%post-new author=ship title=@t snippet=@t]
      [%post-edit author=ship title=@t]
      [%comment author=ship title=@t snippet=@t]
  ==
::
+$  link-body
  $%  [%new author=ship title=@t url=@t]
      [%comment author=ship title=@t snippet=@t]
  ==
--

