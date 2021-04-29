/-  *group, store=graph-store, met=metadata-store
/+  resource
^?
|%
::  $associated: A group to associate, or a policy if it is unmanaged
::
+$  associated
  $%  [%group rid=resource]
      [%policy =policy]
  ==
::
::  $error: An error from a graph-view poke
::  
::    %offline: Ship is offline
::    %bad-perms: Not permitted
::    %unknown: Anything not described above
::
+$  error
  ?(%offline %bad-perms %unknown)
::  $action: A semantic action on graphs
::  
::    %create: Create a graph and associated metadata
::    %delete: Delete a graph
::    %join: Join a graph
::    %invite: Invite users to a graph
::    %groupify: Make graph into managed group
::
+$  action      
  $%
    $:  %create
        rid=resource
        title=@t
        description=@t
        mark=(unit mark)
        =associated
        module=@t
    ==
    [%delete rid=resource]
    [%leave rid=resource]
    [%join rid=resource =ship] 
    ::[%invite rid=resource ships=(set ship)]
    [%groupify rid=resource to=(unit resource)]
    [%forward rid=resource =update:store]
    [%eval =cord]
    [%pending-indices pending=(map hash:store index:store)]
    [%create-group-feed group=resource vip=vip-metadata:met]
    [%disable-group-feed group=resource]
  ==
--

