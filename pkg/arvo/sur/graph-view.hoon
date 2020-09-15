/-  *group
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
        app=@tas
        =associated
    ==
    [%delete rid=resource app=@tas]
    [%leave rid=resource app=@tas]
    [%join rid=resource app=@tas =ship] 
    ::[%invite rid=resource ships=(set ship)]
    [%groupify rid=resource app=@tas to=(unit resource)]
  ==
--

