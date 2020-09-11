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
::  $app: An app that is associated to a graph-store mark
::
+$  app-name  ?(%chat %publish %links)
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
        app=app-name
        =associated
    ==
    [%delete rid=resource app=app-name]
    [%leave rid=resource app=app-name]
    [%join rid=resource app=app-name =ship] 
    ::[%invite rid=resource ships=(set ship)]
    [%groupify rid=resource app=app-name to=(unit resource)]
  ==
--

