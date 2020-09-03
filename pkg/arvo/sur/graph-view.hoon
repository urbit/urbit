/-  group
/+  resource
^?
|%
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
        associated=(each resource policy:group)
    ==
    [%delete rid=resource ~]
    [%join rid=resource =ship] 
    [%invite rid=resource ships=(set ship)]
    [%groupify rid=resource title=@t description=@t]
  ==
--

