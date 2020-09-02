/-  group
/+  resource
^?
|%
++  error
  $%
    [%offline =ship]
    [%no-permissions ~]
  ==
++  action      
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

