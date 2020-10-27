/-  *resource, graph-store
^?
|%
::
+$  action
  $%
    [?(%listen %ignore) graph=resource]
    [%set-mentions mentions=?]
    [%set-watch-on-self watch-on-self=?]
  ==
::
+$  update
  $% 
    action
    $:  %initial
      watching=(set resource)
      mentions=_&
      watch-on-self=_&
    ==
  ==
--

