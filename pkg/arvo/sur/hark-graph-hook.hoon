/-  *resource, graph-store, post
^?
|%
+$  action
  $%
    [?(%listen %ignore) graph=resource =index:post]
    [%set-mentions mentions=?]
    [%set-watch-on-self watch-on-self=?]
  ==
::
+$  update
  $% 
    action
    $:  %initial
      watching=(set [resource index:post])
      mentions=_&
      watch-on-self=_&
    ==
  ==
--
