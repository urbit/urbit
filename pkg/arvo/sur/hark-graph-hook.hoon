/-  *resource, graph-store, post
^?
|%
::
+$  mode  ?(%each %count %none)
::
+$  watch-for  ?(%siblings %children %none)
::
+$  index-len
  [parent=@ud self=@ud] 
::
+$  notif-kind
  [name=@t =index-len =mode =watch-for]
::
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
