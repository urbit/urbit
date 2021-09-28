/+  resource
|%
+$  update
  $~  [%add *resource 0]
  $%  [%add p=resource q=@ud]
      [%sub p=resource q=@ud]
      [%ini p=resource ~]
      [%run p=resource q=(list update)]
  ==
--
