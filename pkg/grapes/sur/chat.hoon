|%
+$  writs  (list [=time writ])
+$  writ   [seal memo]
::
+$  seal
  $:  =time
      feel=(jug term ship)
  ==
::

+$  resource  (pair ship term)
+$  diff
  $%  [%add p=memo]
      [%del p=time] 
      [%add-feel p=time q=term]
      [%del-feel p=time q=term]
  ==
::
+$  memo  
  $:  author=ship
      sent=time
      content=cord :: TODO
  ==
::

+$  action
  (pair resource update)
+$  update
  (pair time diff)
+$  logs
  ((mop time diff) lte)
--
