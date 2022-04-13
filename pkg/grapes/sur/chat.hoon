|%
+$  resource  (pair ship term)
+$  diff
  $%  [%send p=cord]
      [%del p=time] 
      [%react p=time q=@ta]
  ==
+$  action
  (pair resource update)
+$  update
  (pair time diff)
+$  logs
  ((mop time diff) lte)
--
