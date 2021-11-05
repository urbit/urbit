/-  policy, post
|%
++  orm  ((on id:post post) gth)
++  gorm  ((on gid:post post) global-cmp)
++  lorm  ((on time diff) gth)
++  global-cmp  |=([[ship a=time] [ship b=time]] (gth a b))
+$  timeline  ((mop id:post post) gth)
+$  log       ((mop time diff) gth)
+$  feed  
  $:  tl=timeline
      write=policy
      read=policy
      =log
  ==
+$  aggregate   ((mop gid:post post) global-cmp)
+$  policy-kind  ?(%read %write)
+$  action  (pair ship diff)
+$  update    (pair time diff)
+$  diff
  $%  [%replay =log]
      [%post =id:post =update:post]
      [%policy kind=?(%read %write) =diff:policy]
  ==
::
++  allowed
  |=  [=ship =policy]
  ?-  -.policy
    %white  (~(has in p.policy) ship)
  ::
      %black
    ?!  ?|
      (~(has in ships.policy) ship)
      (~(has in ranks.policy) (clan:title ship))
    ==
  ==

--
