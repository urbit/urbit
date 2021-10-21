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
      [%ini =feed:feed]
      [%post =id:post =update:post]
      [%policy kind=?(%read %write) =diff:policy]
  ==
::
++  allowed
  |=  [=ship =policy]
  ?-  -.policy
      %invite
    ?|  (~(has in pending.policy) ship)
        (~(has in current.policy) ship)
    ==
      %open
    ?!  ?|
      (~(has in banned.policy) ship)
      (~(has in ban-ranks.policy) (clan:title ship))
    ==
  ==

--
