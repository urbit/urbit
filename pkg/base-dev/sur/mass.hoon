|%
+$  action
  $%  [%mass ~]
      [%poll every=(unit $@(@dr [d=@ud h=@ud m=@ud]))]
      [%free path=(list @t) before=(unit @da)]
  ==
+$  update
  $%  [%new =time path=(list @t) size=@ud]
      [%new-all =time data=(map (list @t) @ud)]
      [%old path=(list @t) list=(list [=time size=@ud])]
      [%old-all data=(jar (list @t) [=time size=@ud])]
      [%newest p=(unit [=time path=(list @t) size=@ud])]
      [%raw p=(unit [=time quacs=(list quac:dill)])]
      [%poll every=(unit $@(@dr [d=@ud h=@ud m=@ud]))]
  ==
--
