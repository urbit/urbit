|%
++  seer
  |*  [r=mold a=mold]
  $~  [%done *a]
  $%  [%done p=a]
      [%scry p=path k=$-(r (seer r a))]
  ==
++  seen
  |*  a=mold
  $@  ?(%mute %wait)
  [%done p=a]
++  tale
  |*  a=mold
  $%  [%mute =path]
      [%wait =path]
      [%done p=a]
  ==
++  view
  |*  a=mold
  $-(path (seen a))
--
