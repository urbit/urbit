|%
++  seer
  |*  [r=mold a=mold]
  %+  each  a
  %+  pair  path
  $-(r (seer r a))  ::  scry receiver gate
++  tone
  |*  a=mold
  $%  [%done p=a]
      [%blok =path]
      [%mute =path]
  ==
++  boon
  |*  a=mold
  $@  ?(%blok %mute)
  [%done p=a]
++  sky
  |*  a=mold
  $-(path (boon a))
--
