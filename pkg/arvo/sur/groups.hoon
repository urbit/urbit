|%
++  group  (set ship)
::
+$  group-action
  $%  [%add members=group pax=path]
      [%remove members=group pax=path]
      [%bundle pax=path]
      [%unbundle pax=path]
  ==
::
+$  group-diff
  $%  [%keys keys=(set path)]
      [%path members=group pax=path]
      [%add members=group pax=path]
      [%remove members=group pax=path]
      [%bundle pax=path]
      [%unbundle pax=path]
  ==
--

