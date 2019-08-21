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
+$  group-update
  $%  [%keys keys=(set path)]
      [%path members=group pax=path]
      group-action
  ==
--

