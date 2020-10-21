|%
+$  form
  $:  todos=(map id todo)
      order=(list id)
  ==
::
+$  diff
  $%  [%upsert =id =todo]
      [%delete =id]
      [%relist order=(list id)]
  ==
::
+$  id  @uvJ
+$  todo
  $:  =cord
      =time
  ==
--
