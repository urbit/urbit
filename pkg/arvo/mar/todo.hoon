/-  *todo
|_  todo=form
++  grab
  |%
  ++  noun  form
  --
++  grad
  |%
  ++  form  %todo-diff
  ++  dorm  ^diff
  ++  diff  !!
  ++  join  !!
  ++  mash  !!
  ++  pact
    |=  =^diff
    ^+  todo
    ?-    -.diff
        %relist  todo(order order.diff)
        %upsert
      %=    todo
          todos  (~(put by todos.todo) id.diff todo.diff)
          order
        ?:  (lien order.todo (cury test id.diff))
          order.todo
        [id.diff order.todo]
      ==
    ::
        %delete
      %=  todo
        todos  (~(del by todos.todo) id.diff)
        order  (skim order.todo (cury test id.diff))
      ==
    ==
  --
--
