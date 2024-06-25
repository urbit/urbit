/@  todos
/@  node
/-  manx-utils
/-  html-utils
:-  [%node %$ %todos]
|=  nod=node
^-  todos
|^
  =*  mu  ~(. manx-utils nod)
  =*  hu  ~(. mx.html-utils nod)
  :-  (is-checked (need (named:mu "show-done")))
  %+  turn  c.q:(need (gen:hu "todos"))
  |=  =manx
  ^-  [done=? name=@t]
  =.  nod  manx
  :-  (is-checked (need (named:mu "done")))
  (vol:mu "todo-name")
++  is-checked
  |=  =manx
  (~(has by (malt a.g.manx)) %checked)
--
