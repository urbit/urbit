/-  neo
=<  p-title
|%
+$  msg
  [%title title=@t]
++  pike  pike:neo
++  goon  goon:neo
++  get-bowl
  =/  m  (pike bowl:pike)
  ^-  form:m
  |=  in=input:pike
  ^-  output:m 
  [%done bowl.in]
++  grab
  |=  items=(list item:pike)
  =/  m  (pike (pole clot:goon))
  ^-  form:m
  |=  in=input:pike
  ^-  output:m 
  ?^  syn.in
    ?.  ?=(%grab -.u.syn.in)
      [%fail %weird-card ~]
    [%done items.u.syn.in]
  [%emit %grab items]
++  grab-one
  |=  =item:pike
  =/  m  (pike clot:goon)
  ;<  =(pole clot:goon)  bind:m 
    (grab item ~)
  ?>  ?=([clot=* ~] pole)
  (pure:m clot.pole)
++  grab-validate
  |=  [items=(list item:pike) valid=$-((list clot:goon) (list (unit @t)))]
  =/  m  (pike (pole clot:goon))
  ^-  form:m
  |-  =*  loop  $
  ;<  res=(list clot:goon)  bind:m
    (grab items)
  =/  errs  (valid res)
  =/  has-err=?
    %+  roll  errs
    |=  [er=(unit cord) has-err=_|]
    ?:  has-err  &
    ?~  er  |
    &
  ?.  has-err
    (pure:m res)
  =/  new
    =|  new=(list item:pike)
    |-  
    ?~  items
      new
    ?~  errs  
      new
    =.  err.i.items  i.errs
    $(items t.items, new (snoc new i.items), errs t.errs)
  loop(items new)
::
++  peek
  |=  =path
  =/  m  (pike cage)
  ^-  form:m
  |=  in=input:pike
  ^-  output:m
  ?^  syn.in
    ?.  ?=(%peek -.u.syn.in)
      [%fail %weird-card ~]
    [%done cage.u.syn.in]
  [%emit %peek path] 
++  p-title
  =/  m  (pike ewer:neo)
  ^-  form:m
  ;<  p=(pole clot:goon)  bind:m 
    %-  grab 
    =-  ~[-]
    :*  'Title'
        'Enter the new title'
        ~
        %cord
    ==
  ?>  ?=([[%cord p=@t] ~] p)
  ;<  =bowl:pike  bind:m  get-bowl
  (pure:m %chat-diff !>([%title title=p.p]))
::
--
