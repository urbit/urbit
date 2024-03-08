/-  neo
=<  p-addr
|%
+$  address
  [one=@t two=@t city=@t zip=@ud state=@t]
++  pike  pike:neo
++  goon  goon:neo
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
::
++  p-addr
  =/  m  (pike vase)
  ^-  form:m
  ;<  zip=(pole clot:goon)  bind:m 
    =-  %+  grab-validate  ~[-]
        |=  ls=(pole clot:goon)
        ^-  (list (unit cord))
        ?>  ?=([[%patud p=@ud] ~] ls)
        =-  ~[-]
        ?:  &((gth p.ls 10.000) (lth p.ls 99.999))
          ~
        `'Invalid ZIP code'
    :*  'Zip Code'
        'Please enter the zip code of your billing address'
        ~
        %patud
    ==
  ~&  zip
  ?>  ?=([[%patud p=@ud] ~] zip)
  ;<  =cage  bind:m
    (peek /dummy/zipcode/(scot %ud p.zip))
  ?>  =(%addr-info p.cage)
  =+  !<([city=@t state=@t] q.cage)
  ;<  lines=(pole clot:goon)  bind:m
    %-  grab
    :~  ['Address Line 1' '' ~ %cord]
        ['Address Line 2' '' ~ %cord]
    ==
  ?>  ?=([one=[%cord p=cord] two=[%cord p=cord] ~] lines)
  =/  addy=address
    [p.one.lines p.two.lines city p.zip state]
  (pure:m !>(addy))
--
