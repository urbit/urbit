=/  id  <a/# x/a a>
=/  bool   ?(0 1)
=/  true   `bool`1
=/  false  `bool`0
=/  and    <a/bool b/bool `bool`?%(a; 0 false, 1 b)>
=/  or     <a/bool b/bool `bool`?%(a; 1 true, 0 b)>
=/  maybe
  |=  a/#
  $%  some  a
      none  $~
  ==
=/  list-sig
  $:
    list/<|# #|>
    $`
      nil   <|a/# (list a)|>
      cons  <|a/# a (list a) (list a)|>
      head  <|a/# (list a) (maybe a)|>
      tail  <|a/# (list a) (maybe (list a))|>
    ==
  ==
=/  cons-list-module
  =/  list
    |=  a/#
    ..  l/#
    $%  cons  [|a l|]
        nil   $~
    ==
  ^-  list-sig
  :-  list
  |%
  ++  nil  <a/# [%nil ~]>
  ++  cons
    |=  (a/# x/a xs/(list a))
    [%cons x xs]
  ++  head
    |=  (a/# xs/(list a))
    ?#  xs
      [%cons y]  [%some -.y]
      [%nil y]   [%none ~]
    ==
  ++  tail
    |=  (a/# xs/(list a))
    ^-  (maybe (list a))
    ?#  xs
      [%cons y]  [%some +.y]
      [%nil y]   [%none ~]
    ==
  --
=/  snoc-list-module
  =/  list
    |=  a/#
    ..  l/#
    $%  snoc  [|l a|]
        nil   $~
    ==
  ^-  list-sig
  :-  list
  |%
  ++  nil  <a/# [%nil ~]>
  ++  cons
    |=  (a/# x/a xs/(list a))
    [%snoc xs x]
  ++  head
    |=  (a/# xs/(list a))
    ?#  xs
      [%snoc y]  [%some +.y]
      [%nil y]   [%none ~]
    ==
  ++  tail
    |=  (a/# xs/(list a))
    ^-  (maybe (list a))
    ?#  xs
      [%snoc y]  [%some -.y]
      [%nil y]   [%none ~]
    ==
  --
