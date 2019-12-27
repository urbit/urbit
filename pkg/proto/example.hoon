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
  ==  ==
=/  list-abs
  $:
    list/<|# #|>
    $`
      nil   <|a/# (list a)|>
      cons  <|a/# a (list a) (list a)|>
      head  <|a/# (list a) (maybe a)|>
      tail  <|a/# (list a) (maybe (list a))|>
    ==
  ==
=/  cons-list
  =/  list
    |=  a/#
      ..  l/#
      $%  cons  [|a l|]
          nil   $~
    ==  ==
  ^-  list-abs
  :-  list
  |%
  ++  nil  <a/# [%nil ~]>
  ++  cons
    |=  a/#  x/a  xs/(list a)
      [%cons x xs]
    ==
  ++  head
    |=  a/#  xs/(list a)
      ?%  -.xs
        %cons  [%none ~]
        %nil   [%none ~]
    ==  ==
  ++  tail
    |=  a/#  xs/(list a)
      ^-  (maybe (list a))
      ?%  -.xs
        %cons  [%some +.xs]
        %nil   [%none ~]
    ==  ==
  --
