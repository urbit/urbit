=/  list
  |=  a/#
  ..  l/#
  $%  cons  [|a l|]
      nil   $~
  ==
=/  lest
  |=  a/#
  [|$cons a (list a)|]
=/  my-awesome-trelist
  [%cons 1 %cons 2 %cons 3 %nil ~]
=/  my-awesome-list  `(list ?(1 2 3))`my-awesome-trelist
=/  my-awesome-lest  `(lest ?(1 2 3))`my-awesome-trelist
=/  even-cooler      `(list ?(1 2 3 4))`my-awesome-lest
=/  map
  |=  (a/# b/# f/<|a b|>)
  ..  self/<|(list a) (list b)|>
  |=  xs/(list a)
  ?#  xs
    [%cons y]  [%cons (f -.y) (self +.y)]
    [%nil y]   [%nil ~]
  ==
