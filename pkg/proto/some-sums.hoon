=/  bool  ?(true false file-not-found)
=/  my-sum
  ..  s/#
  $%  foo  ?(1 2)
      bar  $hello
      typ  #
      mor  [|s s|]
  ==
=/  bigger-sum
  ..  s/#
  $%  foo  ?(1 2)
      bar  $hello
      typ  #
      mor  [|s s|]
      pow  $pow
  ==
=/  choose-your-poison
  |=  a/?(lite medm stronk schoenberg)
    ^-  my-sum
    ?%  a
      %lite  [%foo 1]
      %medm  [%mor [%foo 1] %bar %hello]
      %stronk
        :-  %mor
        :-  :-  %mor  [[%foo 1] [%foo 2]]
        :-  %mor  :-  [%typ my-sum]  [%mor [%bar %hello] [%bar %hello]]
      %schoenberg
        ..  sch/my-sum  [%mor sch sch]
  ==  ==
