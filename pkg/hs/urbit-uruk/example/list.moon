=/  turn
  ..  $
  |=  (fun l)
  %+  l
    |=  (h t)
    (lcon (fun h) ($ fun t))
  ::
    |=  end
    lnil

=/  one-two-three
  (lcon 1 (lcon 2 (lcon 3 lnil)))

(turn (add 3) one-two-three)
