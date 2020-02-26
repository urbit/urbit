::  Left tagged cons cell
=/  cons
  |=  (head tail)
  (lef [head tail])

::  Right tagged null terminator
=/  null
  (rit uni)

::  Takes a list and a function
=/  turn
  ~/  2  turn
  ..  $
  |=  (data fun)
  %^  cas  data
    |=  p
    (cons (fun (car p)) ($ (cdr p) fun))
  ::
    |=  nil
    null

:: =/  broken-turn
::   ~/  2  broken-turn
::   ..  $
::   |=  (data fun)
::   ?-  data
::     p  (cons (fun (car p)) ($ (cdr p) fun))
::     y  null
::   ==
  
::  Takes two lists and returns the two concatendated
=/  weld
  ~/  2  weld
  ..  $
  |=  (first second)
  %^  cas  first
    |=  p
    (cons (car p) ($ (cdr p) second))
  ::
    |=  nil
    second

::  Take a list of lists and welds each one together
=/  zing
  ~/  1  zing
  ..  $
  |=  rest
  %^  cas  rest
    |=  p
    (weld (car p) ($ (cdr p)))
  ::
    |=  nil
    null

::  Weld the two lists together
%-  zing
%+  cons  (turn (cons 0 (cons 1 (cons 2 null))) (add 1))
%+  cons  (turn (cons 0 (cons 1 (cons 2 null))) (add 4))
%+  cons  (turn (cons 0 (cons 1 (cons 2 null))) (add 7))  null
