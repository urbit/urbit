::  Fibonacci sequence as an arvo kernel
::
=/  fib
  ~/  2  fib
  ..  $
  |=  (cache n)
  ?:  (zer n)    [1 cache]
  ?:  (eql n 1)  [1 cache]
  ::
  ?-    (find-assoc eql cache n)
      val
    [(unbox val) cache]
  ::
      nothing
    =/  n-fec-fec  ($ cache (fec (fec n)))
    =/  n-fec      ($ (cdr n-fec-fec) (fec n))
    =/  total      (add (car n-fec-fec) (car n-fec))
    ::
    [total (add-assoc lth eql (cdr n-fec) n (box total))]
  ==

=/  arvo
  ~/  2  arvo
  ..  $
  |=  (cache n)
  =/  val  (fib cache n)
  [(car val) ($ (cdr val))]

(arvo lnil)
