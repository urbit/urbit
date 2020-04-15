:: Well looking at the Data.Map implementation, a balanced tree map isn't going
:: to be done in a day. Time to do a simple assoc list instead.

:: We want a structure that's easy to jet. So we build an assoc list of cons
:: cells. In the interpreter, this turns into VLis [(VCon Val Val)].

=/  find-assoc
  ..  $
  |=  (eq l k)
  %+  (cas l)
    |=  link
    =/  entry  (car link)
    =/  e-key  (car entry)
    ?:  (eql e-key k)
      (lef (cdr entry))
    ($ eq (cdr link) k)
  <u (rit uni)>

::  always returns a list, replacing the key if equivalent
:: =/  add-assoc
::   ~/  5  add-assoc
::   ..  $
::   |=  (lt eq l k v)
::   %+  (cas l)
::     |=  link
::     =/  entry  (car link)
::     =/  e-key  (car entry)
::     ?:  (eq e-key k)
::       (lcon (con k v) (cdr link))
::     ?:  (lt e-key k)
::       (lcon entry ($ lt eq (cdr link) k v))
::     (lcon (con k v) (lcon entry (cdr link)))
::   ::
::   |=  u
::   :: if we made it to the end, this is the sorted position
::   (lcon (con k v) lnil)

=/  a  (add-assoc lth eql lnil 6 6)
=/  b  (add-assoc lth eql a 3 3)
=/  c  (add-assoc lth eql b 1 1)
=/  d  (add-assoc lth eql c 4 4)
=/  e  (add-assoc lth eql d 2 2)
=/  f  (add-assoc lth eql e 5 5)

(find-assoc eql f 3)
