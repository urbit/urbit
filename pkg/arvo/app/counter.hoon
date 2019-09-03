::  /app/counter
::
|_  [=bowl:gall count=@ud]
++  prep
  |=  old=(unit @ud)
  ?^  old  [~ this(+<+ u.old)]
  :_  this
  ::TODO  create permission if it doesn't exist?
  ::      or take permission path as an input
  ~
::
++  poke-noun
  |=  *
  ^-  (quip move _+>)
  :-  ~
  ?.  (can-increment src.bowl)
    ~&  [%ignoring src.bowl]
    +>
  ~&  [%incrementing src.bowl +(count)]
  +>(count +(count))
::
++  can-increment
  |=  who=ship
  =-  .^(? %gx -)
  :~  (scot %p our.bowl)
      %permissions
      (scot %da now.bowl)
      %permitted
      (scot %p who)
      dap.bowl
      %incrementers
  ==
--