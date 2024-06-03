/@  txt         ::  @t
/@  diary       ::  name=@t
/@  diary-diff  ::  ?([%put-entry id=@da =txt] [%del-entry id=@da])
::
::  outer core of a shrub: define state, pokes,
::  dependencies, and kids
^-  kook:neo
|%
::
::  diary's state is a %diary, just a @t
++  state
  ^-  curb:neo
  [%pro %diary]
::
::  diary takes pokes with stud %diary-diff
++  poke
  ^-  (set stud:neo)
  (sy %diary-diff ~)
::
::  constrain shrubs below diary in the namespace
::  by defining the types of their state and pokes
++  kids
  ::  kids:neo is a (unit port:neo)
  ^-  kids:neo
  %-  some
  ::  port:neo is (pair dare:neo lads:neo)
  ::  dare:neo is ?(%y %z)
  ::  if %y, only constrain our immediate children
  ::  if %z, recursively constrain all descendants
  :-  %y
  ::  lads:neo is (map pish:neo lash:neo)
  %-  ~(gas by *lads:neo)
  :~  :-  ::  pish:neo
          ::  to simplify: [%.n @da] means the kid's
          ::  path contains any @da, and %.n is there
          ::  to signify that the pith can not have more 
          ::  fields afterwards
          [[%.n %da] %.n]
      ::  lash:neo is (pair curb:neo (set stud:neo))
      ::  curb:neo defines the kids' state
      ::  (set stud:neo) defines the kids' pokes
      [[%only %txt] ~]
  ==
::
::  diary has no other shrubs as dependencies
++  deps
  ^-  deps:neo
  *deps:neo
::
::  inner core, business logic
++  form
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo =pail:neo]
  ++  init
    |=  old=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    :-  ~
    ?~  old
      [%diary !>(*diary)]
    u.old
  ::
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    ?>  =(%diary p.pail)
    ?>  =(%diary-diff stud)
    =/  state  !<(diary q.pail)
    =/  act    !<(diary-diff vax)
    ::
    ::  assert the poke comes from our ship
    ::  src.bowl:neo is (pair ship pith)
    ?>  =(our ship.src):bowl
    ?-    -.act
       %put-entry
      :_  [%diary !>(state)]
      ::  create list of one card:neo
      ::  card:neo is (pair pith:neo note:neo)
      :~  :-  %+  welp
                ::  here.bowl is the path of this shrub
                ::  /path/to/diary
                here.bowl
              ::  append post id
              ::  /path/to/diary/~2024.6.3..14.07.15..7098
              ~[[%da id.act]]
          ::  this note will %make a new shrub
          ::  at the pith we defined above
          ^-  note:neo
          ::  [%make stud:neo (unit pail:neo) conf:neo]
          :*  %make
              :: new shrub has state type %txt
              %txt
              ::  new shrub's initial state
              ::  is the text from the poke
              `[%txt !>(txt.act)]
              ::  conf:neo is (map term pith:neo)
              ::  declare this new shrub's dependencies,
              ::  which are also shrubs; 
              ::  in this case, none
              ~
          ==
      ==
        %del-entry
      :_  [%diary !>(state)]
      :~  :-  %+  welp
                here.bowl
              ~[[%da id.act]]
          ^-  note:neo
          [%tomb ~]
      ==
    ==
  --
--
