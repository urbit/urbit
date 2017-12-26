::
::::  /mar/hall/command/hoon
  ::
/?    310
/-    hall
/+    hall-json
::
=,  hall
|_  cod/command
::
++  grab                                                ::  convert from
  |%
  ++  noun  command                                     ::  from %noun
  ++  json                                              ::  from %json
    =>  [. dejs:hall-json]  ::TODO  =,
    =,  dejs-soft:format
    |=  a/json
    ^-  command:hall
    =-  (need ((of -) a))
    :~  publish+(ar thot)
        present+(ot nos+(as so) dif+disa ~)
        ::  bearing not needed
    ==
  --
::
++  grow                                                ::  convert to
  |%
  ++  json                                              ::  to %json
    =>  [. enjs:hall-json]  ::TODO  =,
    =,  enjs:format
    %+  frond  -.cod
    ::  only %publish has just a single piece of data.
    ?:  ?=($publish -.cod)  a+(turn tos.cod thot)
    %-  pairs
    ?+  -.cod  !!
      $present  ~[nos+(sa nos.cod cord:enjs:hall-json) dif+(disa dif.cod)]
      ::  bearing nto needed
    ==
  --
--
