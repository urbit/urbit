::
::::  /hoon/command/talk/mar
  ::
/?    310
/-    talk
/+    old-zuse
::
=,  talk
|_  cod/command
::
++  grab                                                ::  convert from
  |%
  ++  noun  command                                     ::  clam from %noun
  ++  json
    =,  old-zuse
    =,  jo
    |=  a/json  ^-  command
    =-  (need ((of -) a))
    =<  :~  publish+(ar thot)
            bearing+ul
        ==
    |%
    ++  op                                              ::  parse keys of map
      |*  {fel/rule wit/fist}
      %+  cu  malt
      %+  ci
        |=  a/(map cord _(need *wit))
        =,  unity
        ^-  (unit (list _[(wonk *fel) (need *wit)]))
        (drop-list (turn ~(tap by a) (head-rush fel)))
      (om wit)
    ::
    ++  ke                                              ::  callbacks
      |*  {gar/* sef/_|.(fist)}
      |=  jon/json
      ^-  (unit _gar)
      =-  ~!  gar  ~!  (need -)  -
      ((sef) jon)
    ::
    ++  as                                              ::  array as set
      |*  a/fist
      (cu ~(gas in *(set _(need *a))) (ar a))
    ::
    ++  peach
      |*  a/{rule rule}
      |=  tub/nail
      ^-  (like (each _(wonk (-.a)) _(wonk (+.a))))
      %.  tub
      ;~(pose (stag %& -.a) (stag %| +.a))
    ::
    ++  head-rush
      |*  a/rule
      |*  {cord *}
      =+  nit=(rush +<- a)
      ?~  nit  ~
      (some [u.nit +>->])
    ::
    ++  thot
      ^-  $-(json (unit thought))
      %-  ot  :~
        serial+ceri
        audience+audi
        date+di
        speech+spec
      ==
    ::
    ++  ceri
      ^-  $-(json (unit serial))
      (ci (slat %uv) so)
    ::
    ++  audi
      ^-  $-(json (unit audience))
      (as (su circ))
    ::
    ++  circ
      ^-  $-(nail (like circle))
      ;~((glue fas) ;~(pfix sig fed:ag) urs:ab)
    ::
    ++  spec
      ^-  $-(json (unit speech))
      %-  of  :~
        lin+(ot say+bo txt+so ~)
        url+(su aurf:urlp)
        ::  exp+(cu |=(a=cord [a ~]) so)
        ::  inv+(ot ship+(su fed:ag) party+(su urs:ab) ~)
      ==
  --
--  --
