::  Userspace implementation of old  /_  @foo behavior
::                                                      ::  ::
::::  /hoon/rekey/lib                                   ::  ::
  ::                                                    ::  ::
/?  310                                                 ::  version
::                                                      ::  ::
::::                                                    ::  ::
  ::                                                    ::  ::
=,  wired
|*  typ=@tas
|*  inp=(map knot *)  ^-  (map (odo:raid typ) _(~(got by inp)))
=>  .(inp `(map knot _(~(got by inp)))`inp)
%-  malt
%+  murn  ~(tap by inp)
|*  [a=knot b=*]  ^-  (unit [(odo:raid typ) _b])
(both (slaw typ a) `b)
