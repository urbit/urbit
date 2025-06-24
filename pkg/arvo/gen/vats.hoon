::  Print diagnostic information about desks
::
::  Accepts an optional argument of a list of one or more desks, returns info
::  on all desks if no desks are specified.
::
::  Keyword arguments include =filt and =verb. =filt takes one of %running,
::  %suspended, %exists, %exists-not, or %blocking; =verb takes either & or |
::
::  If both a list of desks and a filter are provided, the output will include
::  the desks from the list that match the filter, with the exception of the
::  %blocking filter which always returns all desks that match.
::
/-  *hood
:-  %say
|=  $:  [now=@da * bec=beak]
        deks=(list desk)
        [filt=@tas verb=_|]
    ==
:-  %tang  ^-  tang
(report-vats p.bec now deks filt verb)
