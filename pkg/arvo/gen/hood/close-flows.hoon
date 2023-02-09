::  |close-flows: corks all stale ames flows
::
::    It runs in dry mode by default, printing the flows that can be closed.
::    To actually close the flows, run with |close-flows, =dry |
::
:-  %say
|=  [^ arg=~ dry=?]
::
[%helm-kroc dry]
