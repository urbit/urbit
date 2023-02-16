::  Deletes all stale ames flows from failed (re) subscriptions
::
::    It runs in dry mode by default, printing the flows that can be closed.
::    To actually close the flows, run with |close-flows, =dry |
::
:-  %say
|=  [^ arg=~ dry=?]
::
[%helm-ames-kroc dry]
