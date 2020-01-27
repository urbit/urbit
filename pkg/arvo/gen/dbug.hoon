::  +dbug: tell /lib/dbug app to print some generic state
::
::    :app +dbug
::      the entire bowl
::    :app +dbug [direction] [specifics]
::      all in subs matching the parameters
::      direction: %incoming or %outgoing
::      specifics:
::        [%ship ~ship]  subscriptions to/from this ship
::        [%path /path]  subscriptions on path containing /path
::        [%wire /wire]  subscriptions on wire containing /wire
::        [%term %name]  subscriptions to app %name
::
/+  *dbug
::
:-  %say
|=  $:  ::  environment
        ::
        *
        ::  inline arguments
        ::
        args=?(~ [what=?(%bowl %state) ~] [=what =about ~])
        ::  named arguments
        ::
        ~
    ==
:-  %dbug
?-  args
  ~        [%bowl *about]
  [@ ~]    [what.args *about]
  [@ * ~]  [what about]:args
==
