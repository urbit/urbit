::  +dbug: tell /lib/dbug app to print some generic state
::
::    :app +dbug
::      the entire state
::    :app +dbug %bowl
::      the entire bowl
::    :app +dbug [%state 'thing']
::      data at thing.state. allows for complex hoon, like '(lent thing)'
::    :app +dbug [direction specifics]
::      all in subs matching the parameters
::      direction: %incoming or %outgoing
::      specifics:
::        ~              all subscriptions
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
        args=?(~ [what=?(%bowl %state) ~] [=poke ~])
        ::  named arguments
        ::
        ~
    ==
:-  %dbug
?-  args
  ~          [%state '']
  [@ ~]      ?-(what.args %bowl [%bowl ~], %state [%state ''])
  [[@ *] ~]  poke.args
==
