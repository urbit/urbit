::  Turn on app verbosity for apps using /lib/verb
::
::  For apps that use lib/verb, :app +verb toggles verbosity.
::
:-  %say
|=  [* arg=?(~ [%bowl ~]) ~]
[%verb ?~(arg %loud %bowl)]
