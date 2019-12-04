::  Tell app to print what it's doing
::
::  For apps that use lib/verb, :app +verb toggles verbosity.
::
:-  %say
|=  [* arg=?(~ [%bowl ~]) ~]
[%verb ?~(arg %loud %bowl)]
