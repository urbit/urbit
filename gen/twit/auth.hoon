::  Input twitter keys
/-    sole, twitter
!:
[sole twitter .]
|%
++  baz64  (cook crip (star alp))
--
!:
:-  %ask
|=  $:  {now/@da eny/@uvI bec/beak}
        {$~ $~}
    ==
^-  (sole-result (cask twit-do))
%+  sole-lo  [%& %$ "User: "]         %+  sole-go  urs:ab  |=  acc=span
%+  sole-lo  [%& %$ "App token: "]    %+  sole-go  baz64  |=  ctok=cord
%+  sole-lo  [%& %$ "App secret: "]   %+  sole-go  baz64  |=  csec=cord
%+  sole-lo  [%& %$ "User token: "]   %+  sole-go  baz64  |=  atok=cord
%+  sole-lo  [%& %$ "User secret: "]  %+  sole-go  baz64  |=  asec=cord
(sole-so %twit-do [acc %auth [ctok csec] atok asec])

