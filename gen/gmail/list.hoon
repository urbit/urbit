::  List [number] inbox messages  XX may be broken
::
::::  /hoon/list/gmail/gen
  ::
/?    310
::
::::
  ::
:-  %say
|=  $:  {now/@da eny/@uvJ bec/beak}
        arg/$@($~ {number/@u $~})
        $~
    ==
?~  arg  $(arg [5 ~])
:-  %noun
%+  turn  (scag number.arg .^((list {@t @t}) %gx /=gmail=/read/messages))
|=  {message-id/@t thread-id/@t}
=+  .^({from/@t subject/@t} %gx /=gmail=/read/messages/[message-id])
[from=from (trip subject)]
