::
::::  /hoon/core/users/pro
  ::
/?  314
/-  users
!:
|_  use+users
::
++  grab                                                ::  convert from
  |%
  ++  noun  users                                       ::  convert from %noun
  --
++  grow
  |%
  ++  json
    %+  joba
      %users
    :-  %a
    %+  turn
      use
    |=  {p+@p q+@t}
    %-  jobe
      ~[[%ship (jape <p>)] [%name [%s q]]]
  ++  mime
    [/text/json (tact (pojo json))]
  --  
--
