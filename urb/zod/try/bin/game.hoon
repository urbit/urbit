!:
::  /=try=/bin/game/hoon
::
=>  .(- `[who=@p how=path]`-)
=>  .(+ =>(+ ^/=main=/hume))
|=  [est=time eny=@uw]
|=  [neighbor=tape ~]
^-  bowl
=+  messages=*(list ,@t)
%-    lunt  
:*  who 
    %none
    :-  :~  :*  :~  [%& /localhost]
                    [%| .127.0.0.1]
                ==
                /game
                (shas %game eny)
                /try
            ==
        ==
    ^-  vase  
    !>  |%
        ++  give
          |=  [now=@da fig=weev]
          ^-  [p=(list gift) q=(unit ,[p=(list slip) q=_+>.$])]
          ?+    -.fig
            [~[la/leaf/"give other {<fig>}"] ~]
          ::
              %form
            =+  msg=(~(get by q.fig) %msg)
            ?~  msg
              [~[la/leaf/"give odd form {<q.fig>}"] ~]
            [~ [~ ~ +>.$(messages [u.msg messages])]]
          ==
        ::
        ++  miss
          |=  [pac=pact ced=cred]
          ^-  (unit purl)
          ~ 
        --
==
