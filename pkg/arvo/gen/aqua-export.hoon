/+  *ph-util
:-  %say
|=  $:  [now=@da eny=@uv bec=beak]
        [who=@p ~]
        ~
    ==
|^
:-  %noun
(export-all who)
::
++  export-app
  |=  [who=ship agent=term]
  =/  aqua-pax=path
    :~  %i
        (scot %p who)
        %gx
        (scot %p who)
        agent
        (scot %da now)
        %export
        %noun
        %noun
    ==
  ~|  agent
  %-  need
  (scry-aqua (unit *) p.bec now aqua-pax)
::
++  export-all
  |=  who=ship
  %+  turn
    ^-  (list @tas)
    :~  %group-store
        %metadata-store
        %metadata-hook
        %contact-store
        %contact-hook
        %invite-store
        %chat-store
        %chat-hook
        %graph-store
    ==
  |=  app=@tas
  [app (export-app who app)]
::
--
