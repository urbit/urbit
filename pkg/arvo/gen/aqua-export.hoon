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
  ^-  @
  %-  need
  ;;  (unit @)
  .^  *
      %gx
      (scot %p p.bec)
      %aqua
      (scot %da now)
      %i
      (scot %p who)
      %gx
      (scot %p who)
      agent
      (scot %da now)
      %export
      %noun
      %noun
      ~
  ==
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
        %publish
::        %link-store
::        %link-listen-hook
    ==
  |=  app=@tas
  [app (export-app who app)]
::
--
