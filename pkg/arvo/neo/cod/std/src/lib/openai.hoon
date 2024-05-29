|% 
+$  role  ?(%system %user)
+$  model  ?(%gpt-4o %gpt-4-turbo)
++  headers
  |=  key=@t
  ^-  (list [k=@t v=@t])
  =-  ~&(- -)
  :~  'Content-Type'^'application/json'
      'Authorization'^(crip "Bearer {(trip key)}")
  ==


+$  message
  $:  =role
      content=cord
  ==
+$  choice
  $:  index=@ud
      =message

  ==
++  req
  =<  req
  |%
  +$  req  (pair @uv ask)
  +$  ask
    $%  [%chat-create create:chat]
    ==
  ++  chat
    |%
    +$  create
      $:  messages=(list message)
          =model
      ==
    --
  -- ::
++  res
  =<  res
  |%
  +$  res  (pair @uv ans)
  +$  ans
    $%  [%chat-completion completion:chat]
    ==
  ++  chat
    |%
    +$  completion
      $:  id=cord
          =model
          choices=(list choice)
      ==
    --
  --
++  enjs
  =,  enjs:format
  |%
  ++  message
    |=  m=^message
    %-  pairs
    :~  role/s/role.m
        content/s/content.m
    ==
  ++  chat-create-req
    |=  r=create:chat:req
    %-  pairs
    :~  messages/a/(turn messages.r message)
        model/s/model.r
    ==
  --
++  dejs
  =,  dejs:format
  |%
  ++  model  (su (perk %gpt-4o %gpt-4-turbo ~))
  ++  role   (su (perk %system %user ~))
  ++  choice
    %-  ot
    :~  index/ni
        message/message
    ==
  ++  chat-completion-res
    %-  ot
    :~  id/so
        model/model
        choices/(ar choice)
    ==
  ++  message
    %-  ot
    :~  role/role
        content/so
    ==
  --
::
--
