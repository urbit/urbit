!:
::  /=main=/lib/chat/hoon
::
|%
++  chat                                                ::  user action
  $%  [%all p=? q=mess]                                 ::  broadcast
      [%how ~]                                          ::  help
      [%out ~]                                          ::  log out
      [%say p=@p q=mess]                                ::  private
      [%who ~]                                          ::  query users
  ==                                                    ::
++  mess                                                ::  message 
  $%  [%do p=@t]                                        ::  action
      [%ex p=@t q=tank]                                 ::  expression
      [%qu p=@t]                                        ::  quote
  ==                                                    ::
++  user  ,[p=@p q=@t]                                  ::  downstream identity
++  zing                                                ::  client to server
  $%  [%all p=? q=mess]                                 ::  broadcast
      [%ego p=@da]                                      ::  ping / last active
      [%out ~]                                          ::  log out
      [%who ~]                                          ::  query users
  ==                                                    ::
++  zong                                                ::  server to client
  $%  [%all p=sect q=user r=mess]                       ::  broadcast
      [%new p=user]                                     ::  user joined
      [%out p=user]                                     ::  user left
      [%who p=(list user) q=(list user)]                ::  users 
  ==                                                    ::
--
