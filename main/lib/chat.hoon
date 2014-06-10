!:
::  /=main=/lib/chat/hoon
::
|%
++  chat                                                ::  user action
  $%  [%all p=room q=? r=mess]                          ::  broadcast
      [%def p=mess]                                     ::  default
      [%how ~]                                          ::  help
      [%out ~]                                          ::  log out
      [%say p=@p q=mess]                                ::  private
      [%who p=?(%tis %ttt %tcc) q=(list room)]          ::  query users
      [%kil p=(list ,@p)]                               ::  kill user(s)
      [%res p=(list ,@p)]                               ::  resuscitate(s)
  ==                                                    ::
++  mess                                                ::  message
  $%  [%do p=@t]                                        ::  action
      [%ex p=@t q=tank]                                 ::  expression
      [%qu p=@t]                                        ::  quote
  ==                                                    ::
++  sand                                                ::  chat state
  $%  [& p=?]                                           ::  broadcast
      [| p=@p]                                          ::  private
  ==                                                    ::
++  user  ,[p=@p q=@t]                                  ::  downstream identity
++  room  ,@tas                                         ::  room
++  coci  %mars                                         ::  default room
++  zing                                                ::  client to server
  $%  [%all p=room q=? r=mess]                          ::  broadcast
      [%ego p=room q=@da]                               ::  ping / last active
      [%out ~]                                          ::  log out
      [%who p=room q=(unit (list room))]                ::  query users
  ==                                                    ::
++  zong                                                ::  server to client
  $%  [%all p=@da q=room r=sect s=user t=mess]          ::  broadcast
      [%new p=@da q=room r=user]                        ::  user joined
      [%out p=@da q=room r=user]                        ::  user left
      [%who p=@da q=room r=(map room (list user))]      ::  users
  ==                                                    ::
--

