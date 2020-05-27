|%
+$  serial  @uvH
::
+$  letter
  $%  [%text text=cord]
      [%url url=cord]
      [%code expression=cord output=(list tank)]
      [%me narrative=cord]
  ==
::
+$  envelope
  $:  uid=serial
      number=@
      author=ship
      when=time
      =letter
  ==
::
+$  config
  $:  length=@
      read=@
  ==
::
+$  mailbox
  $:  =config
      envelopes=(list envelope)
  ==
::
+$  inbox  (map path mailbox)
::
+$  chat-base
  $%  [%create =path]               ::  %create: create a mailbox at path
      [%delete =path]               ::  %delete: delete a mailbox at path
      [%message =path =envelope]    ::  %message: append a message to mailbox
      [%read =path]                 ::  %read: set mailbox to read
  ==
::
+$  chat-action
  $%  ::  %messages: append a list of messages to mailbox
      ::
      [%messages =path envelopes=(list envelope)]
      chat-base
  ==
::
+$  chat-update
  $%  [%initial =inbox]
      [%keys keys=(set path)]
      [%messages =path start=@ud end=@ud envelopes=(list envelope)]
      chat-base
  ==
--
