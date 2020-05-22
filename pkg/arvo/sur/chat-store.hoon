^?
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
<<<<<<< HEAD
+$  configs  (map path config)
::
+$  diff
=======
+$  chat-base
>>>>>>> 51f0e5eb3... apps: updated apps to use consistent %initial update
  $%  [%create =path]               ::  %create: create a mailbox at path
      [%delete =path]               ::  %delete: delete a mailbox at path
      [%message =path =envelope]    ::  %message: append a message to mailbox
      [%read =path]                 ::  %read: set mailbox to read
  ==
::
+$  action
  $%  ::  %messages: append a list of messages to mailbox
      ::
      [%messages =path envelopes=(list envelope)]
      diff
  ==
::
<<<<<<< HEAD
+$  update
  $%  [%keys keys=(set path)]
      [%config =path =config]
=======
+$  chat-update
  $%  [%initial =inbox]
      [%keys keys=(set path)]
>>>>>>> 51f0e5eb3... apps: updated apps to use consistent %initial update
      [%messages =path start=@ud end=@ud envelopes=(list envelope)]
      diff
  ==
--
