|%
::
++  serial  @uvH
::
+$  letter
  $%  [%text text=cord]
      [%url url=cord]
      [%code expression=cord output=(list tank)]
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
  $:  owner=ship
      length=@
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
+$  chat-configs  (map path config)
::
+$  chat-action
  $%  [%create =path owner=ship]
      [%delete =path]
      [%message =path =envelope]
      [%read =path]
  ==
::
+$  chat-update
  $%  [%keys keys=(set path)]
      [%config =path =config]
      chat-action
  ==
::
--
