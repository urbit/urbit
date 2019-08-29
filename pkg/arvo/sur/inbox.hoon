|%
::
+$  envelope
  $:  author=ship
      when=time
      message=cord
  ==
::
+$  mailbox
  $:  envelopes=(list envelope)
      read=@
      owner=ship
  ==
::
+$  inbox-action
  $%  [%create =path owner=ship]
      [%delete =path]
      [%message =path =envelope]
      [%read =path read=@]
  ==
::
+$  inbox-update
  $%  [%keys keys=(set path)]
      inbox-action
  ==
+$  inbox-initial  (map path mailbox)
::
--
