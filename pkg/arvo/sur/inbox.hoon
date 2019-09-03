|%
::
++  serial  @uvH                                        ::  unique identifier
::
+$  envelope                                            ::  message container
  $:  uid=serial
      author=ship
      when=time
      message=cord
  ==
::
+$  mailbox                                             ::  envelopes + configs
  $:  envelopes=(list envelope)
      read=@
      owner=ship
  ==
::
+$  inbox  (map path mailbox)
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
::
--
