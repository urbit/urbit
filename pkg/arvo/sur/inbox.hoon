|%
::
++  serial  @uvH                                        ::  unique identifier
::
+$  letter
  $%  [%text msg=cord]
      [%url msg=cord]
      [%code msg=cord]
  ==
::
+$  envelope                                            ::  letter container
  $:  uid=serial
      author=ship
      when=time
      =letter
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
