/-  hall
|%
+$  mailbox
  $:  envelopes=(list envelope:hall)
      read=@
      owner=ship
  ==
::
+$  inbox-action
  $%  [%create =path owner=ship]
      [%delete =path]
      [%message =path =envelope:hall]
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
