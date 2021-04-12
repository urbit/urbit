|%
+$  settings-0  (map key bucket-0)
+$  bucket-0    (map key val-0)
+$  val-0
  $%  [%s p=@t]
      [%b p=?]
      [%n p=@]
  ==
::
+$  settings  (map key bucket)
+$  bucket    (map key val)
+$  key       term
+$  val
  $~  [%n 0]
  $%  [%s p=@t]
      [%b p=?]
      [%n p=@]
      [%a p=(list val)]
  ==
+$  event
  $%  [%put-bucket =key =bucket]
      [%del-bucket =key]
      [%put-entry buc=key =key =val]
      [%del-entry buc=key =key]
  ==
+$  data
  $%  [%all =settings]
      [%bucket =bucket]
      [%entry =val]
  ==
--
