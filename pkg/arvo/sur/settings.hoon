|%
+$  settings  (map key bucket)
+$  bucket    (map key val)
+$  key       term
+$  val
  $%  [%s p=@t]
      [%b p=?]
      [%n p=@]
  ==
+$  event
  $%  [%put-bucket =key =bucket]
      [%del-bucket =key]
      [%put-entry buc=key =key =val]
      [%del-entry buc=key =key]
  ==
--
