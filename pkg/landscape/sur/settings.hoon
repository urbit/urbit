/+  *mip
|%
::
++  settings-0  
  =<  settings
  |%
  +$  settings  (map key bucket)
  +$  bucket    (map key val)
  +$  val
    $%  [%s p=@t]
        [%b p=?]
        [%n p=@]
    ==
  --
::
++  settings-1
  =<  settings
  |%
  +$  settings  (map key bucket)
  --
+$  bucket    (map key val)
+$  key       term
+$  val
  $~  [%n 0]
  $%  [%s p=@t]
      [%b p=?]
      [%n p=@]
      [%a p=(list val)]
  ==
::
+$  settings  (mip desk key bucket) 
+$  event
  $%  [%put-bucket =desk =key =bucket]
      [%del-bucket =desk =key]
      [%put-entry =desk buc=key =key =val]
      [%del-entry =desk buc=key =key]
  ==
+$  data
  $%  [%all =settings]
      [%bucket =bucket]
      [%desk desk=(map key bucket)]
      [%entry =val]
  ==
--
