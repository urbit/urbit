=<  column
|%
+$  id  @uvH
+$  ref 
  $%  [%feed =ship]
      [%twit id=cord]
  ==
::
+$  column
  $:  =id
      =ref
      reversed=_|
      replies=_|
      hist=(list ref)
  ==
::
+$  diff
  $%  [%push-ref p=ref]
      [%pop-ref ~]
      [%replies p=?]
      [%reversed p=?]
  ==
--
      
      
