^?
|%
+$  rid  [in-group=? =ship =term]
++  path-to-rid
  |=  =path
  ^-  rid
  ~|  [%invalid-resource-id path]
  ?+  path          !!
      [%'~' @ @ ~]  [| (need (slaw %p i.t.path)) i.t.t.path]
      [@ @ ~]       [& (need (slaw %p i.path)) i.t.path]
  ==
::
++  rid-to-path
  |=  rid
  ^-  path
  %+  weld
    ?:(in-group ~ /~)
  /(scot %p ship)/[term]
--
