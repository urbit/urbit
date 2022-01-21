/+  resource
^?
|%
+$  badge  term
+$  update
  (pair resource diff)
+$  diff 
  $%  [%add =datum]
      [%remove ~]
      [%perms =perms]
  ==
::  TODO: ban users, but what does that mean?
::    %write implies %comment
::  %read + %comment / dev-team %write
+$  policy
  ?(%read %write %comment %remove-posts)
+$  policies  (set policy)
+$  perms
  [default=policies juris=(map badge policies)]
+$  module
  ?(%chat %book %link)
+$  datum
  $:  title=cord
      description=cord
      date-created=time
      =module
      =perms
  ==
--

