::
|%
::  Splits a path into the endpoint prefix and the remainder,
::  which is assumed to be a path within the JSON object.  We
::  choose the longest legal endpoint prefix.
::
++  split
  |=  pax/path
  ::  =-  ~&  [%pax pax - (valid-endpoint pax)]  -
  =+  l=(lent pax)
  |-  ^-  {path path}
  ?:  ?=(valid-get-endpoint (scag l pax))
    [(scag l pax) (slag l pax)]
  ?~  l
    ~&  %bad-endpoint
    ~|(%bad-endpoint !!)
  $(l (dec l))
::
::  These are all the github GET endpoints, sorted with
::  `env LC_ALL=C sort`
::
::  end-points include required query parameters
++  valid-get-endpoint
  $?  {$drafts id/@t $~}
      {$drafts $~}
      {$history $~}
      {$labels id/@t $~}
      {$labels $~}
      {$messages id/@t $attachments id/@t $~}
      {$messages id/@t $~}
      {$messages $~}
      {$profile $~}
      {$threads id/@t $~}
      {$threads $~}
  ==

++  vaild-post-endpoint
  $?  {$drafts $send $~}
      {$drafts $~}
      {$messages id/@t $modify $~}
      {$messages id/@t $trash $~}
      {$messages id/@t $untrash $~}
      {$messages $import $~}
      {$messages $send $~}
      {$messages $~}
      {$labels $~}
      {$threads id/@t $trash $~}
      {$threads id/@t $untrash $~}
      {$threads id/@t $modify}
      {$stop $~}
      {$watch $~}
  ==

++  valid-delete-endpoint
  $?  {$drafts id/@t $~}
      {$labels id/@t $~}
      {$messages id/@t $~}
      {$thread id/@t $~}
  ==
++  valid-put-endpoint
  $?  {$drafts id/@t $~}
      {$labels id/@t $~}
  ==
++  valid-patch-endpoint
  $?  {$labels id/@t $~}
  ==

--

::
