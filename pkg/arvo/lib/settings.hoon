/-  *settings
|%
++  enjs
  =,  enjs:format
  |%
  ++  event
    |=  evt=^event
    ^-  json
    %+  frond  -.evt
    ?-  -.evt
      %put-bucket  (put-bucket +.evt)
      %del-bucket  (del-bucket +.evt)
      %put-entry   (put-entry +.evt)
      %del-entry   (del-entry +.evt)
    ==
  ::
  ++  put-bucket
    |=  [k=key b=^bucket]
    ^-  json
    %-  pairs
    :~  bucket-key+s+k
        bucket+(bucket b)
    ==
  ::
  ++  del-bucket
    |=  k=key
    ^-  json
    %-  pairs
    :~  bucket-key+s+k
    ==
  ::
  ++  put-entry
    |=  [b=key k=key v=val]
    ^-  json
    %-  pairs
    :~  bucket-key+s+b
        entry-key+s+k
        value+(val v)
    ==
  ::
  ++  del-entry
    |=  [buc=key =key]
    ^-  json
    %-  pairs
    :~  bucket-key+s+key
        entry-key+s+key
    ==
  ::
  ++  value
    |=  =val
    ^-  json
    ?-  -.val
      %s  val
      %b  val
      %n  (numb p.val)
    ==
  ::
  ++  bucket
    |=  b=^bucket
    ^-  json
    [%o (~(run by b) value)]
  --
::
++  dejs
  =,  dejs:format
  |%
  ++  event
    |=  jon=json
    ^-  ^event
    %.  jon
    %-  of
    :~  put-bucket+put-bucket
        del-bucket+del-bucket
        put-entry+put-entry
        del-entry+del-entry
    ==
  ::
  ++  put-bucket
    %-  ot
    :~  bucket-key+so
        bucket+bucket
    ==
  ::
  ++  del-bucket
    %-  ot
    :~  bucket-key+so
    ==
  ::
  ++  put-entry
    %-  ot
    :~  bucket-key+so
        entry-key+so
        value+val
    ==
  ::
  ++  del-entry
    %-  ot
    :~  bucket-key+so
        entry-key+so
    ==
  ::
  ++  value
    |=  jon=json
    ^-  val
    ?+  -.jon  !!
      %s  jon
      %b  jon
      %n  [%n (rash p.jon dem)]
    ==
  ::
  ++  bucket
    |=  jon=json
    ^-  ^bucket
    ?>  ?=([%o *] jon)
    (~(run by p.jon) value)
  --
--
