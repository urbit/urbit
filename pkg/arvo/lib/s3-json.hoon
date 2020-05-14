/-  *s3
|%
++  json-to-action
  |=  =json
  ^-  action
  =,  format
  |^  (parse-json json)
  ++  parse-json
    %-  of:dejs
    :~  [%set-endpoint so:dejs]
        [%set-access-key-id so:dejs]
        [%set-secret-access-key so:dejs]
        [%add-bucket so:dejs]
        [%remove-bucket so:dejs]
        [%set-current-bucket so:dejs]
    ==
  --
::
++  update-to-json
  |=  upd=update
  ^-  json
  =,  format
  %+  frond:enjs  %s3-update
  %-  pairs:enjs
  :~  ?-  -.upd
          %set-current-bucket  [%'setCurrentBucket' s+bucket.upd]
          %add-bucket          [%'addBucket' s+bucket.upd]
          %remove-bucket       [%'removeBucket' s+bucket.upd]
          %set-endpoint        [%'setEndpoint' s+endpoint.upd]
          %set-access-key-id   [%'setAccessKeyId' s+access-key-id.upd]
          %set-secret-access-key
        [%'setSecretAccessKey' s+secret-access-key.upd]
      ::
          %credentials
        :-  %credentials
        %-  pairs:enjs
        :~  [%endpoint s+endpoint.credentials.upd]
            [%'accessKeyId' s+access-key-id.credentials.upd]
            [%'secretAccessKey' s+secret-access-key.credentials.upd]
        ==
      ::
          %configuration
        :-  %configuration
        %-  pairs:enjs
        :~  [%buckets a+(turn ~(tap in buckets.configuration.upd) |=(a=@t s+a))]
            [%'currentBucket' s+current-bucket.configuration.upd]
        ==
      ==
  ==
--
