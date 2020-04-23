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
          %set-current-bucket  [%set-current-bucket s+bucket.upd]
          %add-bucket          [%add-bucket s+bucket.upd]
          %remove-bucket       [%remove-bucket s+bucket.upd]
          %set-endpoint        [%set-endpoint s+endpoint.upd]
          %set-access-key-id   [%set-access-key-id s+access-key-id.upd]
          %set-secret-access-key
        [%set-secret-access-key s+secret-access-key.upd]
      ::
          %credentials
        :-  %credentials
        %-  pairs:enjs
        :~  [%endpoint s+endpoint.credentials.upd]
            [%access-key-id s+access-key-id.credentials.upd]
            [%secret-access-key s+secret-access-key.credentials.upd]
        ==
      ::
          %configuration
        :-  %configuration
        %-  pairs:enjs
        :~  [%buckets a+(turn ~(tap in buckets.configuration.upd) |=(a=@t s+a))]
            [%current-bucket s+current-bucket.configuration.upd]
        ==
      ==
  ==
--
