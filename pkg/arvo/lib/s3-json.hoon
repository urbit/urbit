/-  *s3
|%
++  json-to-action
  |=  =json
  ^-  action
  |^  (parse-json json)
  ++  parse-json
    %-  of:dejs:format
    :~  [%set-endoint so:dejs:format]
        [%set-access-key-id so:dejs:format]
        [%set-secret-access-key so:dejs:format]
    ==
  --
::
++  update-to-json
  |=  upd=update
  ^-  json
  %+  frond:enjs:format  %s3-update
  %-  pairs:enjs:format
  :~  ?-  -.upd
          %set-endpoint       [%set-endpoint s+endpoint.upd]
          %set-access-key-id  [%set-access-key-id s+access-key-id.upd]
          %set-secret-access-key
        [%set-secret-access-key s+secret-access-key.upd]
      ::
          %credentials
        :-  %credentials
        %-  pairs:enjs:format
        :~  [%endpoint s+endpoint.credentials.upd]
            [%access-key-id s+access-key-id.credentials.upd]
            [%secret-access-key s+secret-access-key.credentials.upd]
        ==
      ==
  ==
--
