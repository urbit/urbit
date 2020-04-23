/-  *s3
|%
++  json-to-action
  |=  =json
  ^-  action
  |^  (parse-json json)
  ++  parse-json  (of:dejs:format [%set-api-key so:dejs:format]~)
  --
::
++  update-to-json
  |=  upd=update
  ^-  json
  %+  frond:enjs:format  %s3-update
  %-  pairs:enjs:format
  :~  ?-  -.upd
          %credentials
        [%credentials (pairs:enjs:format [%api-key s+api-key.credentials.upd]~)]
          %set-api-key
        [%set-api-key (pairs:enjs:format [%api-key s+api-key.upd]~)]
      ==
  ==
--
