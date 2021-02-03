|%
+$  credentials
  $:  ::  oauth2 credentials. Modeled after gcloud's
      ::  application_default_credentials.json.
      ::  TODO: pull into oauth.hoon?
      ::
      client-id=@t
      client-secret=@t
      refresh-token=@t
      type=@t
  ==
::
::  TODO: pull this, and the same in s3.hoon, into
::        something like store.hoon?
::
+$  configuration
  $:  buckets=(set @t)
      current-bucket=@t
  ==
::
+$  action
  $%  [%set-client-id client-id=@t]
      [%set-client-secret client-secret=@t]
      [%set-refresh-token refresh-token=@t]
      [%set-type type=@t]
      ::  TODO: pull into store.hoon?
      [%add-bucket bucket=@t]
      [%remove-bucket bucket=@t]
      [%set-current-bucket bucket=@t]
  ==
::
+$  update
  $%  [%credentials =credentials]
      [%configuration =configuration]
      action
  ==
--
