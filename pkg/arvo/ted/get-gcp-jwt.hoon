::  Thread that gets a JWT for use with Google Storage.
::
::  This thread expects settings-store to contain relevant fields from a GCP
::  service account JSON file, e.g. as poked by urbit/sh/poke-gcp-account-json.
::  Specifically, it depends on the `token_uri`, `client_email`,
::  `private_key_id`, and `private_key` fields. If these fields are not in
::  settings-store at the time the thread is run, it will fail.
::
::  The thread works by first constructing a self-signed JWT using the fields
::  in settings-store. Then, it sends this JWT to the specified token URI
::  (usually https://oauth2.googleapis.com/token), which gives us a JWT signed
::  by Google. This token can then be used as a bearer token for requests to
::  Google Storage.
::
::  The returned token has an expiration time of 60 minutes after the time at
::  which the thread was called.
::
::
/-  spider, settings
/+  jose, pkcs, primitive-rsa, strandio
=,  strand=strand:spider
=,  rsa=primitive-rsa
^-  thread:spider
|^
|=  *
=/  m  (strand ,vase)
^-  form:m
;<  =bowl:spider  bind:m  get-bowl:strandio
;<  iss=@t        bind:m  (read-setting %client-email)
;<  =key:rsa      bind:m  read-private-key
;<  kid=@t        bind:m  (read-setting %private-key-id)
;<  aud=@t        bind:m  (read-setting %token-uri)
=/  sot=@t
  %:  self-jwt
    key  kid  iss
    'https://www.googleapis.com/auth/cloud-platform'
    aud  now.bowl
  ==
;<  p=[tok=@t exp=@da]  bind:m  (sign-jwt sot aud)
(pure:m !>(p))
::
++  read-setting
  |=  key=term
  =/  m  (strand @t)  ^-  form:m
  ;<  has=?  bind:m
    (scry:strandio ? /gx/settings-store/has-entry/gcp-store/[key]/noun)
  ?.  has
    (strand-fail:strandio %no-setting key ~)
  ;<  =data:settings  bind:m
    %+  scry:strandio
      data:settings
    /gx/settings-store/entry/gcp-store/[key]/settings-data
  ?>  ?=([%entry %s @] data)
  (pure:m p.val.data)
::
++  read-private-key
  =/  m  (strand ,key:rsa)  ^-  form:m
  ;<  dat=@t  bind:m  (read-setting %private-key)
  %-  pure:m
  %.  dat
  ;:  cork
    to-wain:format
    ring:de:pem:pkcs8:pkcs
    need
  ==
::  construct and return a self-signed JWT issued now, expiring in ~h1.
::  TODO: maybe move this into lib/jose/hoon
::
++  self-jwt
  |=  [=key:rsa kid=@t iss=@t scope=@t aud=@t iat=@da]
  ^-  @t
  =/  job=json
    =,  enjs:format
    %^  sign:jws:jose  key
      ::  the JWT's "header"
      %:  pairs
        alg+s+'RS256'
        typ+s+'JWT'
        kid+s+kid
        ~
      ==
    ::  the JWT's "payload"
    %:  pairs
      iss+s+iss
      sub+s+iss                                 ::  per g.co, use iss for sub
      scope+s+scope
      aud+s+aud
      iat+(sect iat)
      exp+(sect (add iat ~h1))
      ~
    ==
  =/  [pod=@t pad=@t sig=@t]
    =,  dejs:format
    ((ot 'protected'^so 'payload'^so 'signature'^so ~) job)
  (rap 3 (join '.' `(list @t)`~[pod pad sig]))
::  RPC to get a signed JWT. Probably only works with Google.
::  Described at:
::  https://developers.google.com/identity/protocols/oauth2/service-account
::
++  sign-jwt
  |=  [jot=@t url=@t]
  =/  m  (strand ,[@t @da])  ^-  form:m
  ;<  ~  bind:m
    %:  send-request:strandio
      method=%'POST'
      url=url
      header-list=['Content-Type'^'application/json' ~]
      ^=  body
      %-  some  %-  as-octt:mimes:html
      %-  en-json:html
      %:  pairs:enjs:format
        ['grant_type' s+'urn:ietf:params:oauth:grant-type:jwt-bearer']
        assertion+s+jot
        ~
      ==
    ==
  ;<  rep=client-response:iris  bind:m
    take-client-response:strandio
  ?>  ?=(%finished -.rep)
  ?~  full-file.rep
    (strand-fail:strandio %no-response ~)
  =/  body=@t  q.data.u.full-file.rep
  =/  jon=(unit json)  (de-json:html body)
  ?~  jon
    (strand-fail:strandio %bad-body ~[body])
  =*  job  u.jon
  ~|  job
  =,  dejs:format
  =/  [typ=@t exp=@da tok=@t]
    ((ot 'token_type'^so 'expires_in'^du 'access_token'^so ~) job)
  ?>  =('Bearer' typ)
  %-  pure:m
  [tok exp]
--
