::    this mark is used to receive incoming oauth2 refresh#access tokens
::
::::  /hoon#oauth-tokens#mar
  ::
/?  310
!:
|_  {tok/@t ref/@t}
::
++  grab                          ::  converter arm
  |%
  ++  noun  {@t @t}               ::  clam from noun
  ++  json  (corl need (ot 'access_token'^so 'refresh_token'^so ~):jo)
  --
--

