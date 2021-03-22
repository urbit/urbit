::  Tells whether GCP Storage appears to be configured.
::
::  Thread since it needs to be called from Landscape.
::
::
/-  gcp, spider, settings
/+  strandio
=,  strand=strand:spider
=,  enjs:format
^-  thread:spider
|^
|=  *
=/  m  (strand ,vase)
^-  form:m
;<  has=?  bind:m
  %:  has-settings
    %client-email
    %private-key
    %private-key-id
    %token-uri
    ~
  ==
%-  pure:m
!>
%+  frond  %gcp-configured
b+has
::
++  has-settings
  |=  set=(list @tas)
  =/  m  (strand ?)
  ^-  form:m
  ?~  set
    (pure:m %.y)
  ;<  has=?  bind:m  (has-setting i.set)
  ?.  has
    (pure:m %.n)
  ;<  has=?  bind:m  (has-settings t.set)
  (pure:m has)
::
++  has-setting
  |=  key=@tas
  =/  m  (strand ?)
  ^-  form:m
  ;<  has=?  bind:m
    %+  scry:strandio  ?
    /gx/settings-store/has-entry/gcp-store/[key]/noun
  (pure:m has)
::
--
