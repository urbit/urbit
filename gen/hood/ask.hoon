::  Request a planet from Tlon Inc.
::
::::  /hoon/ask/hood/gen
  ::
/?    310
:-  %say
|=  {^ {mel/cord ~} ~}
=+  adr=(star ;~(less (mask "\"\\()[],:;<>@") prn))
=+  dom=[;~(plug dlab dot (most dot dlab))]:de-purl:html
=+  ~|(bad-email+mel (rash mel ;~((glue vat) adr dom)))
helm-send-ask+mel
