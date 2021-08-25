::  multipart: multipart/form-data request decoding
::
|%
+$  part
  $:  file=(unit @t)                   ::  filename
      type=(unit mite)                 ::  content-type
      code=(unit @t)                   ::  content-transfer-encoding
      body=@t                          ::  content
  ==
::
++  de-request
  |=  [=header-list:http body=(unit octs)]
  ^-  (unit (list [@t part]))
  ?~  body  ~
  ?~  cot=(get-header:http 'content-type' header-list)      ~
  ?.  =('multipart/form-data; boundary=' (end 3^30 u.cot))  ~
  %+  rush  q.u.body
  (dep (rsh 3^30 u.cot))
::
++  dep
  |=  del=@t
  |^
  %+  knee    *(list [@t part])        |.  ~+
  ;~  pose  (cold ~ (full tip))        ::  end, or
  ;~  pfix  dim  nip                   ::  section start
  ;~  plug  ;~  plug                   ::  containing:
          (ifix [cof doq] nom)         ::  name
    (punt (ifix [cup doq] nod))        ::  filename
    (punt ;~(pfix nip cut nab))        ::  content-type
    (punt ;~(pfix nip cue nom))        ::  con-tra-encoding
          (ifix [sip nip] nag)         ::  content
  ==  ^$  ==  ==  ==
  ::
  ++  cof   (jest 'Content-Disposition: form-data; name="')
  ++  cue   (jest 'Content-Transfer-Encoding: ')
  ++  cup   (jest '; filename="')
  ++  cut   (jest 'Content-Type: ')
  ++  dim   (jest (cat 3 '--' del))
  ++  nip   (jest '\0d\0a')
  ++  nab   (more fas urs:ab)
  ++  nag   (dine ;~(less ;~(plug nip dim) next))
  ++  nod   (dine ;~(less doq next))
  ++  nom   (dine alp)
  ++  sip   ;~(plug nip nip)
  ++  tip   ;~(plug dim hep hep nip)
  ::
  ++  dine  |*(r=rule (cook (cury rep 3) (star r)))
  --
--
