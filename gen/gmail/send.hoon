::  Send e-mail via gmail API
::
::::  /hoon/send/gmail/gen
  ::
/?    310
/-    rfc
:-  %say
|=  {^ {to/tape subject/tape opt/$@($~ {mess/tape $~})} _from="urbit-test@gmail.com"}
:-  %gmail-req
:^  %post  /messages/'send'  ~['uploadType'^'simple']
^-  message:rfc
=+  parse-adr=;~((glue pat) (cook crip (star ;~(less pat next))) (cook crip (star next)))
:+  (scan from parse-adr)
  (scan to parse-adr)
[(crip subject) ?~(opt '' (crip mess.opt))]
