/-  lens
/+  *server
/=  lens-mark  /:  /===/mar/lens/command
                   /!noun/
=,  format
|%
:: +move: output effect
::
+$  move  [bone card]
:: +card: output effect payload
::
+$  card
  $%  [%connect wire binding:http-server term]
      [%http-response =http-event:http]
      [%peel wire dock mark path]
      [%poke wire dock poke]
      [%pull wire dock ~]
  ==
::
+$  poke
  $%  [%lens-command command:lens]
  ==
::
+$  state
  $%  $:  %0
          job=(unit [=bone com=command:lens])
      ==
  ==
::
--
::
|_  [bow=bowl:gall state=state]
::
++  this  .
::
++  prep
  |=  old=(unit *)
  ^-  (quip move _this)
  [~ this]
::
::  alerts us that we were bound. we need this because the vane calls back.
::
++  bound
  |=  [wir=wire success=? binding=binding:http-server]
  ^-  (quip move _this)
  [~ this]
::
++  poke-handle-http-request
  %-  (require-authorization:app ost.bow move this)
  |=  =inbound-request:http-server
  ^-  (quip move _this)
  ?^  job.state
    :_  this
    [ost.bow %http-response %start [%500 ~] ~ %.y]~
  ::
  =/  request-line  (parse-request-line url.request.inbound-request)
  =/  site  (flop site.request-line)
  ::
  =/  jon=json
    (need (de-json:html q:(need body.request.inbound-request)))
  =/  com=command:lens
    (json:grab:lens-mark jon)
  :_  this(job.state (some [ost.bow com]))
  [ost.bow %peel /sole [our.bow %dojo] %lens-json /sole]~
::
++  diff-lens-json
  |=  [=wire jon=json]
  ^-  (quip move _this)
  ?~  jon
    [~ this]
  ?>  ?=(^ job.state)
  :_  this(job.state ~)
  [bone.u.job.state %http-response (json-response:app (json-to-octs jon))]~
::
++  quit
  |=  =wire
  ^-  (quip move _this)
  ~&  [%quit wire]
  [~ this]
::
++  reap
  |=  [=wire saw=(unit tang)]
  ^-  (quip move _this)
  ~&  [%reap wire]
  ?^  saw
    [((slog u.saw) ~) this]
  ?>  ?=(^ job.state)
  :_  this
  :~  [ost.bow %poke /sole [our.bow %dojo] %lens-command com.u.job.state]
      [ost.bow %pull /sole [our.bow %dojo] ~]
  ==
::
++  coup
  |=  [=wire saw=(unit tang)]
  ^-  (quip move _this)
  ~&  [%coup wire]
  ?^  saw
    [((slog u.saw) ~) this]
  [~ this]
::
::  +poke-handle-http-cancel: received when a connection was killed
::
++  poke-handle-http-cancel
  |=  =inbound-request:http-server
  ^-  (quip move _this)
  ::  the only long lived connections we keep state about are the stream ones.
  ::
  [~ this]
::
++  poke-noun
  |=  a=*
  ^-  (quip move _this)
  ~&  poke+a
  [~ this]
::
--
