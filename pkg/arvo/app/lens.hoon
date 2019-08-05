/-  lens,   *sole
/+  base64, *server
/=  lens-mark  /:  /===/mar/lens/command
                   /!noun/
=,  format
|%
::  +lens-out: json or named octet-stream
::
+$  lens-out
  $%  [%json =json]
      [%mime =mime]
  ==
::  +move: output effect
::
+$  move  [bone card]
::  +card: output effect payload
::
+$  card
  $%  [%connect wire binding:eyre term]
      [%http-response =http-event:http]
      [%peer wire dock path]
      [%peer wire dock path]
      [%poke wire dock poke]
      [%pull wire dock ~]
  ==
::
+$  poke
  $%  [%lens-command command:lens]
      [%import *]
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
  |=  [wir=wire success=? binding=binding:eyre]
  ^-  (quip move _this)
  [~ this]
::
++  poke-handle-http-request
  %-  (require-authorization:app ost.bow move this)
  |=  =inbound-request:eyre
  ^-  (quip move _this)
  ::
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
  ::
  ?:  ?=(%export -.source.com)
    ~&  [%export app.source.com]
    :_  this(job.state (some [ost.bow com]))
    [ost.bow %peer /export [our.bow app.source.com] /export]~
  ::
  ?:  ?=(%import -.source.com)
    ?~  enc=(de:base64 base64-jam.source.com)
      :_  this
      [ost.bow %http-response %start [%500 ~] ~ %.y]~
    ::
    =/  c=*  (cue q.u.enc)
    ::
    :_  this(job.state (some [ost.bow com]))
    [ost.bow %poke /import [our.bow app.source.com] %import c]~
  ::
  :_  this(job.state (some [ost.bow com]))
  [ost.bow %peer /sole [our.bow %dojo] /sole]~
::
++  diff-sole-effect
  |=  [=wire fec=sole-effect]
  ^-  (quip move _this)
  =/  out
    |-  ^-  (unit lens-out)
    =*  loop  $
    ?+  -.fec
      ~
    ::
        %tan
      %-  some
      :-  %json
      %-  wall:enjs:format
      (turn (flop p.fec) |=(=tank ~(ram re tank)))
    ::
        %txt
      (some %json s+(crip p.fec))
    ::
        %sag
      %-  some
      [%mime p.fec (as-octs:mimes:html (jam q.fec))]
    ::
        %sav
      ::  XX use +en:base64 or produce %mime a la %sag
      ::
      %-  some
      :-  %json
      %-  pairs:enjs:format
      :~  file+s+(crip <`path`p.fec>)
          data+s+(crip (en-base64:mimes:html q.fec))
      ==
    ::
        %mor
      =/  all  `(list lens-out)`(murn p.fec |=(a=sole-effect loop(fec a)))
      ?~  all  ~
      ~|  [%multiple-effects all]
      ?>  ?=(~ t.all)
      (some i.all)
    ==
  ::
  ?~  out
    [~ this]
  ?>  ?=(^ job.state)
  :_  this(job.state ~)
  :_  ~
  :+  bone.u.job.state
    %http-response
  ?-  -.u.out
      %json
    (json-response:app (json-to-octs json.u.out))
  ::
      %mime
    :*  %start
        :~  200
            ['content-type' 'application/octet-stream']
            ?>  ?=([@ @ ~] p.mime.u.out)
            :-  'content-disposition'
            ^-  @t
            %^  cat  3
              'attachment; filename='
            (rap 3 '"' i.p.mime.u.out '.' i.t.p.mime.u.out '"' ~)
        ==
        (some q.mime.u.out)
        %.y
    ==
  ==
::
++  diff-export
  |=  [=wire data=*]
  ^-  (quip move _this)
  ::
  ?>  ?=(^ job.state)
  ::  herb will do whatever we tell it to, so by convention have it write to an
  ::  app name based on the file name.
  ::
  ?>  ?=(%export -.source.com.u.job.state)
  =/  app-name=tape  (trip app.source.com.u.job.state)
  =/  output=@t  (crip "/{app-name}/jam")
  ::
  =/  jon=json
    =/  =atom  (jam data)
    =/  =octs  [(met 3 atom) atom]
    =/  enc  (en:base64 octs)
    (pairs:enjs:format file+s+output data+s+enc ~)
  ::
  :_  this(job.state ~)
  :~  [bone.u.job.state %http-response (json-response:app (json-to-octs jon))]
      [ost.bow %pull /export [our.bow app.source.com.u.job.state] ~]
  ==
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
  ::
  ?:  =([%export ~] wire)
    [~ this]
  ::
  ?^  saw
    [((slog u.saw) ~) this]
  ?>  ?=(^ job.state)
  :_  this
  :~  [ost.bow %poke /sole [our.bow %dojo] %lens-command com.u.job.state]
      ::  XX move to +diff-sole-effect?
      ::
      [ost.bow %pull /sole [our.bow %dojo] ~]
  ==
::
++  coup
  |=  [=wire saw=(unit tang)]
  ^-  (quip move _this)
  ::
  ?:  =([%import ~] wire)
    ?>  ?=(^ job.state)
    :_  this(job.state ~)
    :_  ~
    :*  bone.u.job.state
        %http-response
        %start
        [%200 ~]
        [~ (as-octt:mimes:html "\"Imported data\"")]
        %.y
    ==
  ::
  ?^  saw
    [((slog u.saw) ~) this]
  [~ this]
::
::  +poke-handle-http-cancel: received when a connection was killed
::
++  poke-handle-http-cancel
  |=  =inbound-request:eyre
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
