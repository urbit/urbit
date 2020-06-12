/-  *s3
/+  s3-json, default-agent, verb, dbug
~%  %s3-top  ..is  ~
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state-zero
  ==
::
+$  state-zero  [%0 =credentials =configuration]
--
::
=|  state-zero
=*  state  -
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
~%  %s3-agent-core  ..card  ~
|_  =bowl:gall
+*  this       .
    def        ~(. (default-agent this %|) bowl)
::
++  on-init   on-init:def
++  on-save   !>(state)
++  on-load
  |=  old-vase=vase
  [~ this(state !<(state-zero old-vase))]
::
++  on-poke
  ~/  %s3-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  |^
  ?>  (team:title our.bowl src.bowl)
  =^  cards  state
    ?+  mark        (on-poke:def mark vase)
        %s3-action  (poke-action !<(action vase))
    ==
  [cards this]
  ::
  ++  poke-action
    |=  act=action
    ^-  (quip card _state)
    :-  [%give %fact [/all]~ %s3-update !>(act)]~
    ?-  -.act
        %set-endpoint
      state(endpoint.credentials endpoint.act)
    ::
        %set-access-key-id
      state(access-key-id.credentials access-key-id.act)
    ::
        %set-secret-access-key
      state(secret-access-key.credentials secret-access-key.act)
    ::
        %set-current-bucket
      %_  state
          current-bucket.configuration  bucket.act
          buckets.configuration  (~(put in buckets.configuration) bucket.act)
      ==
    ::
        %add-bucket
      state(buckets.configuration (~(put in buckets.configuration) bucket.act))
    ::
        %remove-bucket
      state(buckets.configuration (~(del in buckets.configuration) bucket.act))
    ==
  --
::
++  on-watch
  ~/  %s3-watch
  |=  =path
  ^-  (quip card _this)
  |^
  ?>  (team:title our.bowl src.bowl)
  =/  cards=(list card)
    ?+  path      (on-watch:def path)
        [%all ~]
      :~  (give %s3-update !>([%credentials credentials]))
          (give %s3-update !>([%configuration configuration]))
      ==
    ==
  [cards this]
  ::
  ++  give
    |=  =cage
    ^-  card
    [%give %fact ~ cage]
  --
::
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent  on-agent:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
