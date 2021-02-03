::  gcp-store [landscape]:
::
::  stores GCP credentials for object storage. This is
::  "heavily influenced by" s3-store.hoon, and probably
::  should have the common logic extracted some day.
::
/-  *gcp
::  TODO?  /+  gcp-json
/+  default-agent, verb, dbug
~%  %gcp-top  ..part  ~
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
%+  verb  &  :: TODO: |
^-  agent:gall
~%  %gcp-agent-core  ..card  ~
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
  ~/  %gcp-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  |^
  ?>  (team:title our.bowl src.bowl)
  =^  cards  state
    ?+  mark         (on-poke:def mark vase)
        %gcp-action  (poke-action !<(action vase))
    ==
  [cards this]
  ::
  ++  poke-action
    |=  act=action
    ^-  (quip card _state)
    :-  [%give %fact [/all]~ %gcp-update !>(act)]~
    ?-  -.act
        %set-client-id
      state(client-id.credentials client-id.act)
    ::
        %set-client-secret
      state(client-secret.credentials client-secret.act)
    ::
        %set-refresh-token
      state(refresh-token.credentials refresh-token.act)
    ::
        %set-type
      state(type.credentials type.act)
    ::
        %set-current-bucket
      %_  state
          current-bucket.configuration  bucket.act
          buckets.configuration  (~(put in buckets.configuration) bucket.act)
      ==
    ::
        %add-bucket
      state(buckets.configuration (~(put in buckets.configuration) bucket.act)
    ::
        %remove-bucket
      state(buckets.configuration (~(del in buckets.configuration) bucket.act)
    ==
  --
::
++  on-watch
  ~/  %gcp-watch
  |=  =path
  ^-  (quip card _this)
  |^
  ?>  (team:title our.bowl src.bowl)
  =/  cards=(list card)
    ?+  path      (on-watch:def path)
        [%all ~]
      :~  (give %gcp-update !>([%credentials credentials]))
          (give %gcp-update !>([%configuration configuration]))
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
++  on-peek
  ~?  %gcp-peek
  |=  =path
  ^-  (unit (unit cage))
  ?.  (team:title our.bowl src.bowl)  ~
  ?+    path  [~ ~]
      [%x %credentials ~]
    [~ ~ %gcp-update !>(`update`[%credentials credentials])]
  ::
      [%x configuration ~]
    [~ ~ %gcp-update !>(`update`[%configuration configuration])]
  ==
++  on-agent  on-agent:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
