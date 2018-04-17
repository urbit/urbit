/-  constitution, ethereum
/+  ethereum
=>  [^constitution ethereum]
|%
++  event-log-to-hull-diffs
  ::=,  ships-events
  =>  [ships-events .]
  |=  log=event-log
  ^-  (list (pair ship diff-hull))
  ~?  ?=(~ mined.log)  %processing-unmined-event
  ::
  ?:  =(event.log transferred)
    =+  ^-  [who=@ wer=address]  ::TODO  should we make @p work here?
        (decode-results data.log ~[%uint %address])
    [who %owner wer]~
  ::
  ?:  =(event.log activated)
    =+  ^-  [who=@ wer=address]
        (decode-results data.log ~[%uint %address])
    :~  ^-  (pair ship diff-hull)
        :+  who  %full
        %*(. *hull owner wer, sponsor (sein:title who))
      ::
        ^-  (pair ship diff-hull)
        [(sein:title who) %spawned who]
    ==
  ::
  ?:  =(event.log escape-requested)
    =+  ^-  [who=@ wer=@]
        (decode-results data.log ~[%uint %uint])
    [who %escape `wer]~
  ::
  ?:  =(event.log escape-canceled)
    =/  who=@  (decode-results data.log ~[%uint])
    [who %escape ~]~
  ::
  ?:  =(event.log escape-accepted)
    =+  ^-  [who=@ wer=@]
        (decode-results data.log ~[%uint %uint])
    [who %sponsor wer]~
  ::
  ?:  =(event.log changed-keys)
    =+  ^-  [who=@ enc=octs aut=octs rev=@ud]
        %+  decode-results  data.log
        ~[%uint [%bytes-n 32] [%bytes-n 32] %uint]
    ?>  &(=(p.enc 32) =(p.aut 32))  ::  sanity
    [who %keys q.enc q.aut rev]~
  ::
  ?:  =(event.log changed-spawn-proxy)
    =+  ^-  [who=@ sox=address]
        (decode-results data.log ~[%uint %address])
    [who %spawn-proxy sox]~
  ::
  ?:  =(event.log changed-transfer-proxy)
    =+  ^-  [who=@ tox=address]
        (decode-results data.log ~[%uint %address])
    [who %transfer-proxy tox]~
  ::
  ::NOTE  0x8be0...57e0 is Owneable's OwnershipTransferred(address,address).
  ::      changed-dns is handled separately since it doesn't affect hulls.
  ~&  [%unimplemented-event event.log]
  ~
::
++  apply-hull-diff
  |=  [hul=hull dif=diff-hull]
  ^-  hull
  ?-  -.dif
    %full     new.dif
    %owner    hul(owner new.dif)
    %spawned  =+  (~(put in spawned.hul) who.dif)
              hul(spawn-count +(spawn-count.hul), spawned -)
    %keys     hul(encryption-key enc.dif, authentication-key aut.dif)
    %sponsor  hul(sponsor new.dif, escape ~)
    %escape   hul(escape new.dif)
    %spawn-proxy      hul(spawn-proxy new.dif)
    %transfer-proxy   hul(transfer-proxy new.dif)
  ==
::
++  parse-id
  |=  id=@t
  ^-  ships:function
  |^
    %+  rash  id
    ;~  pose
      (function %ships 'ships' shipname)
      (function %get-spawned 'getSpawned' shipname)
      (function %dns-domains 'dnsDomain' dem:ag)
    ==
  ::
  ++  function
    |*  [tag=@tas fun=@t rul=rule]
    ;~(plug (cold tag (jest fun)) (ifix [pel per] rul))
  ::
  ++  shipname
    ;~(pfix sig fed:ag)
  --
--
