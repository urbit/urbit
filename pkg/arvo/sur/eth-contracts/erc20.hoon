/-  ethers
=/  builders  builders:ethers
|%
  +$  gift
    $%  [$history =loglist]
        [$log event-log=event-update]
        [$read-call read-call]
        [$read-tx read-tx]
    ==
  +$  poke
    $%  (make-action:builders $call call)
        (make-action:builders $send-tx send-tx)
        [$event-subscribe =path config=(watch-config:builders watch-config)]
    ==
  +$  watch-config
    %-  watch-config:builders
    event-subscribe
  +$  loglist  (list (event-log-config:builders event-subscribe))
  +$  call
    $%  [$allowance owner=@ux spender=@ux]
        [$balance-of account=@ux]
        [$total-supply ~]

    ==
  +$  send-tx
    $%  [$transfer-from sender=@ux recipient=@ux amount=@ud]
        [$approve spender=@ux amount=@ud]
        [$decrease-allowance spender=@ux subtracted-value=@ud]
        [$increase-allowance spender=@ux added-value=@ud]
        [$transfer recipient=@ux amount=@ud]

    ==
  +$  read-call
    $%  [$allowance out=@ud]
        [$balance-of out=@ud]
        [$total-supply out=@ud]

    ==
  +$  read-tx
    $%  [$transfer-from out=?]
        [$approve out=?]
        [$decrease-allowance out=?]
        [$increase-allowance out=?]
        [$transfer out=?]

    ==
  +$  event-update
    $%  [$approval owner=@ux spender=@ux value=@ud]
        [$transfer from=@ux to=@ux value=@ud]

    ==
  +$  event-subscribe
    $%  [$approval owner=?(@ux (list @ux)) spender=?(@ux (list @ux)) ~]
        [$transfer from=?(@ux (list @ux)) to=?(@ux (list @ux)) ~]

    ==
--