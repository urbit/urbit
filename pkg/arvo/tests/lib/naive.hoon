/+  *test, naive, ethereum
|%
++  dumver
  ^-  ^verifier:naive
  |=  [dat=@ v=@ r=@ s=@]
  =,  secp256k1:secp:crypto
  %-  address-from-pub:key:ethereum
  %-  serialize-point
  (ecdsa-raw-recover dat v r s)
--
::
|%
++  test-log
  %+  expect-eq
    !>
    `[[[~bud %*(. *point:naive dominion %l1, owner.own 0x123)] ~ ~] ~ ~]
  ::
    !>
    %^  naive  dumver  *^state:naive
    :*  %log  ~  *@ux  *@t
        (hash-log-name:naive 'OwnerChanged(uint32,address)')  (@ux ~bud)  0x123  ~
    ==
--
