/-  spider, claz
/+  *strand, azio, *ethereum, *azimuth
::
=/  az
  %~  .  azio
  :-  'https://mainnet.infura.io/v3/2599df54929b47099bda360958d75aaf'
  =>  mainnet-contracts
  :*  azimuth
      ecliptic
      linear-star-release
      delegated-sending
  ==
::
=/  deep-safe=address
  0x1111.1111.1111.1111.1111.1111.1111.1111.1111.1111
=/  lockup-safe=$-(@p address)
  |=  g=@p
  ::  0x2222.2222.2222.2222.2222.2222.2222.2222.2222.00xx
   `address`(rep 4 g (reap 9 0x2222))
=/  shallow-safe=address
  0x3333.3333.3333.3333.3333.3333.3333.3333.3333.3333
=/  proxy-safe=address
  0x4444.4444.4444.4444.4444.4444.4444.4444.4444.4444
::
=/  gax=(list [gal=@p own=? loc=?])
  :~  [~bus & |]
      [~def & |]
      [~dev & |]
      [~lur & |]
      [~pem & |]
      [~pub & |]
      [~sud & |]
      [~ten & |]
      [~zod & |]
      [~nus & |]
      [~feb & |]
      [~fet & |]
      [~nes & |]
      [~rud & |]
      [~rel & |]
      [~bec & &]
      [~bep & &]
      [~ber & &]
      [~byl & &]
      [~byr & &]
      [~byt & &]
      [~deb & &]
      [~dem & &]
      [~dun & |]
      [~dyt & &]
      [~fen & &]
      [~fur & &]
      [~fyl & &]
      [~hul & &]
      [~hus & &]
      [~hut & &]
      [~lec & &]
      [~len & &]
      [~lep & &]
      [~ler & &]
      [~lev & &]
      [~luc & &]
      [~lud & &]
      [~lyn & &]
      [~lyr & &]
      [~lys & &]
      [~mel & &]
      [~mex & &]
      [~mug & &]
      [~mun & &]
      [~mur & &]
      [~myl & &]
      [~ned & &]
      [~nel & &]
      [~ner & &]
      [~nev & &]
      [~nyd & &]
      [~nyl & &]
      [~nys & &]
      [~pec & &]
      [~pex & &]
      [~rem & &]
      [~ret & &]
      [~ryc & &]
      [~ryd & &]
      [~ryl & &]
      [~rym & &]
      [~seb & &]
      [~sed & &]
      [~sen & &]
      [~sug & &]
      [~syl & &]
      [~tec & &]
      [~teg & &]
      [~tel & &]
      [~tes & &]
      [~tuc & &]
      [~tun & &]
      [~tus | &]
      [~wed | &]
      [~weg | &]
      [~wer & &]
      [~bel & &]
      [~ben | &]
      [~bet | &]
      [~bex & &]
      [~deg & &]
      [~det & &]
      [~dus & &]
      [~dut & &]
      [~dux & &]
      [~dyl & &]
      [~fel & &]
      [~lyx & &]
      [~meb & &]
      [~mec & &]
      [~med & &]
      [~meg & &]
      [~mes & &]
      [~myn & &]
      [~myr & &]
      [~neb & &]
      [~nub & &]
      [~nux & &]
      [~pel & &]
      [~rec & &]
      [~ren & &]
      [~res & &]
      [~rev & &]
      [~rux & &]
      [~ryn & &]
      [~seg & &]
      [~set & &]
      [~sur & &]
      [~syp & &]
      [~ted & &]
      [~ter & &]
      [~tex & &]
      [~tud & &]
      [~tyn & &]
      [~wet & &]
      [~wyt & &]
  ==
::
=/  saz=(list @p)
  :~  ~marzod
      ~binzod
      ~samzod
      ~wanzod
      ~litzod
      ::TODO
  ==
::
::TODO  for each asset in gax:
::      - if we expect to own it:
::        - find ownership address,
::        - add configuration & transfer txs,
::        - check for lockup:
::          - print if unexpected,
::          - if we have it, transfer
::        - find other owned assets
::      - if we expect lockup, but don't expect to own it:
::        - ??? manually look up address maybe?
::
::TODO  for each asset in saz:
::      - find ownership address,
::      - configure, transfer txs
::
^-  thread:spider
|=  args=vase
=/  m  (strand ,vase)
^-  form:m
=|  out=(jar address batch:claz)
::  handle galaxies and lockups
::
|-
=*  loop-gax  $
?^  gax
  =,  i.gax
  ::  if we don't expect to own it, just no-op check for lockups
  ::
  ?.  own
    ;<  =deed:eth-noun  bind:m
      (rights:azimuth:az (rep 3 gal 1 ~))
    =/  lockup=?
      .=  owner.deed
      linear-star-release:mainnet-contracts
    ~?  &(loc lockup)
      [%need-manual-lockup-discovery gal]
    ~?  &(!loc lockup)
      [%make-sure-we-dont-own-lockup-for gal]
    loop-gax(gax t.gax)
  ::  get the owner address, use it to config & transfer the galaxy
  ::
  ;<  =deed:eth-noun  bind:m
    (rights:azimuth:az gal)
  =.  out
    %+  ~(add ja out)  owner.deed
    ^-  batch:claz
    :-  %more
    :~  [%single %set-management-proxy gal shallow-safe]
        [%single %set-voting-proxy gal proxy-safe]
        [%single %transfer-ship gal deep-safe]
    ==
  ::  if it controls a lockup, transfer that too
  ::
  ;<  =batch:linear:az  bind:m
    (batches:linear:az owner.deed)
  ?:  =(0 amount.batch)
    ~?  loc  [%missing-lockup gal owner.deed]
    loop-gax(gax t.gax)
  ~?  !loc  [%unexpected-lockup gal owner.deed amount.batch]
  =.  out
    %+  ~(add ja out)  owner.deed
    [%single %approve-batch-transfer (lockup-safe gal)]
  =.  out
    %+  ~(add ja out)  (lockup-safe gal)
    [%single %transfer-batch owner.deed]
  ::
  loop-gax(gax t.gax)
::
::NOTE  flop because +ja adds to list head
(pure:m !>((~(run by out) flop)))
