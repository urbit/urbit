/-  spider, claz
/+  *strand, strandio, azio, *ethereum, *azimuth
::
::NOTE  be sure to empty out the /migrations dir prior to re-running export
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
=/  ceremony=address
  0x740d.6d74.1711.163d.3fca.cecf.1f11.b867.9a7c.7964
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
      ~marnus
    ::
      ::  ceremony address
      :: ~hocdyt  ::  outgoing transfer
      :: ~fitdyt  ::  outgoing transfer
      ~fipbyt
      :: ~nimdyt  ::  outgoing transfer
      :: ~lardyt  ::  outgoing transfer
      :: ~waldyt  ::  outgoing transfer
      ~mipbyt
      :: ~rapdyt  ::  outgoing transfer
    ::
      ::NOTE  ~tonwet owned but is outgoing transfer
  ==
::
=/  known=(list @p)
  (weld saz (turn gax head))
::
~&  [%ecliptic ecliptic:mainnet-contracts]
::
^-  thread:spider
|=  args=vase
=+  !<([~ export=?] args)
~&  ?:(export %will-write-txs-to-disk %just-checking)
=/  m  (strand ,vase)
^-  form:m
=|  owned=(map address (list @p))  ::  owned points cache
=|  trapd=(map address (list @p))  ::  transferring for cache
=|  out=(jar address batch:claz)
::  handle galaxies and lockups
::
~&  %galaxies
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
  ~?  !=(0x0 transfer-proxy.deed)
    [%unexpected-transfer-proxy gal transfer-proxy.deed]
  ::  set spawn proxy only if needed, pick safe depending on spawn count
  ::
  ;<  count=@ud  bind:m
    (get-spawn-count:azimuth:az gal)
  =/  spawn-proxy=(unit address)
    =/  remaining  (sub 0xff count)
    ?:  =(0 remaining)  ~
    ?:  (lth remaining 50)
      `shallow-safe
    `deep-safe
  ::
  =.  out
    %+  ~(add ja out)  owner.deed
    ^-  batch:claz
    :-  %more
    =;  txs=(list (unit batch:claz))
      (murn txs same)
    :~  `[%single %set-management-proxy gal shallow-safe]
        `[%single %set-voting-proxy gal proxy-safe]
      ::
        ?~  spawn-proxy  ~
        `[%single %set-spawn-proxy gal u.spawn-proxy]
      ::
        `[%single %transfer-ship gal deep-safe]
    ==
  ::  if it controls a lockup, transfer that too
  ::
  ;<  =batch:linear:az  bind:m
    (batches:linear:az owner.deed)
  =/  transferring=?
    &(!=(0x0 approved.batch) !=(owner.deed approved.batch))
  ?:  =(0 amount.batch)
    ~?  loc  [%missing-lockup gal owner.deed]
    loop-gax(gax t.gax)
  ~?  &(!loc !transferring)  [%unexpected-lockup gal owner.deed amount.batch]
  ~?  &(!loc transferring)   [%unexpected-lockup-still-transfer gal owner.deed amount.batch approved.batch]
  ~?  &(loc transferring)    [%unexpected-lockup-transfer gal owner.deed amount.batch approved.batch]
  ::  only transfer the lockup if we expected it
  ::
  =?  out  loc
    %+  ~(add ja out)  owner.deed
    [%single %approve-batch-transfer (lockup-safe gal)]
  =?  out  loc
    %+  ~(add ja out)  (lockup-safe gal)
    [%single %transfer-batch owner.deed]
  ::  find other assets owned by this address
  ::
  ;<  others=(list @p)  bind:m
    =/  m  (strand ,(list @p))
    ?^  h=(~(get by owned) owner.deed)  (pure:m u.h)
    (get-owned-points:azimuth:az owner.deed)
  =.  owned  (~(put by owned) owner.deed others)
  =.  others  (skip others |=(=@p ?=(^ (find [p]~ known))))
  ~?  !=(~ others)
    [%has-others gal owner.deed others]
  ::  find other assets this address may transfer
  ::
  ;<  transferrable=(list @p)  bind:m
    =/  m  (strand ,(list @p))
    ?^  h=(~(get by trapd) owner.deed)  (pure:m u.h)
    (get-transferring-for:azimuth:az owner.deed)
  =.  trapd  (~(put by trapd) owner.deed transferrable)
  =.  transferrable  (skip transferrable |=(=@p ?=(^ (find [p]~ known))))
  ~?  !=(~ transferrable)
    [%has-transferrable gal owner.deed transferrable]
  ::
  loop-gax(gax t.gax)
::
::  handle stars
::
~&  %stars
|-
=*  loop-saz  $
?^  saz
  =*  star  i.saz
  ::  get the owner address, use it to config & transfer the star
  ::
  ;<  =deed:eth-noun  bind:m
    (rights:azimuth:az star)
  ~?  !=(0x0 transfer-proxy.deed)
    [%unexpected-transfer-proxy star transfer-proxy.deed]
  =.  out
    %+  ~(add ja out)  owner.deed
    ^-  batch:claz
    :-  %more
    :~  [%single %set-management-proxy star shallow-safe]
        [%single %set-spawn-proxy star proxy-safe]
        [%single %transfer-ship star deep-safe]
    ==
  ::  find other assets owned by this address
  ::
  ;<  others=(list @p)  bind:m
    =/  m  (strand ,(list @p))
    ?^  h=(~(get by owned) owner.deed)  (pure:m u.h)
    (get-owned-points:azimuth:az owner.deed)
  =.  owned  (~(put by owned) owner.deed others)
  =.  others  (skip others |=(=@p ?=(^ (find [p]~ known))))
  =/  [planets=(list @p) others=(list @p)]
    (skid others (cury lth 0xffff))
  ~?  !=(~ others)
    [%has-others star owner.deed others]
  ::  if controlling any planets, make sure shallow safe can get them out
  ::
  =?  out  !=(~ planets)
    ~&  [%has-planets star owner.deed (lent planets)]
    ~&  %approving-all-for-shallow-safe
    ::TODO  this might be done multiple times if this address owns multiple
    ::      stars. the solution is that the cache is dumb and we should just
    ::      skip processing any address in it.
    %+  ~(add ja out)  owner.deed
    :+  %custom  ecliptic:mainnet-contracts
    [0 'setApprovalForAll' ~[address+shallow-safe bool+&]]
  ::  find other assets this address may transfer
  ::
  ;<  transferrable=(list @p)  bind:m
    =/  m  (strand ,(list @p))
    ?^  h=(~(get by trapd) owner.deed)  (pure:m u.h)
    (get-transferring-for:azimuth:az owner.deed)
  =.  trapd  (~(put by trapd) owner.deed transferrable)
  =.  transferrable  (skip transferrable |=(=@p ?=(^ (find [p]~ known))))
  ~?  !=(~ transferrable)
    [%has-transferrable star owner.deed transferrable]
  ::
  loop-saz(saz t.saz)
::
::  ceremony address
::
;<  others=(list @p)  bind:m
  =/  m  (strand ,(list @p))
  ?^  h=(~(get by owned) ceremony)  (pure:m u.h)
  (get-owned-points:azimuth:az ceremony)
=.  others  (skip others |=(=@p ?=(^ (find [p]~ known))))
~?  !=(~ others)
  [%ceremony-controls-others others]
::
;<  transferrable=(list @p)  bind:m
  =/  m  (strand ,(list @p))
  ?^  h=(~(get by trapd) ceremony)  (pure:m u.h)
  (get-transferring-for:azimuth:az ceremony)
=.  transferrable  (skip transferrable |=(=@p ?=(^ (find [p]~ known))))
~?  !=(~ transferrable)
  [%ceremony-has-transferrable transferrable]
::
=.  out
  %+  ~(add ja out)  ceremony
  ^-  batch:claz
  :-  %more
  =,  mainnet-contracts
  :~  [%custom ecliptic 0 'transferOwnership' [%address deep-safe]~]
      [%custom linear-star-release 0 'transferOwnership' [%address shallow-safe]~]
      [%custom conditional-star-release 0 'transferOwnership' [%address deep-safe]~]
  ==
::
::  exporting
::
?.  export  (pure:m !>(~))
~&  [%generating address-count=~(wyt by out)]
::
=/  outs=(list [=address batches=(list batch:claz)])
  ~(tap by out)
|-
=*  loop-export  $
?~  outs
  ~&  %done
  (pure:m !>(~))
=,  i.outs
::
=/  file=path  /migration/(crip '0x' ((x-co:co 40) address))/eth-txs
::
;<  =bowl:spider  bind:m  get-bowl:strandio
;<  ~  bind:m
  %+  poke-our:strandio  %claz
  :-  %noun
  !>  ^-  command:claz
  ::NOTE  flop because +ja adds to list head
  =-  [%generate - %mainnet address %more (flop batches)]
  [(scot %p our.bowl) %home (scot %da now.bowl) file]
::  we must wait for claz to be done before proceeding to the next one
::
|-
=*  loop-check  $
;<  done=?  bind:m  (scry:strandio ? %cu %home file)
?.  done
  ;<  ~  bind:m  (sleep:strandio ~s1)
  loop-check
loop-export(outs t.outs)
