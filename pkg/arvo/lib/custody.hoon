/+  csv
|%
++  fed  ;~(pfix sig fed:ag)
++  parse-enum
  |*  form=mold
  %-  sear  :_  text:csv
  |=  =cord
  ((soft form) cord)
::
++  keys
  =<  form
  |%
  +$  form
    $:  =location:locations
        ship=(unit ship)
        =role
        shard=(unit shard)
        copy=(unit copy)
        =physical-form
        note=cord
        formal-ship=(unit ship)
    ==
  ::
  +$  role
    $?  %empty
        %other
        %master
        %own
        %spawn
        %manage
        %vote
        %transfer
    ==
  ::
  +$  shard           @udshard
  +$  copy            ?(%a %b %c)
  +$  physical-form   ?(%usb %dvd %paper %ledger)
  ++  parsers
    :~  text:csv
        (punt fed)
        (parse-enum role)
        (punt dim:ag)
        (punt (parse-enum copy))
        (parse-enum physical-form)
        text:csv
        (punt fed)
    ==
  ::
  ++  parse
    %+  cook
      |*  arg=*
      ^-  (list form)
      arg
    (parse:csv parsers)
  ::
  ++  load-forms
    |=  =path
    ^-  (list form)
    (rash (of-wain:format .^(wain %cx path)) parse)
  ::
  ++  load
    |=  now=@da
    ^-  (list vase)
    =/  =path  /=home/(scot %da now)/keys/txt
    (turn (load-forms path) |=(=form !>(form)))
  --
::
++  locations
  =<  form
  |%
  +$  form
    $:  =location
        description=@t
        =physical-location
    ==
  ::
  +$  location           @tlocation
  +$  physical-location  @tphysicallocation
  ++  parsers
    :~  text:csv
        text:csv
        text:csv
    ==
  ::
  ++  parse
    %+  cook
      |*  arg=*
      ^-  (list form)
      arg
    (parse:csv parsers)
  ::
  ++  load-forms
    |=  =path
    ^-  (list form)
    (rash (of-wain:format .^(wain %cx path)) parse)
  ::
  ++  load
    |=  now=@da
    ^-  (list vase)
    =/  =path  /=home/(scot %da now)/locations/txt
    (turn (load-forms path) |=(=form !>(form)))
  --
::
++  galaxies
  =<  form
  |%
  +$  form
    $:  =ship
        =policy:policies
    ==
  ::
  ++  parsers
    :~  fed
        text:csv
    ==
  ::
  ++  parse
    %+  cook
      |*  arg=*
      ^-  (list form)
      arg
    (parse:csv parsers)
  ::
  ++  load-forms
    |=  =path
    ^-  (list form)
    (rash (of-wain:format .^(wain %cx path)) parse)
  ::
  ++  load
    |=  now=@da
    ^-  (list vase)
    =/  =path  /=home/(scot %da now)/galaxies/txt
    (turn (load-forms path) |=(=form !>(form)))
  --
::
++  ships
  =<  form
  |%
  +$  form
    $:  =ship
        note=@t
    ==
  ::
  ++  parsers
    :~  fed
        text:csv
    ==
  ::
  ++  parse
    %+  cook
      |*  arg=*
      ^-  (list form)
      arg
    (parse:csv parsers)
  ::
  ++  load-forms
    |=  =path
    ^-  (list form)
    (rash (of-wain:format .^(wain %cx path)) parse)
  ::
  ++  load
    |=  now=@da
    ^-  (list vase)
    =/  =path  /=home/(scot %da now)/ships/txt
    (turn (load-forms path) |=(=form !>(form)))
  --
::
++  policies
  =<  form
  |%
  +$  form
    $:  =policy
        =lockup
        provenance=@t
        description=@t
        usage=@t
    ==
  ::
  +$  policy  @tpolicy
  +$  lockup
    $?  %'1+4'
        %'0+2'
        %'0+1'
        %'0+3'
        %'0+1/3'
        %'0'
    ==
  ::
  ++  parsers
    :~  text:csv
        (parse-enum lockup)
        text:csv
        text:csv
        text:csv
    ==
  ::
  ++  parse
    %+  cook
      |*  arg=*
      ^-  (list form)
      arg
    (parse:csv parsers)
  ::
  ++  load-forms
    |=  =path
    ^-  (list form)
    (rash (of-wain:format .^(wain %cx path)) parse)
  ::
  ++  load
    |=  now=@da
    ^-  (list vase)
    =/  =path  /=home/(scot %da now)/policies/txt
    (turn (load-forms path) |=(=form !>(form)))
  --
--
