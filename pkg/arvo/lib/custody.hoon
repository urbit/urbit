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
  +$  physical-form   ?(%usb %dvd %paper)
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
    |=  =path
    ^-  (list vase)
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
    |=  =path
    ^-  (list vase)
    (turn (load-forms path) |=(=form !>(form)))
  --
::
++  policies  !!
++  galaxies  !!
++  ships     !!
--
