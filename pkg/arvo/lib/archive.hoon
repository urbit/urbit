/-  *archive
|%
::
++  export-app
  |=  [app=@tas our=@p now=@da]
  .^(* %gx /(scot %p our)/[app]/(scot %da now)/export/noun)
::
++  export-all
  |=  [our=@p now=@da]
  ^-  archive-0
  :*  %0
      group-store+(export-app %group-store our now)
      metadata-store+(export-app %metadata-store our now)
      contact-store+(export-app %contact-store our now)
      invite-store+(export-app %invite-store our now)
      graph-store+(export-app %graph-store our now)
      settings-store+(export-app %settings-store our now)
  ==
::
::  +refine-archive: type raw noun as an archive, and migrate it to the latest
::                   archive version
::
++  refine-archive
  |=  arc-raw=*
  ^-  archive-0
  |^
  ?@  -.arc-raw
    =/  arc  ;;(versioned-archive arc-raw)
    (migrate arc)
  =/  arc  ;;(pre-versioned-archive arc-raw)
  =/  new-arc=versioned-archive
    :*  %0
        group-store.arc
        metadata-store.arc
        contact-store.arc
        invite-store.arc
        graph-store.arc
        [%settings-store %0 ~]
    ==
  (migrate new-arc)
  ::
  ++  migrate
    |=  arc=versioned-archive
    ^-  archive-0
    ?-  -.arc
      %0  arc
    ==
  --
--
