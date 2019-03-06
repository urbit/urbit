::
::::   /ren/collections/hoon
  ::
/?  309
/+  collections
::
::  collections:
::
::    get collection-config file at rendered path,
::    and all collections-item files  the subpath.
::    outputs a +collection defined in /lib/collections/hoon
::
::    recursive renderer, see its counterpart in /ren/collections/item/hoon
::
/=  collection
  /^  collection:collections
  /;  |=  [a=config:collections b=(map knot item:collections) ~]
          [a b]
      /.  /collections-config/
          /_  /collections-item/
      ==
collection
