::
::::  /ren/collections/item/hoon
  ::
/?  309
/+  collections
::
::  item: 
::
::    render a collection-item at this path 
::    outputs a +item defined in /lib/collections/hoon
::
::    recursive renderer, see its counterpart in /ren/collections/hoon
::
/=  item
  /^  item:collections
  ::
  ::  run a gate which checks if the output of the renderers below are null or not,
  ::  crash in the case that both are null
  ::  tag them with %collection, %raw, or %both for the 3 remaining permissible cases,
  ::
  /;  |=  $:  raw=?(~ raw-item:collections)
              col=?(~ collection:collections)
              ~
          ==
      ?~  raw
        ?~  col
          [%error ~]
        [%collection col]
      ?~  col
        [%raw raw]
      [%both col raw]
  ::
  ::  run a pair of renderers
  ::
  ::  1. get a .umd file together with its frontmatter, or else return ~
  ::
  ::  2. run the collections renderer, if it fails return ~
  ::     (it fails if .collections-config file does not exist at this path)
  ::
  /.
    ::
    /|  /;  |=  [a=(map knot cord) b=@t ~]
            [%umd a b]
            /.  /front/
                /umd/
            ==
        /~  ~
    ==
    ::
    /|  /collections/
        /~  ~
    ==
    ::
  ==
::
item
