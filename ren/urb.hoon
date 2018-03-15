::::  /hoon/urb/ren
  ::
/?    310
/+    urb-split,    :: for single-page apps
      nutalk        ::FIXME write ren/urb/nutalk
/%    /^  {hed/{@uvH marl} bod/{@uvH marl}}
      /,      /web/pages/nutalk
          /;  urb-split  /#  /;  nutalk  /!htm/
              /web/pages
          /;  urb-split  /#  /!hymn/
              /web/collections
          :: put collections through the same .htm
          :: routing structure as nutalk
          :: this is basically the same code as urb-split, just eliminates the dependency
          /;  |=  {dep/@uvH urb/manx}  ^-  {hed/{@uvh marl} bod/{@uvH marl}}
              ~|  [%malformed-urb urb]  :: XX types
              ?>  ?=({{$html $~} {{$head $~} *} {{$body $~} *} $~} urb)
              =+  `{{$html $~} {{$head $~} hed/marl} {{$body $~} bod/marl} $~}`urb
              :-  [0v0 hed]                         
              [0v0 bod]
              /#  /;  nutalk  /|(/!htm/ /htm/)  ::a lot of stuff in here isn't .hoon files
              /
          /urb-tree/
      ==
-.-
