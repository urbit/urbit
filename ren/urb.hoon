::
::::  /hoon/urb/ren
  ::
/?    310
/+    urb-split    :: for single-page apps
/%    /^  {hed/{@uvH marl} bod/{@uvH marl}}
      /,      /web/pages
          /;  urb-split  /#  /!hymn/
              /web/static
          ::
          ::  TODO: remove once we no longer need static site generator
          ::
          /;  |=  urb=manx
              ^-  [hed=[@uvH marl] bod=[@uvH marl]]
              ?:  ?=({{$html $~} {{$head $~} *} {{$body $~} *} $~} urb)
                =+  `{{$html $~} {{$head $~} hed/marl} {{$body $~} bod/marl} $~}`urb
                :-  [0v0 hed]
                    [0v0 bod]
              :-  [0v0 ~]
                  [0v0 [urb ~]]
          /&hymn&/html/
          ::
              /
          /urb-tree/
      ==
-.-
