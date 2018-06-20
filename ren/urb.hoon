::::  /hoon/urb/ren
  ::
/?    310
/-    urb
/+    urb-split,    :: for single-page apps
      nutalk        ::FIXME write ren/urb/nutalk
/%    /^  {hed/{@uvH marl} bod/{@uvH marl}}
      /,      /web/pages/nutalk
          /;  nutalk  /#  /!htm/
      ::
              /web/pages
          /;  urb-split  /#  /!hymn/
      ::
              /web/collections
          :: put collections through the same .htm
          :: routing structure as nutalk
          /;  nutalk  /#  /htm/  ::a lot of stuff in here isn't .hoon files
      ::
              /
          /urb-tree/
      ==
-.-
