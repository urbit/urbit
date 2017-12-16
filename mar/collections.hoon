::  /mar/collections/config/hoon
::
/-  *collections
|_  cos=collections
::
::
:: ++  get-bump
::   |=  col=collection  ^-  @da
::   =/  wen  mod.conf.col
::   %-  ~(rep by tops.col)
::   =<  .(wen ^wen)
::   |=  [[@da top=topicful] wen=@da]
::   %+  max  mod.info.top
::   %-  ~(rep by coms.top)
::   =<  .(wen ^wen)
::   |=  [[@da mod=@da ^] wen=@da]
::   (max mod wen)
::
++  grow
  |%
  ++  elem  :: web display
    ;div
      ;h1: Collections:
      ;ul
        ;*  %+  turn  (sort ~(tap by cos) dor)
            |=  [top=@da col=collection]
            ;li
              ;a/"{<top>}/":"{(trip desc.conf.col)}"
::               ;span: (updated {<(get-bump col)>})
            ==
      ==
      ;h2: Post new:
      ;script@"/lib/js/easy-form.js";
      ;form(onsubmit "return easy_form.submit(this)")
        ;input(type "hidden", name "easy_form:mark", value "collections-action");
        ;input(type "hidden", name "easy_form:tag", value "create");
        ::
        ;input(name "desc", placeholder "Description");
        ;br;
        ;select(name "kind"):(option:"blog" option:"fora" option:"note")
        ;input(name "visi", type "checkbox", checked ""): Visible
        ;input(name "publ", type "checkbox"): Public
        ;br;
        ;input(name "mems", placeholder "Exception list");
        ;br;
        ;input(type "submit");
      ==
    ==
  --
::
++  grab
  |%
  ++  noun  collections
  --
--
