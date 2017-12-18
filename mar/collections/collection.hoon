::  /mar/collections/config/hoon
::
/-  *collections
|_  col=collection
::
++  grow
  |%
  ++  elem  :: web display
    ;div
      ;h1: {(trip desc.conf.col)}
      ;p: updated {<mod.conf.col>}
      ::TODO public, visible
      ::
      ;h2: Topics:
      ;ul
        ;*  %+  turn  (sort ~(tap by tops.col) dor)
            |=  [top=@da topicful]
            ;li
              ;a/"{<top>}/": {(trip tit.info)} ({<~(wyt by coms)>} comments)
            ==
      ==
      ::
      ;hr;
      ::
      ;h2: Post topic:
      ;script@"/lib/js/easy-form.js";
      ;form(onsubmit "return easy_form.submit(this)")
        ;input(type "hidden", name "easy_form:mark", value "collections-action");
        ;input(type "hidden", name "easy_form:tag", value "submit");
        ::
        ::FIXME we really want the filename here
        ;input(type "hidden", name "col", value "{<mod.conf.col>}");
        ::
        ;input(name "tit", placeholder "Title");
        ;br;
        ;textarea(name "wat", placeholder "Enter topic text");
        ;br;
        ;input(type "submit");
      ==
      ::
      ;hr;
      ::
      ;form(onsubmit "return easy_form.submit(this)")
        ;input(type "hidden", name "easy_form:mark", value "collections-action");
        ;input(type "hidden", name "easy_form:tag", value "delete");
        ;input(type "hidden", name "easy_form:confirm");
        ::
        ::FIXME we really want the filename here
        ;input(type "hidden", name "col", value "{<mod.conf.col>}");
        ::
        ;input.red(type "submit", value "Archive this collection");
      ==
    ==
  --
::
++  grab
  |%
  ++  noun  collection
  --
--
