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
      ;*  ?~  tops.col  ~
          ;=  ;h2: Topics:
          ==
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
        ;input(type "hidden", name "easy_form:url_end", value "collections/:col");
        ::
        ;input(name "tit", placeholder "Title");
        ;br;  ;textarea(name "wat", placeholder "Enter topic text");
        ;br;  ;input(type "submit");
      ==
      ::
      ;hr;
      ::
      ;form(onsubmit "return easy_form.submit(this)")
        ;input(type "hidden", name "easy_form:mark", value "collections-action");
        ;input(type "hidden", name "easy_form:tag", value "delete");
        ;input(type "hidden", name "easy_form:url_end", value "collections/:col");
        ;input(type "hidden", name "easy_form:confirm");
        ::
        ;input.red(type "submit", value "Archive this collection");
      ==
    ==
  ++  json
    =,  enjs:format
    %-  pairs
        :~  :-  %config
            %-  pairs
            :~  description+[%s desc.conf.col]
                public+[%b publ.conf.col]
                visible+[%b visi.conf.col]
                comments+[%b comm.conf.col]
                xenopost+[%b xeno.conf.col]
                :-  %except
                :-  %a
                %+  turn
                  ~(tap in mems.conf.col)
                |=  a/@p
                [%s (scot %p a)]
            ==
            :-  %topics
            :-  %a
            %+  turn
              ~(tap by tops.col)
            |=  [e=@da f=topicful]
            %-  pairs
            :~  title+[%s tit.info.f]
                who+[%s (scot %p who.info.f)]
                ::what+[%s (of-wain:format wat.info.f)]
                id+[%s (scot %da e)]
                modified+[%s (scot %da mod.info.f)]
            ==
        ==
  --
::
++  grab
  |%
  ++  noun  collection
  --
--
