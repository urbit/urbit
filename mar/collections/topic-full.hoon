::  /mar/collections/topic-full/hoon
::
/-  *collections
/+  time-to-id
|_  top=topicful
::
::
++  grow
  |%
  ++  elem  :: web display
    ;div
      ;h1: {(trip tit.info.top)}
      ;b: {<who.info.top>}
      ;pre: {(of-wall:format (turn wat.info.top trip))}
      ;h2: Post comment:
      ;script@"/lib/js/easy-form.js";
      ;form(onsubmit "return easy_form.submit(this)")
        ;input(type "hidden", name "easy_form:mark", value "collections-action");
        ;input(type "hidden", name "easy_form:tag", value "comment");
        ;input(type "hidden", name "easy_form:url_end", value "collections/:col/:top");
        ::
        ;input(type "hidden", name "com", value "~");
        ::
        ;br;
        ;textarea(name "wat", placeholder "Enter comment");
        ;br;
        ;input(type "submit");
      ==
      ;*  ?:  =(~ coms.top)  ~
          :-  ;h2: Comments
          %+  turn  (sort ~(tap by coms.top) dor)
          |=  [wen=@da mod=@da com=comment]
          =/  id  (time-to-id wen) 
          ;div(id id)
            ;hr;
            ;pre: {(of-wall:format (turn wat.com trip))}
            ; ;{b -<who.com>}, at ;{a/"#{id}" -<wen>}
    ==    ==
  --
::
++  grab
  |%
  ++  noun  topicful
  --
--
