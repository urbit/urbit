::  /mar/collections/topic-full/hoon
::
/-  *collections
/+  time-to-id
|%
++  form-archive
  ;form(onsubmit "return easy_form.submit(this)")
    ;input(type "hidden", name "easy_form:mark", value "collections-action");
    ;input(type "hidden", name "easy_form:tag", value "delete-topic");
    ;input(type "hidden", name "easy_form:url_end", value "collections/:col/:top");
    ;input(type "hidden", name "easy_form:confirm");
    ::
    ;input.red(type "submit", value "Archive this topic");
  ==
++  form-comment-edit
  |=  [com=@da txt=tape]
  ;form.edit-on(onsubmit "return easy_form.submit(this)")
    ;input(type "hidden", name "easy_form:mark", value "collections-action");
    ;input(type "hidden", name "easy_form:tag", value "comment");
    ;input(type "hidden", name "easy_form:url_end", value "collections/:col/:top");
    ::
    ;input(type "hidden", name "com", value <com>);
    ::
    ;br;
    ;textarea(name "wat"): {txt}
    ;br;
    ;input(type "submit");
  ==
++  form-comment-x
  |=  com=@da
  ;form.inline(onsubmit "return easy_form.submit(this)")
    ;input(type "hidden", name "easy_form:mark", value "collections-action");
    ;input(type "hidden", name "easy_form:tag", value "delete-comment");
    ;input(type "hidden", name "easy_form:url_end", value "collections/:col/:top");
    ;input(type "hidden", name "easy_form:confirm");
    ::
    ;input(type "hidden", name "com", value <com>);
    ;input.red(type "submit", value "X");
  ==
++  form-comment-post
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
--
::
::::
  ::
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
      ::
      ;hr;
      ::
      ;script@"/lib/js/easy-form.js";
      ;+  form-archive
      ::
      ;hr;
      ::
      ;*  ?:  =(~ coms.top)  ~
          :-  ;h2: Comments
          %+  turn  (sort ~(tap by coms.top) dor)
          |=  [wen=@da mod=@da com=comment]
          =/  id  (time-to-id wen)
          =/  txt  (of-wall:format (turn wat.com trip))
          ;div(id id)
            ;input.edit-toggle(type "checkbox", id "edit-{id}");
            ;label(for "edit-{id}"): ✎
            ;pre.edit-off: {txt}
            ;div.edit-on: +{(form-comment-edit wen txt)}
            ; ;{b -<who.com>}, at ;{a/"#{id}" -<wen>}
            ::
            ;+  (form-comment-x wen)
            ;hr;
          ==
      ;h2: Post comment:
      ;+  form-comment-post
    ==
  --
::
++  grab
  |%
  ++  noun  topicful
  --
--
