::  /mar/collections/topic-full/hoon
::
/-  *collections
/+  time-to-id
::TODO factor out as lib/easy-form?
|%
++  form-edit
  |=  [tit=cord who=@p wat=tape]
  ;form(onsubmit "return easy_form.submit(this)")
    ;input(type "hidden", name "easy_form:mark", value "collections-action");
    ;input(type "hidden", name "easy_form:tag", value "resubmit");
    ;input(type "hidden", name "easy_form:url_end", value "collections/:col/:top");
    ::
    ;input(name "tit", value (trip tit));
    ;br;  ;b: {<who>}
    ;br;  ;textarea(name "wat"): {wat}
    ;br;  ;input(type "submit");
  ==
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
    ;br;  ;textarea(name "wat"): {txt}
    ;br;  ;input(type "submit");
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
    ;br;  ;textarea(name "wat", placeholder "Enter comment");
    ;br;  ;input(type "submit");
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
    =*  info  info.top
    =/  wat-info  (of-wall:format (turn wat.info trip))
    ::      
    ;div
      ;input.edit-toggle(type "checkbox", id "edit-post");
      ;label(for "edit-post"): ✎
      ;div.edit-on: +{(form-edit tit.info who.info wat-info)}
      ;h1.edit-off: {(trip tit.info)}
      ;b.edit-off: {<who.info>}
      ;pre.edit-off: {wat-info}
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
            ;div.edit-on: +{(form-comment-edit wen txt)}
            ;pre.edit-off: {txt}
            ::
            ; ;{b -<who.com>}, at ;{a/"#{id}" -<wen>}
            ::
            ;+  (form-comment-x wen)
            ;hr;
          ==
      ;h2: Post comment:
      ;+  form-comment-post
    ==
  ++  json
    =,  enjs:format
    %-  pairs
    :~  title+[%s tit.info.top]
        who+[%s (scot %p who.info.top)]
        what+[%s (of-wain:format wat.info.top)]
        :-  %comments
        %-  pairs
        %+  turn
          ~(tap by coms.top)
        |=  [a=@da b=[c=@da d=comment]]
        :-  (scot %da a)
        %-  pairs
        :~  who+[%s (scot %p who.d.b)]
            what+[%s (of-wain:format wat.d.b)]
        ==
    ==
  --
::
++  grab
  |%
  ++  noun  topicful
  --
--
