=,  eyre
|%
:: sort knots by date
:: TODO when we change the write path to have trailing sig, remove here before sort
++  dor
  |=  [a=knot b=knot]
  (gth (unt:chrono:userlib (slav %da a)) (unt:chrono:userlib (slav %da b)))
:: checks if authorized
++  authed
  |=  gas/epic
  ^-  ?
  %+  lien
    ~(tap in (~(get ju aut.ced.gas) %$))
  |=  b/knot
  =((scot %p p.bem.gas) b)
++  no-title
  |=  wat/wain
  ^-  wain
  ?:  =((scag 2 (trip -:wat)) "# ")
    +:wat
  wat
++  esoo
  |=  d/@d
  ^-  tape
  =/  t  (yore d)
  ;:  welp
      (scag 1 (scow %ud y.t))
      (swag [2 3] (scow %ud y.t))
      "-"
      (double m.t)
      "-"
      (double d.t.t)
      "T"
      (double h.t.t)
      ":"
      (double m.t.t)
      ":"
      (double s.t.t)
      "Z"
  ==
:: ud to leading zero tape
++  double
  |=  a/@ud
  ^-  tape
  =/  x  (scow %ud a)
  ?:  (lth a 10)
    (welp "0" x)
  x
:: takes a map of knot * where knot is a serialized @da and returns the newest
++  latest
  |*  a/(map knot *)
  ^-  (pair knot *)
  =/  sa
  %+  sort
    ~(tap by a)
  |=  [b=(pair knot *) c=(pair knot *)]
  (dor p.b p.c)
  ?~  sa
    *(pair knot *)
  i.sa
++  latest-post
'''
/-  collections
/+  colls
/=  gas  /$  fuel:html
/=  configs  /:  /===/web/collections
             /^  (map knot config:collections)  /_  /collections-config/
:: tried to pull this func into a lib, but couldn't get the gill working correctly. grr.
/=  metawcom  /;  |=  a/(map knot topicful:collections)
               ^-  (pair knot topicful:collections)
               =/  sa
               %+  sort
                 ~(tap by a)
               |=  [b=(pair knot *) c=(pair knot *)]
               (gth (unt:chrono:userlib (slav %da p.b)) (unt:chrono:userlib (slav %da p.c)))
               ?~  sa
                 *(pair knot topicful:collections)
               i.sa
           /:  /%%/
           /^  (map knot topicful:collections)  /_  /collections-topic-full/
/=  content  /;  |=  a/(map knot manx)
               ^-  (pair knot manx)
               =/  sa
               %+  sort
                 ~(tap by a)
               |=  [b=(pair knot *) c=(pair knot *)]
               (gth (unt:chrono:userlib (slav %da p.b)) (unt:chrono:userlib (slav %da p.c)))
               ?~  sa
                 *(pair knot manx)
               i.sa
             /:  /%%/
             /^  (map knot manx)  /_
             /&elem&md&/collections-topic/
=/  config  (~(get by configs) +<:s.bem.gas)
~&  metawcom
::
^-  manx
;div.container
  ;div.row
    ;input(type "hidden", name "urb-header", value "collection-index", title "{(trip desc:(need config))}", id "{(trip +<:s.bem.gas)}", ship "{(scow %p p.bem.gas)}");
    ;div.col-sm-10.col-sm-offset-2
      ;div.post.collection-post-page
        ;div.row.collection-date
          ;span.mr-2.text-black.text-500(urb-component "Elapsed", urb-timestring "{(esoo:colls mod.info.q.metawcom)}");
          ;span: {(trip -:s.bem.gas)}
        ==
        ::
        ;div#show
          ;div.row.tit.mt-6.collection-title
            ;h3: {(trip tit.info.q.metawcom)}
          ==
          ;*  ?:  (authed:colls gas)
                ;=
                  ;a(href ".collections-edit")
                    ;button#edit-btn.btn.btn-primary.mb-4
                      ; Edit →
                    ==
                  ==
                ==
              ;=
                ;div;
              ==
          ;div.row.content.mb-18.mt-6
            ;div: +{q.content}
          ==
          ;*  ?:  comm:(need config)
            ;=
              ;div
                ;div.mb-2
                  ;span(urb-component "IconComment");
                  ;span: {<~(wyt by coms.q.metawcom)>}
                ==
                ;ul
                  ;*  %+  turn
                        %+  sort
                          ~(tap by coms.q.metawcom)
                        |=  [a=[c=@da d=[mod=@da who=@p wat=wain]] b=[c=@da d=[mod=@da who=@p wat=wain]]]
                        (lth (unt c.a) (unt c.b))
                        ::
                      |=  [c=@da d=[mod=@da who=@p wat=wain]]
                      ;li.collection-comment
                        ;div.collection-comment-avatar
                          ;div(urb-component "AvatarSample1");
                        ==
                        ;div
                          ;div
                            ;a.collection-comment-author.text-mono(href "/~~/pages/nutalk/profile"): {(trip (scot %p who.d))}
                          ==
                          ;p: {(trip (of-wain:format wat.d))}
                        ==
                        ;span.collection-date.text-black.mr-2(urb-component "Elapsed", urb-timestring "{(esoo:colls mod.d)}");
                        ;span.collection-date: {(esoo:colls mod.d)}
                      ==
                ==
                ;div(urb-component "CommentCreate", urb-coll "{(trip +<:s.bem.gas)}", urb-top "{(trip -:s.bem.gas)}");
              ==
            ==
          ~
        ==
      ==
    ==
  ==
==
'''
--
