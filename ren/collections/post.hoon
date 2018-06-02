/-  collections
/+  old-zuse, colls
/=  gas  /$  fuel:html
:: reach up and get config file?
/=  configs  /:  /===/web/collections
            /^  (map knot config:collections)  /_  /collections-config/
:: this was a dumb way to do do this, clean up
/=  content  /&elem&md&/collections-topic/
/=  metawcom  /^  topicful:collections  /collections-topic-full/
=/  config  (~(get by configs) +<:s.bem.gas)
=,  old-zuse
::
^-  manx
;div.container
  ;div.row
    ;input(type "hidden", name "urb-header", value "collection-index", title "{(trip desc:(need config))}", id "{(trip +<:s.bem.gas)}", ship "{(scow %p p.bem.gas)}");
    ;div.col-sm-10.col-sm-offset-2
      ;div.post.collection-post-page
        ;div.row.collection-date
          ;span.mr-2.text-black.text-500(urb-component "Elapsed", urb-timestring "{(esoo:colls mod.info.metawcom)}");
          ;span: {(trip -:s.bem.gas)}
        ==
        ::
        ;div#show
          ;div.row.tit.mt-6.collection-title
            ;h3: {(trip tit.info.metawcom)}
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
            ;div: +{content}
          ==
          ;*  ?:  comm:(need config)
            ;=
              ;div
                ;div.mb-2
                  ;span(urb-component "IconComment");
                  ;span: {<~(wyt by coms.metawcom)>}
                ==
                ;ul
                  ;*  %+  turn
                        %+  sort
                          ~(tap by coms.metawcom)
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
