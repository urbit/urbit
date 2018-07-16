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
^-  manx
;*  ?:  (~(has by qix.gas) 'edit')
      ;div
        ;div#edit(urb-component "TopicCreatePage", urb-coll "{(trip +<:s.bem.gas)}", urb-top "{(trip -:s.bem.gas)}", urb-text "{(trip (of-wain:format wat.info.metawcom))}", urb-ship "{(scow %p p.bem.gas)}", urb-lastedit "{<mod.info.metawcom>}");
      ;input(type "hidden", name "urb-header", value "collection-edit", title "{(trip desc:(need config))}", publ "{<publ:(need config)>}", id "{(trip +<:s.bem.gas)}", postid "{(trip -:s.bem.gas)}", ship "{(scow %p p.bem.gas)}", station "{(scow %p p.bem.gas)}/collection_~{(trip +<:s.bem.gas)}");
      ==
  ;div.container
    ;div.row
      ;input(type "hidden", name "urb-header", value "collection-item", title "{(trip desc:(need config))}", publ "{<publ:(need config)>}", id "{(trip +<:s.bem.gas)}", postid "{(trip -:s.bem.gas)}", ship "{(scow %p p.bem.gas)}", station "{(scow %p p.bem.gas)}/collection_~{(trip +<:s.bem.gas)}");
      ;div.col-sm-10.col-sm-offset-2
        ;div.post.collection-post-page
          ;div.row.collection-date
            ;span.mr-2.text-black.text-500(urb-component "Elapsed", urb-timestring "{(esoo:colls mod.info.metawcom)}");
            ;span: {<mod.info.metawcom>}
          ==
          ::
          ;div#show
            ;div.row.tit.mt-6.collection-title
              ;h3: {(trip tit.info.metawcom)}
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
                  ;ul.vanilla
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
                  ;div(urb-component "CommentCreate", urb-coll "{(trip +<:s.bem.gas)}", urb-top "{(trip -:s.bem.gas)}", urb-ship "{(scow %p p.bem.gas)}");
                ==
              ==
            ~
          ==
        ==
      ==
    ==
  ==
