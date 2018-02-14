/-  collections
/+  rekey, old-zuse, colls
/=  gas  /$  fuel:html
:: reach up and get config file?
/=  configs  /:  /===/web/collections
            /^  (map knot config:collections)  /_  /collections-config/
/=  extratopic  /^  {a=manx b=topicful:collections ~}  
       /%  
       /.    /&  elem 
             /&  md
             ::  don't render first line if there's a title
             /;  |=  a/topic:collections 
                 ?:  =((scag 2 (trip -:wat.a)) "# ")
                   (of-wain:format +:wat.a)
                 (of-wain:format wat.a)
             /collections-topic/
           /collections-topic-full/
       ==
=/  config  (~(get by configs) +<:s.bem.gas)
^-  manx
;div
  ;div.row.coll-title
    {(trip desc:(need config))} /
  ==
  ;div.row.mod.text-mono
    {(trip (scot %da mod.info.b.extratopic))}
  ==
  ;div.row.tit
    ;h1: {(trip tit.info.b.extratopic)}
  ==
  ::
  ;*  ?:  (authed:colls gas)
        ;=
          ;a(href "/~~/pages/nutalk/collection/post?coll={(trip +<:s.bem.gas)}&top={(trip -:s.bem.gas)}")
            ;button.btn.btn-primary.mb-4
              ; Edit →
            ==
          ==
        ==
      ~
  ;div.row.content.mb-18
    +{a.extratopic}
  ==
  ;*  ?:  comm:(need config)
    ;=
      ;div.coms
      ;h3: Comments
        ;*  %+  turn
              ~(tap by coms.b.extratopic)
            |=  [c=@da d=[mod=@da who=@p wat=wain]]
            ;div.com.ml-12.mb-6
              ;div.who
                ;a(href "")
                  {(trip (scot %p who.d))}
                ==
              ==
              ;div.com-body
                ; {(trip (of-wain:format wat.d))}
              ==
            ==
        ;div.ml-12(data-component "CommentCreate", data-coll "{(trip +<:s.bem.gas)}", data-top "{(trip -:s.bem.gas)}");
      ==
    ==
  ~
==


