/=  gas  /$  fuel:html
/=  content  /%  /collections-topic-full/
^-  manx
;div
  ;div#edit(urb-component "TopicCreatePage", urb-coll "{(trip +<:s.bem.gas)}", urb-top "{(trip -:s.bem.gas)}", urb-text "{(trip (of-wain:format wat.info.content))}", urb-ship "{(scow %p p.bem.gas)}", urb-lastedit "{<mod.info.content>}");
  ;input(type "hidden", name "urb-header", value "collection", station "{(scow %p p.bem.gas)}/collection_~{(trip +<:s.bem.gas)}");
==
