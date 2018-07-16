/-  collections
/+  colls
/=  gas  /$  fuel:html
/=  all-colls  /:  /===/web/collections
            /^  (map knot collection:collections)  /_  /collections-collection/
|%
  :: is this collection on the profile?
  ++  is-pro
  |=  {a/knot coll/collection:collections}
  visi.conf.coll
--
^-  manx
;div.profile-page
  ;input(type "hidden", name "urb-header", value "profile", ship "{(scow %p p.bem.gas)}");
  ;div.profile-banner
    ;div.container
      ;div.row
        ;div.col-sm-offset-2.col-sm-10
          ;div.profile-avatar
            ;div(urb-component "AvatarLg");
            ;div.profile-shipname: {(scow %p p.bem.gas)}
            ;a.no-underline.mb-6.btn.btn-sm.profile-message-btn(href "/~~/pages/nutalk/stream/create?dm={(scow %p p.bem.gas)}"): Message
          ==
        ==
      ==
    ==
  ==
  ;div.container
    ;div.row
      ;div.col-sm-offset-2.col-sm-10
        ::;div.text-700.mt-8: Blogs, Forum and Notes
        ::;ul.vanilla
        ::  ;div;
        ::  ;*  %+  turn
        ::        %+  sort
        ::          %+  skim
        ::            ~(tap by all-colls)
        ::          is-pro
        ::        |=  [a=(pair knot *) b=(pair knot *)]
        ::        (gth (unt:chrono:userlib (slav %da p.a)) (unt:chrono:userlib (slav %da p.b)))
        ::      |=  [t=knot col=collection:collections]
        ::      ;div.mt-2.text-500
        ::        ;a(href "/~~/{(scow %p p.bem.gas)}/==/web/collections/{(trip t)}")
        ::          ; {(trip desc.conf.col)}
        ::        ==
        ::      ==
        ::==
        ::;div.text-700.mt-8: Chats
        ;div(urb-component "ChatList", urb-hostship "{(scow %p p.bem.gas)}");
        ;div.text-700.mt-8: Meta
        ;div.mt-2.text-500.row
          ;span.col-sm-2: Started:
          ;span.col-sm-10.text-mono: ~2018.4.12..6.45.12
        ==
        ;div.mt-2.text-500.row
          ;span.col-sm-2: Issued:
          ;span.col-sm-10.text-mono
            ;a(href "/~~/{(scow %p (sein:title p.bem.gas))}/==/web/pages/nutalk/profile"): {(scow %p (sein:title p.bem.gas))}
          ==
        ==
        ;div.mt-2.text-500.row
          ;span.col-sm-2: Events:
          ;span.col-sm-10.text-mono: 852.129.320
        ==
      ==
    ==
  ==
==
