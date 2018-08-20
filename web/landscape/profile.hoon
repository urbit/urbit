/+  collections, colls
/=  gas  /$  fuel:html
::/=  all-colls  /:  /===/web/collections
::               /collection-web-item/
|%
  :: is this collection on the profile?
  ++  is-pro
  |=  col=collection:collections
  visible.meta.col
--
^-  manx
;div.profile-page
  ;input(type "hidden", name "urb-header", value "profile", ship "{(scow %p p.bem.gas)}");
  ;div.container
    ;div.row
      ;div.col-sm-offset-2.col-sm-10
        ;div.profile-avatar
          ;div(urb-component "AvatarLg", urb-ship "{(scow %p p.bem.gas)}");
          ;div(urb-component "ProfileMsgBtn", urb-ship "{(scow %p p.bem.gas)}");
        ==
      ==
    ==
    ;div.row
      ;div.col-sm-offset-2.col-sm-10
        ;div(urb-component "ChatList", urb-hostship "{(scow %p p.bem.gas)}");
        ;div.text-700.mt-8: Meta
        ;div.mt-2.text-500.row
          ;span.col-sm-2: Started:
          ;span.col-sm-10.text-mono: ~2018.4.12..6.45.12
        ==
        ;div.mt-2.text-500.row
          ;span.col-sm-2: Issued:
          ;span.col-sm-10.text-mono
            ;a(href "/~~/{(scow %p (sein:title p.bem.gas))}/==/web/landscape/profile"): {(scow %p (sein:title p.bem.gas))}
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
