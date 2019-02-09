/=  gas  /$  fuel:html
/=  error  /:  /===/web/404  /!hymn/
::  only authenticated user should be allowed to see this
::
?.  (~(has in (~(got by aut.ced.gas) %$)) (scot %p p.bem.gas))
  error
=/  cod=tape
  %+  slag  1
  %+  scow  %p
  .^(@p %j /(scot %p p.bem.gas)/code/(scot r.bem.gas)/(scot %p p.bem.gas))
^-  manx
;div
  ;input(type "hidden", name "urb-metadata", urb-structure-type "header-profile", urb-author "{(scow %p p.bem.gas)}");
  ;div.container(urb-devices "")
    ;div.row.mt-4
      ;div.flex-col-2;
      ;div.flex-col-x
        ;a.vanilla.btn.btn-primary(href (trip 'javascript:(function(){document.querySelectorAll("[urb-devices]")[0].classList.add("hide"); document.querySelectorAll("[urb-qr]")[0].classList.remove("hide");})()')): Connect device
        ;h2.mt-6: Devices
        ;h3.text-mono.mt-4: 108.208.53.121
        ;div: Current session
        ;h3.text-mono.mt-4: 67.188.43.52
        ;div: Chrome on OS X 10.12.6
        ;div
          ;span.mr-3: Last login:
          ;span.text-mono: 2018.4.21
        ==
        ;h3.text-mono.mt-4: 43.222.12.64
        ;div: iOS 14.11
        ;div
          ;span.mr-3: Last login:
          ;span.text-mono: 2018.3.12
        ==
        ;div.mt-6
          ;a.h3.vanilla.text-red(href "javascript:void(0)"): Log Out ↓
        ==
      ==
    ==
  ==
  ;div.container.hide(urb-qr "")
    ;div.row.mt-4
      ;div.flex-col-2;
      ;div.flex-col-x
        ;div
          =urb-component  "QRCodeComponent"
          =urb-ship       "{(scow %p p.bem.gas)}"
          =urb-code       "{cod}";
        ;h2.mt-8.mt-0.text-500.profile-qr-desc: Scan this code to connect your device
        ;a.mt-4.vanilla.btn.btn-primary(href (trip 'javascript:(function(){document.querySelectorAll("[urb-qr]")[0].classList.add("hide"); document.querySelectorAll("[urb-devices]")[0].classList.remove("hide");})()')): Done
      ==
    ==
  ==
==
