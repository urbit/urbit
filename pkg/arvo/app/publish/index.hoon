|=  inject=json
^-  manx
;html
::
  ;head
    ;title: Publish
    ;meta(charset "utf-8");
    ;meta
      =name     "viewport"
      =content  "width=device-width, initial-scale=1, shrink-to-fit=no";
    ;link(rel "stylesheet", href "/~publish/index.css");
    ;link(rel "icon", type "image/png", href "/~launch/img/Favicon.png");
    ;script@"/~channel/channel.js";
    ;script@"/~modulo/session.js";
    ;script: window.injectedState = {(en-json:html inject)}
  ==
::
  ;body
    ;div#root.w-100.h-100;
    ;script@"/~publish/index.js";
  ==
==
