|=  inject=json
^-  manx
;html
::
  ;head
    ;title: Write
    ;meta(charset "utf-8");
    ;meta
      =name     "viewport"
      =content  "width=device-width, initial-scale=1, shrink-to-fit=no";
    ;link(rel "stylesheet", href "/~publish/index.css");
    ;script@"/~/channel/channel.js";
    ;script@"/session.js";
    ;script: window.injectedState = {(en-json:html inject)}
  ==
::
  ;body
    ;div#root;
    ;script@"/~publish/index.js";
  ==
==
