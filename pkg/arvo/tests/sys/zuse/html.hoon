::  tests for html
/+  *test
=,  de-xml:html
=,  en-xml:html
|%
:: de-xml takes a cord but en-xml returns a tape?
::
++  test-de-xml
  ;:  weld
    :: Basic use
    ::
    %+  expect-eq
      !>  ^-  manx  +:(de-xml:html (crip "<html><head><title>My first webpage</title></head><body><h1>Welcome!</h1>Hello, world! We are on the web.\0a<div></div><script src=\"http://unsafely.tracking.you/cookiemonster.js\"></script></body></html>"))
    !>  ^-  manx
    ;html
        ;head
          ;title: My first webpage
        ==
        ;body
          ;h1: Welcome!
          ; Hello, world! We are on the web.
          ;div;
          ;script(src "http://unsafely.tracking.you/cookiemonster.js");
        ==
      ==
    :: CDATA sections
    ::
    %+  expect-eq
      !>  ^-  manx
      +:(de-xml:html (crip "<elem><![CDATA[text]]></elem>"))
    !>  ^-  manx
    ;elem: text
    :: comments
    %+  expect-eq
      !>  ^-  manx
      ;elem: text
    !>  +:(de-xml:html (crip "<elem>text<!-- comment --></elem>"))
    %+  expect-eq
      !>  ^-  manx
      ;elem;
    !>  +:(de-xml:html (crip "<elem><!-- comment --></elem>"))
    :: entities
    ::
    %+  expect-eq
      !>  ^-  manx
      +:(de-xml:html (crip "<elem>&#62;</elem>"))
      !>  ^-  manx
      ;elem: >
    :: self-closing tag
    ::
    %+  expect-eq
      !>  ^-  manx
      +:(de-xml:html (crip "<img />"))
      !>  ^-  manx
      ;img;

  ==

::
++  test-en-xml
  ;:  weld
    :: Entities
    ::
    %+  expect-eq
      !>  "<elem>&gt;</elem>"
    !>  %-  en-xml:html
    ;elem: >
    :: Basic use
    ::
    %+  expect-eq
      !>   %-  en-xml:html
      ;html
        ;head
          ;title: My first webpage
        ==
        ;body
          ;h1: Welcome!
          ; Hello, world!
          ; We are on the web.
          ;div;
          ;script@"http://unsafely.tracking.you/cookiemonster.js";
        ==
      ==
    !>  "<html><head><title>My first webpage</title></head><body><h1>Welcome!</h1>Hello, world!\0aWe are on the web.\0a<div></div><script src=\"http://unsafely.tracking.you/cookiemonster.js\"></script></body></html>"
    :: Attributes
    ::
    %+  expect-eq
      !>  "<input type=\"submit\">Submit</input>"
      !>  %-  en-xml:html
      ;input(type "submit"): Submit
  ==
--
