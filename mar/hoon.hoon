::
::::  /hoon/hoon/mar
  ::
/?    310
::
=,  eyre
|_  own/@t
::
++  grow                                                ::  convert to
  |%
  ++  mime  `^mime`[/text/x-hoon (as-octs:mimes:html own)] ::  convert to %mime
  ++  elem                                              ::  convert to %html
    ;div:pre(urb_codemirror "", mode "hoon"):"{(trip own)}"
    :: =+  gen-id="src-{<`@ui`(mug own)>}"
    :: ;div
    ::   ;textarea(id "{gen-id}"):"{(trip own)}"
    ::   ;script:"""
    ::           CodeMirror.fromTextArea(
    ::             window[{<gen-id>}],
    ::             \{lineNumbers:true, readOnly:true}
    ::           )
    ::           """
    :: ==
  ++  hymn
    :: ;html:(head:title:"Source" "+{elem}")
    ;html
      ;head
        ;title:"Source"
        ;script@"//cdnjs.cloudflare.com/ajax/libs/codemirror/4.3.0/codemirror.js";
        ;script@"/lib/syntax/hoon.js";
        ;link(rel "stylesheet", href "//cdnjs.cloudflare.com/ajax/libs/".
          "codemirror/4.3.0/codemirror.min.css");
        ;link/"/lib/syntax/codemirror.css"(rel "stylesheet");
      ==
      ;body
        ;textarea#src:"{(trip own)}"
        ;script:'CodeMirror.fromTextArea(src, {lineNumbers:true, readOnly:true})'
      ==
    ==
  ++  txt
    (to-wain:format own)
  --
++  grab
  |%                                            ::  convert from
  ++  mime  |=({p/mite q/octs} q.q)
  ++  noun  @t                                  ::  clam from %noun
  ++  txt   of-wain:format
  --
++  grad  %txt
--
