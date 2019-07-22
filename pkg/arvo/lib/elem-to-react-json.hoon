::  Serialize &elem as &json, with special handling for urb:* attributes
::
::::  /hoon/elem-to-react-json/lib
  ::
/?    310
=,  format
|%
++  react-attrs                                         ::  uppercase mapping
  ~+  ^-  (map term cord)
  %-  molt  ^-  (list (pair term cord))
  :-  [%class 'className']
  =-  (rash - (more next (cook |=(a/tape [(crip (cass a)) (crip a)]) (star alf))))
  '''
  accept acceptCharset accessKey action allowFullScreen allowTransparency alt
  async autoComplete autoFocus autoPlay cellPadding cellSpacing charSet checked
  classID className colSpan cols content contentEditable contextMenu controls
  coords crossOrigin data dateTime defer dir disabled download draggable encType
  form formAction formEncType formMethod formNoValidate formTarget frameBorder
  headers height hidden high href hrefLang htmlFor httpEquiv icon id label lang
  list loop low manifest marginHeight marginWidth max maxLength media mediaGroup
  method min multiple muted name noValidate open optimum pattern placeholder
  poster preload radioGroup readOnly rel required role rowSpan rows sandbox
  scope scoped scrolling seamless selected shape size sizes span spellCheck
  src srcDoc srcSet start step style tabIndex target title type useMap value
  width wmode
  '''
::
::  special handling for <pre urb:codemirror>foo</pre>
++  urb-codemirror                                      ::  render code blocks
  |=  src/manx  ^-  manx
  ?>  ?=({{$pre *} _;/(**) ~} src)
  ;codemirror(value "{v.i.a.g.i.c.src}");
::
++  elem-to-react-json                                  ::  serialize DOM as json
  |=  src/manx  ^-  json
  ?:  ?=(_;/(**) src)
    (tape:enjs v.i.a.g.src)
  =+  atr=(molt `(list (pair mane tape))`a.g.src)
  ?:  (~(has by atr) [%urb %codemirror])
    $(src (urb-codemirror src))
  %-  pairs:enjs  :~
    c+a+(turn c.src ..$)
    gn+s+(mane-to-cord n.g.src)
    =<  ga+(pairs:enjs (turn a.g.src .))
    |=  {a/mane b/tape}  ^-  {cord json}
    :_  (tape:enjs b)
    ?^  a  (mane-to-cord a)
    (~(gut by react-attrs) a a)
  ==
::
++  mane-to-cord                                        ::  namespaced xml names
  |=(a/mane `cord`?@(a a (rap 3 -.a ':' +.a ~)))
--
::
::::
  ::
elem-to-react-json                                      ::  export conversion gate
