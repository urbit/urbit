::    Twig reprinter
::
::::  /hook/hymn/unparse/pub
  ::
/=   gas  /$  fuel
/=   src  /:  /%/mear  /hoon/
/=   src2  /:  /%/  /hoon/
::
::::    ~sondel-forsut
  ::
//    /%/mear
!:
::::
  ::
=+  inp=`@t`(fall %.(%src ;~(biff ~(get by qix.gas) (slat %t))) '%no-source')
^-  manx
;html
  ;head
    ;title:"Uneval"
    ;style:'''
           #inp {
             height: 40%;
             width: 100%;
             font-family: Monospace;
           }
           '''
  ==
  ;body
    ;h3: Abstract
    ;p: Hoon linter, under construction.
    ;h3: Input
    ;textarea#inp(onchange "move()"):"{(trip inp)}"
    ;h3: Twig (subset)
    ;pre
      ;*  %-  turn  :_  |=(a=tape :/((weld a "\0a")))
          (wash 0^80 >(flag (ream inp))<)
    ==
    ;h3: Rectified output
    ;pre: {(trip (mear (ream inp)))}
    ;script:'''
            function move(){
              document.location = '?src=' + escp(inp.value)
            }
            function escp(s){
              acc = '~~'
              while(s.length){
                var cha = s[0];
                s = s.slice(1)
                switch(true){
                  case /[a-z-]/.test(cha):
                    acc += cha
                  break;
                  case cha === ' ':
                    acc += '.'
                  break;
                  case /[.-]/.test(cha):
                    acc += '~' + cha
                  break;
                  default:
                    acc += '~' + cha.charCodeAt(0).toString(16) + '.'
              } }
              return acc
            }
            '''
    ::  includes
    ;*  =-  (turn `wall`- |=(a=tape ;script(type "text/javascript", src a);))
        :~  "//cdnjs.cloudflare.com/ajax/libs/codemirror/4.3.0/codemirror.js"
            "/lib/syntax/hoon.js"
        ==
    ;link(rel "stylesheet", href "//cdnjs.cloudflare.com/ajax/libs/".
                                 "codemirror/4.3.0/codemirror.min.css");
    ;style:'''
           .CodeMirror {height: 70%}
           .cm-s-default .cm-atom {color: #70f}
           .cm-s-default .cm-operator {color: #097}
           '''
    ::  sources
    ;h3: Demo source: algorithm
    ;textarea#src: {(trip src)}
    ;script:'''
            CodeMirror.fromTextArea(src, {lineNumbers:true, readOnly:true})
            '''
    ;h3: Demo source: page
    ;textarea#src2: {(trip src2)}
    ;script:'''
            CodeMirror.fromTextArea(src2, {lineNumbers:true, readOnly:true})
            '''
  ==
==
