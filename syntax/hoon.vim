"hoon.vim: Hoon syntax file
"Credit goes to Fode
"
" With contributions from Philip C Monk


if exists("b:current_syntax")
  finish
endif

syn case match


" Declerations
hi def link     hoonDeclaration   Define 
hi def link     hoonSymbol        Constant 
hi def link     hoonAtom          Identifier
hi def link     hoonRune          Operator
hi def link     hoonIdentifier    Identifier
hi def link     hoonBranch        Conditional
hi def link     hoonType          Type
hi def link     hoonName          Constant
hi def link     hoonNumber        Number
hi def link     hoonComment       Comment
hi def link     hoonTodo          Todo
hi def link     hoonString        String

syn match       hoonDeclaration   "++" nextgroup=hoonSymbolDec skipwhite 
syn match       hoonSymbol        /%\w*/
"syn match       hoonBranch        /?[^\w\s]/ 
syn match       hoonAtom          /@\w*/
syn match       hoonName          "\w*" contained
syn match       hoonSymbolDec     "\w*" contained contains=hoonName

" numbers
" Numbers are in decimal, binary, hex, base32, or base64, and they must
" contain dots (optionally followed by whitespace), as in the German manner.

syn sync        linebreaks=1
syn match       hoonNumber        "\d\{1,3\}\%(\.\_s\?\d\{3\}\)*"
syn match       hoonNumber        "0x\x\{1,4\}\%(\.\_s*\x\{4\}\)*"
syn match       hoonNumber        "0b[01]\{1,4\}\%(\.\_s*[01]\{4\}\)*"
syn match       hoonNumber        "0v[0-9a-v]\{1,4\}\%(\.\_s*[0-9a-v]\{4\}\)*"
syn match       hoonNumber        "0w[-~0-9a-zA-Z]\{1,4\}\%(\.\_s*[-~0-9a-zA-Z]\{4\}\)*"

" comments

syn region      hoonComment       start="::" end="$" contains=@spell,hoonTodo
syn keyword     hoonTodo          contained XX XXX TODO FIXME

" strings

syn region      hoonString        start=+'+ skip=+\\[\\']+ end=+'+ contains=@spell
syn region      hoonString        start=+"+ skip=+\\[\\"]+ end=+"+ contains=@spell

" match digraphs
" XXX digraphs starting with '=' in e.g. paramater naming when this is really
" the monograph '=' followed by a digraph.  Example:  hoon.hoon line 218
" This is now fixed when '= is followed by a digraph, but not when followed by a monograph
" XXX we should match some of the monographs, I'm just not totally sure which
" ones.  Certainly, $ and ~ seem important, but I'm not sure of others.

syn match       hoonRune          "||"
syn match       hoonRune          "|_"
syn match       hoonRune          "|%"
syn match       hoonRune          "|:"
syn match       hoonRune          "|\."
syn match       hoonRune          "|-"
syn match       hoonRune          "|\^"
syn match       hoonRune          "|+"
syn match       hoonRune          "|\*"
syn match       hoonRune          "|="
syn match       hoonRune          "|?"
syn match       hoonRune          "%_"
syn match       hoonRune          "%:"
syn match       hoonRune          "%\."
syn match       hoonRune          "%\^"
syn match       hoonRune          "%+"
syn match       hoonRune          "%-"
syn match       hoonRune          "%\~"
syn match       hoonRune          "%\*"
syn match       hoonRune          "%="
syn match       hoonRune          "\$|"
syn match       hoonRune          "\$_"
syn match       hoonRune          "\$:"
syn match       hoonRune          "\$%"
syn match       hoonRune          "\$,"
syn match       hoonRune          "\$&"
syn match       hoonRune          "\$?"
syn match       hoonRune          ":_"
syn match       hoonRune          ":\~"
syn match       hoonRune          ":/"
syn match       hoonRune          ":\^"
syn match       hoonRune          ":+"
syn match       hoonRune          ":-"
syn match       hoonRune          ":\~"
syn match       hoonRune          ":\*"
syn match       hoonRune          "\.+"
syn match       hoonRune          "\.\*"
syn match       hoonRune          "\.="
syn match       hoonRune          "\.?"
syn match       hoonRune          "\.\^"
syn match       hoonRune          "#<"
syn match       hoonRune          "#>"
syn match       hoonRune          "\^|"
syn match       hoonRune          "\^\."
syn match       hoonRune          "\^-"
syn match       hoonRune          "\^+"
syn match       hoonRune          "\^&"
syn match       hoonRune          "\^\~"
syn match       hoonRune          "\^="
syn match       hoonRune          "\^?"
syn match       hoonRune          "\~|"
syn match       hoonRune          "\~\$"
syn match       hoonRune          "\~%"
syn match       hoonRune          "\~:"
syn match       hoonRune          "\~/"
syn match       hoonRune          "\~<"
syn match       hoonRune          "\~>"
syn match       hoonRune          "\~#"
syn match       hoonRune          "\~\^"
syn match       hoonRune          "\~+"
syn match       hoonRune          "\~&"
syn match       hoonRune          "\~="
syn match       hoonRune          "\~!"
syn match       hoonRune          ";_"
syn match       hoonRune          ";,"
syn match       hoonRune          ";%"
syn match       hoonRune          ";:"
syn match       hoonRune          ";\."
syn match       hoonRune          ";<"
syn match       hoonRune          ";>"
syn match       hoonRune          ";-"
syn match       hoonRune          ";+"
syn match       hoonRune          ";&"
syn match       hoonRune          ";\~"
syn match       hoonRune          ";;"
syn match       hoonRune          ";\*"
syn match       hoonRune          ";="
syn match       hoonRune          ";?"
syn match       hoonRune          "=|\ze[^-|_%:.^+\=?]"
syn match       hoonRune          "=\.\ze[^+*=?^]"
syn match       hoonRune          "=\^\ze[^-+|.&~=?]"
syn match       hoonRune          "=:\ze[^-_~/^+~*]"
syn match       hoonRune          "=<"
syn match       hoonRune          "=>"
syn match       hoonRune          "=-"
syn match       hoonRune          "=+"
syn match       hoonRune          "=\~\ze[^|\%:/<>#\+&=!]"
syn match       hoonRune          "?|"
syn match       hoonRune          "?:"
syn match       hoonRune          "?\."
syn match       hoonRune          "?<"
syn match       hoonRune          "?>"
syn match       hoonRune          "?-"
syn match       hoonRune          "?\^"
syn match       hoonRune          "?="
syn match       hoonRune          "?+"
syn match       hoonRune          "?&"
syn match       hoonRune          "?@"
syn match       hoonRune          "?\~"
syn match       hoonRune          "?!"
syn match       hoonRune          "!:"
syn match       hoonRune          "!,"
syn match       hoonRune          "!;"
syn match       hoonRune          "!\^"
syn match       hoonRune          "!>"
syn match       hoonRune          "!="

let b:current_syntax = "hoon"

