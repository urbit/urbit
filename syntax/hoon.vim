"hoon.vim: Hoon syntax file
"Credit goes to Fode
"


if exists("b:current_syntax")
  finish
endif

syn case match


" Declerations
hi def link     hoonDeclaration   Define 
hi def link     hoonSymbol        Constant 
hi def link     hoonAtom          Keyword
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
" As I understand it, numbers may be in decimal, hex, or binary, and they may
" contain dots (functioning merely as separators, as in the American comma).
" XXX It appears that a number can span lines if (and only if?) the lines end
" in a dot.  This mostly causes issues with hex numbers across mulitple lines
" (as in hoon.hoon line 3067).
syn match       hoonNumber        "[0123456789]\+[0123456789\.]*"
syn match       hoonNumber        "0x[0123456789abcdef]\+[0123456789abcdef\.]*"
syn match       hoonNumber        "0b[01]\+[01\.]*"

" comments

syn region      hoonComment       start="::" end="$" contains=@spell,hoonTodo
syn keyword     hoonTodo          contained XX XXX TODO FIXME

" strings

syn region      hoonString        start=+'+ skip=+\\[\\']+ end=+'+ contains=@spell

" match digraphs
" XXX digraphs starting with '=' in e.g. paramater naming when this is really
" the monograph '=' followed by a digraph.  Example:  hoon.hoon line 218
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
syn match       hoonRune          "=|"
syn match       hoonRune          "=\."
syn match       hoonRune          "=\^"
syn match       hoonRune          "=:"
syn match       hoonRune          "=<"
syn match       hoonRune          "=>"
syn match       hoonRune          "=-"
syn match       hoonRune          "=+"
syn match       hoonRune          "=\~"
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

" match identifiers
" These are just pulled from hoon.hoon using:
" cat hoon.hoon | sed -n -e 's/^++  \<\([^ ]*\)\>.*/\1/p'

let b:current_syntax = "hoon"

