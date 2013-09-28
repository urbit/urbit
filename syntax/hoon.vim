"hoon.vim: Hoon syntax file
"Credit goes to Fode
"


if exists("b:current_syntax")
  finish
endif

syn case match


" comments
" Declerations
hi def link     hoonDeclaration   Define 
hi def link     hoonSymbol        Constant 
hi def link     hoonAtom          Keyword
hi def link     hoonRune          Keyword
hi def link     hoonBranch        Conditional
hi def link     hoonType          Type
hi def link     hoonName          Constant

syn match       hoonDeclaration   "++" nextgroup=hoonSymbolDec skipwhite 
syn match       hoonBranch        /?[^\w\s]/ 
syn match       hoonSymbol        /%\w*/
syn keyword     hoonAtom          @
syn match       hoonName          "\w*" contained
syn match       hoonSymbolDec     "\w*" contained contains=hoonName

" strings

hi def link     hoonComment       Comment

syn region      hoonComment       start="::" end="$" contains=@spell


hi def link     hoonString        String
syn region      hoonString        start=+'+ end=+'+ contains=@spell

let b:current_syntax = "hoon"

