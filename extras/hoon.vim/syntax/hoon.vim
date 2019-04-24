"hoon.vim: Hoon syntax file
" Eric Fode, Philip C Monk

if exists("b:current_syntax")
  finish
endif

set autoindent
map g/ /++
nmap gs :let varname = '\<<C-R><C-W>\>'<CR>?++  <C-R>=varname<CR><CR>
set tabstop=2
" nmap gc :let &colorcolumn=join(range(81,999),",")<CR>
" nmap gC :let &colorcolumn=join(range(999,999),",")<CR>
" nmap ge :vertical resize 85<CR>

" Because symobls are used much more than numbers, some
" developers swap the number and symbol keys in insert
" mode.  This is disabled by default.  Uncomment the
" following lines to enable.
" inoremap 1 !
" inoremap 2 @
" inoremap 3 #
" inoremap 4 $
" inoremap 5 %
" inoremap 6 ^
" inoremap 7 &
" inoremap 8 *
" inoremap 9 (
" inoremap 0 )
" inoremap ! 1
" inoremap @ 2
" inoremap # 3
" inoremap $ 4
" inoremap % 5
" inoremap ^ 6
" inoremap & 7
" inoremap * 8
" inoremap ( 9
" inoremap ) 0

syn case match

" Declarations
hi def link     hoonDeclaration   Define
hi def link     hoonSymbol        Constant
hi def link     hoonAtom          Identifier
hi def link     hoonRune          Operator
hi def link     hoonIdentifier    Identifier
hi def link     hoonBranch        Conditional
hi def link     hoonType          Type
" hi def link     hoonName          Constant
hi def link     hoonNumber        Number
hi def link     hoonComment       Comment
hi def link     hoonTodo          Todo
hi def link     hoonString        String

syn match       hoonDeclaration   "+[+-]" nextgroup=hoonSymbolDec skipwhite
syn match       hoonSymbol        /%\%(\%(\%(\w\|-\)\+\)\|[|&$]\|\%(\.n\)\|\%(\.y\)\)/
syn match       hoonAtom          /\%(@\w*\)\|\^/
syn match       hoonName          "\w*" contained
syn match       hoonSymbolDec     "\w\w\+" contained contains=hoonName

" numbers
" Numbers are in decimal, binary, hex, base32, or base64, and they must
" contain dots (optionally followed by whitespace), as in the German manner.

syn sync        linebreaks=1
syn match       hoonNumber        "\d\{1,3\}\%(\.\_s\?\d\{3\}\)*"
syn match       hoonNumber        "0x\x\{1,4\}\%(\.\_s*\x\{4\}\)*"
syn match       hoonNumber        "0b[01]\{1,4\}\%(\.\_s*[01]\{4\}\)*"
syn match       hoonNumber        "0v[0-9a-v]\{1,5\}\%(\.\_s*[0-9a-v]\{5\}\)*"
syn match       hoonNumber        "0w[-~0-9a-zA-Z]\{1,5\}\%(\.\_s*[-~0-9a-zA-Z]\{5\}\)*"

" comments

syn region      hoonComment       start="::" end="$" contains=@spell,hoonTodo
syn keyword     hoonTodo          contained XX XXX TODO FIXME

" strings

syn region      hoonString        start=+'+ skip=+\\[\\']+ end=+'+ contains=@spell
syn region      hoonBlock         start=+'''+ end=+'''+
syn region      hoonString        start=+"+ skip=+\\[\\"]+ end=+"+ contains=@spell


" match digraphs

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
syn match       hoonRune          "|\/"
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
syn match       hoonRune          "\$+"
syn match       hoonRune          "\$="
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
syn match       hoonRune          "\~?"
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
syn match       hoonRune          "\%([^a-zA-Z]\|^\)\zs=|"
syn match       hoonRune          "\%([^a-zA-Z]\|^\)\zs=\."
syn match       hoonRune          "\%([^a-zA-Z]\|^\)\zs=\^"
syn match       hoonRune          "\%([^a-zA-Z]\|^\)\zs=:"
syn match       hoonRune          "\%([^a-zA-Z]\|^\)\zs=<"
syn match       hoonRune          "\%([^a-zA-Z]\|^\)\zs=>"
syn match       hoonRune          "\%([^a-zA-Z]\|^\)\zs=-"
syn match       hoonRune          "\%([^a-zA-Z]\|^\)\zs=+"
syn match       hoonRune          "\%([^a-zA-Z]\|^\)\zs=\*"
syn match       hoonRune          "\%([^a-zA-Z]\|^\)\zs=\~"
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

" Not technically runes, but we highlight them like that.
syn match       hoonRune          "\%([^a-zA-Z]\|^\)\zs=="
syn match       hoonRune          "--"

let b:current_syntax = "hoon"

