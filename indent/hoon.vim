" Public Domain
" Credit Goes to fode

if exists("b:did_indent")
  finish
endif

let b:did_indent = 1


setlocal indentexpr=HoonIndent(v:lnum)
setlocal nolisp
setlocal autoindent

if exists("*HoonIndent")
  finish
endif

if exists('g:hoon_ninja')
  inoremap bar \|
  inoremap gal  <          
  inoremap per  )
  inoremap gar  >
  inoremap sel  [
  inoremap bas  \
  inoremap hax  # 
  inoremap sem  ;
  inoremap buc  $          
  inoremap hep  -          
  inoremap ser  ]
  inoremap cab  _
  inoremap kel  {
  inoremap sig  ~
  inoremap cen  %
  inoremap ker  }
  inoremap soq  '
  inoremap col  : 
  inoremap ket  ^          
  inoremap tar  *
  inoremap com  ,
  inoremap lus  +
  inoremap tec  `
  inoremap doq  "
  inoremap pam  & 
  inoremap tis  =
  inoremap dot  . 
  inoremap pat  @ 
  inoremap wut  ?
  inoremap fas  /
  inoremap pel  ( 
  inoremap zap  !
endif

function! HoonIndent(lnum)
  let prevlnum = prevnonblank(a:lnum-1)
  if prevlnum == 0
    return 0
  endif
  let prevl = substitute(getline(prevlnum),'::.*$','','')
  
  let ind = indent(prevlnum)
  if prevl =~ '++\s*\w*\s*$'
    " luslus operator
    let ind += &sw
  endif

  return ind
endfunction

