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

