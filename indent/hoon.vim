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

if exists("b:hoon_did_submode_mapping")
  finish
endif

let g:submode_timeout = 0

"call submode#enter_with('hoon_ninja', 'i', '', '/ninja') 
call submode#enter_with('hoon_ninja', 'i', '', '//') 
call submode#map('hoon_ninja', 'i', '', 'bar', '\|') 
call submode#map('hoon_ninja', 'i', '', 'gal', '<') 
call submode#map('hoon_ninja', 'i', '', 'per', ')') 
call submode#map('hoon_ninja', 'i', '', 'gar', '>') 
call submode#map('hoon_ninja', 'i', '', 'sel', '[') 
call submode#map('hoon_ninja', 'i', '', 'bas', '\') 
call submode#map('hoon_ninja', 'i', '', 'hax', '#') 
call submode#map('hoon_ninja', 'i', '', 'sem', ';') 
call submode#map('hoon_ninja', 'i', '', 'buc', '$') 
call submode#map('hoon_ninja', 'i', '', 'hep', '-') 
call submode#map('hoon_ninja', 'i', '', 'ser', ']') 
call submode#map('hoon_ninja', 'i', '', 'cab', '_') 
call submode#map('hoon_ninja', 'i', '', 'kel', '{') 
call submode#map('hoon_ninja', 'i', '', 'sig', '~') 
call submode#map('hoon_ninja', 'i', '', 'cen', '%') 
call submode#map('hoon_ninja', 'i', '', 'ker', '}') 
call submode#map('hoon_ninja', 'i', '', 'soq', "'") 
call submode#map('hoon_ninja', 'i', '', 'col', ':') 
call submode#map('hoon_ninja', 'i', '', 'ket', '^') 
call submode#map('hoon_ninja', 'i', '', 'tar', '*') 
call submode#map('hoon_ninja', 'i', '', 'com', ',') 
call submode#map('hoon_ninja', 'i', '', 'lus', '+') 
call submode#map('hoon_ninja', 'i', '', 'tec', '`') 
call submode#map('hoon_ninja', 'i', '', 'doq', '"') 
call submode#map('hoon_ninja', 'i', '', 'pam', '&') 
call submode#map('hoon_ninja', 'i', '', 'tis', '=') 
call submode#map('hoon_ninja', 'i', '', 'dot', '.') 
call submode#map('hoon_ninja', 'i', '', 'pat', '@') 
call submode#map('hoon_ninja', 'i', '', 'wut', '?') 
call submode#map('hoon_ninja', 'i', '', 'fas', '/') 
call submode#map('hoon_ninja', 'i', '', 'pel', '(') 
call submode#map('hoon_ninja', 'i', '', 'zap', '!')

call submode#enter_with('hoon_ajnin', 'i', '', '??') 
call submode#map('hoon_ajnin', 'i', '', '\|', 'bar') 
call submode#map('hoon_ajnin', 'i', '', '<', 'gal') 
call submode#map('hoon_ajnin', 'i', '', ')', 'per') 
call submode#map('hoon_ajnin', 'i', '', '>', 'gar') 
call submode#map('hoon_ajnin', 'i', '', '[', 'sel') 
call submode#map('hoon_ajnin', 'i', '', '\', 'bas') 
call submode#map('hoon_ajnin', 'i', '', '#', 'hax') 
call submode#map('hoon_ajnin', 'i', '', ';', 'sem') 
call submode#map('hoon_ajnin', 'i', '', '$', 'buc') 
call submode#map('hoon_ajnin', 'i', '', '-', 'hep') 
call submode#map('hoon_ajnin', 'i', '', ']', 'ser') 
call submode#map('hoon_ajnin', 'i', '', '_', 'cab') 
call submode#map('hoon_ajnin', 'i', '', '{', 'kel') 
call submode#map('hoon_ajnin', 'i', '', '~', 'sig') 
call submode#map('hoon_ajnin', 'i', '', '%', 'cen') 
call submode#map('hoon_ajnin', 'i', '', '}', 'ker') 
call submode#map('hoon_ajnin', 'i', '', '"', 'soq') 
call submode#map('hoon_ajnin', 'i', '', ':', 'col') 
call submode#map('hoon_ajnin', 'i', '', '^', 'ket') 
call submode#map('hoon_ajnin', 'i', '', '*', 'tar') 
call submode#map('hoon_ajnin', 'i', '', ',', 'com') 
call submode#map('hoon_ajnin', 'i', '', '+', 'lus') 
call submode#map('hoon_ajnin', 'i', '', '`', 'tec') 
call submode#map('hoon_ajnin', 'i', '', '"', 'doq') 
call submode#map('hoon_ajnin', 'i', '', '&', 'pam') 
call submode#map('hoon_ajnin', 'i', '', '=', 'tis') 
call submode#map('hoon_ajnin', 'i', '', '.', 'dot') 
call submode#map('hoon_ajnin', 'i', '', '@', 'pat') 
call submode#map('hoon_ajnin', 'i', '', '?', 'wut') 
call submode#map('hoon_ajnin', 'i', '', '/', 'fas') 
call submode#map('hoon_ajnin', 'i', '', '(', 'pel') 
call submode#map('hoon_ajnin', 'i', '', '!', 'zap')
let b:hoon_did_submode_mapping = 1

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

