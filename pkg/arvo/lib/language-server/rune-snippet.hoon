/-  lsp-sur=language-server
/+  auto=language-server-complete
=>
|%
++  snippet
  |=  [rune=tape text=tape]
  ^-  json
  =,  enjs:format
  %-  pairs
  :~  'label'^(tape rune)
      'insertTextFormat'^(numb 2)
      'insertText'^(tape text)
  ==
::
++  runes
  ^-  (list (option:auto tape))
  :~  :-  '|$'
      """
        $\{1:sample}
      $\{2:body}
      """
      :-  '|_'
      """
        $\{1:sample}
      ++  $\{2:arm}
        $\{3:body}
      --
      """
      :-  '|:'
      """
        $\{1:sample}
      $\{2:body}
      """
      :-  '|%'
      """

      ++  $\{1:arm}
        $\{2:body}
      --
      """
      :-  '|.'
      """
        $\{1:body}
      """
      :-  '|^'
      """

      $\{1:body}
      ::
      ++  $\{2:arm}
        $\{3:body}
      --
      """
      :-  '|-'
      """
        $\{1:body}
      """
      :-  '|~'
      """
        $\{1:sample}
      $\{2:body}
      """
      :-  '|*'
      """
        $\{1:sample}
      $\{2:body}
      """
      :-  '|='
      """
        $\{1:sample}
      $\{2:body}
      """
      :-  '|@'
      """
      ++  $\{1:arm}
        $\{2:body}
      --
      """
      :-  '|?'
      """
        $\{1:sample}
      """
  ::
      :-  ':_'
      """
        $\{1:tail}
      $\{2:head}
      """
      :-  ':^'
      """
          $\{1:car}
          $\{2:cadr}
        $\{3:caddr}
      $\{4:cddr}
      """
      :-  ':-'
      """
        $\{1:tail}
      $\{2:head}
      """
      :-  ':+'
      """
        $\{1:car}
        $\{2:cadr}
      $\{3:cddr}
      """
      :-  ':~'
      """
        $\{1:item}
      ==
      """
      :-  ':*'
      """
        $\{1:item}
      ==
      """
  ::
      :-  '%_'
      """
        $\{1:target}
        $\{2:wing}  $\{3:new-value}
      ==
      """
      :-  '%.'
      """
        $\{1:arg}
      $\{2:gate}
      """
      :-  '%-'
      """
        $\{1:gate}
      $\{2:arg}
      """
      :-  '%:'
      """
        $\{1:gate}
        $\{2:args}
      ==
      """
      :-  '%*'
      """
        $\{1:target-wing}  $\{2:from}
        $\{3:wing}  $\{4:new-value}
      ==
      """
      :-  '%^'
      """
          $\{1:gate}
          $\{2:arg1}
        $\{3:arg2}
      $\{4:arg3}
      """
      :-  '%+'
      """
        $\{1:gate}
        $\{2:arg1}
      $\{3:arg2}
      """
      :-  '%~'
      """
        $\{1:arm}
        $\{2:core}
      $\{3:arg}
      """
      :-  '%='
      """
        $\{1:target}
        $\{2:wing}  $\{3:new-value}
      ==
      """
  ::
      :-  '.^'
      """
        $\{1:mold}
      $\{2:path}
      """
      :-  '.+'
      """
        $\{1:atom}
      """
      :-  '.*'
      """
        $\{1:subject}
      $\{2:formula}
      """
      :-  '.='
      """
        $\{1:a}
      $\{2:b}
      """
      :-  '.?'
      """
        $\{1:noun}
      """
  ::
      :-  '^|'
      """
        $\{1:iron-core}
      """
      :-  '^%'
      """
        $\{1:body}
      """
      :-  '^.'
      """
        $\{1:a}
      $\{2:b}
      """
      :-  '^+'
      """
        $\{1:like}
      $\{2:body}
      """
      :-  '^-'
      """
        $\{1:type}
      $\{2:body}
      """
      :-  '^&'
      """
        $\{1:zinc-core}
      """
      :-  '^~'
      """
        $\{1:constant}
      """
      :-  '^='
      """
        $\{1:face}
      $\{2:body}
      """
      :-  '^?'
      """
        $\{1:lead-core}
      """
      :-  '^*'
      """
        $\{1:type}
      """
      :-  '^:'
      """
        $\{1:type}
      """
  ::
      :-  '~|'
      """
        $\{1:trace}
      $\{2:body}
      """
      :-  '~_'
      """
        $\{1:tank}
      $\{2:body}
      """
      :-  '~%'
      """
          $\{1:name}
          $\{2:parent}
        ~
      $\{3:body}
      """
      :-  '~/'
      """
        $\{1:name}
      $\{2:body}
      """
      :-  '~<'
      """
        $\{1:hint}
      $\{2:body}
      """
      :-  '~>'
      """
        $\{1:hint}
      $\{2:body}
      """
      :-  '~$'
      """
        $\{1:name}
      $\{2:body}
      """
      :-  '~+'
      """

      $\{1:body}
      """
      :-  '~&'
      """
        $\{1:printf}
      $\{2:body}
      """
      :-  '~='
      """
        $\{1:a}
      $\{2:b}
      """
      :-  '~?'
      """
        $\{1:condition}
        $\{2:printf}
      $\{3:body}
      """
      :-  '~!'
      """
        $\{1:type}
      $\{2:body}
      """
  ::
      :-  ';='
      """
        $\{1:manx}
      ==
      """
      :-  ';:'
      """
        $\{1:gate}
        $\{2:args}
      ==
      """
      :-  ';/'
      """
        $\{1:tape}
      """
      :-  ';<'
      """
        $\{1:type}  bind:m  $\{2:body1}
      $\{3:body2}
      """
      :-  ';~'
      """
        $\{1:gate}
        $\{2:args}
      ==
      """
      :-  ';;'
      """
        $\{1:type}
      $\{2:body}
      """
  ::
      :-  '=|'
      """
        $\{1:type}
      $\{2:body}
      """
      :-  '=:'
      """
        $\{1:wing}  $\{2:value}
        ==
      $\{3:body}
      """
      :-  '=/'
      """
        $\{1:face}
        $\{2:value}
      $\{3:body}
      """
      :-  '=;'
      """
        $\{1:face}
        $\{2:body}
      $\{3:value}
      """
      :-  '=.'
      """
        $\{1:wing}
        $\{2:value}
      $\{3:body}
      """
      :-  '=?'
      """
        $\{1:wing}  $\{2:condition}
        $\{3:value}
      $\{4:body}
      """
      :-  '=<'
      """
        $\{1:formula}
      $\{2:subject}
      """
      :-  '=-'
      """
        $\{1:body}
      $\{2:value}
      """
      :-  '=>'
      """
        $\{1:subject}
      $\{2:formula}
      """
      :-  '=^'
      """
        $\{1:face}  $\{2:wing}
        $\{3:computation}
      $\{4:body}
      """
      :-  '=+'
      """
        $\{1:value}
      $\{2:body}
      """
      :-  '=~'
      """

      $\{1:body}
      """
      :-  '=*'
      """
        $\{1:alias}  $\{2:value}
      $\{3:body}
      """
      :-  '=,'
      """
        $\{1:alias}
      $\{3:body}
      """
  ::
      :-  '?|'
      """
        $\{1:condition}
      ==
      """
      :-  '?-'
      """
        $\{1:case}
        $\{2:type}  $\{3:value}
      ==
      """
      :-  '?:'
      """
        $\{1:if}
        $\{2:then}
      $\{3:else}
      """
      :-  '?.'
      """
        $\{1:if}
        $\{2:else}
      $\{3:then}
      """
      :-  '?^'
      """
        $\{1:value}
        $\{2:if-cell}
      $\{3:if-atom}
      """
      :-  '?<'
      """
        $\{1:assertion}
      $\{2:body}
      """
      :-  '?>'
      """
        $\{1:assertion}
      $\{2:body}
      """
      :-  '?+'
      """
        $\{1:case}  $\{2:else}
        $\{3:type}  $\{4:value}
      ==
      """
      :-  '?&'
      """
        $\{1:condition}
      ==
      """
      :-  '?@'
      """
        $\{1:value}
        $\{2:if-atom}
      $\{3:if-cell}
      """
      :-  '?~'
      """
        $\{1:value}
        $\{2:if-null}
      $\{3:if-nonnull}
      """
      :-  '?#'
      """
        $\{1:skin}
      $\{2:wing}
      """
      :-  '?='
      """
        $\{1:type}
      $\{2:wing}
      """
      :-  '?!'
      """
        $\{1:loobean}
      """
  ::
      :-  '!,'
      """
        *hoon
      $\{1:ast}
      """
      :-  '!>'
      """
        $\{1:value}
      """
      :-  '!;'
      """
        $\{1:type}
      $\{2:body}
      """
      :-  '!='
      """
        $\{1:body}
      """
      :-  '!@'
      """
        $\{1:wing}
        $\{2:if-exists}
      $\{3:if-not-exists}
      """
      :-  '!?'
      """
        $\{1:version}
      $\{2:body}
      """
      :-  '!!'
      ""
  ==
--
|=  rune=tape
^-  (list completion-item:lsp-sur)
=?  rune  =(' ' (snag 0 rune))
  (slag 1 rune)
~&  rune
%+  turn  (search-prefix:auto (crip rune) runes)
|=  [name=cord snippet=tape]
^-  completion-item:lsp-sur
[name 1 '' '' (crip snippet) 2]
