::  Writer monad.
::
::  We assume that output is a list of items.  This could be any monoid
::  if we took in mempty and mappend functions.
::
|*  output=mold
=>  |%
    ++  form-raw
      |*  result-type=mold
      ,[output=(list output) result=result-type]
    ::
    ::  Emit a single output
    ::
    ++  tell
      |=  =output
      ^-  (form-raw ~)
      [[output ~] ~]
    ::
    ::  Emit a list of outputs
    ::
    ++  rant
      |=  output=(list output)
      ^-  (form-raw ~)
      [output ~]
    --
|*  result=mold
=<  form
|%
::  Data structure
::
+$  form  (form-raw result)
::
::  Identity
::
++  pure
  |=  =result
  ^-  form
  [~ result]
::
::  Combine two writers
::
++  bind
  |*  other-result=mold
  |=  [m-b=(form-raw other-result) fun=$-(other-result form)]
  ^-  form
  =/  other  (fun result.m-b)
  [(weld output.m-b output.other) result.other]
::
::  Inspect written output
::
++  listen
  |=  =form
  ^-  (form-raw [(list output) result])
  [output.form output.form result.form]
::
::  Modify written output
::
++  pass
  |=  comp=(form-raw [=result fun=$-((list output) (list output))])
  ^-  form
  [(fun.result.comp output.comp) result.result.comp]
--
