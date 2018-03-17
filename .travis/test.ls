{Urbit,ERROR} = require './runner.ls'

urbit = new Urbit <[-B urbit.pill -A .. -cFI zod zod]> 
Promise.resolve urbit
.then (urb)->
  urb.note "Booting urbit"
  Promise.race [
    urb.expect ERROR .then ->
      urb.warn "Boot error detected"
      throw Error "Stack trace while booting"
  , do
    <- urb.expect /dojo> / .then
    <- urb.expect-echo "%dojo-booted" .then
    urb.reset-listeners!
  ]
.then (urb)->
  urb.note "Testing compilation"
  errs = {} #REVIEW stream reduce?
  cur = "init"
  urb.every />> (\/[ -~]+)/ ([_,path])-> cur := path
  urb.every ERROR, ->
    unless errs[cur]
      errs[cur] = true
      urb.warn "Compile error detected"
  #
  <- urb.line "|start %test" .then
  <- urb.line ":test [%cores /]" .then
  <- urb.expect-echo "%compilation-tested" .then
  errs := Object.keys errs
  if errs.length => throw Error "in #errs"
  urb.reset-listeners!
.then (urb)->
  urb.note "Running /===/tests"
  errs = "" #REVIEW stream reduce?
  urb.every /(\/[ -~]* (FAILED|CRASHED))/, ([_,result])->
    if !errs => urb.warn "First error"
    errs += "\n  #result"
  <- urb.line "+test, =defer |, =seed `@uvI`(shaz %reproducible)" .then
  <- urb.expect-echo "%ran-tests" .then
  if errs => throw Error errs
  urb.reset-listeners!
.then ->
  urbit.exit 0
.catch (err)->
  <- urbit.wait-silent!then # assumptions?
  urbit.warn "Test aborted:" err
  urbit.exit 1
