{Urbit,ERROR} = require './runner.ls'

urbit = new Urbit <[-B urbit.pill -A .. -cFI zod zod]> 
Promise.resolve urbit
.then (urb)->
  urb.note "Booting urbit"
  Promise.race [
    urb.expect ERROR .then ->
      urb.warn "Error detected"
      throw Error "Stack trace while booting"
  , do
    <- urb.expect /dojo> / .then
    <- urb.expect-echo "%dojo-booted" .then
    urb.unpipe!
  ]
.then (urb)->
  urb.note "Testing compilation"
  # TODO tally ford stack traces
  #      urb.warn etc
  <- urb.line "|start %test" .then
  <- urb.line ":test [%cores /]" .then
  <- urb.expect-echo "%compilation-tested" .then
  #if tally => throw # a fit
  urb.unpipe!
.then (urb)->
  urb.note "Running /===/tests"
  # TODO tally FAILED and CRASHED
  <- urb.line "+test, =defer |, =seed `@uvI`(shaz %reproducible)" .then
  <- urb.expect-echo "%ran-tests" .then
  #if tally => throw # a fit
  urb.unpipe!
.then ->
  urbit.exit 0
.catch (err)->
  <- urbit.wait-silent!then # assumptions?
  urbit.warn "Test aborted:" err
  urbit.exit 1
