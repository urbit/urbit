{Urbit} = require './runner.ls'

urbit = new Urbit <[-B urbit.pill -A .. -cFI zod zod]> 
Promise.resolve urbit
.then (urb)->
  urb.note "Booting urbit"
  # TODO exit on ford stack trace
  <- urb.expect /dojo> / .then
  <- urb.expect-echo "%dojo-booted" .then
  urb
.then (urb)->
  urb.note "Testing compilation"
  # TODO tally ford stack traces
  #      urb.warn etc
  <- urb.line "|start %test" .then
  <- urb.line ":test [%cores /]" .then
  <- urb.expect-echo "%compilation-tested" .then
  #if tally => throw # a fit
  urb
.then (urb)->
  urb.note "Running /===/tests"
  # TODO tally FAILED and CRASHED
  <- urb.line "+test, =defer |, =seed `@uvI`(shaz %reproducible)" .then
  <- urb.expect-echo "%ran-tests" .then
  #if tally => throw # a fit
  urb
.then ->
  urbit.exit 0
.catch (err)->
  <- urbit.wait-silent!then # assumptions?
  urbit.warn err
  urbit.exit 1
