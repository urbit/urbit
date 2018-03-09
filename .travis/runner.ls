require! <[ stream-snitch ]>
pty = require \pty.js

export class Urbit
  (args)->
    @stdout = process.stdout # overridable
    @pty = pty.spawn \urbit args
    @pty.on \data ~> @stdout.write it # TODO pipe?
    #
    @last-output = Date.now()
    @pty.on \data ~> @last-output = Date.now()
  #
  wait-silent: ~> # this feels hacky
    new Promise (resolve)~>
      a = set-interval ~>
        if Date.now! > @last-output + 1000
          clear-interval a
          resolve @last-output
      , 200
  expect: (re)~>
    new Promise (resolve)~>
      @pty.pipe (new stream-snitch re).once "match" resolve
  expect-error: -> @expect /\r\x1b\[K(\/~|ford: )/
  #
  line: ->
    @pty.write it
    <~ @wait-silent!then
    @stdout.write "\n"
    @pty.write "\r"
  #
  exit: (code)->
    @pty.on \exit -> process.exit code #REVIEW just return promise?
    @pty.write "\05\25" # ^E^U to clear prompt
    @pty.write "\04"    # ^D to checkpoint
