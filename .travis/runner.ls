require! <[ stream-snitch colors escape-string-regexp ]>
pty = require \pty.js

export class Urbit
  (args)->
    @stdout = process.stdout # overridable
    @pty = pty.spawn \urbit args
    @pty.on \data ~> @stdout.write it # TODO pipe?
    console.log "FIXME Running Ubuntu 14.04, which causes a libtinfo version info warning. Should update to 16.04."
    #
    @last-output = Date.now()
    @pty.on \data ~> @last-output = Date.now()
    #
    process.on \exit ~> @pty.write '\04' # send EOF to gracefully checkpoint
  #
  note: (...args)-> console.log "\nnode:".blue, ...args
  warn: (...args)-> console.log "\nnode:".red, ...args
  wait-silent: ~> # this feels hacky
    new Promise (resolve)~>
      a = set-interval ~>
        if Date.now! > @last-output + 2000
          clear-interval a
          resolve @last-output
      , 200
  expect: (re)~>
    new Promise (resolve)~>
      #@listeners.push 
      @pty.pipe (new stream-snitch re).once "match" resolve
  expect-error: -> @expect /(ford: |\r\x1b\[K\/~)/ #ALT if-error
  expect-immediate: (re)->
    Promise.race [
      @expect re
      @wait-silent!then -> throw Error "Expected #re during event"
    ]
  #
  line: ->
    @pty.write it
    <~ @wait-silent!then
    @stdout.write "\n"
    @pty.write "\r"
  #
  expect-echo: (s)-> #ALT send-and-expect
    <~ @line s .then
    @expect-immediate new RegExp escape-string-regexp s
  #
  exit: (code)->
    @pty.on \exit -> process.exit code #REVIEW just return promise?
    # @pty.write "\03"    # ^C running event
    @pty.write "\05\25" # ^E^U to clear prompt
    @pty.write "\04"    # ^D to checkpoint
    # set-timeout # hard exit
