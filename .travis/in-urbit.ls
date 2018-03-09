require! <[ split ]>
{through} = require 'promise-streams'
{Urbit,ERROR} = require './runner.ls'

urbit = new Urbit process.argv[2 to]

urbit.expect ERROR .then -> process.exit 1

<- urbit.expect /dojo> / .then

process.stdin.pipe split!
.pipe through -> 
  urbit.line it.trim!replace /\$[a-zA-Z0-9_]+/g ->
    process.env[it.slice 1] ? '__unknown-var__'
.wait!then -> urbit.exit 0
