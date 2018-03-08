require! <[ stream-snitch once ]>
pty = require \pty.js

urbit =
  # TODO abort on failure
  pty.spawn 'urbit' <[-B urbit.pill -A .. -cFI zod zod]> 
     .on \data -> process.stdout.write it

console.log "FIXME Running Ubuntu 14.04, which causes a libtinfo version info warning. Should update to 16.04.\n"


urbit.on \exit (code)->
  console.log "\nnode: urbit exited with code #code\n"
  process.exit code

process.on \exit -> urbit.write '\04' # send EOF to gracefully checkpoint

exit-code = 0


on-next = (re,cb)->
  urbit.pipe (new stream-snitch re).on \match once cb
  
on-next /\r\x1b\[K(\/~|ford: )/ ->
  console.log "\n\n---\nnode: detected error, exiting in ~s30\n---\n\n"
  exit-code := 1
  set-timeout (-> process.exit 1), 30000

<- on-next /dojo> /

urbit.write "%got-dojo\r"
<- on-next /%got-dojo/


urbit.write "|start %test\r:test [%cores /]\r"
<- on-next /%cores-tested/

if exit-code
  process.exit exit-code

urbit.write "+test, =defer |, =seed `@uvI`(shaz %reproducible)\r"
on-next /(FAILED|CRASHED)/ ->
  console.log "\n\n---\nnode: detected error\n---\n\n"
  exit-code := 2

urbit.write "%tested\r"
<- on-next /%tested/


if exit-code
  process.exit exit-code

console.log "\n\n---\nnode: STUB insert further tests here\n---\n\n"

urbit.write '\04'
set-timeout (-> process.exit exit-code), 1000
