require! \stream-snitch
pty = require \pty.js

term =
  # XX use -s instead of travis wget'd pill
  pty.spawn './urbit' <[-B urbit.pill -A .. -cFI zod zod]> 
     .on \data -> process.stdout.write it
     
term.pipe (new stream-snitch /dojo> /g).on \match ->
    console.log "\n\n---\nnode: got dojo!\n---"
    set-timeout -> process.exit 0 # should probably test further
    
term.pipe (new stream-snitch /ford: /g).on \match ->
    console.log "\n\n---\nnode: detected error\n---"
    set-timeout -> process.exit 1
    
set-timeout ...
  -> console.log "\n\n---\nnode: timed out after 5 min\n---"
  5*60000
