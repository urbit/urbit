require! \stream-snitch
pty = require \pty.js

term =
  # TODO abort on failure
  # XX use -s instead of travis wget'd pill
  pty.spawn './urbit' <[-B urbit.pill -A .. -cFI zod zod]> 
     .on \data -> process.stdout.write it
     
fin = no
term.pipe (new stream-snitch /dojo> /g).on \match ->
    return if fin
    fin := yes
    console.log "\n\n---\nnode: got dojo!\n---\n\n"
    set-timeout (-> process.exit 0), 1000 # should probably test further
    
term.pipe (new stream-snitch /ford: /g).on \match ->
    return if fin
    fin := yes
    console.log "\n\n---\nnode: detected error\n---\n\n"
    set-timeout (-> process.exit 1), 1000
    
set-timeout ...
  -> console.log "\n\n---\nnode: timed out after 5 min\n---"
  5*60000
