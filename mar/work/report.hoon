::
::::  /hoon+report+work+mar
  ::
/-    work
!:
::::
  ::
[work .]
|_  client
++  grow  
|%  ++  json
  =+  jope=|=(a/ship [%s (rsh 3 1 (scot %p a))])
  %-  jobe  :~
         sort+[%a (turn sort |=(a/@uv [%s (scot %uv a)]))]
    =<  tasks+(jobe (turn (~(tap by tasks)) .))
    |=  {@ client-task}
    =+  tax
    :-  (scot %uv id)
    %-  jobe  :~  id+[%s (scot %uv id)]
                tags+[%a (turn (^sort (~(tap in tags)) aor) |=(a/cord s+a))]
                doer+?~(doer ~ (jope u.doer))
               title+[%s title]
             creator+(jope creator)
             version+(jone version)
            archived+[%b archived]
        =<  audience+[%a (turn (~(tap in audience)) .)]
        |=(a/station:talk [%s (crip "{<p.a>}/{(trip q.a)}")])
      'date_created'^(jode date-created)
     'date_modified'^(jode date-modified)
         description+[%s description]
      =<  discussion+[%a (turn discussion .)]
      |=(comment (jobe date+(jode date) ship+(jope ship) body+[%s body] ~))
          'date_due'^?~(date-due ~ (jode u.date-due))
                done+?~(done ~ (jode u.done))
    ==
  ==
--  --
::  sort: ["0v111id" ...]
::  tasks: [ {
::      id:"0v111id"
::      tags:["str" ...]
::      doer:|("~ship" null)
::      title:"str"
::      creator:"~ship"
::      version:12345
::      archived:false
::      audience:["~ship+chan" ...]
::      date_created:1262304000000
::      date_modified:1262304000000
::      description:"str"
::      discussion:[{date:1262304000000 ship:"~ship" body:"str"} ...]
::      date_due:?(1262304000000 null)
::      done:?(1262304000000 null)
::    }
::  ...]
