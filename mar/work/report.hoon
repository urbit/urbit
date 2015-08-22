::
::::  /hoon/report/work/mar
  ::
/-    *work
!:
::::
  ::
|_  client
++  grow  
|%  ++  json
  =+  jope=|=(a=ship [%s (rsh 3 1 (scot %p a))])
  %-  jobe  :~
         sort/[%a (turn sort |=(a=@uv [%s (scot %uv a)]))]
    =<  tasks/(jobe (turn (~(tap by tasks)) .))
    |=  [@ client-task]
    =+  tax
    :-  (scot %uv id)
    %-  jobe  :~  id/[%s (scot %uv id)]
                tags/[%a (turn (~(tap in tags)) |=(a=cord s/a))]
               owner/(jope owner)
               title/[%s title]
              status/[%s status]
             version/(jone version)
            claiming/[%b claiming]
        =<  audience/[%a (turn (~(tap in audience)) .)]
        |=(a=station:talk [%s (crip "{<p.a>}/{(trip q.a)}")])
      'date_created'^(jode date-created)
     'date_modified'^(jode date-modified)
         description/[%s description]
      =<  discussion/[%a (turn discussion .)]
      |=(comment (jobe date/(jode date) ship/(jope ship) body/[%s body] ~))
          'date_due'^?~(date-due ~ (jode u.date-due))
                done/?~(done ~ (jode u.done))
    ==
  ==
--  --
::  {id}_{date-created} {version}{date-modified}{|(" {date-due}" ~)}
::    {tag1}
::    {tag2}
::    ...
::  {title}
::  {owner}.{status}
::    {description}
::    {more description}
::  {ship1} {date}
::    {comment}
::    {more comment}
::    {more comment}
::  {ship2} {date}
::    {comment}
::    {more comment}
::    {more comment}
