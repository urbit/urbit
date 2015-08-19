::
::::  /hoon/command/work/mar
  ::
/-    *work
!:
::::
  ::
|_  mad=command
++  grab
  |%  ++  noun  command
      ++  json
    =>  [jo ..command]
    =<  (corl need (cu |=(a=command a) coma))
    |%
    ++  as
      :: |*(a=fist (cu sa (ar a)))  ::  XX  types
      |*  a=fist 
      %-  cu  :_  (ar a)
      ~(gas in *(set ,_(need *a)))
    ++  id  (ci (slat %uw) so)
    ++  ship  (su fed:ag)
    ++  coma  (of new/task old/(ot id/id dif/uppd ~) sort/(ar id) ~)
    ++  task
      %-  ot  :~
        id/id           date-created/di
        version/ni      date-modified/di
        owner/ship      status/(ci (soft status) so)
        tags/(as so)    due-date/(mu di)    title/so
        description/so  discussion/(ar (ot date/di ship/ship body/so ~))
      ==
    ++  uppd
      %-  of  :~
        own/(of announce/ul claim/ul ~)
        add/(of comment/(ot date/di body/so ~) ~)
        :-  %set
        %-  of  :~
          due-date/di
          title/so
          description/so
          tags/(as so)
          done/(mu di)
          audience/(as (ot ship/ship span/so ~))
        ==
      ==
    --
  --
++  grow
  |%
  ++  elem  ;pre: {(zing `wall`(turn (wash 0^120 >mad<) |=(a=tape ['\0a' a])))}
  --
--
:: {new: {
::   id:'0waoF.6Df9U.2-gc3.dXDP1',
::   date-created:1440011611215,
::   version:1,
::   date-modified:1440011611215,
::   owner:'fyr',
::   status:'gave',
::   tags:['tag'],
::   due-date:null,
::   title:'Test task',
::   description:'The converter owrks right?',
::   discussion:[{date:1440011611215,ship:'sondel',body:'hi'}]
:: } }
