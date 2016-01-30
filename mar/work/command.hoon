::
::::  /hoon+command+work+mar
  ::
/-    work
!:
::::
  ::
[work .]
|_  mad/command
++  grab
  |%  ++  noun  command
      ++  json
    =>  [jo ..command]
    =<  (corl need (cu |=(a/command a) coma))
    |%
    ++  as
      :: |*(a/fist (cu sa (ar a)))  ::  XX  types
      |*  a/fist 
      %-  cu  :_  (ar a)
      ~(gas in *(set _(need *a)))
    ++  ot
      |*  a/(pole {@tas fist})
      |=  b/json
      %.  ((^ot a) b)
      %-  slog
      ?+  b  ~
          {$o *} 
        %+  murn  `(list {@tas fist})`a
        |=  {c/term d/fist}  ^-  (unit tank)
        =+  (~(get by p.b) c)
        ?~  -  (some >[c (turn (~(tap by p.b)) head)]<)
        =+  (d u)
        ?~  -  (some >[c u]<)
        ~
      ==
    ++  of
      |*  a/(pole {@tas fist})
      |=  b/json
      %.  ((of:jo a) b)
      %-  slog
      ?+  b  ~
          {$o *} 
        %+  murn  `(list {@tas fist})`a
        |=  {c/term d/fist}  ^-  (unit tank)
        =+  (~(get by p.b) c)
        ?~  -  ~
        =+  (d u)
        ?~  -  (some >[c u]<)
        ~
      ==
    ++  id  (ci (slat %uv) so)
    ++  ship  (su fed:ag)
    ++  coma
      %-  of  :~
        new+task      old+(ot id+id dif+uppd ~)
        sort+(ar id)
      ==
    ++  task
      %-  ot  :~
        ::index+ni
        audience+audi
        id+id           'date_created'^di
        version+ni      'date_modified'^di
        creator+ship    doer+(mu ship)
        tags+(as so)    'date_due'^(mu di)
        done+(mu di)    title+so
        description+so  discussion+(ar (ot date+di ship+ship body+so ~))
      ==
    ++  audi  (as stan)
    ++  stan  (su ;~((glue fas) ;~(pfix sig fed:ag) urs:ab))
    ++  uppd
      %-  of  :~
        doer+(of release+ul claim+ul ~)
        add+(of comment+so ~)
        :-  %set
        %-  of  :~
          audience+audi
          date-due+(mu di)
          title+so
          description+so
          tags+(as so)
          done+bo
        ==
      ==
    --
  --
++  grow
  |%
  ++  elem  ;pre: {(zing `wall`(turn (wash 0^120 >mad<) |=(a/tape ['\0a' a])))}
  --
--
:: {new: {
::   id:'0vaof.6df9u.2agc3.d0dp1',
::   date-created:1440011611215,
::   version:1,
::   date-modified:1440011611215,
::   creator:'fyr',
::   tags:['tag'],
::   date-due:null,
::   done:false,
::   title:'Test task',
::   description:'The converter owrks right?',
::   discussion:[{date:1440011611215,ship:'sondel',body:'hi'}]
:: } }
