::
::::  /hoon/task/work/mar
  ::
/-    work
!:
::::
  ::
[work .]
|%
++  rend
  |=  a+(list $@(char dime))  ^-  cord
  %-  crip
  |-  ^-  tape
  ?~  a  ~
  ?@  i.a  [i.a $(a t.a)]
  (weld (scow i.a) $(a t.a))
::
++  indent  |=(a+wain (turn a |=(b+cord (cat 3 '  ' b))))
::
++  undent
  |*  {a+wain b+$+(wain *)}  ^+  [*b a]
  =^  c  a
    |-  ^-  {c+wain a+wain}
    ?~  a  [~ a]
    ?.  =('  ' (end 3 2 i.a))
      [~ a]
    [[- c] a]:[(rsh 3 2 i.a) $(a t.a)]
  [(b `wain`c) a]
::
++  keen  |*({a+* b+rule} |=(c+nail `(like a)`(b c)))
++  parse
  |*  {hed+$?($~ $@(@tas tape)) tal+(pole)}   
  ?~  hed  (..$ tal)
  ?^  hed  ;~(pfix (just (crip hed)) (..$ tal))
  =-  ?~(tal had ;~(plug had (..$ tal)))
  =<  had=(sear . nuck:so)
  |=  a+coin  ^-  (unit (odo:raid hed))
  ?.  &(?=({$$ @ @} a) =(hed p.p.a))  ~
  (some q.p.a)
::
++  advance
  |*  {a+wain b+rule}  ^+  [(wonk *b) a]
  ?~(a !! ~|(i.a [(rash i.a b) t.a]))
--
!:
::::
  ::
|_  taz+task
++  grab
  |%  ++  txt
    |=  a+wain  ^+  taz
    =+  ~[id=%uv "_" date-created=%da " " version=%ud date-modified=%da]
    =^  b  a  (advance a ;~(plug (parse -) (punt (parse " " %da ~))))
    =+  [-.b `date-due+(unit @da)`+.b]
    =^  tags   a  (undent a ~(gas in *(set cord)))
    =^  title  a  ?~(a !! a)
    =^  b  a  %+  advance  a
      ;~(plug (parse %p ~) (punt (parse ">" %p ~)) (punt (parse "X" %da ~)))
    =+  `{creator+@p doer+(unit @p) done+(unit @da)}`b
    =^  description  a  (undent a role)
    :*  id  date-created  version   date-modified  creator
        doer  tags  date-due  done  title  description  ::  XX done
        |-  ^-  (list comment)
        ?:  =(~ a)  ~
        =^  b  a  (advance a (parse ship=%p " " date=%da ~))
        =+  b
        =^  body  a  (undent a role)
        [[date ship body] $]
    ==
  --
++  grow
  |%
  ++  elem  ;pre: {(zing `wall`(turn (wash 0^120 >taz<) |=(a+tape ['\0a' a])))}
  ++  mime  [/text/x-task (taco (role txt))]
  ++  txt
    =+  taz
    =+  due=?~(date-due ~ ~[' ' da#u.date-due])
    :-  (rend uv#id '_' da#date-created ' ' ud#version da#date-modified due)
    %+  welp  (indent (sort (~(tap in tags)) aor))
    :-  title
    =+  do=[=-(?~(doer - ['>' p#u.doer -]) ?~(done ~ ~['X' da#u.done]))]
    :-  (rend p#creator do)
    %-  zing  ^-  (list wain)
    :-  (indent (lore description))
    %+  turn  discussion
    |=  comment  ^-  wain
    [(rend p#ship ' ' da#date ~) (indent (lore body))]
  --
++  grad  %txt
--
::  {id}_{date-created} {version}{date-modified}{|(" {date-due}" ~)}
::    {tag1}
::    {tag2}
::    ...
::  {title}
::  {creator}{|(">{doer}" ~)}{|("X{done}" ~)}
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
