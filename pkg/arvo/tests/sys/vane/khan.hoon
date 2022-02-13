/+  *test
/=  khan-raw  /sys/vane/khan
=/  khan-gate  (khan-raw ~nul)
|%
++  test-khan-fyrd-start-args
  =^  born-moves  khan-gate
    %-  khan-call  :*
      khan-gate
      now=~1111.1.1
      scry=scry-provides-mark
      call-args=[duct=~[/initial-born-duct] ~ [%born ~]]
    ==
  =/  results-0  (expect-eq !>(~) !>(born-moves))
  =/  fyrd=(fyrd:khan (pair mark (cask)))  [%base %nonexistent %noun %noun ~]
  =/  now=@da  (add ~1111.1.1 ~s1)
  =/  =dais:clay  dais-noun
  =/  args
    :*  ~  `%'khan-fyrd--0vsome.ductt'  [~nul %base %da now]
        %nonexistent  (vale.dais ~)
    ==
  =^  start-moves  khan-gate
    %-  khan-call  :*
      khan-gate
      now
      scry=scry-provides-mark
      ^=  call-args
        :*  duct=~[//khan/1/0vsome.ductt]  ~
            %fyrd  fyrd
    ==  ==
  =/  results-1  (expect !>(=(1 (lent start-moves))))
  =/  mev  (head start-moves)
  =/  results-2
    %+  expect-eq
      !>  ~[//khan/1/0vsome.ductt]
      !>  p.mev
  =-  :(weld results-0 results-1 results-2 rest)
  ^=  rest
  ?.  ?=(%pass -.q.mev)  !!
  =/  results-3
    %+  expect-eq
      !>  /fyrd/~nul/base/da/~1111.1.1..00.00.01/noun
      !>  wire.q.mev
  =/  results-4  (expect-eq !>(%k) !>(-.note.q.mev))
  =-  :(weld results-3 results-4 rest)
  ^=  rest
  ?.  ?=(%fard +<.note.q.mev)  !!
  =/  fad  p.note.q.mev
  ;:  weld
    (expect-eq !>(%base) !>(bear.fad))
    (expect-eq !>(%nonexistent) !>(name.fad))
    (expect-eq !>(`~) args.fad)
  ==
++  test-khan-fard-start-args
  =^  born-moves  khan-gate
    %-  khan-call  :*
      khan-gate
      now=~1111.1.1
      scry=scry-provides-mark
      call-args=[duct=~[/initial-born-duct] ~ [%born ~]]
    ==
  =/  fard=(fyrd:khan vase)  [%base %nonexistent !>(~)]
  =/  now=@da  (add ~1111.1.1 ~s1)
  =/  args
    :*  ~  `%'khan-fyrd--0vthat.ductt'  [~nul %base %da now]
        %nonexistent  !>(~)
    ==
  =^  start-moves  khan-gate
    %-  khan-call  :*
      khan-gate
      now
      scry=scry-provides-mark
      ^=  call-args
        :*  duct=~[//khan/1/0vthat.ductt]  ~
            %fard  fard
    ==  ==
  =/  results-0  (expect !>(=(2 (lent start-moves))))
  =/  results-1
    %+  expect-eq
      !>  :*  ~[//khan/1/0vthat.ductt]
              %pass  //g  %g  %deal
              [~nul ~nul]  %spider  %watch
              /thread-result/'khan-fyrd--0vthat.ductt'
          ==
      !>  (head start-moves)
  =/  start-move  (rear start-moves)
  =/  results-2
    (expect !>(=(~[//khan/1/0vthat.ductt] p.start-move)))
  =-  :(weld results-0 results-1 results-2 rest)
  ^=  rest
  ?.  ?=(%pass -.q.start-move)
    ~&  >>>  [exp+%pass act+-.q.start-move]
    (expect !>(|))
  ?.  =(//g wire.q.start-move)
    ~&  >>>  [exp+//g act+wire.q.start-move]
    (expect !>(|))
  ?.  ?=(%deal +<.note.q.start-move)
    ~&  >>>  [exp+%deal [%act +<.note.q.start-move]]
    (expect !>(|))
  ?.  =([~nul ~nul] p.note.q.start-move)
    ~&  >>>  [exp+[~nul ~nul] act+p.note.q.start-move]
    (expect !>(|))
  ?.  =(%spider q.note.q.start-move)
    ~&  >>>  [exp+%spider act+q.note.q.start-move]
    (expect !>(|))
  ?.  ?=(%poke -.r.note.q.start-move)
    ~&  >>>  [exp+%poke act+-.r.note.q.start-move]
    (expect !>(|))
  ?.  ?=(%spider-start p.cage.r.note.q.start-move)
    ~&  >>>  [exp+%spider-start act+p.cage.r.note.q.start-move]
    (expect !>(|))
  =/  args
    ::  XX  $start-args in %/app/spider/hoon
    ::
    !<  [p=(unit @ta) q=(unit @ta) r=beak s=term t=vase]
    q.cage.r.note.q.start-move
  ?.  =(~ p.args)
    ~&  >>>  bad-par+p.args
    (expect !>(|))
  ?.  =(`'khan-fyrd--0vthat.ductt' q.args)
    ~&  >>>  bad-tid+q.args
    (expect !>(|))
  ?.  =([~nul %base %da now] r.args)
    ~&  >>>  bad-beak+r.args
    (expect !>(|))
  ?.  =(%nonexistent s.args)
    ~&  >>>  bad-name+s.args
    (expect !>(|))
  (expect-eq !>(~) t.args)
++  test-khan-take-full-run-fard
  =^  born-moves  khan-gate
    %-  khan-call  :*
      khan-gate
      now=~1111.1.1
      scry=scry-provides-mark
      call-args=[duct=~[/a] ~ [%born ~]]
    ==
  =/  results-0  (expect-eq !>(~) !>(born-moves))
  =/  fard=(fyrd:khan vase)  [%base %fake !>(~)]
  =^  start-moves  khan-gate
    %-  khan-call  :*
      khan-gate
      now=(add ~1111.1.1 ~s1)
      scry=scry-provides-mark
      ^=  call-args
        :*  duct=~[//khan/2/0v0]  ~
            %fard  fard
    ==  ==
  =^  watch-ack-moves  khan-gate
    %-  khan-take  :*
      khan-gate
      now=(add ~1111.1.1 ~s2)
      scry=scry-provides-mark
      ^=  take-args
        wire=//g
        duct=~[//khan/2/0v0]
        dud=~
        [%gall %unto %watch-ack ~]
    ==
  =^  poke-ack-moves  khan-gate
    %-  khan-take  :*
      khan-gate
      now=(add ~1111.1.1 ~s3)
      scry=scry-provides-mark
      ^=  take-args
        wire=//g
        duct=~[//khan/2/0v0]
        dud=~
        [%gall %unto %poke-ack ~]
    ==
  =^  thread-done-moves  khan-gate
    %-  khan-take  :*
      khan-gate
      now=(add ~1111.1.1 ~s4)
      scry=scry-provides-mark
      ^=  take-args
        wire=//g
        duct=~[//khan/2/0v0]
        dud=~
        [%gall %unto %fact %thread-done !>(%res)]
    ==
  =^  kick-moves  khan-gate
    %-  khan-take  :*
      khan-gate
      now=(add ~1111.1.1 ~s5)
      scry=scry-provides-mark
      ^=  take-args
        wire=//g
        duct=~[//khan/2/0v0]
        dud=~
        [%gall %unto %kick ~]
    ==
  =/  all-take-moves
    ;:  weld
      watch-ack-moves
      poke-ack-moves
      thread-done-moves
      kick-moves
    ==
  =/  results-1
    %-  expect  !>(=(1 (lent all-take-moves)))
  =/  results-2
    %+  expect-eq
      !>([~[//khan/2/0v0] %give %arow %& !>(%res)])
      !>((head all-take-moves))
  :(weld results-0 results-1 results-2)
++  test-khan-take-full-run-fyrd
  =^  born-moves  khan-gate
    %-  khan-call  :*
      khan-gate
      now=~1111.1.1
      scry=scry-provides-mark
      call-args=[duct=~[/a] ~ [%born ~]]
    ==
  =^  fyrd-moves  khan-gate
    %-  khan-call  :*
      khan-gate
      now=(add ~1111.1.1 ~s1)
      scry=scry-provides-mark
      ^=  call-args
        duct=~[//khan/0v0/1/0v2]  ~
        %fyrd  [%base %fake %noun %noun ~]
    ==
  =/  results-0  (expect !>(=(1 (lent fyrd-moves))))
  =/  fard-move  (head fyrd-moves)
  ?>  ?=(%pass -.q.fard-move)
  ?>  ?=(%k -.note.q.fard-move)
  =*  wir  wire.q.fard-move
  ::  don't assert the structure of wir; we leave that up to
  ::  %khan as an implementation detail.
  ::
  =/  =duct  [wir ~[//khan/0v0/1/0v2]]
  =^  fard-moves  khan-gate
    %-  khan-call  :*
      khan-gate
      now=(add ~1111.1.1 ~s2)
      scry=scry-provides-mark
      ^=  call-args
        duct=duct  ~
        +.note.q.fard-move
    ==
  ::  we do not simulate the inner gall moves here, which might
  ::  cause weirdness if the implementation becomes stateful.
  ::
  =^  arow-moves  khan-gate
    %-  khan-take  :*
      khan-gate
      now=(add ~1111.1.1 ~s3)
      scry=scry-provides-mark
      ^=  take-args
        wire=wir
        duct=~[//khan/0v0/1/0v2]
        dud=~
        [%khan %arow %& !>(%res)]
    ==
  =/  results-1  (expect !>(=(1 (lent arow-moves))))
  =/  row  (head arow-moves)
  =/  results-2
    %+  expect-eq
      !>(~[//khan/0v0/1/0v2])
      !>(p.row)
  =/  results-3
    %+  expect-eq
      !>([%give %avow %& %noun %res])
      !>(q.row)
  :(weld results-0 results-1 results-2 results-3)
::  remaining cases to test:
::    {%fard, %fyrd} x
::    {dud, watch-ack fail, poke-ack fail, thread-fail}
::  watch-ack, poke-ack, thread-fail should all produce negative
::  arow/avow. hen is dud taken? what should it produce?
::
++  khan-call
  |=  $:  khan-gate=_khan-gate
          now=@da
          scry=roof
          $=  call-args
            $:  =duct
                dud=(unit goof)
                wrapped-task=(hobo task:khan)
      ==    ==
  ^-  [(list move:khan-gate) _khan-gate]
  =/  khan-core
    (khan-gate now eny=`@uvJ`0xdead.beef scry=scry)
  (call:khan-core [duct dud wrapped-task]:call-args)
++  khan-take
  |=  $:  khan-gate=_khan-gate
          now=@da
          scry=roof
          $=  take-args
            $:  =wire
                =duct
                dud=(unit goof)
                =sign:khan-gate
      ==    ==
  ^-  [(list move:khan-gate) _khan-gate]
  =/  khan-core
    (khan-gate now eny=`@uvJ`0xdead.beef scry=scry)
  (take:khan-core [wire duct dud sign]:take-args)
++  dais-noun  ^-  dais:clay
  |_  sam=vase
  ++  diff  !!
  ++  form  !!
  ++  join  !!
  ++  mash  !!
  ++  pact  !!
  ++  vale  |=(=noun !>(;;(^noun noun)))
  --
++  scry-provides-mark  ^-  roof
  |=  [gang =view =beam]
  ^-  (unit (unit cage))
  ?:  &(=(%cb view) =(/noun s.beam))
    :^  ~  ~  %dais
    !>  ^-  dais:clay
    dais-noun
  ~
--
