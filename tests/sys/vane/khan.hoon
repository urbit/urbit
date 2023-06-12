::  remaining cases to test:
::    call dud
::    take dud
::  TODO  when can dud happen?
::
/+  *test
/=  khan-raw  /sys/vane/khan
=/  khan-gate  (khan-raw ~nul)
|%
++  test-khan-fyrd-start-args
  =^  born-moves  khan-gate
    %-  khan-call  :*
      khan-gate
      now=~1162.1.1
      scry=scry-provides-mark
      call-args=[duct=~[/initial-born-duct] ~ [%born ~]]
    ==
  =/  results-0  (expect-eq !>(~) !>(born-moves))
  =/  fyrd=(fyrd:khan cast:khan)  [%base %nonexistent %noun %noun ~]
  =/  now=@da  (add ~1162.1.1 ~s1)
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
  ?>  ?=(%pass -.q.mev)
  =/  results-3
    %+  expect-eq
      !>  /fyrd/~nul/base/~1162.1.1..00.00.01/noun
      !>  wire.q.mev
  =/  results-4  (expect-eq !>(%k) !>(-.note.q.mev))
  ?>  ?=(%home +<.note.q.mev)
  ?>  ?=(%fard -.task.note.q.mev)
  =/  fad  p.task.note.q.mev
  ;:  weld
    results-0  results-1  results-2
    results-3  results-4
    (expect-eq !>(%base) !>(bear.fad))
    (expect-eq !>(%nonexistent) !>(name.fad))
    (expect-eq !>(%noun) !>(p.args.fad))
    (expect-eq !>(`~) q.args.fad)
  ==
++  test-khan-fard-start-args
  =^  born-moves  khan-gate
    %-  khan-call  :*
      khan-gate
      now=~1162.1.1
      scry=scry-provides-mark
      call-args=[duct=~[/initial-born-duct] ~ [%born ~]]
    ==
  =/  fard=(fyrd:khan cage)  [%base %nonexistent %noun !>(~)]
  =/  now=@da  (add ~1162.1.1 ~s1)
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
  ::  XX  overspecified
  ::
  =/  expected-tid  (cat 3 'khan-fyrd--' (scot %uv (sham 0xdead.beef)))
  =/  results-1
    %+  expect-eq
      !>  :*  ~[//khan/1/0vthat.ductt]
              %pass  //g  %g  %deal
              [~nul ~nul /khan]  %spider  %watch
              /thread-result/[expected-tid]
          ==
      !>  (head start-moves)
  =/  mev  (rear start-moves)
  =/  results-2  (expect-eq !>(~[//khan/1/0vthat.ductt]) !>(p.mev))
  ?>  ?=(%pass -.q.mev)
  =/  results-3  (expect-eq !>(//g) !>(wire.q.mev))
  =*  not  note.q.mev
  =/  results-4  (expect-eq !>(%g) !>(-.not))
  ?>  ?=(%deal +<.not)
  =/  results-5  (expect-eq !>([~nul ~nul /khan]) !>(p.not))
  =/  results-6  (expect-eq !>(%spider) !>(q.not))
  ?>  ?=(%poke -.r.not)
  =*  cag  cage.r.not
  ?>  ?=(%spider-start p.cag)
  =/  rag
    ::  XX  $start-args in %/app/spider/hoon
    ::
    !<  [p=(unit @ta) q=(unit @ta) r=beak s=term t=vase]
    q.cag
  =/  results-7
    %+  expect-eq
      !>  :*  ~  `expected-tid
              [~nul %base %da now]  %nonexistent  ~
          ==
      !>(rag(t ~))
  =/  results-8  (expect-eq !>(~) t.rag)
  ;:  weld
    results-0  results-1  results-2
    results-3  results-4  results-5
    results-6  results-7  results-8
  ==
++  test-khan-take-full-run-fard
  =^  born-moves  khan-gate
    %-  khan-call  :*
      khan-gate
      now=~1162.1.1
      scry=scry-provides-mark
      call-args=[duct=~[/a] ~ [%born ~]]
    ==
  =/  results-0  (expect-eq !>(~) !>(born-moves))
  =/  fard=(fyrd:khan cage)  [%base %fake %noun !>(~)]
  =^  start-moves  khan-gate
    %-  khan-call  :*
      khan-gate
      now=(add ~1162.1.1 ~s1)
      scry=scry-provides-mark
      ^=  call-args
        :*  duct=~[//khan/2/0v0]  ~
            %fard  fard
    ==  ==
  =^  take-moves  khan-gate
    %-  khan-take-all  :*
      khan-gate  now=~1162.1.2  sep=~s1  scry=scry-provides-mark
      :~  [//g ~[//khan/2/0v0] ~ %gall %unto %watch-ack ~]
          [//g ~[//khan/2/0v0] ~ %gall %unto %poke-ack ~]
          [//g ~[//khan/2/0v0] ~ %gall %unto %fact %thread-done !>(%res)]
          [//g ~[//khan/2/0v0] ~ %gall %unto %kick ~]
      ==
    ==
  =/  results-1
    %-  expect  !>(=(1 (lent take-moves)))
  =/  results-2
    %+  expect-eq
      !>([~[//khan/2/0v0] %give %arow %& %noun !>(%res)])
      !>((head take-moves))
  :(weld results-0 results-1 results-2)
++  test-khan-multi-fard
  =^  born-moves  khan-gate
    %-  khan-call  :*
      khan-gate
      now=~1162.1.1
      scry=scry-provides-mark
      call-args=[duct=~[/a] ~ [%born ~]]
    ==
  =/  fard=(fyrd:khan cage)  [%base %fake %noun !>(~)]
  =/  khan-call-args  :*
    now=(add ~1162.1.1 ~s1)
    scry=scry-provides-mark
    ^=  call-args  :*
      duct=~[//khan/2/0va]  ~  %fard  fard
    ==
  ==
  =^  start-1-moves  khan-gate
    %-  khan-call  :*
      khan-gate
      khan-call-args
    ==
  =^  start-2-moves  khan-gate
    %-  khan-call  :*
      khan-gate
      khan-call-args
    ==
  =/  results-1
    %+  expect-spider-start-tid
      'khan-fyrd--0vir6kv.ci3nm.a8rcs.kua3e.9sp7o'
    start-1-moves
  =/  results-2
    %+  expect-spider-start-tid
      'khan-fyrd--0v4.la9d1.uc5cu.ngv3f.pbo8a.mlc5f'
    start-2-moves
  (weld results-1 results-2)
++  test-khan-take-full-run-fyrd
  =^  born-moves  khan-gate
    %-  khan-call  :*
      khan-gate
      now=~1162.1.1
      scry=scry-provides-mark
      call-args=[duct=~[/a] ~ [%born ~]]
    ==
  =^  fyrd-moves  khan-gate
    %-  khan-call  :*
      khan-gate
      now=(add ~1162.1.1 ~s1)
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
  ::  XX may erroneously break if %khan keeps state dependent on
  ::  its inner %fard.
  ::
  =^  arow-moves  khan-gate
    %-  khan-take  :*
      khan-gate
      now=(add ~1162.1.1 ~s3)
      scry=scry-provides-mark
      ^=  take-args
        wire=wir
        duct=~[//khan/0v0/1/0v2]
        dud=~
        [%khan %arow %& %noun !>(%res)]
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
++  test-khan-fard-watch-ack-fail
  =^  born-moves  khan-gate
    %-  khan-call-all  :*
      khan-gate  now=~1162.1.1  sep=~s1  scry=scry-provides-mark
      :~  [~[/a] ~ %born ~]
          [~[//khan/0v0/1/0v0] ~ %fard %base %hi %noun %noun ~]
      ==
    ==
  =^  watch-ack-moves  khan-gate
    %-  khan-take  :*
      khan-gate  now=~1162.1.2  scry=scry-provides-mark
      ^=  take-args
        //g  ~[//khan/0v0/1/0v0]  ~
        %gall  %unto  %watch-ack  `~['fail']
    ==
  =/  results-0  (expect !>(=(1 (lent watch-ack-moves))))
  =/  mev  (head watch-ack-moves)
  =/  results-1
    %+  expect-eq
      !>([~[//khan/0v0/1/0v0] %give %arow %| %watch-ack ~['fail']])
      !>(mev)
  (weld results-0 results-1)
++  test-khan-fard-poke-ack-fail
  =^  call-moves  khan-gate
    %-  khan-call-all  :*
      khan-gate  now=~1162.1.1  sep=~s1  scry=scry-provides-mark
      :~  [~[/a] ~ %born ~]
          [~[//khan/0v0/1/0v0] ~ %fard %base %hi %noun %noun ~]
      ==
    ==
  =^  take-moves  khan-gate
    %-  khan-take-all  :*
      khan-gate  now=~1162.1.2  sep=~s1  scry=scry-provides-mark
      :~  [//g ~[//khan/0v0/1/0v0] ~ %gall %unto %watch-ack ~]
          :*  //g  ~[//khan/0v0/1/0v0]  ~
              %gall  %unto  %poke-ack  `~['fail']
          ==
          [//g ~[//khan/0v0/1/0v0] ~ %gall %unto %kick ~]
      ==
    ==
  =/  results-0  (expect !>(=(1 (lent take-moves))))
  =/  mev  (head take-moves)
  =/  results-1
    %+  expect-eq
      !>([~[//khan/0v0/1/0v0] %give %arow %| %poke-ack ~['fail']])
      !>(mev)
  (weld results-0 results-1)
++  test-khan-fard-thread-fail
  =^  call-moves  khan-gate
    %-  khan-call-all  :*
      khan-gate  now=~1162.1.1  sep=~s1  scry=scry-provides-mark
      :~  [~[/a] ~ %born ~]
          [~[//khan/0v0/1/0v0] ~ %fard %base %hi %noun %noun ~]
      ==
    ==
  =^  take-moves  khan-gate
    %-  khan-take-all  :*
      khan-gate  now=~1162.1.2  sep=~s1  scry=scry-provides-mark
      :~  [//g ~[//khan/0v0/1/0v0] ~ %gall %unto %watch-ack ~]
          [//g ~[//khan/0v0/1/0v0] ~ %gall %unto %poke-ack ~]
          :*  //g  ~[//khan/0v0/1/0v0]  ~
              %gall  %unto  %fact  %thread-fail
              !>([%woops ~['fail']])
          ==
          [//g ~[//khan/0v0/1/0v0] ~ %gall %unto %kick ~]
      ==
    ==
  =/  results-0  (expect !>(=(1 (lent take-moves))))
  =/  mev  (head take-moves)
  =/  results-1
    %+  expect-eq
      !>  :*  ~[//khan/0v0/1/0v0]  %give
              %arow  %|  %thread-fail  ~['woops' 'fail']
          ==
      !>(mev)
  (weld results-0 results-1)
++  test-khan-fyrd-arow-fail
  =^  call-moves  khan-gate
    %-  khan-call-all  :*
      khan-gate  now=~1162.1.1  sep=~s1  scry=scry-provides-mark
      :~  [~[/a] ~ %born ~]
          [~[//khan/0v0/1/0v0] ~ %fyrd %base %a %noun %noun ~]
      ==
    ==
  =/  results-0  (expect !>(=(1 (lent call-moves))))
  =/  fard-move  (head call-moves)
  ?>  ?=(%pass -.q.fard-move)
  =*  wir  wire.q.fard-move
  =^  arow-moves  khan-gate
    %-  khan-take  :*
      khan-gate  now=~1162.1.2  scry=scry-provides-mark
      ^=  take-args
        wir  ~[//khan/0v0/1/0v0]  ~
        %khan  %arow  %|  %watch-ack  ~['fail']
    ==
  =/  results-1  (expect !>(=(1 (lent arow-moves))))
  =/  mev  (head arow-moves)
  =/  results-2
    %+  expect-eq
      !>([~[//khan/0v0/1/0v0] %give %avow %| %watch-ack ~['fail']])
      !>(mev)
  :(weld results-0 results-1 results-2)
++  test-khan-fyrd-no-input-mark
  =^  born-moves  khan-gate
    %-  khan-call  :*
      khan-gate
      ~1162.1.1
      scry-provides-mark
      ~[/a]  ~  %born  ~
    ==
  %-  expect-fail
  |.
  %-  khan-call  :*
    khan-gate
    (add ~1162.1.1 ~s1)
    scry-provides-mark
    ~[//khan/0v0/1/0v0]  ~
    %fyrd  %base  %a  %noun  %bad-mark  ~
  ==
++  test-khan-fyrd-no-output-mark
  =^  call-moves  khan-gate
    %-  khan-call-all  :*
      khan-gate  ~1162.1.1  ~s1  scry-provides-mark
      :~  [~[/a] ~ %born ~]
          [~[//khan/0v0/1/0v0] ~ %fyrd %base %a %bad-mark %noun ~]
      ==
    ==
  %-  expect-fail
  |.
  %-  khan-take  :*
    khan-gate
    ~1162.1.2
    scry-provides-mark
    /fyrd/~nul/base/da/~1162.1.1..00.00.01/bad-mark
    ~[//khan/0v0/1/0v0]  ~
    [%khan %arow %& %noun !>(~)]
  ==
++  expect-spider-start-tid
  |=  [tid=@ta mev=(list move:khan-gate)]
  ?>  ?=([^ ^ ~] mev)
  =*  watch-move  i.mev
  =*  start-move  i.t.mev
  ?>  ?=([* %pass * %g %deal * %spider %watch *] watch-move)
  =/  results-1
    %+  expect-eq
      !>(/thread-result/[tid])
      !>(path.r.note.q.watch-move)
  ?>  ?=([* %pass * %g %deal * %spider %poke %spider-start *] start-move)
  =/  start-args
    !<  [p=(unit @ta) q=(unit @ta) r=beak s=term t=vase]
    q.cage.r.note.q.start-move
  =/  results-2
    %+  expect-eq
      !>  `tid
      !>  q.start-args
  (weld results-1 results-2)
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
++  khan-call-all
  |=  $:  khan-gate=_khan-gate
          now=@da
          sep=@dr
          scry=roof
          call-list=(list [p=duct q=(unit goof) r=(hobo task:khan)])
      ==
  ^-  [(list move:khan-gate) _khan-gate]
  =+  i=0
  =/  mev=(list move:khan-gate)  ~
  |-
  ?~  call-list  [mev khan-gate]
  =^  mov  khan-gate
    %-  khan-call  :*
      khan-gate
      now=(add now (mul sep i))
      scry=scry
      call-args=i.call-list
    ==
  $(i +(i), call-list t.call-list, mev (weld mev mov))
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
++  khan-take-all
  |=  $:  khan-gate=_khan-gate
          now=@da
          sep=@dr
          scry=roof
          take-list=(list [p=wire q=duct r=(unit goof) s=sign:khan-gate])
      ==
  ^-  [(list move:khan-gate) _khan-gate]
  =+  i=0
  =/  mev=(list move:khan-gate)  ~
  |-
  ?~  take-list  [mev khan-gate]
  =^  mov  khan-gate
    %-  khan-take  :*
      khan-gate
      now=(add now (mul sep i))
      scry=scry
      take-args=i.take-list
    ==
  $(i +(i), take-list t.take-list, mev (weld mev mov))
++  dais-noun  ^-  dais:clay
  |_  sam=vase
  ++  diff  !!
  ++  form  !!
  ++  join  !!
  ++  mash  !!
  ++  pact  !!
  ++  vale  |=(=noun !>(;;(^noun noun)))
  --
++  tube-noun-noun  ^-  tube:clay
  |=  =vase
  !>(!<(noun vase))
++  scry-provides-mark  ^-  roof
  |=  [gang pov=path =view =beam]
  ^-  (unit (unit cage))
  ?:  &(=(%cb view) =(/noun s.beam))
    :^  ~  ~  %dais
    !>  ^-  dais:clay
    dais-noun
  ?:  &(=(%cc view) =(/noun/noun s.beam))
    :^  ~  ~  %tube
    !>  ^-  tube:clay
    tube-noun-noun
  ~
--
