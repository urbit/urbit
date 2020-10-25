|%
++  tide                                                :: init or compiler
  |=  {typ/?(%pill type) txt/@}  ^-  *
  ?.  ?=(%pill typ)  (ride typ txt)
  (textual:bootstrap /pill txt)
::
++  bootstrap                                           :: glass pill system: hoon
  |%
  ++  get  |=([a=@u b=@t] (cut 3 [a 1] b))
  ++  get-line
    |=  [a=@u b=@t]  ^-  [cord @u]
    ?.  =('\0A' (get a b))
      ~|(expected-line/[a `@t`(cut 3 [a 40] b)] !!)
    =;  c
      ::~&  [a c b]
      [(cut 3 [+(a) (sub c +(a))] b) c]
    =/  z  (met 3 b)
    !.
    |-  ^+  a
    ?:  (gth a z)  ~|(%eof-expected-line !!)
    ?.  =('\0A' (get +(a) b))
      $(a +(a))
    +(a)
  ::
  ++  next-sep
    |=  [a=@u b=@t]  ^-  (unit @u)
    =/  z  (met 3 b)
    !.
    |-  ^-  (unit @u)
    ?:  (gth a z)  ~
    ?:  &(=('\0A' (get a b)) =('\0A\0A/' (cut 3 [a 3] b)))
      (some +(a))
    $(a +(a))
  ::
  ++  textual
    ::  boot from text
    |=  [wir=path system-source=cord]  ^-  *
    =/  len  (met 3 system-source)
    =+  [hoon-hed hoon-a]=(get-line 0 system-source)
    ?.  =('/sys/hoon/hoon' hoon-hed)  ~|([%not-hoon-hoon hoon-hed] !!)
    =/  hoon-z  (need (next-sep hoon-a system-source))
    =+  [arvo-hed arvo-a]=(get-line hoon-z system-source)
    ?.  =('/sys/arvo/hoon' arvo-hed)  ~|([%not-arvo-hoon arvo-hed] !!)
    =/  arvo-z  (need (next-sep arvo-a system-source))
    ::
    ::  event 4: hoon compiler source, compiling to event 2
    ::
    =/  compiler-source=@t  (cut 3 [hoon-a (sub hoon-z hoon-a)] system-source)
    ::
    ::  event 5: arvo kernel source
    ::
    =/  arvo-source=@t  (cut 3 [arvo-a (sub arvo-z arvo-a)] system-source)
    ::
    ::  we use raw nock to erase the type, so we can later replace
    ::  the compiler gate with the input hoon
    ::
    =/  compiler-gate=*  tide
    ::
    ::  compile the compiler source, producing (pair span nock).
    ::  the compiler ignores its input so we use a trivial span.
    ::
    ~>  %slog.[0 leaf+"1-c (compiling compiler, wait a few minutes)"]
    =+  ^=  compiler-tool
        .*(compiler-gate [%9 2 %10 [6 %1 [%noun compiler-source]] %0 1])
    ::
    ::  switch to the second-generation compiler.  we want to be
    ::  able to generate matching reflection nouns even if the
    ::  language changes -- the first-generation formula will
    ::  generate last-generation spans for `!>`, etc.
    ::
    ~>  %slog.[0 leaf+"1-d"]
    =.  compiler-gate  .*(0 +:compiler-tool)
    ::
    ::  get the span (type) of the kernel core, which is the context
    ::  of the compiler gate.  we just compiled the compiler,
    ::  so we know the span (type) of the compiler gate.  its
    ::  context is at tree address `+>` (ie, `+7` or Lisp `cddr`).
    ::  we use the compiler again to infer this trivial program.
    ::
    ~>  %slog.[0 leaf+"1-e"]
    =/  kernel-span
      -:.*(compiler-gate [%9 2 %10 [6 %1 [-.compiler-tool '+>']] %0 1])
    ::
    ::  compile the arvo source against the kernel core.
    ::
    ~>  %slog.[0 leaf+"1-f"]
    =/  kernel-tool
      .*(compiler-gate [%9 2 %10 [6 %1 [kernel-span arvo-source]] %0 1])
    ::
    ::  create the arvo kernel, whose subject is the kernel core.
    ::
    =/  arvo-kernel
      ~>  %slog.[0 leaf+"1-g"]
      ~|  kernel-tool=kernel-tool
      .*(+>:compiler-gate +:kernel-tool)
    ::
    ::  pass it the packed source for its own initialization
    ::
    .*(arvo-kernel [%9 2 %10 [6 %1 [*@da wir %pill system-source]] %0 1])
  --
--
