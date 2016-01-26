section 3bB, cryptosuites
=========================

### `++crua`

    ++  crua  !:                                            ::  cryptosuite A (RSA)
      ^-  acru
      =|  [mos=@ pon=(unit ,[p=@ q=@ r=[p=@ q=@] s=_*fu])]
      =>  |%

XX document

### `++mx`

          ++  mx  (dec (met 0 mos))                         ::  bit length

XX document

### `++dap`

          ++  dap                                           ::  OEAP decode
            |=  [wid=@ xar=@ dog=@]  ^-  [p=@ q=@]
            =+  pav=(sub wid xar)
            =+  qoy=(cut 0 [xar pav] dog)
            =+  dez=(mix (end 0 xar dog) (shaw %pad-b xar qoy))
            [dez (mix qoy (shaw %pad-a pav dez))]
          ::

XX document

### `++pad`

          ++  pad                                           ::  OEAP encode
            |=  [wid=@ rax=[p=@ q=@] meg=@]  ^-  @
            =+  pav=(sub wid p.rax)
            ?>  (gte pav (met 0 meg))
            ^-  @
            =+  qoy=(mix meg (shaw %pad-a pav q.rax))
            =+  dez=(mix q.rax (shaw %pad-b p.rax qoy))
            (can 0 [p.rax dez] [pav qoy] ~)
      |%

XX document

### `++pull`

          ++  pull  |=(a=@ (~(exp fo mos) 3 a))

XX document

### `++push`

          ++  push  |=(a=@ (~(exp fo mos) 5 a))

XX document

### `++pump`

          ++  pump
            |=  a=@  ^-  @
            ?~  pon  !!
            (out.s.u.pon (exp.s.u.pon p.r.u.pon (sit.s.u.pon a)))
          ::

XX document

### `++punt`

          ++  punt
            |=  a=@  ^-  @
            ?~  pon  !!
            (out.s.u.pon (exp.s.u.pon q.r.u.pon (sit.s.u.pon a)))
      |%

XX document

### `++as`

      ++  as
        =>  |%

XX document

### `++haul`

            ++  haul                                        ::  revealing haul
              |=  a=pass
              !!
        ^?
        |%  ++  seal
              |=  [a=pass b=@ c=@]
              ^-  @
              !!

XX document

### `++seal`


    XX document

    ###++sign

    ```
            ++  sign
              |=  [a=@ b=@]  ^-  @
              !!
    ```

    XX document

    ###++sure

    ```
            ++  sure
              |=  [a=@ b=@]
              ^-  (unit ,@)
              !!
    ```

    XX document

    ###++tear

    ```
            ++  tear
              |=  [a=pass b=@]
              ^-  (unit ,[p=@ q=@])
              !!
      ::
    ```

    XX document

    ###++de

    ```
      ++  de
        |+  [key=@ cep=@]  ^-  (unit ,@)
        !!
      ::
    ```

    XX document

    ###++dy

    ```
      ++  dy
        |+  [a=@ b=@]  ^-  @
        !!
    ```

    XX document

    ###++en

    ```
      ++  en
        |+  [key=@ msg=@]  ^-  @ux
        !!
      ::
    ```

    XX document

    ###++ex

    ```
      ++  ex  ^?
        |%  ++  fig  ^-  @uvH  (shaf %bfig puc)
    ```

    XX document

    ###++fig

XX document

### `++pac`

            ++  pac  ^-  @uvG  (end 6 1 (shaf %acod sec))

XX document

### `++pub`

            ++  pub  ^-  pass  (cat 3 'b' puc)

XX document

### `++sec`

            ++  sec  ^-  ring  sed
      ::

XX document

### `++nu`

      ++  nu
        ^?
        |%  ++  com
              |=  a=@
              ^+  ^?(..nu)
              ..nu(sed ~, puc a)
            ::

XX document

### `++elcm`

            ++  elcm
              |=  [a=@ b=@]
              (div (mul a b) d:(egcd a b))
            ::

XX document

### `++eldm`

            ++  eldm
              |=  [a=@ b=@ c=@]
              (~(inv fo (elcm (dec b) (dec c))) a)
            ::

XX document

### `++ersa`

            ++  ersa
              |=  [a=@ b=@]
              [a b [(eldm 3 a b) (eldm 5 a b)] (fu a b)]
        ^?
        |%  ++  com
              |=  a=@
              ^+  ^?(..nu)
              ..nu(mos a, pon ~)
            ::

XX document

### `++com`


    XX document

    ###++pit

    ```
            ++  pit
              |=  [a=@ b=@]
              ^+  ^?(..nu)
              ..nu(sed b, puc (puck:ed b))
            ::
    ```

    XX document

    ###++nol

    ```
            ++  nol
              |=  a=@
              ^+  ^?(..nu)
              ..nu(sed a, puc (puck:ed a))
    ```

    XX document

    ###++bruw

    ```
    ++  bruw                                                ::  create keypair
      |=  [a=@ b=@]                                         ::  width seed
      ^-  acru
      (pit:nu:crua a b)
    ::
    ```

    XX document

    ###++haul

    ```
            ++  haul                                        ::  revealing haul
              |=  a=pass
              !!
        ^?
        |%  ++  seal
              |=  [a=pass b=@ c=@]
              ^-  @
              !!
    ```

    XX document

    ###++weur

    ```
    ++  weur                                                ::  activate secret key
      |=  a=ring
      ^-  acru
      =+  [mag=(end 3 1 a) bod=(rsh 3 1 a)]
      ?>  =('A' mag)
      (nol:nu:crua bod)
    ::
    ```

    XX document

    ###++trua

    ```
    ++  trua                                                ::  test rsa
      |=  msg=@tas
      ^-  @
      =+  ali=(bruw 1.024 (shax 'ali'))
      =+  bob=(bruw 1.024 (shax 'bob'))
      =+  tef=(sign:as.ali [0 msg])
      =+  lov=(sure:as.ali [0 tef])
      ?.  &(?=(^ lov) =(msg u.lov))
        ~|(%test-fail-sign !!)
      =+  key=(shax (shax (shax msg)))
      =+  sax=(seal:as.ali pub:ex.bob key msg)
      =+  tin=(tear:as.bob pub:ex.ali sax)
      ?.  &(?=(^ tin) =(key p.u.tin) =(msg q.u.tin))
        ~|(%test-fail-seal !!)
      msg
    ::
    ```

    XX document

    ###++crub

    ```
    ++  crub                                                ::  cryptosuite B (Ed)
      ^-  acru
      =|  [puc=pass sed=ring]
      =>  |%
    ```

    XX document

    ###++dap

    ```
          ++  dap                                           ::  OEAP decode
            |=  [wid=@ xar=@ dog=@]  ^-  [p=@ q=@]
            =+  pav=(sub wid xar)
            =+  qoy=(cut 0 [xar pav] dog)
            =+  dez=(mix (end 0 xar dog) (shaw %pad-b xar qoy))
            [dez (mix qoy (shaw %pad-a pav dez))]
          ::
    ```

    XX document

    ###++pad

    ```
          ++  pad                                           ::  OEAP encode
            |=  [wid=@ rax=[p=@ q=@] meg=@]  ^-  @
            =+  pav=(sub wid p.rax)
            ?>  (gte pav (met 0 meg))
            ^-  @
            =+  qoy=(mix meg (shaw %pad-a pav q.rax))
            =+  dez=(mix q.rax (shaw %pad-b p.rax qoy))
            (can 0 [p.rax dez] [pav qoy] ~)
      |%
    ```

    XX document

    ###++as

    ```
      ++  as
        =>  |%
    ```

    XX document

    ###++haul

    ```
            ++  haul                                        ::  revealing haul
              |=  a=pass
              !!
        ^?
        |%  ++  seal
              |=  [a=pass b=@ c=@]
              ^-  @
              !!
    ```

    XX document

    ###++seal

XX document

### `++sign`

            ++  sign
              |=  [a=@ b=@]  ^-  @
              !!

XX document

### `++sure`

            ++  sure
              |=  [a=@ b=@]
              ^-  (unit ,@)
              !!

XX document

### `++tear`

            ++  tear
              |=  [a=pass b=@]
              ^-  (unit ,[p=@ q=@])
              !!
      ::

XX document

### `++de`

      ++  de
        |+  [key=@ cep=@]  ^-  (unit ,@)
        !!
      ::

XX document

### `++dy`

      ++  dy
        |+  [a=@ b=@]  ^-  @
        !!

XX document

### `++en`

      ++  en
        |+  [key=@ msg=@]  ^-  @ux
        !!
      ::

XX document

### `++ex`

      ++  ex  ^?
        |%  ++  fig  ^-  @uvH  (shaf %bfig puc)

XX document

### `++fig`


    XX document

    ###++pac

    ```
            ++  pac  ^-  @uvG  (end 6 1 (shaf %acod sec))
    ```

    XX document

    ###++pub

    ```
            ++  pub  ^-  pass  (cat 3 'b' puc)
    ```

    XX document

    ###++sec

    ```
            ++  sec  ^-  ring  sed
      ::
    ```

    XX document

    ###++nu

    ```
      ++  nu
        ^?
        |%  ++  com
              |=  a=@
              ^+  ^?(..nu)
              ..nu(sed ~, puc a)
            ::
    ```

    XX document

    ###++com

XX document

### `++pit`

            ++  pit
              |=  [a=@ b=@]
              ^+  ^?(..nu)
              ..nu(sed b, puc (puck:ed b))
            ::

XX document

### `++nol`

            ++  nol
              |=  a=@
              ^+  ^?(..nu)
              ..nu(sed a, puc (puck:ed a))

XX document

### `++brew`

    ++  brew                                                ::  create keypair
      |=  [a=@ b=@]                                         ::  width seed
      ^-  acru
      (pit:nu:crub a b)
    ::

XX document

### `++hail`

    ++  hail                                                ::  activate public key
      |=  a=pass
      ^-  acru
      =+  [mag=(end 3 1 a) bod=(rsh 3 1 a)]
      ?>  =('b' mag)
      (com:nu:crub bod)
    ::

XX document

### `++wear`

    ++  wear                                                ::  activate secret key
      |=  a=ring
      ^-  acru
      =+  [mag=(end 3 1 a) bod=(rsh 3 1 a)]
      ?>  =('b' mag)
      (nol:nu:crub bod)
    ::

XX document

### `++trub`

    ++  trub                                                ::  test ed
      |=  msg=@tas
      ^-  @
      =+  ali=(brew 1.024 (shax 'ali'))
      =+  bob=(brew 1.024 (shax 'bob'))
      =+  tef=(sign:as.ali [0 msg])
      =+  lov=(sure:as.ali [0 tef])
      ?.  &(?=(^ lov) =(msg u.lov))
        ~|(%test-fail-sign !!)
      =+  key=(shax (shax (shax msg)))
      =+  sax=(seal:as.ali pub:ex.bob key msg)
      =+  tin=(tear:as.bob pub:ex.ali sax)
      ?.  &(?=(^ tin) =(key p.u.tin) =(msg q.u.tin))
        ~|(%test-fail-seal !!)
      msg
    ::

XX document

### `++hmac`

    ++  hmac                                                ::  HMAC-SHA1
      |=  [key=@ mes=@]
      =+  ip=(fil 3 64 0x36)
      =+  op=(fil 3 64 0x5c)
      =+  ^=  kex
          ?:  (gth (met 3 key) 64)
            (lsh 3 44 (shan (swap 3 key)))
          (lsh 3 (sub 64 (met 3 key)) (swap 3 key))
      =+  inn=(shan (swap 3 (cat 3 (swap 3 mes) (mix ip kex))))
      (shan (swap 3 (cat 3 inn (mix op kex))))
    ::

XX document
