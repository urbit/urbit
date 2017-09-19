!:
::::
  ::
:-  %say
|=  $:  {now/@da * bec/beak}
        *
    ==
=<  :-  %noun
    %hello
|%
::
++  ap
  |_  gen/twig
  ++  gi
    =|  whit
    =*  wit  -
    |%
    ++  gray
      ^-  ?
      |
      ::  on reflection, perhaps just obsessive linting
      ::
      ::  ?|  ?=(^ lab)
      ::      ?=(^ boy)
      ::      |-  ^-  ?
      ::      ?~  def  | 
      ::      |($(def l.def) $(def r.def) !(~(has in use) p.n.def))
      ::  ==
    ::
    ++  grad
      |=  $:  gen/twig 
              wit/whit 
              aid/$-({? twig whit} {twig whit})
          ==
      ^-  (unit (pair twig whit))
      =:  ^gen  gen
          ^wit  wit
        ==
      ?:  =([~ ~ ~ ~] wit)  `[gen wit]
      =<  apex
      |%
      ++  apex
        ^-  (unit (pair twig whit))
        =^  one  wit  prim
        =^  two  wit  senc(gen one)
        ?:  =(gen two) 
          ~
        `(aid & two wit)
      ::
      ::  resolve body and label issues
      ::
      ++  prim
        ^-  (pair twig whit)
        ?:  ?=(^ -.gen)  flam
        ?+  -.gen  flam
          $halo   flam
          $base   runk
          $leaf   runk
          $bcpt   runk
          $bccb   runk
          $bccl   runk
          $bccn   runk
          $bchp   runk
          $bckt   runk
          $bcwt   runk
          $bcts   flam 
          $bcsm   runk
          $brcb   ((doof -.gen +>.gen) p.gen)
          $brcl   ((doof -.gen +>.gen) p.gen)
          $brcn   ((doof -.gen +>.gen) p.gen)
          $brdt   ((doof -.gen +>.gen) p.gen)
          $brkt   ((doof -.gen +>.gen) p.gen)
          $brhp   ((doof -.gen +>.gen) p.gen)
          $brsg   ((doof -.gen +>.gen) p.gen)
          $brtr   ((doof -.gen +>.gen) p.gen)
          $brts   ((doof -.gen +>.gen) p.gen)
          $brwt   ((doof -.gen +>.gen) p.gen)
        ==
      ::
      ::  resolve variable issues
      ::
      ++  senc
        ^-  (pair twig whit)
        ?:  ?=(^ -.gen)  flam
        ?+  -.gen  flam
          $ktts  ((helk -.gen +>.gen) p.gen)
          $bcts  ((helk -.gen +>.gen) p.gen)
          $var   ((hulp -.gen +>.gen) p.gen)
          $rev   ((hulp -.gen +>.gen) p.gen)
          $sip   ((hulp -.gen +>.gen) p.gen)
          $aka   ((humm -.gen +>.gen) p.gen)
        ==
      ::
      ++  flam  [gen wit]
      ++  grif
        |=  {cog/term wat/what}
        ^-  {what whit}
        ?:  =(~ def)
          ?~  boy  [wat wit]
          [boy wit(boy ~)]
        =+  yeb=(~(get by def) cog)
        ?~  yeb  [wat wit]
        [`u.yeb wit(use (~(put in use) cog))]
      ::
      ++  doof
        |*  {pif/@tas suf/*}
        |=  pac/chap
        ^-  (pair twig whit)
        :_  wit(lab ~, boy ~)
        =-  [pif - suf]
        ^-  chap
        :-  ?~(lab p.pac [u.lab ~])
            ?~(boy q.pac boy)
      ::
      ++  helk
        |*  {pif/@tas suf/*}
        |=  got/toga
        ^-  (pair twig whit)
        =^  gef  wit  (tong got)
        [[pif gef suf] wit]
      ::
      ++  hulp
        |*  {pif/@tas suf/*}
        |=  hot/toro
        ^-  (pair twig whit)
        =^  tog  wit  (tong p.hot)
        [[pif [tog q.hot] suf] wit] 
      ::
      ++  humm
        |*  {pif/@tas suf/*}
        |=  {cog/term wat/what)
        ^-  (pair twig whit)
        =^  taw  wit  (grif cog wat)
        [[pif [cog taw] suf] wit] 
      ::
      ++  runk
        ^-  (pair twig whit)
        ?~  boy  flam
        [[%halo boy gen] wit(boy ~)]
      ::
      ++  tong
        |=  got/toga
        ^-  {toga whit}
        ?@  got
          =^  wat  wit  (grif got ~)
          ?~  wat  [got wit]
          [[%1 [wat got] [%0 ~]] wit]
        ?-  -.got
          $0  [got wit]
          $1  =^  wat  wit  (grif q.p.got p.p.got)
              =^  sub  wit  $(got q.got)
              [[%1 [wat q.p.got] sub] wit]
          $2  =^  one  wit  $(got p.got)
              =^  two  wit  $(got q.got)
              [[%2 one two] wit]
        ==
      --
    --
  --
--
