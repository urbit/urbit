|%
++  sign
  |$  [fact]
  $%  [%poke-ack p=(unit tang)]
      [%watch-ack p=(unit tang)]
      [%fact =mark =fact]
      [%kick ~]
  ==
--
::
|%
+$  my-fact
  $%  [%three =@t]
      [%four =tape]
  ==
::
+$  input
  $%  [%one =@t]
      [%two =tape]
  ==
--
=|  state=wain
|_  =bowl:gall
++  this  .
++  on-poke
  |=  [=mark =input]
  ~&  [input=input state=state]
  ^-  [(list card:agent:gall) _this]
  =.  state
    :_  state
    ^-  @t
    ?-(-.input %one t.input, %two (crip tape.input))
  `this
::
++  on-agent
  |=  [=wire my-sign=(sign my-fact)]
  ~&  my-sign=my-sign
  `this
--
