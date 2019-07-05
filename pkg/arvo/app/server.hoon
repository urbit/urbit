|%
:: +move: output effect
::
+$  move  [bone card]
:: +card: output effect payload
::
+$  card
  $%  [%connect wire [(unit @t) (list @t)] %server]
      [%wait wire @da]
      [%http-response =http-event:http]
      [%diff %json json]
  ==
--
::  utilities:
::
|%
::
++  parse-request-line
  |=  url=@t
  ^-  [[(unit @ta) site=(list @t)] args=(list [key=@t value=@t])]
  (fall (rush url ;~(plug apat:de-purl:html yque:de-purl:html)) [[~ ~] ~])
::  +hello:
::
++  hello
  |=  name=@t
  ^-  octs
  %-  as-octs:mimes:html
  %-  crip
  %-  en-xml:html
  ;html
    ;head
      ;title:"Hello, {(trip name)}"
    ==
    ;body
      ;h1:"Hello, {(trip name)}"
      ;p
        ; Time is
        ;span#time:"?????"
      ==
      ;button#start:"Start Timer"
      ;button#poke:"Random Poke"
      ;script(type "module", src "/~server/hello.js");
    ==
  ==
::
++  hello-js
  ^-  octs
  %-  as-octs:mimes:html
  '''
  import * as urb from '/~/channel/channel.js';

  var c = urb.newChannel();

  // The poke button just sends a poke
  document.getElementById("poke").addEventListener("click", function(){
    c.poke("zod", "server", "json", 5,
         function() {
           console.log("Poke worked");
         },
         function(err) {
           console.log("Poke failed: " + err);
         });
  });

  function doSubs() {
    // The subscription sends the time which makes the thing work.
    //
    c.subscribe("zod", "server", "/timer",
                function(err) {
                  console.log("Failed initial connection: " + err);
                },
                function(json) {
                  console.log("Subscription update: ", json);
                  var message = document.getElementById("time");
                  message.innerHTML = json;
                },
                function() {
                  console.log("Subscription quit");

                  //  resubscribe because Gall is broken
                  //
                  //    Galls queuing mechanism is broken and will
                  //    break subscriptions whenever 20 messages have
                  //    been sent.
                  //
                  doSubs();
                });
  }
  doSubs();
  '''
::  +require-authorization: redirect to the login page when unauthenticated
::
++  require-authorization
  |*  [=bone move=mold this=*]
  |=  handler=$-(inbound-request:eyre (quip move _this))
  |=  =inbound-request:eyre
  ^-  (quip move _this)
  ::
  ?:  authenticated.inbound-request
    (handler inbound-request)
  ::
  :_  this
  ^-  (list move)
  =/  redirect=cord
    %-  crip
    "/~/login?redirect={(trip url.request.inbound-request)}"
  [bone [%http-response %start [307 ['location' redirect]~] ~ %.y]]~
--
|%
::
+$  state
  $:  next-timer=(unit @da)
  ==
--
::
|_  [bow=bowl:gall state]
::
++  this  .
::
++  prep
  |=  old=(unit state)
  ^-  (quip move _this)
  ~&  %prep
  :-  [`move`[ost.bow [%connect / [~ /'~server'] %server]] ~]
  ?~  old
    this
  this(+<+ u.old)
::  alerts us that we were bound. we need this because the vane calls back.
::
++  bound
  |=  [wir=wire success=? binding=binding:eyre]
  ~&  [%bound success]
  [~ this]
::
::  +wake: responds to a %wait send from +handle-start-stream
::
++  wake
  |=  [wir=wire ~]
  ^-  (quip move _this)
  ::
  ~&  [%timer-tick wir now.bow]
  ::
  =/  moves=(list move)
    %+  turn  (prey:pubsub:userlib /timer bow)
    |=  [=bone ^]
    [bone %diff %json %s (scot %da now.bow)]
  ::  if we have outbound moves, say that we have another timer.
  ::
  =.  next-timer
    ?:  ?=(^ moves)
      `(add now.bow ~s1)
    ~
  ::  if we have any subscribers, add another timer for the future
  ::
  =?  moves  ?=(^ moves)
    [[ost.bow %wait /timer (add now.bow ~s1)] moves]
  ::
  [moves this]
::  +poke-handle-http-request: received on a new connection established
::
++  poke-handle-http-request
  %-  (require-authorization ost.bow move this)
  |=  =inbound-request:eyre
  ^-  (quip move _this)
  ::
  =+  request-line=(parse-request-line url.request.inbound-request)
  ~&  [%request-line request-line]
  =/  name=@t
    =+  back-path=(flop site.request-line)
    ?~  back-path
      'World'
    i.back-path
  ::
  ?:  =(name 'hello')
    :_  this
    :~  ^-  move
        :-  ost.bow
        :*  %http-response
            [%start [200 ['content-type' 'application/javascript']~] [~ hello-js] %.y]
        ==
    ==
  ::
  :_  this
  :~  ^-  move
      :-  ost.bow
      :*  %http-response
          [%start [200 ['content-type' 'text/html']~] [~ (hello name)] %.y]
      ==
  ==
::  +poke-handle-http-cancel: received when a connection was killed
::
++  poke-handle-http-cancel
  |=  =inbound-request:eyre
  ^-  (quip move _this)
  ::  the only long lived connections we keep state about are the stream ones.
  ::
  [~ this]
::
++  poke-json
  |=  =json
  ^-  (quip move _this)
  ~&  [%poke-json json]
  [~ this]
::
++  peer-timer
  |=  pax/path
  ^-  (quip move _this)
  ::  if we don't have a timer, set a timer.
  ?:  ?=(^ next-timer)
    [~ this]
  ::
  :-  [ost.bow %wait /timer (add now.bow ~s1)]~
  this(next-timer `(unit @da)`[~ (add now.bow ~s1)])
--
