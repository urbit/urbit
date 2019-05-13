::  Nuevo is a kernel in Hoon which collaborates with vere to be multi-process.
d::
::  An instance of Nuevo wraps one and only one hoon program and communicates
::  with other instances of Nuevo via message passing. Individual nuevo
::  instances may be run in serial or in parallel depending on the
::  computational resources available to the host machine.
::
::  The point of an operating system is to have only one minimal piece of code
::  which can screw up. The point is to make the system as resilient as
::  possible against not only malicious code, but incorrect code. Thus,
::  instances of nuevo are the only piece of Nock in the system which runs
::  unvirtualized; all other pieces of code are run inside the virtualized nock
::  interpreter. Likewise, we want to not expose low level handles to user code
::  because we don't want user code to leak handles.
::
::  Nuevo and Vere operate as part of a standardized state machine around
::  process creation, execution and event dispatch. The initial process is
::  created with %init, which is given a single initial handle which represents
::  communication with vere.
::
::  Nuevo processes have no actual relation to OS processes and an implementer
::  should feel free to do any mapping that is semantically equivalent, which
::  includes both performing no actual parallelism in practice and running all
::  processes on a single event queue, and complicated green thread solutions.
::
|%
::  There is a difference between how a handle to an external resource is
::  represented between vere and nuevo and how it is represented between nuevo
::  and the program it wraps. We will call the opaque handles sent between
::  nuevo instances `+handle`s, after the existing meaning of handles in modern
::  operating systems, which are usually opaque integers.
::
+$  handle
  $%  ::  A handle which represents the ownership of the first process by vere.
      ::
      [%top ~]
      ::  An io handle is a handle to a toplevel io driver.
      ::
      [%io driver=term]
      ::  A pid handle is a handle to a different process.
      ::
      ::    Processes are hierarchical by name. The userland part of a program
      ::    can only see the current name, not the full path. Likewise, for an
      ::    individual program, the names of its child processes are unique.
      ::
      ::    TODO: There's some pretty deep questions about divergence between
      ::    the path specified by the pid's creator, and who might end up with
      ::    each side of the handle.
      ::
      [%pid pid=path id=@u]
  ==
::
::  Each handle represents a one-to-one connection with another part of the
::  system. While there might be 30 handles to `[%pid pid=/blah ...]` live in
::  the system, across the entire fleet of Nuevo instances that make up your
::  Urbit, there are only two instances of `[%pid pid=/blah id=7]`. There is
::  only a single real `[%top ~]` and `[%io driver id=@]` inside your Urbit,
::  since vere conceptually holds the other end of the handle.
::
::  A handle by itself is often not enough to perform a negotiation; we often
::  need explicit typing information about the data which is coming over a
::  handle when we receive it. Better names welcome.
::
+$  handle-type
  $%  recv=xtype
      send=xtype
  ==
::
::  Nuevo maintains a mapping from a handle to a bone, an opaque type which
::  represents a handle. It's just an integer, but we make it entirely untyped
::  because the user program shouldn't be introspecting on it.
::
+$  bone
  *
::
::  But what can Nuevo do with handles? Handles are references to resources
::  outside of a single instance of Nuevo, and instances of Nuevo pass them as
::  part of the message machinery. Nuevo instances trust each other to not
::  forge handles.
::
::  So an instance of Nuevo can receives events and emit effects. These are
::  messages received from and sent to vere, which is actually mediating
::  between IO drivers and individual nuevo processes.
::
+$  events
  $%  ::  -- Message Passing Primitives --
      ::
      ::  For there to be a program to receive a message, that program needs to
      ::  be started first. This is the first message in the lifecycle of a
      ::  nuevo instance.
      ::
      ::  Each program is given as a vase of nock code to be executed as a
      ::  persistent process, along with an initial state used as the permanent
      ::  state of the program.
      ::
      ::  At the user program level, after initialization, this will be treated
      ::  like a normal %recv.
      ::
      $:  %init
          name=path
          program=vase
          state=vase
          sent-over=[handle handle-type]
          gaining=(list [handle handle-type])
          message=*
      ==
      ::  Receives a message on a handle. Each %recv is translated into a user
      ::  level call, with the :sent-over and :gaining are translated into
      ::  bones in order, and :message is validated against the handle-type
      ::  definition of :sent-over.
      ::
      ::  If a %recv causes a program to crash, this instance of nuevo will
      ::  shut down. Nuevo will send close events for every [%pid ...] handle
      ::  it has. TODO: What about IO handles? If I'm the http server, and I
      ::  crash while holding on to [%io %http 0], how does my server ever
      ::  recover?
      ::
      [%recv sent-over=handle gaining=(list [handle handle-type]) message=*]
      ::  Remote handles have been closed; we must have a notification that we
      ::  understand.
      ::
      [%closed sent-over=handle closed=(list handle)]
      ::  Sent to each process on vere restart. all existing io handles are
      ::  defunct. This is the only incoming event which doesn't come over a
      ::  handle, since this process may not have a reference to outside, or
      ::  may have multiple io handles which need to all be cleared at once.
      ::
      [%cleario ~]
      ::
      ::  -- Async Primitives --
      ::
      ::  Returns a result from a previous %async call back into the program.
      ::
      [%back id=@async result=xvase]
      ::
      ::  -- Forking Primitives --
      ::
      ::  Notification that a forked process launched, and here's the
      ::  channel. Or error.
      ::
      [%forked name=term handle=(each [handle handle-type] tang)]



      ::  Called on nondeterministic crashes. If an instance of nuevo runs out
      ::  of memory or otherwise causes a nondeterministic crash, vere replaces
      ::  the crashed event with one of these events to represent and handle
      ::  the effects of the crash.
      ::
      [%crash trace=tang]


      :: PER CONVERSATION WITH JOE: We probably want to build timers right into
      :: the kernel. If we want to be efficient with write batching, we don't
      :: want a timer to do the equivalent of setTimeout(0) as one of its
      :: effects since that doesn't let us write batch efficiently.
  ==
::
+$  effects
  $%  ::  Sends a message over handle. a handle represents a bidirectional
      ::  pipe. To give a handle we own to some other process, we must lose it
      ::  ourselves; any attempt to send on these handles from user code will
      ::  cause the process to crash.
      ::
      [%send send-over=handle losing=(list channel) message=*]
      ::  Closes a set of handles.
      ::
      ::    Handle closing is done either explicitly or on process crash. Since
      ::    a process has
      ::
      [%close send-over=handle close=(list handle)]
      ::
      ::  -- Async Primitives --
      ::
      ::  Performs a calculation asynchronously.
      ::
      [%do id=@async to-compute=nock]
      ::  Cancels an outstanding %async calculation. No %result will be
      ::  injected back into
      ::
      [%stop id=@async]
      ::
      ::  -- Forking Primitives --
      ::
      ::  Launches a process, passing a list of handles we're losing in this
      ::  process to one that the new process will gain.
      ::
      [%fork name=term logged=? program=vase losing=(list channel) message=*]
  ==
::
:: Handles send typed messages, so the messages that come over [%top ~] also
:: have a type. Technically, this [recv send] pair is passed in on %top, but we
:: document it here anyway.
::
+$  top-channel
  |%
  ++  recv
    $%  ::  Initializes the first process.
        ::
        ::    The program is initialized, and we give short names to each
        ::    zero-indexed handle passed in.
        ::
        [%init handles=(map term @ud)]
        ::  The underlying process was restarted; here are the new handles.
        ::
        [%born handles=(map term @ud)]
    ==
  ++  send
    $%  :: privileged operations on the entire event log, if any, go here.
    ==
  --
--
::
::  OK, so we've defined the kernel level data structures and interfaces. But
::  how do these connect into a user program?
::
::  While this entire sketch of Nuevo will need significant revision when we
::  get support from the language, the user level specification here suffers
::  from this extremely. For now, we'll specify what a program that works on
::  top of Nuevo looks like in the old notation in the old inside out
::  state-machine.
::
::  The main problem we have at the user layer is that it is entirely
::  reasonable for a message to also contain references to handles, and we
::  don't want the user program to be able to know the identity of the channel
::  it's talking to. An urbit running in aquarium should get a channel whose
::  [%top ~] equivalent pipe is a [%pid ...], but this should be unperceivable
::  by programs running on it. So handles shouldn't be referenced directly in
::  user code. We solve this by having :losing/:gaining passed along the side,
::  and any message that has to deal with handles can reference these by index.
::
::  Let's put this together with an additional full program core, which would
::  be passed in as a vase via an %init call.
::
|%
++  sample-program
  |_  $:  ::  Bowl equivalent
          $:  our=ship
              now=@da
              eny=@uvJ
              ::  open bones that we can receive or send messages on
              bones=(map bone handle-type)
          ==
          ::  permanent state which gets modified only by %save
          state=*
          ::  temporary state which gets bunted or something
          temp=*
      ==
  ::  +do: handles incoming message from %init or %recv
  ::
  ::    Please overlook that this gate is impossible in current hoon. The point
  ::    is to illustrate that when Nuevo receives a message, it then translates
  ::    its parameters into the bowl equivalent and the.
  ::
  ::    The system converts the :sent-over and :gaining parameters from full
  ::    handles into opaque bone references.
  ::
  ++  do
    |=  [cause=bone gaining=(list bone) msg=<input channel type>]
    ^-  [(list sysmsg) _temp]
    :_  temp
    [[%commit stae] ~]
  --
--
::
::  ==========
::
::  Example 1: Boot flow and reboot flow with the HTTP server
::
::
::  [Step one]: pid:1 is launched as part of the boot sequence. We're passed
::  the first program which we're going to run, along with its initial state.
::
::  The interesting thing to note is that this is a nuevo level [%init ...]
::  which contains a user level [%init ...] message. The nuevo %init message
::  initializes a program with a state along with passing a user level message
::  which is delivered immediately to the program.
::
::  {pid:/}
::  [%init name=/
::         program=<userboot vase>
::         state=<initial state with the core system services to launch>
::         sent-over=[[%top ~] [recv=<xtyp> send=<xtype>]]
::         gaining=[[[%io %http 0] [<xtype> <xtype>]] ~]
::         message=[%init handles={[%http 0]}]]
::  ===> We have the first program passed in from the outside. This instance
::       of nuevo assigns bone 0 to [%top ~] and bone 1 to [%io %http 0], takes
::       the passed in program vase and uses that as the program to run, puts
::       the various bone mappings in the state with types, and finally calls:
::
::         (~(do ...) 0 [1 ~] [%init handles={[%http 0]}])
::
::       Which then produces the effect:
::  -->  [%fork name=%http
::              logged=%.y
::              program=<http server vase>
::              state=<initial state of the http server>
::              sent-over=[[%pid / 0] <http server type>]
::              losing=[[[%io %http 0] [<type> <type>]] ~]
::              message=[%start ~]]
::
::
::  [Step two]: pid:/http is started from the %fork given to vere. Vere
::  understands the full state machine so it takes the %fork and turns it into
::  an %init. Since logged=%.y, the events on this instance of nuevo go into
::  the event log and are replayable.
::
::  {pid:/http}
::  [%init name=/http
::         program=<http server vase>
::         state=<initial state of the http server>
::         sent-over=[[%pid / 0] [<type> <type>]]
::         gaining=[[[%io %http 0] [<type> <type>]] ~]
::         message=[%start ~]]
::  ===> We're starting a new process here. Like the userboot process, we now
::       are starting up from initial state, and the system will now
::       initialize stuff.
::  -->  [%send [%io http 0] losing=~ [%start-http-thing port=80]]
::
::
::  [Step three]: pid:/ receives back a message that its forked process was
::  created.
::
::  {pid:/}
::  [%forked name=/http
::           handle=[| [%pid / 0] [<type> <type>]]]
::  ===> This gets passed into the user program. Assuming we have some sort of
::       await/async system, this is the "return" value of the fork call.
::  -->  ~
::
::
::  [Step four]: Look, an incoming http request! But it comes directly from the
::  io driver instead of from [%top ~] or from [%pid / 0].
::
::  {pid:/http}
::  [%recv sent-over=[%io %http 0]
::         gaining=[[%io %http 1] <session type> <session type>]
::         message=[%session-opened 0]]
::  ===> Someone is making an http request of us! We've been passed a new handle
::       directly to this session over the main http handle. This handle has a
::       different type from the toplevel http handle, which should make sense
::       since this handle represents a specific http request. It asks us to do
::       the ackerman function, for some reason. We oblige, but we do it on an
::       asynchronous thread.
::  --> [%do 0 to-compute=|.((ack 500))]
::
::
::  [Step five]: We wake up, and realize that we've just come back up after
::  being shut down. We learn this by receiving the [%cleario ~] message, which
::  instructs us to drop all %io handles. Only processes which have %io handles
::  receive the [%cleario ~] message.
::
::  {pid:/http}
::  [%cleario ~]
::  ===> Well, that means we're going to have to cancel our current asynchronous
::       thread, since the handle which caused it is gone. (We'll need to have
::       the same logic for the normal user disconnected case.)
::  --> [%stop 0]
::
::
::  [Step six]: Now that Vere has made sure all processes have completed
::  clearing their io handles, it sends a new [%born ...] event on [%top ~] to
::  the first process, who now distributes the new %io handles to its child
::  processes.
::
::  {pid:/}
::  [%recv sent-over=[[%top ~] [recv=<xtyp> send=<xtype>]]
::         gaining=[[[%io %http 3] [<xtype> <xtype>]] ~]
::         message=[%born handles={[%http 0]}]]
::  ===> Userboot now distributes the new handles to its children
::  -->  [%send send-over=[%pid / 0]
::              losing=[[[%io %http 3] [<xtype> <xtype>]] ~]
::              message=[%born 0]]
::
::
::  ==========
::
::  Example 2: Handles as subscriptions
::
::  [Step 1]: You are pid:/foo and have a handle to pid:/bar of the following
::  type:
::
::    |%
::    ++  recv
::      $%  [%get-sub bone]
::      ==
::    ++  send
::      $%  [%subscribe-to =path]
::      ==
::    --
::
::  So you send off your subscription request
::
::  {pid:/foo}
::  --> [%send send-over[%pid /bar 5]
::             losing=~
::             message=[%subscribe-to=/data/path]]
::
::
::  [Step 2]: You subscribed to the remote resource and hear back a handle of
::  this type over the requested type:
::
::    |%
::    ++  recv
::      $%  [%subscription-result =tape]
::      ==
::    ++  send
::      ~
::    --
::
::  {pid:/foo}
::  [%recv sent-over=[%pid /bar 5]
::         gaining=[[[%pid /bar 6] <recv> <send>] ~]
::         message=[%get-sub 0]]
::  ===> /foo has received a new handle which represents the subscription
::       Since it's a handle (like everything else in the system), it is legible
::       to the kernel, the handle will be closed on the other side, as we'll see
::       later.
::  -->  ~
::
::     --- TODO: You need to think through how new handles get spawned here! ---
::
::  [Step 3]: Our program receives a new thing on the subscription handle.
::
::  {pid:/foo}
::  [%recv sent-over=[%pid /bar 6]
::         gaining=~
::         message=[%subscription-result "some data"]]
::  ===> /foo does some internal processing on the result.
::  -->  ~
::
::
::  [Step 4]: Our program reacts to the subscription handle being closed.
::
::  {pid:/foo}
::  [%closed [[%pid /bar 6] ~]]
::  ===>  /foo can now react to the subscription handle closign. it could try
::        to resubscribe, or maybe this is expected.
::  -->  ~
::
::
::  ==========
::
::  Example 3: Crash semantics
::
::  [Step 1]: Your program is chugging along and has reached a place where it
::  wants to commit its permanent state. Recall in the above sample program
::  core, that a program's state is divided into the :state and the :temp part,
::  where one must emit an explicit [%commit ...] effect to save that part of
::  the state, while :temp can be thrown away on any crash. Most data should be
::  temp data.
::
::  {pid:/child/parent}
::  ...
::  ===>  A long running transaction has completed! Time to commit our permanent
::        state! The program emits a [%commit ...] effect...but this causes no
::        nuevo level effects to be emitted!
::  -->  ~
::
::
::  [Step 2]: Your program keeps chugging along, but hits a non-deterministic
::  error. Maybe you're using too much memory. It doesn't matter, vere then
::  sends the nuevo instance a %crash event, telling the nuevo instance to
::  crash its process and to preform the shutdown steps.
::
::  {pid:/child/parent}
::  [%crash some-error-tang]
::  -->  nuevo doesn't even run the program. all it knows is that the last
::       instance of it crashed.
::  -->  :~  [%crashed parent=[%pid /parent 0]
::                     committed=<the vase of the last commit>
::                     losing=[[%io %thing 7] [%pid /parent 8] ~]]
::           ::  Nuevo keeps track of handles from some other part of the
::           ::  system, and must notify them the crash, too.
::           [%close parent=[%pid /other 5]
::                   losing=[[%pid /other 3] ~]]
::           ::  This nuevo process is over.
::           [%terminate ~]
::       ==
::  -->  (This nuevo instance shuts down)
::
::  When a process crashes, only then does it send upward the vase from the
::  last %commit to the parent. Because Nuevo is trusted and is trusted to deal
::  with crashes correctly, we don't have to send each committed permanent
::  state each time a %commit occurs, incurring event log writes of possibly
::  large pieces of state. We only have to do this on crash because we can
::  trust nuevo.
::
::  Nuevo keeps track of which handles were passed over which other
::  handles. All those handles need to be closed.
::
::
::  [Step 3]: Parent receives the
::
::  {pid:/parent}
::  [%crashed sent-on=[%pid /parent 0]
::            committed=<vase of commit>
::            losing=...]
::  ===>  the parent, which spawned /child/parent in the first place, can now
::        restart the child, from the last known good state.
::  -->  [%fork name=child
::              logged=%.y
::              program=<program vase>
::              state=<the last committed state, which was just passed to us>
::              sent-over=[[%pid / 0] <http server type>]
::              losing=[[[%io %http 0] [<type> <type>]] ~]
::              message=[%start ~]]


::  TODO:
::
::  - Handle closing needs to be thought through more.
::
::  Do you need to receive one message for all the handles closing in one
::  process? I think so because otherwise you could try to close handle 1 which
::  could have side effects on closing handle 2.
::
::  This entire thing implies that the %pid handle that represents a process
::  starting is actually different from other handles on top of it. This is
::  already sort of true! %init asks for a handle with a special type and
::  %forked returns a handle. 
::
::  What happens when userboot crashes?
::
::  What happens with updating userboot? What happens to update nuevo itself?
::
::  What happens when the child crashes and the parent crashes at almost the
::  same time, to the point that the parent can't handle the child's crash?
::  This implies that a checkpoint becomes lost if we try to be fancy and only
::  pass upwards on crash.
::
::
:: CASES THAT MUST BE SHOWN
::
:: - Process crash and restart
:: - The process termination case
:: - The persistence stuff
:: - Passing a handle two nodes away and then having one of the pieces crash (show both).

::  TODO: This is a good first draft, but I think I should do a deep dive on
::  how Erlang handled some of these process related issues before I finalize
::  the design; they have 20 years of experience doing this sort of process
::  oriented message passing and we should learn from them.
::
