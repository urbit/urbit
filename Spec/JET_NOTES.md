# Jets and Batteries

When the battery is invoked with Nock operator 9, we link the formula
axis of the operation (a in [9 a b]) to a specific function in the driver.

XX "driver test flags"? What mean?

We need to validate that the payload is appropriate for the battery.

To execute a one-armed core like the above, we run `nock(a -.a)`

	The normal structure of a gate, which is simply Urbitese for
	“function,” is:

		`[formula [sample context]]`

	XX What does this transform from? I'm asking: What is the shape of
	   a gate before we do this thing?

	The context is generally just another core.

		(except the top-most context, who's context is an atom -- The
		kernel version).

	To `slam` is to call a gate.

		We simply replace the default sample with the caller’s
		data, then nock the formula on the entire gate.

			(otherwise, how would one recurse?)

	Thus a (highly desirable) static core is one of the following
	forms (ie,a solid stack of nested libraries without any dynamic
	data).

		```
		[battery constant]
		[battery static-core]
		```

	The typical gate will thus be, for example,

		`[formula [sample [battery battery battery constant]]]`

	but we would be most foolish to restrict the jet mechanism to
	cores of this particular structure. We cannot constrain a payload
	to be `[sample static-core]`, or even `[sample core]`. Any such
	constraint would not be rich enough to handle Hoon, let alone
	other languages.

		XX Why not? This seems totally reasonable to me.
		   What are some jets that we actually *need* that have
		   this requirment.

There are three different jet state systems.

	`Hot`: associated with a running Unix process. It's a pure C
	data structure.

		The on-disk pier is portable across machines and OSs.

			In theory, the set of just loaded into a pier may
			even change during the lifetime of the process.

				(ORLY?)

	`Cold`: associated with the logical execution history of the pier.
	It consists entirely of nouns and ignores restarts.

		(ORLY?) @ entirely of nouns

	`Warm`: all the dependencies between cold and hot state.
	It consists of C structures allocated on the loom.

		Warm state is purely a function of cold and hot states.

			On restart, if the hot state might have changed
			(how can we determine this?), we clear the warm
			state with `u3j_ream()`.

		`#define u3D u3j_Dash` is the global jet dashboard. It
		embeds function pointers to all the jets, and is defined
		in `jets/tree.c`.

			`u3j_harm`: driver arm.

				| typedef struct _u3j_harm {
				|   c3_c*               fcs_c;            //  `.axe` or name
				|   u3_noun           (*fun_f)(u3_noun);  //  compute or 0 / semitransfer
				|   c3_o                ice;              //  perfect (don't test)
				|   c3_o                tot;              //  total (never punts)
				|   c3_o                liv;              //  live (enabled)
				| } u3j_harm;

			`u3j_core`: C core driver.

				| typedef struct _u3j_core {
				|  c3_c*             cos_c;              //  control string
				|  struct _u3j_harm* arm_u;              //  blank-terminated static list
				|  struct _u3j_core* dev_u;              //  blank-terminated static list
				|  struct _u3j_core* par_u;              //  dynamic parent pointer
				|  c3_l              jax_l;              //  dynamic jet index
				| } u3j_core;

			`u3e_dash`, `u3_Dash`, `u3D`: jet dashboard singleton

				| typedef struct _u3e_dash {
				|   u3j_core* dev_u;                      //  null-terminated static list
				|   c3_l      len_l;                      //  ray_u filled length
				|   c3_l      all_l;                      //  ray_u allocated length
				|   u3j_core* ray_u;                      //  dynamic driver array
				| } u3j_dash;

			In the current implementation, this is a static
			data structure, compiled into the executable
			itself.

				(therefore, yes, not on the Loom)

			It *does* number itself on process initialization,
			however?

				XX By what mechanism? What does "number
				   itself" mean? Where is said number
				   stored?

	Warm and cold state is per road. In other words, as we nest roads,
	we also nest jet state. The jet state in the road is:

		| struct {                                //  jet dashboard
		|   u3p(u3h_root) har_p;                  //  warm state
		|   u3_noun       das;                    //  cold state
		| } jed;

		XX Why is that? What is the signifigance of the nesting?

	`har_p` (warm state) is a map from `++batt` to `++calx`:

	Cold state (`dash`):

		| ++  bane  ,@tas                                 ::  battery name
		| ++  bash  ,@uvH                                 ::  label hash
		| ++  bosh  ,@uvH                                 ::  local battery hash
		| ++  batt  ,*                                    ::  battery
		| ++  clog  (pair cope (map batt club))           ::  identity record
		| ++  club  (pair corp (map term nock))           ::  battery pattern
		| ++  cope  (trel bane axis (each bash noun))     ::  core pattern
		| ++  core  ,*                                    ::  core
		| ++  corp  (each core batt)                      ::  parent or static
		| ++  dash  (map bash clog)                       ::  jet system

	Warm state:

		| ++  calx  (trel calf (pair bash cope) club)     ::  cached by battery
		| ++  calf                                        ::  
		|   $:  jax=,@ud                                  ::  hot core index
		|       hap=(map ,@ud ,@ud)                       ::  axis/hot arm index
		|       lab=path                                  ::  label as path
		|       jit=*                                     ::  arbitrary data
		|   ==                                            ::

	The driver index `jax` in the warm state is a pointer into the
	hot state, so it needs be regenerated for each execution.

	Why is jet state nested?

		Nock of course is a functional system, so as we compute we
		don’t explicitly create state. Jet state is an exception
		to this principle (which works only because it can’t
		be semantically detected from Nock/Hoon) - but it can’t
		violate the fundamental rules of the allocation system.

			XX In which way would this violate the fundamental
			   rules of the allocation system?

	For instance, when we’re on an inner road, we can’t allocate
	on an outer road, ur point from an outer road to an inner. So
	if we learn something - like a mapping from battery to jet -
	in the inner road, we have to keep it in the inner road.

		Some rules:

			1. When we're in an inner road, we can't allocate
			   on an outer road.

			2. Pointers from an inner road to an outer road
			   are forbidden.

	So, when we do a `u3m_love()`, we need to also call `u3j_reap()`,
	which promotes jet information from the dying road. This promotes
	anything we've learned about any battery that:

		1. Already existetd in an outer road.

		2. Is being saved to the inner road.

### Jet Binding

To bind a jet:

	| [10 [%fast clue-formula] core-formula]
	|   where
	|     clue = [chum parent=nock (list [term nock])]
	|     chum = [term historical-baggage]

The `parent` is a formula that computes the axis of the core's parent,
which must also be jetted.

	The parent axis is typically `7`, since the context is usually the
	parent and the context of a jetted core is usually jetted as well.

		| [formula [sample context]]

The `clue` also contains a list of hooks, named nock formulas on the core.

	These are typically arms, but could be anything.

	These are used for calling into nock from C.

		XX Would it be possible to stop doing this? It's gross.

All the information in the %fast hint goes to `u3j_mine()`, which
registers the battery in cold state (`das` in `jed` in `u3R`), then warm
state (`har_p` in `jed`).

Performance is hyper-important here. We will `u3j_mine()` every time a
jetted function is called.

#### The Cold Jet Dashboard

The jet tree is a tree of battery labels.

	A label is a `[axis term]` path from the root of the tree.

Each label is associated with a set of nock batteries that the
jet-tree-node applies to.

	Different copies for debugging hints or no, etc.

	We might even have changed the semantics of the battery without
	changing the label - so long as those semantics don’t invalidate
	any attached driver.

The jet tree is a semantic hierarchy.

	The root of the hierarchy is a constant. Thus if the core is

		| [foo-battery [bar-battery [moo-battery 164]]]

	then we can reverse the nesting to construct a hierarchical core
	path. The static core `164/moo/bar/foo` extends the static core
	`164/moo/bar` by wrapping the `foo` battery around it. With the
	core above, you can compute `foo` stuff, `bar` stuff, and `moo`
	stuff.

Some cores are not static.

	For example, the sample in a gate is not static.

The dashboard is a map from battery hashes to the jet location record.

	That record contains a per-location noun

		(A name, the axis of the parent core, and a reference
		to parent core -- either a hash or, for the root node,
		a constant atom)

	and a map of batteries to a per-battery club.

		XX What are these batteries that we're using as
		   keys? This is all nested under the hash of the
		   battery, so what are the other batteries that we're
		   referencing here?)

In any case, `das`, the dashboard, is a map from `bash` to jet location
record `++clog`. A `clog` in turn contains two kinds of information: the
`++cope`, or per-location noun; and a map of batteries to a per-battery
++club`.

The cope is a triple of ++bane (battery name, right now just a term);
++axis, the axis, within this core, of the parent; and (each bash noun),
which is either [0 bash] if the parent is another core, or [1 noun],
for the constant noun (like 164) if there is no parent core.

A bash is just the noun hash (++sham) of a cope, which uniquely expresses
the battery’s hierarchical location without depending on the actual
formulas.

The club contains a ++corp, which we use to actually validate the
core. Obviously jet execution has to be perfectly compatible with
Nock. We search on the battery, but getting the battery right is not
enough - a typical battery is dependent on its context. For example,
your jet-propelled library function is very likely to call ++dec or
other advanced kernel technology. If you’ve replaced the kernel in your
context with something else, we need to detect this and not run the jet.

There are two cases for a jet-propelled core - either the entire core
is a static constant, or it isn’t. Hence the definition of corp:

	| ++  corp  (each core batt)                ::  parent or static

Ie, a corp is [0 core] or [1 batt]. If it’s static - meaning that
the jet only works with one specific core, ie, the parent axis of
each location in the hierarchy is 3 - we can validate with a single
comparison. Otherwise, we have to recurse upward by checking the parent.

Note that there is at present no way to force a jet to depend on static data.

#### The Warm Jet Dashboard

We use the cold state to

	register jets as we find them.

	rebuild the warm state after the hot state is reset.

The warm state is what we actually use at runtime.

	We use `jed->har_p`.

		It's a map from battery to `calx` -- a hashtable on
		the loom.

| calx = [calf [bash cope] club]

	Everything besides `calf` is taken straight from cold state.

| calf = [jax hap]

	`calf` contains warm data that's dependent on hot state.

	`jax` is the hot drive index (in `ray_u` in `u3j_dash`).

	`hap` is a table from arm axis (the axis of the formula with
	the battery) to driver arm index.

		We construct `hap`, when we create the `calx`.

			We do this by iterating through the arms
			registered in the `u3j_core`.

			Note the way a `u3j_harm` declares itself, with
			the string `fcs_c` which can contain either an
			axis or a name.

	`lab` is the complete label path

	`jit` is any other dynamic data that may speed up execution.

		`jit`, as its name suggests, is a stub where any sort
		of optimization data computed on battery registration
		might go.  To use it, fill in the `_cj_jit()` function.

Most jetted cores are of course gates, which have one formula at one
axis within the core: `fcs_c` is `".3"`.

	But lots of cores are not gates.

	We don't want to have to manage their axes by hand.

	So, To use an `fcs_c` with a named arm, it's sufficient to make
	sure the name is bound to a formula `[0 axis]` in the hook table.

#### The Hot Dashboard

Every time we run a nock `9` instruction, we have a core and an axis.

	We pass these to `u3j_kick()`, which will try to execute them.

Because nouns with a reference count of 1 are precious, `u3j_kick()`
has a tricky reference control definition.

	It reserves the right to return `u3_none` in the case where
	there is no driver, or the driver does not apply for this case;
	in this case, it retains argument `cor`.  If it succeeds, though,
	it transfers `cor`.

`u3j_kick()` searches for the battery in the hot dashboard.

	If the battery is registered, it searches for the axis in `hap`
	in the `calx`.

	If it exists, the core matches a driver and the driver jets
	this arm.

	If not, we return `u3_none`.

Otherwise, we call `fun_f` in our `u3j_harm`.  This obeys the same
protocol as `u3j_kick()`; it can refuse to function by returning
`u3_none`, or consume the noun.

Besides the actual function pointer `fun_f`, we have some flags in the
`u3j_harm` which tell us how to call the arm function.

If `ice` is `c3y`, the jet is known to be perfect and we can just trust
the product of `fun_f`.  Otherwise, we need to run *both* the Nock arm
and `fun_f`, and compare their results.

(Note that while executing the C side of this test, we have to set `ice`
to yes; on the Nock side, we have to set `liv` to no.  Otherwise, many
non-exponential functions become exponential.  When auto-testing jets
in this way, the principle is that the test is on the outermost layer
of recursion.)

If `tot` is yes, (`&`, `0`), the arm function is *total* and has to
return properly (though it can still return *u3_none*).  Otherwise, it
is *partial* and can `u3_cm_bail()` out with `c3__punt`.  This feature
has a cost: the jet runs in a subroad.

Finally, if `liv` is no (`|`, 1), the jet is off and doesn't run.

It should be easy to see how the tree of cores gets declared - precisely,
in `jets/dash.c`.  We declare the hierarchy as a tree of `u3j_core`
structures, each of which comes with a static list of arms `arm_u`
and sub-cores `dev_u`.

In `u3j_boot()`, we traverse the hierarchy, fill in parent pointers
`par_u`, and enumerate all `u3j_core` structures into a single flat array
`u3j_dash.ray_u`.  Our hot state then appears ready for action.


#### Jet Functions

TODO

