# Jets and Batteries

When the battery is invoked with Nock operator 9, we link the formula
axis of the operation (a in [9 a b]) to a specific function in the driver.

(XX: "driver test flags"?)

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

			What are some jets that we actually *need*
			that have this requirment.

There are three different jet state systems.

	`Hot`: associated with a running Unix process. It's a pure C
	data structure.

		XX What does this mena "pure C data structure"? Implying
		that this is not allocated on the loom?

		The on-disk pier is portable across machines and OSs.

			In theory, the set of just loaded into a pier may
			even change during the lifetime of the process.

				(ORLY?)

	`Cold`: associated with the logical execution history of the pier.
	It consists entirely of nouns and ignores restarts.

		(ORLY?) @ entirely of nouns

	`Warm`: all the dependencies between cold and hot state.
	It consists of C structures allocated on the loom.  (XX: On the
	home road?)

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

				XX: By what mechanism? What does "number
				itself" mean? Where is said number stored?

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
	|  where
	|    clue = [chum parent=nock (list [term nock])]
	|    chum = [term historical-baggage]

The `parent` is a formula that computes the axis of the core's parent,
which must also be jetted.

	The parent axis is typically `7`, since the context is usually the
	parent and the context of a jetted core is usually jetted as well.

		| [formula [sample context]]

The `clue` also contains a list of hooks, named nock formulas on the core.

	These are typically arms, but could be anything.

	These are used for calling into nock from C.

		(XX: Would it be possible to stop doing this? It's gross.)

All the information in the %fast hint goes to `u3j_mine()`, which
registers the battery in cold state (`das` in `jed` in `u3R`), then warm
state (`har_p` in `jed`).

Performance is hyper-important here. We will `u3j_mine()` every time a
jetted function is called.

#### The Cold Jet Dashboard

TODO

#### The Warm Jet Dashboard

TODO

#### The Hot Dashboard

TODO

#### Jet Functions

TODO

