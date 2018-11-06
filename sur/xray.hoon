::
::  These are the public types for the `xray` library.  Analysing a type
::  yields an `ximage`, and everything else here is just some structure
::  within that.
::
::  `ximage`s can be printed as specs (hoon syntax for types), and can
::  be used to pretty-print typed data.
::
/?  310
::
|%
::
::  An `xtable` is a graph of types referenced by the top-level type,
::  and the `root` `key` points to the node which corresponds to the
::  type under analysis.
::
+$  ximage  [root=key =xtable]
::
::  A `key` is just an identifier for a node in the xray graph.
::
+$  key  @
::
::  An `xtable` is the xray graph itself. It contains one node for for
::  the type that was analyzed and one node for every type referenced
::  within that type.
::
::  The `next` field is the the next available key (used when inserting
::  new xrays), `xrays` maps keys to graph nodes and `type-map` gives
::  the key corresponding to a type.
::
::  The `type-map` is basically just the reverse of the `xrays` map. It
::  doesn't contain any new information, but is needed for performance
::  reasons.
::
+$  xtable  [next=key xrays=(map key xray) =type=(map type key)]
::
::  An `xray` is a node in the `ximage` graph. It contains everything
::  we know about a certain `type`. `key` is it's identifier in the graph,
::  `type` is the type that it's an xray of, and `data` is the basic
::  information we derived about the type.  The basic references to other
::  nodes are inside the `data` structure, though some of the other
::  fields may contain references as well.
::
::  - `shape` is some more information about the shape of data within
::     a cell.
::  - `role` expands on `shape`, adding further information about the
::     role that a node has within a fork.
::  - `pats` is used for printing data: we want to know if this type
::    can be printed as a list, as json, as a tape literal, etc.
::  - `recipes` contains information about how a type was
::     constructed. It's used to get much nicer output when printing types.
::  - `studs` contains "standards names". I actually don't know what this is.
::  - `helps` contains all the documentation about a type.
::  - `loop` indicates whether or not a node references itself. The list
::    type is cyclical, for example. This is used when printing an
::    `ximage`.
::
+$  xray
  $:  =key
      =type
      data=(unit data)
      role=(unit role)
      pats=(unit pattern)
      studs=(set stud)
      recipes=(set recipe)
      helps=(set help)
      shape=(unit shape)
      loop=(unit ?)
  ==
::
::  - `%void` -- impossible to create.
::  - `%noun` -- could be any noun.
::  - `%atom` -- An atom of some aura, possibly constant
::  - `%cell` -- A cell with a head and a tail.
::  - `%core` -- A core, it's garb, it's context type, and the types of
::     each of it's arms.
::  - `%face` -- A face on another type.
::  - `%fork` -- Could be one or more other types.
::  - `%pntr` -- This is an internal hack, it should never survive
::     analysis; ignore.
::
+$  data
  $@  ?(%noun %void)
  $%  [%atom =aura constant=(unit @)]
      [%cell head=key tail=key]
      [%core =garb xray=key batt=xbattery]
      [%face face=$@(term tune) xray=key]
      [%fork =(set key)]
      [%pntr xray=key]
  ==
::
::  The basic shape of a type:
::
::  - `%void` -- impossible to create.
::  - `%noun` -- could be any noun.
::  - `%atom` -- always some type of atom; never a cell
::  - `%cell` -- always some type of cell; never an atom.
::  - `%junc` -- is a fork of a cell type and an atom type.
::
+$  shape  ?(%void %noun %atom %cell %junc)
::
::  A `role` is the of a type, including a more refined understanding
::  of what role it plays within a fork.
::
::  Nodes referenced within a `role` often do not actually exist in the
::  original type, since we need to reorganize forks in order to make
::  them more coherent.
::
::  - `%void` -- impossible to create.
::  - `%noun` -- could be any noun.
::  - `%atom` -- always some type of atom; never a cell
::  - `%constant` -- a cell type who's head is a constant atom.
::  - `%tall` -- a cell type who's head is an atom.
::  - `%wide` -- a cell type who's head is also a cell
::  - `%instance` -- a cell type who's head is a constant atom.
::  - `%option` -- a union of types which are all constant atoms.
::  - `%union` -- a union of types which are all instances (cells who's
::    head is a constant atom).
::  - `%junction` -- a union of an atom type and a cell type.
::  - `%conjunction` -- a union of two cell types, one of them %wide
::     and the other %tall.
::  - `%misjunction` -- any other union type. There's no efficient way
::    to tell which branch to take when analyzing a fork which is a
::    %misjunction, and the type is probably improperly constructed.
::
+$  role
  $@  $?  %void  %noun  %atom  %tall  %wide  ==
  $%  [%constant =atom]
      [%instance =atom]
      [%option =(map atom key)]
      [%union =(map atom key)]
      [%junction flat=key deep=key]
      [%conjunction wide=key tall=key]
      [%misjunction one=key two=key]
  ==
::
::  This is just a utility type, it encodes the "battery" structure
::  within a core.
::
::  It's a map from chapter names to the documentation and arms within
::  that chapter.
::
+$  xbattery  (map term (pair what (map term key)))
::
::  A recipe tells us how a type was constructed.Direct
::
::  - `%direct` is a simple type like `term`, or `xray`.
::  - `%synthetic` is a constructed type, like `(list @)`.
::
+$  recipe
  $%  [%direct =term]
      [%synthetic =term =(list key)]
  ==
::
::  A `pattern` is high-level information about the shape of a type. This
::  is used for printing data.
::
::  This is fairly heuristic. [%a %b %c ~] is recognized as a `path`,
::  `[3 ~[4 5 6]]` is recognized as a list, etc.
::
::  Most of the patterns have names that make their purpose obvious:
::  for example, the %tape pattern means that data of type type can be
::  printed as if it had the `tape` type. However, `%gear` and `%gate`
::  might not be entirely obvious.
::
::  - The %gear pattern is any core with a cell subject.
::  - The %gate pattern is a core that looks like a gate.
::
+$  pattern
  $@  ?(%hoon %manx %json %nock %path %plum %skin %spec %tape %tour %type %vase)
  $%  [%gate sample=key product=key]
      [%gear sample=key context=key batt=xbattery]
      [%list item=key]
      [%tree item=key]
      [%unit item=key]
  ==
::
--
