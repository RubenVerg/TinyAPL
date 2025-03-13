# TinyAPL

<center><img src="https://raw.githubusercontent.com/RubenVerg/tinyapl/main/logo.png" width="100" alt="TinyAPL logo"></center>

TinyAPL (read like *tiny apple*) is a tiny APL dialect and interpreter in Haskell. It is being implemented as a series of articles available [here](https://blog.rubenverg.com/?tag=tinyapl).

Featured on the [Array Cast](https://www.arraycast.com/episodes/episode88-tinyapl)!

* Part 1: [Introduction & Arrays](https://blog.rubenverg.com/tinyapl_1_arrays)
* Part 2: [Functions & Operators](https://blog.rubenverg.com/tinyapl_2_functions_operators)
* Part 3: [More Primitives](https://blog.rubenverg.com/tinyapl_3_more_primitives)
* Part 4: [Finally, Parsing!](https://blog.rubenverg.com/tinyapl_4_parsing)
* Part 5: [Array Notation and Reductions](https://blog.rubenverg.com/tinyapl_5_array_notation_reductions)
* Part 6: [Tests, Docs, Each](https://blog.rubenverg.com/tinyapl_6_tests_docs_each)
* Part 7: [Quads, Key, Index](https://blog.rubenverg.com/tinyapl_7_quads_key_index)
* Part 8: [All About Rank, and a Web Interface](https://blog.rubenverg.com/tinyapl_8_rank_web)
* Part 9: [More Tacit!](https://blog.rubenverg.com/tinyapl_9_more_tacit)
* Part 10: [Wraps, Structs, a Standard Library](https://blog.rubenverg.com/tinyapl_10_wraps_structs_stdlib)
* Part 11: [Bases and Searching](https://blog.rubenverg.com/tinyapl_11_bases_searching)
* Part 12: [At Depth and Ordering](https://blog.rubenverg.com/tinyapl_12_at_depth_ordering)
* Part 13: [The Biggest Mistake, The Biggest Change](https://blog.rubenverg.com/tinyapl_13_biggest_mistake_biggest_change)
* Part 14: [(Wrap Your Head Around) Dictionaries](https://blog.rubenverg.com/tinyapl_14_dictionaries)
* Part 15: [Extra Arguments](https://blog.rubenverg.com/tinyapl_15_extra_arguments)

Documentation is available [here](https://tinyapl.rubenverg.com) and you can run the latest interpreter [here](https://tinyapl.rubenverg.com/run/latest)

## Features

Marked features are supported, unmarked features are planned

* Types
	* [x] Arrays
		* [x] complex numbers
		* [x] unicode characters
		* [x] boxes (nested arrays)
	* [x] Functions
	* [x] Monadic Operators ("Adverbs")
	* [x] Dyadic Operators ("Conjunctions")
* Syntax
	* [x] number literals
		* [x] decimal point `.`
		* [x] negative sign `¯`
		* [x] exponent notation `⏨`
		* [x] complex notation `ᴊ`
		* [ ] complex phase notation? `∠`
		* [x] infinities `∞`
	* [x] character literals `'`
	* [x] string literals `"`
		* [x] escapes with `⍘`
			* [x] `⍘⍘` -> `⍘`
			* [x] `⍘"` -> `"`
			* [x] `⍘n` -> newline
			* [x] `⍘r` -> carriage return
			* [x] `⍘t` -> tab
			* [ ] `⍘{22be}` -> unicode
	* [x] names
		* [x] array names (`abc`)
		* [x] function names (`Abc`)
		* [x] monadic operator names (`_Abc`)
		* [x] dyadic operator names (`_Abc_`)
	* [x] assignment `←`
	* [x] dfns/dops
		* [x] multiple statements `⋄`
		* [x] guards `:`
		* [x] early return `■`
		* [x] dfns `{...}`, dadvs `_{...}`, dconjs `_{...}_`
		* [x] refer to arguments and operands: `⍺` left argument, `⍵` right argument, `⍺⍺` left array operand, `⍶⍶` left function operand, `⍵⍵` right array operand, `⍹⍹` right function operand
		* [x] recursion: `∇` recurse function, `_∇` recurse adverb, `_∇_` recurse conjunction
	* [x] comments `⍝`
		* [x] inline comments? `⟃`/`⟄`
	* [x] console I/O with `⎕` and `⍞`
	* [x] array notation `⟨`/`⋄`/`⟩` and `[`/`⋄`/`]`
	* [x] trains and modifier trains \[`_`\]`⦅`/`⋄`/`⦆`\[`_`\]
	* [ ] multiple statements in an expression `(`/`⋄`/`)`
	* [x] ternary expressions `⍰`/`⍠`
* primitives
	* nouns
		* [x] `⍬` empty vector
		* [x] `∻` empty dictionary
		* [ ] `⦻` null
		* [ ] `∅` nan
	* number functions
		* [x] `+` conjugate
		* [x] `+` add
		* [x] `-` negate
		* [x] `-` subtract
		* [x] `×` direction
		* [x] `×` multiply
		* [x] `÷` reciprocal
		* [x] `÷` divide
			* `0÷0` is `1`
			* `x÷0` is an error
		* [x] `*` exponential
		* [x] `*` power
		* [x] `⍟` ln
		* [x] `⍟` log
			* `1⍟1` is `1`
			* `1⍟y` is an error
			* `x⍟0` is an error
		* [x] `√` square root
		* [x] `√` root
		* [x] `⌊` floor
		* [x] `⌊` minimum
		* [x] `⌈` ceiling
		* [x] `⌈` maximum
		* [x] `⸠` round
		* [x] `⸠` round to nearest
		* [x] `⌹` matrix inverse
		* [x] `⌹` matrix divide
		* [x] `!` factorial/gamma
		* [x] `!` binomial
		* [x] `|` magnitude
		* [x] `|` remainder
		* [x] `∨` greatest common divisor
		* [x] `∧` least common multiple
		* [x] `⊕` cartesian (`x⊕y` is $x + iy$)
		* [x] `⊕` pure imaginary (`⊕y` is $iy$)
		* [x] `⊗` polar (`x⊗y` is $xe^{iy}$)
		* [x] `⊗` unit polar (`⊗y` is $e^{iy}$)
		* [x] `∡` phase
		* [x] `∡` atan2
		* [x] `ℜ` real part
		* [x] `ℑ` imaginary part
		* [x] `⧺` increment
		* [x] `ⵧ` decrement
		* [x] `ⵧ` span
		* [x] `⊥` decode
		* [x] `⊥` base 2 decode
		* [x] `⊤` encode
		* [x] `⊤` base 2 encode
	* boolean functions
		* [x] `=` equals
		* [x] `≠` not equals
		* [x] `<` less
		* [x] `≤` less or equal
		* [x] `≥` greater or equal
		* [x] `>` greater
		* [x] `≡` identical
		* [x] `≢` not identical
		* [x] `⊲` precedes (TAO less)
		* [x] `⊴` precedes or identical (TAO less or equal)
		* [x] `⊵` succeeds or identical (TAO greater or equal)
		* [x] `⊳` succeeds (TAO greater)
		* [x] `∧` and
		* [x] `∨` or
		* [x] `⍲` nand
		* [x] `⍱` nor
		* [x] `~` not
	* character functions
		* [x] `⌊` lowercase
		* [x] `⌈` uppercase
		* [x] `|` case fold
		* [x] `×` case
	* set functions
		* [x] `∪` unique
		* [x] `∪` union
		* [x] `∩` intersection
		* [x] `~` difference
		* [x] `§` symmetric difference
		* [x] `≠` nub sieve
		* [ ] multisets?
	* property functions
		* [x] `⍳` index of
		* [x] `⍸` interval index
		* [x] `∊` element of
		* [x] `⍷` find
		* [x] `⋷` mask
		* [x] `⋵` histogram (inverse of where)
		* [x] `⋵` count
		* [x] `≡` depth
		* [x] `⍴` shape
		* [x] `≢` tally
		* [x] `ϼ` rank
	* array creation functions
		* [x] `⍳` index generator
		* [x] `?` roll
		* [x] `?` deal
		* [x] `…` range
		* [x] `…` one range
		* [x] `⍮` pair
		* [x] `⍮` singleton
	* dictionary functions
		* [x] `‥` from keys and values
		* [x] `߹` key-value pair
		* [x] `‥` from inverted table
		* [x] `߹` from pairs
		* [x] `⍉` inverted table
		* [x] `↓` key-value pairs
		* [x] `⊃` keys
		* [x] `⊇` values
	* manipulation functions
		* [x] `↑` take
		* [x] `↓` drop
		* [x] `↑` mix
		* [x] `↓` major cells (split is `⊂⍤1`)
		* [x] `⊂` enclose
		* [ ] `⊆` nest
		* [x] `⊂` partitioned enclose
		* [x] `⊆` partition
		* [x] `⫇` group
		* [x] `⍋` grade up
		* [x] `⍒` grade down
		* [x] `⍋` sort by up
		* [x] `⍒` sort by down
		* [x] `⊴` sort up
		* [x] `⊵` sort down
		* [x] `⍸` where
		* [x] `∊` enlist
		* [x] `⌿` replicate
		* [x] `,` ravel
		* [x] `⍪` join
		* [x] `⍪` catenate
		* [x] `⍴` reshape
		* [x] `ϼ` rerank (generalized version of promote/demote: introduce leading length-1 axes or combine leading axes)
		* [x] `⊖` reverse
		* [x] `⊖` rotate
		* [x] `⍉` transpose
		* [x] `⍉` reorder axes
		* [x] `∧` promote (introduce leading axis)
		* [x] `∨` demote (combine two leading axes)
		* [x] `,` laminate
	* lookup functions
		* [x] `⊃` first
		* [x] `⊇` last
		* [x] `>` first cell
		* [x] `≥` last cell
		* [x] `⊇` from
		* [x] `⌷` index
		* [ ] `⊃` pick
	* misc functions
		* [x] `⊢`/`⊣` same
		* [x] `⊢` right
		* [x] `⊣` left
		* [x] `⍎` execute
		* [x] `⍕` format
		* [x] `↗` raise
		* [x] `⇂` minimal (TAO minimum)
		* [x] `↾` maximal (TAO maximum)
	* operators
		* [x] `/` reduce (left-to-right)
		* [x] `/` fold (seeded reduce)
		* [x] `\` reduce back (right-to-left)
		* [x] `\` fold back
		* [x] `↟` on prefixes
		* [x] `↡` on suffixes
		* [x] `↡` on infixes
		* [ ] `↟` on outfixes
		* [x] `¨` each
		* [x] `ᐵ` each-left
		* [x] `ᑈ` each-right
		* [x] `ᑣ` boxed (`⊂⍤`)
		* [x] `ᑒ` on contents (`⍥⊃`)
		* [x] `⍣` repeat
		* [x] `⍣` until
		* [x] `∙` inner product
		* [ ] `∙` alternant?
		* [x] `⊞` table
		* [x] `⍤` at rank
		* [x] `◡` on cells (`⍤¯1`)
		* [x] `◠` on scalars (`⍤0`)
		* [x] `ᓗ` cells-left (`⍤¯1‿∞`)
		* [x] `ᓚ` cells-right (`⍤∞‿¯1`)
		* [x] `⍥` at depth
		* [x] `⌓` on simple scalars (`⍥0`)
		* [ ] `@` at
		* [x] `⌸` key
		* [ ] `⌸` key with vocabulary
		* [ ] `⍁` diagonals?
		* [ ] `⌺` stencil
		* [x] `⁖` valences (call left if monad and right if dyad)
		* [x] `⍢` strucutral under
		* [x] `∵` bitwise
	* combinators
		* [x] `⍨` constant
		* [x] `⍨` commute
		* [x] `⍨` duplicate
		* [x] `∘` bind argument
		* [x] `∘` after
		* [x] `⍛` default argument (uses operand if called monadically, and argument if called dyadically)
		* [x] `⍛` before (not a hook!)
		* [x] `⊸` left hook
		* [x] `⟜` right hook
		* [x] `⍤` atop
		* [x] `⍥` over
		* [x] `⸚` mirror (`_{(⍵ ⍹⍹ ⍺)⍶⍶(⍺ ⍹⍹ ⍵)}_`)
		* [x] `«`/`»` fork
		* [x] `⇾` left fork (`_{(⍺ ⍶⍶ ⍵) ⍹⍹ ⍵}_`)
		* [x] `⇽` right fork (`_{⍺ ⍶⍶ (⍺ ⍹⍹ ⍵)}_`)
		* [x] `⫤` ident (adverb returning the operand)
		* [x] `⫣` lev (conjunction returning the left operand)
		* [x] `⊩` dex (conjunction returning the right operand)
* system names (quad names)
	* [x] support for system names in parsing and interpreting
	* system nilads
		* [x] `⎕u` (constant) the uppercase alphabet
		* [x] `⎕l` (constant) the lowercase alphabet
		* [x] `⎕d` (constant) the digits
		* [x] `⎕ts` current timestamp (year, month, day, hour, minute, second, millisecond)
		* [x] `⎕unix` current Unix timestamp
		* [x] `⎕io` (constant) `1`
		* [x] `⎕ct` (constant) `1⏨¯14`
		* [x] `⎕seed` (set only) seed the random number generator
		* [x] `⎕math` math functions
		* [x] `⎕systemInfo` system information
	* system functions
		* [ ] `⎕File` read/write files
		* [ ] `⎕DateTime` (or `⎕DT` maybe?) convert between time formats
		* [ ] `⎕HTTP` http requests
		* [ ] `⎕CSV` convert from/to CSV
		* [ ] `⎕JSON` convert from/to JSON
		* [x] `⎕Unicode` convert between unicode representations
		* [x] `⎕Exists` does a variable exist?
		* [x] `⎕Repr` convert an array to a representation that can be read with Execute
		* [ ] `⎕Assert` assertions
		* [x] `⎕Delay` sleep for n seconds
		* [ ] `⎕Import` import a module
		* [x] `⎕Type` type of a scalar
	* system adverbs
		* [ ] `⎕_BinFile` read/write binary files, with format chosen from the operand
	* system conjunctions
