module TinyAPL.Glyphs where

-- * Syntax

decimal = '.'
negative = '¯'
exponent = '⏨'
imaginary = 'ᴊ'
infinity = '∞'
charDelimiter = '\''
stringDelimiter = '"'
stringEscape = '⍘'
parens = ('(', ')')
braces = ('{', '}')
vector = ('⟨', '⟩')
highRank = ('[', ']')
underscore = '_'
separator = '⋄'
assign = '←'
assignModify = '↩'
assignConstant = '⇇'
assignPrivate = '↚'
guard = ':'
exit = '■'
comment = '⍝'
inlineComment = ('⟃', '⟄')
tie = '‿'
train = ('⦅', '⦆')
wrap = '⊏'
unwrap = '⊐'
struct = ('⦃', '⦄')
access = '→'
ternary = ('⍰', '⍠')
extraArgs = ('⦋', '⦌')
nothing = '·'

syntax =
  [ TinyAPL.Glyphs.decimal
  , TinyAPL.Glyphs.negative
  , TinyAPL.Glyphs.exponent
  , TinyAPL.Glyphs.imaginary
  , TinyAPL.Glyphs.infinity
  , TinyAPL.Glyphs.charDelimiter
  , TinyAPL.Glyphs.stringDelimiter
  , TinyAPL.Glyphs.stringEscape
  , fst TinyAPL.Glyphs.parens
  , snd TinyAPL.Glyphs.parens
  , fst TinyAPL.Glyphs.braces
  , snd TinyAPL.Glyphs.braces
  , fst TinyAPL.Glyphs.vector
  , snd TinyAPL.Glyphs.vector
  , fst TinyAPL.Glyphs.highRank
  , snd TinyAPL.Glyphs.highRank
  , TinyAPL.Glyphs.underscore
  , TinyAPL.Glyphs.separator
  , TinyAPL.Glyphs.assign
  , TinyAPL.Glyphs.assignModify
  , TinyAPL.Glyphs.assignConstant
  , TinyAPL.Glyphs.assignPrivate
  , TinyAPL.Glyphs.guard
  , TinyAPL.Glyphs.exit
  , TinyAPL.Glyphs.comment
  , fst TinyAPL.Glyphs.inlineComment
  , snd TinyAPL.Glyphs.inlineComment
  , TinyAPL.Glyphs.tie
  , fst TinyAPL.Glyphs.train
  , snd TinyAPL.Glyphs.train
  , TinyAPL.Glyphs.wrap
  , TinyAPL.Glyphs.unwrap
  , fst TinyAPL.Glyphs.struct
  , snd TinyAPL.Glyphs.struct
  , TinyAPL.Glyphs.access
  , fst TinyAPL.Glyphs.ternary
  , snd TinyAPL.Glyphs.ternary
  , fst TinyAPL.Glyphs.extraArgs
  , snd TinyAPL.Glyphs.extraArgs
  , TinyAPL.Glyphs.nothing ]

escapes =
  [ (stringDelimiter, stringDelimiter)
  , (charDelimiter, charDelimiter)
  , (stringEscape, stringEscape)
  , ('n', '\n')
  , ('r', '\r')
  , ('t', '\t')
  , ('0', toEnum 0) ]

-- * Identifiers

delta = '∆'
deltaBar = '⍙'
alpha = '⍺'
alphaBar = '⍶'
omega = '⍵'
omegaBar = '⍹'
epsilon = 'ɛ'
quad = '⎕'
quadQuote = '⍞'
del = '∇'

identifiers =
  [ TinyAPL.Glyphs.delta
  , TinyAPL.Glyphs.deltaBar
  , TinyAPL.Glyphs.alpha
  , TinyAPL.Glyphs.alphaBar
  , TinyAPL.Glyphs.omega
  , TinyAPL.Glyphs.omegaBar
  , TinyAPL.Glyphs.epsilon
  , TinyAPL.Glyphs.quad
  , TinyAPL.Glyphs.quadQuote
  , TinyAPL.Glyphs.del ]

-- * Arrays

zilde = '⍬'
cilde = '∻'

arrays =
  [ TinyAPL.Glyphs.zilde
  , TinyAPL.Glyphs.cilde ]

-- * Functions

plus = '+'
minus = '-'
times = '×'
divide = '÷'
power = '*'
logarithm = '⍟'
root = '√'
floor = '⌊'
ceil = '⌈'
round = '⸠'
less = '<'
lessEqual = '≤'
equal = '='
greaterEqual = '≥'
greater = '>'
notEqual = '≠'
and = '∧'
or = '∨'
nand = '⍲'
nor = '⍱'
cartesian = '⊕'
polar = '⊗'
identical = '≡'
notIdentical = '≢'
rho = '⍴'
ravel = ','
reverse = '⌽'
pair = '⍮'
enclose = '⊂'
first = '⊃'
last = '⊇'
take = '↑'
drop = '↓'
left = '⊣'
right = '⊢'
iota = '⍳'
indices = '⍸'
replicate = '⌿'
abs = '|'
phase = '∡'
real = 'ℜ'
imag = 'ℑ'
union = '∪'
intersection = '∩'
difference = '~'
symdiff = '§'
element = '∊'
roll = '?'
squad = '⌷'
rank = 'ϼ'
catenate = '⍪'
gradeUp = '⍋'
gradeDown = '⍒'
precedes = '⊲'
precedesOrIdentical = '⊴'
succeedsOrIdentical = '⊵'
succeeds = '⊳'
minimal = '⇂'
maximal = '↾'
transpose = '⍉'
matrixInverse = '⌹'
factorial = '!'
raise = '⨳'
decode = '⊥'
encode = '⊤'
histogram = '⋵'
increment = '⧺'
decrement = 'ⵧ'
range = '…'
keyValue = '߹'
invertedTable = '‥'
group = '⫇'
partition = '⊆'
execute = '⍎'
format = '⍕'
find = '⍷'
mask = '⋷'
raises = '↗'
lcm = '⩓'
gcd = '⩔'

functions =
  [ TinyAPL.Glyphs.plus
  , TinyAPL.Glyphs.minus
  , TinyAPL.Glyphs.times
  , TinyAPL.Glyphs.divide
  , TinyAPL.Glyphs.power
  , TinyAPL.Glyphs.logarithm
  , TinyAPL.Glyphs.root
  , TinyAPL.Glyphs.floor
  , TinyAPL.Glyphs.ceil
  , TinyAPL.Glyphs.round
  , TinyAPL.Glyphs.less
  , TinyAPL.Glyphs.lessEqual
  , TinyAPL.Glyphs.equal
  , TinyAPL.Glyphs.greaterEqual
  , TinyAPL.Glyphs.greater
  , TinyAPL.Glyphs.notEqual
  , TinyAPL.Glyphs.and
  , TinyAPL.Glyphs.or
  , TinyAPL.Glyphs.nand
  , TinyAPL.Glyphs.nor
  , TinyAPL.Glyphs.cartesian
  , TinyAPL.Glyphs.polar
  , TinyAPL.Glyphs.identical
  , TinyAPL.Glyphs.notIdentical
  , TinyAPL.Glyphs.rho
  , TinyAPL.Glyphs.ravel
  , TinyAPL.Glyphs.reverse
  , TinyAPL.Glyphs.pair
  , TinyAPL.Glyphs.enclose
  , TinyAPL.Glyphs.first
  , TinyAPL.Glyphs.last
  , TinyAPL.Glyphs.take
  , TinyAPL.Glyphs.drop
  , TinyAPL.Glyphs.left
  , TinyAPL.Glyphs.right
  , TinyAPL.Glyphs.iota
  , TinyAPL.Glyphs.indices
  , TinyAPL.Glyphs.replicate
  , TinyAPL.Glyphs.abs
  , TinyAPL.Glyphs.phase
  , TinyAPL.Glyphs.real
  , TinyAPL.Glyphs.imag
  , TinyAPL.Glyphs.union
  , TinyAPL.Glyphs.intersection
  , TinyAPL.Glyphs.difference
  , TinyAPL.Glyphs.symdiff
  , TinyAPL.Glyphs.element
  , TinyAPL.Glyphs.roll
  , TinyAPL.Glyphs.squad
  , TinyAPL.Glyphs.rank
  , TinyAPL.Glyphs.catenate
  , TinyAPL.Glyphs.gradeUp
  , TinyAPL.Glyphs.gradeDown
  , TinyAPL.Glyphs.precedes
  , TinyAPL.Glyphs.precedesOrIdentical
  , TinyAPL.Glyphs.succeedsOrIdentical
  , TinyAPL.Glyphs.succeeds
  , TinyAPL.Glyphs.minimal
  , TinyAPL.Glyphs.maximal
  , TinyAPL.Glyphs.transpose
  , TinyAPL.Glyphs.matrixInverse
  , TinyAPL.Glyphs.factorial
  , TinyAPL.Glyphs.raise
  , TinyAPL.Glyphs.decode
  , TinyAPL.Glyphs.encode
  , TinyAPL.Glyphs.histogram
  , TinyAPL.Glyphs.increment
  , TinyAPL.Glyphs.decrement
  , TinyAPL.Glyphs.range
  , TinyAPL.Glyphs.keyValue
  , TinyAPL.Glyphs.invertedTable
  , TinyAPL.Glyphs.group
  , TinyAPL.Glyphs.partition
  , TinyAPL.Glyphs.execute
  , TinyAPL.Glyphs.format
  , TinyAPL.Glyphs.find
  , TinyAPL.Glyphs.mask
  , TinyAPL.Glyphs.raises
  , TinyAPL.Glyphs.lcm
  , TinyAPL.Glyphs.gcd ]

-- * Adverbs

selfie = '⍨'
reduce = '/'
onPrefixes = '\\'
onInfixes = '⌺'
each = '¨'
eachLeft = 'ᐵ'
eachRight = 'ᑈ'
key = '⌸'
onCells = '◡'
onScalars = '◠'
boxed = 'ᑣ'
onContents = 'ᑒ'
table = '⊞'
ident = '⫤'
onSimpleScalars = '⌓'
originOne = '⑴'
backward = '⤺'
bitwise = '∵'
cellsLeft = 'ᓗ'
cellsRight = 'ᓚ'
inverse = '˝'
onLeft = '⥽'
onRight = '⥼'
multisets = '⍦'

adverbs =
  [ TinyAPL.Glyphs.selfie
  , TinyAPL.Glyphs.reduce
  , TinyAPL.Glyphs.onPrefixes
  , TinyAPL.Glyphs.onInfixes
  , TinyAPL.Glyphs.each
  , TinyAPL.Glyphs.eachLeft
  , TinyAPL.Glyphs.eachRight
  , TinyAPL.Glyphs.key
  , TinyAPL.Glyphs.onCells
  , TinyAPL.Glyphs.onScalars
  , TinyAPL.Glyphs.boxed
  , TinyAPL.Glyphs.onContents
  , TinyAPL.Glyphs.table
  , TinyAPL.Glyphs.ident
  , TinyAPL.Glyphs.onSimpleScalars
  , TinyAPL.Glyphs.originOne
  , TinyAPL.Glyphs.backward
  , TinyAPL.Glyphs.bitwise
  , TinyAPL.Glyphs.cellsLeft
  , TinyAPL.Glyphs.cellsRight
  , TinyAPL.Glyphs.inverse
  , TinyAPL.Glyphs.onLeft
  , TinyAPL.Glyphs.onRight
  , TinyAPL.Glyphs.multisets ]

-- * Conjunctions

atop = '∘'
over = '○'
reverseAtop = '⍛'
reverseOver = '⍜'
leftHook = '⊸'
rightHook = '⟜'
mirror = '⸚'
leftFork = '⇾'
rightFork = '⇽'
atRank = '⍤'
atDepth = '⍥'
repeat = '⍣'
valences = '⁖'
under = '⍢'
innerProduct = '∙'
lev = '⫣'
dex = '⊩'
forkA = '«'
forkB = '»'
approximate = '≈'
fill = '⬚'
catch = '⎊'
at = '@'

conjunctions =
  [ TinyAPL.Glyphs.atop
  , TinyAPL.Glyphs.over
  , TinyAPL.Glyphs.reverseAtop
  , TinyAPL.Glyphs.reverseOver
  , TinyAPL.Glyphs.leftHook 
  , TinyAPL.Glyphs.rightHook
  , TinyAPL.Glyphs.mirror
  , TinyAPL.Glyphs.atRank
  , TinyAPL.Glyphs.atDepth
  , TinyAPL.Glyphs.leftFork
  , TinyAPL.Glyphs.rightFork
  , TinyAPL.Glyphs.repeat
  , TinyAPL.Glyphs.valences
  , TinyAPL.Glyphs.under
  , TinyAPL.Glyphs.innerProduct
  , TinyAPL.Glyphs.lev
  , TinyAPL.Glyphs.dex
  , TinyAPL.Glyphs.forkA
  , TinyAPL.Glyphs.forkB
  , TinyAPL.Glyphs.approximate
  , TinyAPL.Glyphs.fill
  , TinyAPL.Glyphs.catch
  , TinyAPL.Glyphs.at ]
