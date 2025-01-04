module TinyAPL.Primitives where

import TinyAPL.ArrayFunctionOperator
import qualified TinyAPL.Functions as F
import qualified TinyAPL.Glyphs as G
import TinyAPL.Error
import TinyAPL.Util (headPromise)

-- * Primitive arrays

zilde = vector []
cilde = dictionary []

arrays =
  [ (G.zilde, zilde)
  , (G.cilde, cilde) ]

-- * Primitive functions

plus = PrimitiveFunction (Just $ const F.conjugate') (Just $ const F.add') [G.plus] Nothing
minus = PrimitiveFunction (Just $ const F.neg') (Just $ const F.sub') [G.minus] Nothing
times = PrimitiveFunction (Just $ const F.sign') (Just $ const F.times') [G.times] Nothing
divide = PrimitiveFunction (Just $ const F.reciprocal') (Just $ const F.divide') [G.divide] Nothing
power = PrimitiveFunction (Just $ const F.ePow') (Just $ const F.pow') [G.power] Nothing
logarithm = PrimitiveFunction (Just $ const F.ln') (Just $ const F.log') [G.logarithm] Nothing
root = PrimitiveFunction (Just $ const F.squareRoot') (Just $ const F.root') [G.root] Nothing
floor = PrimitiveFunction (Just $ const F.floor') (Just $ const F.min') [G.floor] Nothing
ceil = PrimitiveFunction (Just $ const F.ceil') (Just $ const F.max') [G.ceil] Nothing
round = PrimitiveFunction (Just $ const F.round') (Just $ const F.roundTo') [G.round] Nothing
less = PrimitiveFunction Nothing (Just $ const F.less') [G.less] Nothing
lessEqual = PrimitiveFunction Nothing (Just $ const F.lessEqual') [G.lessEqual] Nothing
equal = PrimitiveFunction Nothing (Just $ const F.equal') [G.equal] Nothing
greaterEqual = PrimitiveFunction Nothing (Just $ const F.greaterEqual') [G.greaterEqual] Nothing
greater = PrimitiveFunction Nothing (Just $ const F.greater') [G.greater] Nothing
notEqual = PrimitiveFunction (Just $ const F.nubSieve') (Just $ const F.notEqual') [G.notEqual] Nothing
and = PrimitiveFunction (Just $ const F.promote) (Just $ const F.lcm') [G.and] Nothing
or = PrimitiveFunction (Just $ const F.demote) (Just $ const F.gcd') [G.or] Nothing
nand = PrimitiveFunction Nothing (Just $ const F.nand') [G.nand] Nothing
nor = PrimitiveFunction Nothing (Just $ const F.nor') [G.nor] Nothing
cartesian = PrimitiveFunction (Just $ const F.imaginary') (Just $ const F.cartesian') [G.cartesian] Nothing
polar = PrimitiveFunction (Just $ const F.unitPolar') (Just $ const F.polar') [G.polar] Nothing
identical = PrimitiveFunction (Just $ const F.depth') (Just $ const F.identical') [G.identical] Nothing
notIdentical = PrimitiveFunction (Just $ const F.tally') (Just $ const F.notIdentical') [G.notIdentical] Nothing
rho = PrimitiveFunction (Just $ const F.shape') (Just $ const F.reshape') [G.rho] Nothing
ravel = PrimitiveFunction (Just $ const F.ravel') (Just $ const F.laminate) [G.ravel] Nothing
reverse = PrimitiveFunction (Just $ const F.reverse') (Just $ const F.rotate') [G.reverse] Nothing
pair = PrimitiveFunction (Just $ const F.singleton) (Just $ const F.pair) [G.pair] Nothing
enclose = PrimitiveFunction (Just $ const F.enclose') (Just $ const F.partitionEnclose') [G.enclose] Nothing
first = PrimitiveFunction (Just $ const F.first) Nothing [G.first] Nothing
last = PrimitiveFunction (Just $ const F.last) (Just $ const F.from) [G.last] Nothing
take = PrimitiveFunction (Just $ const F.mix) (Just $ const F.take') [G.take] Nothing
drop = PrimitiveFunction (Just $ const F.majorCells') (Just $ const F.drop') [G.drop] Nothing
left = PrimitiveFunction (Just $ \_ x -> pure x) (Just $ \_ x _ -> pure x) [G.left] Nothing
right = PrimitiveFunction (Just $ \_ x -> pure x) (Just $ \_ _ y -> pure y) [G.right] Nothing
iota = PrimitiveFunction (Just $ const F.indexGenerator') (Just $ const F.indexOf) [G.iota] Nothing
indices = PrimitiveFunction (Just $ const F.indices) (Just $ const F.intervalIndex) [G.indices] Nothing
replicate = PrimitiveFunction Nothing (Just $ const F.replicate') [G.replicate] Nothing
abs = PrimitiveFunction (Just $ const F.abs') (Just $ const F.remainder') [G.abs] Nothing
phase = PrimitiveFunction (Just $ const F.phase') (Just $ const F.arctan') [G.phase] Nothing
real = PrimitiveFunction (Just $ const F.real') Nothing [G.real] Nothing
imag = PrimitiveFunction (Just $ const F.imag') Nothing [G.imag] Nothing
union = PrimitiveFunction (Just $ const F.unique') (Just $ const F.union') [G.union] Nothing
intersection = PrimitiveFunction Nothing (Just $ const F.intersection') [G.intersection] Nothing
difference = PrimitiveFunction (Just $ const F.not') (Just $ const F.difference') [G.difference] Nothing
symdiff = PrimitiveFunction Nothing (Just $ const F.symmetricDifference') [G.symdiff] Nothing
element = PrimitiveFunction (Just $ const F.enlist') (Just $ const F.elementOf) [G.element] Nothing
roll = PrimitiveFunction (Just $ const F.roll') (Just $ const F.deal') [G.roll] Nothing
squad = PrimitiveFunction Nothing (Just $ const F.squad) [G.squad] Nothing
rank = PrimitiveFunction (Just $ const F.rank') (Just $ const F.rerank') [G.rank] Nothing
catenate = PrimitiveFunction (Just $ const F.join') (Just $ const F.catenate) [G.catenate] Nothing
gradeUp = PrimitiveFunction (Just $ const F.gradeUp') (Just $ const F.sortByUp') [G.gradeUp] Nothing
gradeDown = PrimitiveFunction (Just $ const F.gradeDown') (Just $ const F.sortByDown') [G.gradeDown] Nothing
precedes = PrimitiveFunction Nothing (Just $ const F.precedes') [G.precedes] Nothing
precedesOrIdentical = PrimitiveFunction (Just $ const F.sortUp') (Just $ const F.precedesOrIdentical') [G.precedesOrIdentical] Nothing
succeedsOrIdentical = PrimitiveFunction (Just $ const F.sortDown') (Just $ const F.succeedsOrIdentical') [G.succeedsOrIdentical] Nothing
succeeds = PrimitiveFunction Nothing (Just $ const F.succeeds') [G.succeeds] Nothing
minimal = PrimitiveFunction Nothing (Just $ const F.minimal) [G.minimal] Nothing
maximal = PrimitiveFunction Nothing (Just $ const F.maximal) [G.maximal] Nothing
transpose = PrimitiveFunction (Just $ const F.transpose) (Just $ const F.reorderAxes') [G.transpose] Nothing
matrixInverse = PrimitiveFunction (Just $ const F.matrixInverse') (Just $ const F.matrixDivide') [G.matrixInverse] Nothing
factorial = PrimitiveFunction (Just $ const F.factorial') (Just $ const F.binomial') [G.factorial] Nothing
raise = PrimitiveFunction (Just $ const F.raise1) (Just $ const F.raise') [G.raise] Nothing
decode = PrimitiveFunction (Just $ const F.decodeBase2) (Just $ const F.decode') [G.decode] Nothing
encode = PrimitiveFunction (Just $ const F.encodeBase2) (Just $ const F.encode') [G.encode] Nothing
histogram = PrimitiveFunction Nothing (Just $ const F.count) [G.histogram] Nothing
increment = PrimitiveFunction (Just $ const F.increment') Nothing [G.increment] Nothing
decrement = PrimitiveFunction (Just $ const F.decrement') (Just $ const F.span') [G.decrement] Nothing
range = PrimitiveFunction (Just $ const F.oneRange) (Just $ const F.range) [G.range] Nothing
keyValue = PrimitiveFunction (Just $ const F.fromPairs) (Just $ const F.keyValuePair) [G.keyValue] Nothing
invertedTable = PrimitiveFunction (Just $ const F.fromInvertedTable) (Just $ const F.fromKeysAndValues') [G.invertedTable] Nothing
group = PrimitiveFunction Nothing (Just $ const F.group') [G.group] Nothing
partition = PrimitiveFunction Nothing (Just $ const F.partition') [G.partition] Nothing
execute = PrimitiveFunction (Just $ const F.execute') Nothing [G.execute] Nothing
format = PrimitiveFunction (Just $ const F.format') Nothing [G.format] Nothing

functions = (\x -> (headPromise $ functionRepr x, x)) <$>
  [ TinyAPL.Primitives.plus
  , TinyAPL.Primitives.minus
  , TinyAPL.Primitives.times
  , TinyAPL.Primitives.divide
  , TinyAPL.Primitives.power
  , TinyAPL.Primitives.logarithm
  , TinyAPL.Primitives.root
  , TinyAPL.Primitives.floor
  , TinyAPL.Primitives.ceil
  , TinyAPL.Primitives.round
  , TinyAPL.Primitives.less
  , TinyAPL.Primitives.lessEqual
  , TinyAPL.Primitives.equal
  , TinyAPL.Primitives.greaterEqual
  , TinyAPL.Primitives.greater
  , TinyAPL.Primitives.notEqual
  , TinyAPL.Primitives.and
  , TinyAPL.Primitives.or
  , TinyAPL.Primitives.nand
  , TinyAPL.Primitives.nor
  , TinyAPL.Primitives.cartesian
  , TinyAPL.Primitives.polar
  , TinyAPL.Primitives.identical
  , TinyAPL.Primitives.notIdentical
  , TinyAPL.Primitives.rho
  , TinyAPL.Primitives.ravel
  , TinyAPL.Primitives.reverse
  , TinyAPL.Primitives.pair
  , TinyAPL.Primitives.enclose
  , TinyAPL.Primitives.first
  , TinyAPL.Primitives.last
  , TinyAPL.Primitives.take
  , TinyAPL.Primitives.drop
  , TinyAPL.Primitives.left
  , TinyAPL.Primitives.right
  , TinyAPL.Primitives.iota
  , TinyAPL.Primitives.indices
  , TinyAPL.Primitives.replicate
  , TinyAPL.Primitives.abs
  , TinyAPL.Primitives.phase
  , TinyAPL.Primitives.real
  , TinyAPL.Primitives.imag
  , TinyAPL.Primitives.union
  , TinyAPL.Primitives.intersection
  , TinyAPL.Primitives.difference
  , TinyAPL.Primitives.symdiff
  , TinyAPL.Primitives.element
  , TinyAPL.Primitives.roll
  , TinyAPL.Primitives.squad
  , TinyAPL.Primitives.rank
  , TinyAPL.Primitives.catenate
  , TinyAPL.Primitives.gradeUp
  , TinyAPL.Primitives.gradeDown
  , TinyAPL.Primitives.precedes
  , TinyAPL.Primitives.precedesOrIdentical
  , TinyAPL.Primitives.succeedsOrIdentical
  , TinyAPL.Primitives.succeeds
  , TinyAPL.Primitives.minimal
  , TinyAPL.Primitives.maximal
  , TinyAPL.Primitives.transpose
  , TinyAPL.Primitives.matrixInverse
  , TinyAPL.Primitives.factorial
  , TinyAPL.Primitives.raise
  , TinyAPL.Primitives.decode
  , TinyAPL.Primitives.encode
  , TinyAPL.Primitives.histogram
  , TinyAPL.Primitives.increment
  , TinyAPL.Primitives.decrement
  , TinyAPL.Primitives.range
  , TinyAPL.Primitives.keyValue
  , TinyAPL.Primitives.invertedTable
  , TinyAPL.Primitives.group
  , TinyAPL.Primitives.partition
  , TinyAPL.Primitives.execute
  , TinyAPL.Primitives.format ]

-- * Primitive adverbs

selfie = PrimitiveAdverb
  { adverbRepr = [G.selfie]
  , adverbContext = Nothing
  , adverbOnNoun = Just $ \_ x -> pure $ DerivedFunctionNoun (Just $ const $ F.constant1 x) (Just $ const $ F.constant2 x) Nothing selfie x
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction (Just $ \ea -> F.duplicate $ callDyad f ea) (Just $ \ea -> F.commute $ callDyad f ea) Nothing selfie f }
reduce = PrimitiveAdverb
  { adverbRepr = [G.reduce]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction (Just $ \ea -> F.reduce' $ callDyad f ea) (Just $ \ea -> F.fold' $ callDyad f ea) Nothing reduce f }
reduceBack = PrimitiveAdverb
  { adverbRepr = [G.reduceBack]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction (Just $ \ea -> F.reduceBack' $ callDyad f ea) (Just $ \ea -> F.foldBack' $ callDyad f ea) Nothing reduceBack f }
onPrefixes = PrimitiveAdverb
  { adverbRepr = [G.onPrefixes]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction (Just $ \ea -> F.onPrefixes' $ callMonad f ea) Nothing Nothing onPrefixes f }
onSuffixes = PrimitiveAdverb
  { adverbRepr = [G.onSuffixes]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction (Just $ \ea -> F.onSuffixes' $ callMonad f ea) (Just $ \ea -> F.onInfixes' $ callMonad f ea) Nothing onSuffixes f }
each = PrimitiveAdverb
  { adverbRepr = [G.each]
  , adverbContext = Nothing
  , adverbOnNoun = Just $ \_ x -> pure $ DerivedFunctionNoun (Just $ const $ F.each1 $ F.constant1 x) (Just $ const $ F.each2 $ F.constant2 x) Nothing each x
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction (Just $ \ea -> F.each1 $ callMonad f ea) (Just $ \ea -> F.each2 $ callDyad f ea) Nothing each f }
eachLeft = PrimitiveAdverb
  { adverbRepr = [G.eachLeft]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction Nothing (Just $ \ea -> F.eachLeft $ callDyad f ea) Nothing eachLeft f }
eachRight = PrimitiveAdverb
  { adverbRepr = [G.eachRight]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction Nothing (Just $ \ea -> F.eachRight $ callDyad f ea) Nothing eachRight f }
key = PrimitiveAdverb
  { adverbRepr = [G.key]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction (Just $ \ea -> F.keyMonad $ callDyad f ea) (Just $ \ea -> F.key' $ callDyad f ea) Nothing key f }
onCells = PrimitiveAdverb
  { adverbRepr = [G.onCells]
  , adverbContext = Nothing
  , adverbOnNoun = Just $ \_ x -> pure $ DerivedFunctionNoun (Just $ const $ F.onCells1 $ F.constant1 x) (Just $ const $ F.onCells2 $ F.constant2 x) Nothing onCells x
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction (Just $ \ea -> F.onCells1 $ callMonad f ea) (Just $ \ea -> F.onCells2 $ callDyad f ea) Nothing onCells f }
onScalars = PrimitiveAdverb
  { adverbRepr = [G.onScalars]
  , adverbContext = Nothing
  , adverbOnNoun = Just $ \_ x -> pure $ DerivedFunctionNoun (Just $ const $ F.onScalars1 $ F.constant1 x) (Just $ const $ F.onScalars2 $ F.constant2 x) Nothing onScalars x
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction (Just $ \ea -> F.onScalars1 $ callMonad f ea) (Just $ \ea -> F.onScalars2 $ callDyad f ea) Nothing onScalars f }
boxed = PrimitiveAdverb
  { adverbRepr = [G.boxed]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction (Just $ \ea -> F.boxed1 $ callMonad f ea) (Just $ \ea -> F.boxed2 $ callDyad f ea) Nothing boxed f }
onContents = PrimitiveAdverb
  { adverbRepr = [G.onContents]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction (Just $ \ea -> F.onContents1 $ callMonad f ea) (Just $ \ea -> F.onContents2 $ callDyad f ea) Nothing onContents f }
table = PrimitiveAdverb
  { adverbRepr = [G.table]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction Nothing (Just $ \ea -> F.table $ callDyad f ea) Nothing table f }
ident = PrimitiveAdverb
  { adverbRepr = [G.ident]
  , adverbContext = Nothing
  , adverbOnNoun = Just $ \_ x -> pure $ DerivedFunctionNoun (Just $ const $ F.constant1 x) (Just $ const $ F.constant2 x) Nothing ident x
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction (Just $ callMonad f) (Just $ callDyad f) Nothing ident f }
onSimpleScalars = PrimitiveAdverb
  { adverbRepr = [G.onSimpleScalars]
  , adverbContext = Nothing
  , adverbOnNoun = Just $ \_ x -> pure $ DerivedFunctionNoun (Just $ const $ F.onSimpleScalars1 (F.constant1 x)) (Just $ const $ F.onSimpleScalars2 (F.constant2 x)) Nothing onSimpleScalars x
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction (Just $ \ea -> F.onSimpleScalars1 $ callMonad f ea) (Just $ \ea -> F.onSimpleScalars2 $ callDyad f ea) Nothing onSimpleScalars f }

adverbs = (\x -> (headPromise $ adverbRepr x, x)) <$>
  [ TinyAPL.Primitives.selfie
  , TinyAPL.Primitives.reduce
  , TinyAPL.Primitives.reduceBack
  , TinyAPL.Primitives.onPrefixes
  , TinyAPL.Primitives.onSuffixes
  , TinyAPL.Primitives.each
  , TinyAPL.Primitives.eachLeft
  , TinyAPL.Primitives.eachRight
  , TinyAPL.Primitives.key
  , TinyAPL.Primitives.onCells
  , TinyAPL.Primitives.onScalars
  , TinyAPL.Primitives.boxed
  , TinyAPL.Primitives.onContents
  , TinyAPL.Primitives.table
  , TinyAPL.Primitives.ident
  , TinyAPL.Primitives.onSimpleScalars ]

-- * Primitive conjunctions

atop = PrimitiveConjunction
  { conjRepr = [G.atop]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Just $ \_ f r -> pure $ DerivedFunctionFunctionNoun (Just $ \ea -> F.atRank1' (callMonad f ea) r) (Just $ \ea -> F.atRank2' (callDyad f ea) r) Nothing atop f r
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (Just $ \ea -> F.compose (callMonad f ea) (callMonad g ea)) (Just $ \ea -> F.atop (callMonad f ea) (callDyad g ea)) Nothing atop f g }
over = PrimitiveConjunction
  { conjRepr = [G.over]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Just $ \_ f d -> pure $ DerivedFunctionFunctionNoun (Just $ \ea -> F.atDepth1' (callMonad f ea) d) (Just $ \ea -> F.atDepth2' (callDyad f ea) d) Nothing over f d
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (Just $ \ea -> F.compose (callMonad f ea) (callMonad g ea)) (Just $ \ea -> F.over (callDyad f ea) (callMonad g ea)) Nothing over f g }
after = PrimitiveConjunction
  { conjRepr = [G.after]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Just $ \_ x g -> pure $ DerivedFunctionNounFunction (Just $ \ea y -> callDyad g ea x y) Nothing Nothing after x g
  , conjOnFunctionNoun = Just $ \_ f y -> pure $ DerivedFunctionFunctionNoun (Just $ \ea x -> callDyad f ea x y) Nothing Nothing after f y
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (Just $ \ea -> F.compose (callMonad f ea) (callMonad g ea)) (Just $ \ea -> F.after (callDyad f ea) (callMonad g ea)) Nothing after f g }
before = PrimitiveConjunction
  { conjRepr = [G.before]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Just $ \_ x g -> pure $ DerivedFunctionNounFunction (Just $ \ea y -> callDyad g ea x y) (Just $ \ea x' y -> callDyad g ea x' y) Nothing before x g
  , conjOnFunctionNoun = Just $ \_ f y -> pure $ DerivedFunctionFunctionNoun (Just $ \ea x -> callDyad f ea x y) (Just $ \ea x y' -> callDyad f ea x y') Nothing before f y
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (Just $ \ea -> F.reverseCompose (callMonad f ea) (callMonad g ea)) (Just $ \ea -> F.before (callMonad f ea) (callDyad g ea)) Nothing before f g }
leftHook = PrimitiveConjunction
  { conjRepr = [G.leftHook]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Nothing
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (Just $ \ea -> F.leftHook (callMonad f ea) (callDyad g ea)) (Just $ \ea -> F.before (callMonad f ea) (callDyad g ea)) Nothing leftHook f g }
rightHook = PrimitiveConjunction
  { conjRepr = [G.rightHook]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Nothing
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (Just $ \ea -> F.rightHook (callDyad f ea) (callMonad g ea)) (Just $ \ea -> F.after (callDyad f ea) (callMonad g ea)) Nothing rightHook f g }
mirror = PrimitiveConjunction
  { conjRepr = [G.mirror]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Nothing
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction Nothing (Just $ \ea -> F.mirror (callDyad f ea) (callDyad g ea)) Nothing mirror f g }
leftFork = PrimitiveConjunction
  { conjRepr = [G.leftFork]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Nothing
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (Just $ \ea -> F.leftHook (callMonad f ea) (callDyad g ea)) (Just $ \ea -> F.leftFork (callDyad f ea) (callDyad g ea)) Nothing leftFork f g }
rightFork = PrimitiveConjunction
  { conjRepr = [G.rightFork]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Nothing
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (Just $ \ea -> F.rightHook (callDyad f ea) (callMonad g ea)) (Just $ \ea -> F.rightFork (callDyad f ea) (callDyad g ea)) Nothing rightFork f g }
repeat = PrimitiveConjunction
  { conjRepr = [G.repeat]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Just $ \_ f y -> pure $ DerivedFunctionFunctionNoun (Just $ \ea -> F.repeat1 (callMonad f ea) y) (Just $ \ea -> F.repeat2 (callDyad f ea) y) Nothing TinyAPL.Primitives.repeat f y
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (Just $ \ea -> F.until1 (callMonad f ea) (callDyad g ea)) (Just $ \ea -> F.until2 (callDyad f ea) (callDyad g ea)) Nothing TinyAPL.Primitives.repeat f g }
valences = PrimitiveConjunction
  { conjRepr = [G.valences]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Nothing
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (Just $ callMonad f) (Just $ callDyad g) Nothing valences f g }
under = PrimitiveConjunction
  { conjRepr = [G.under]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Just $ \_ x g -> pure $ DerivedFunctionNounFunction (Just $ \ea -> F.underK x (callMonad g ea)) Nothing Nothing under x g
  , conjOnFunctionNoun = Nothing
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (Just $ \ea -> F.under (callMonad f ea) (callMonad g ea)) (Just $ \ea -> F.under2 (callDyad f ea) (callMonad g ea)) Nothing under f g }
innerProduct = PrimitiveConjunction
  { conjRepr = [G.innerProduct]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Nothing
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction Nothing (Just $ \ea -> F.innerProduct (callMonad f ea) (callDyad g ea)) Nothing innerProduct f g }
lev = PrimitiveConjunction
  { conjRepr = [G.lev]
  , conjContext = Nothing
  , conjOnNounNoun = Just $ \_ x y -> pure $ DerivedFunctionNounNoun (Just $ const $ F.constant1 x) (Just $ const $ F.constant2 x) Nothing lev x y
  , conjOnNounFunction = Just $ \_ x g -> pure $ DerivedFunctionNounFunction (Just $ const $ F.constant1 x) (Just $ const $ F.constant2 x) Nothing lev x g
  , conjOnFunctionNoun = Just $ \_ f y -> pure $ DerivedFunctionFunctionNoun (Just $ callMonad f) (Just $ callDyad f) Nothing lev f y
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (Just $ callMonad f) (Just $ callDyad f) Nothing lev f g }
dex = PrimitiveConjunction
  { conjRepr = [G.dex]
  , conjContext = Nothing
  , conjOnNounNoun = Just $ \_ x y -> pure $ DerivedFunctionNounNoun (Just $ const $ F.constant1 y) (Just $ const $ F.constant2 y) Nothing dex x y
  , conjOnNounFunction = Just $ \_ x g -> pure $ DerivedFunctionNounFunction (Just $ callMonad g) (Just $ callDyad g) Nothing dex x g
  , conjOnFunctionNoun = Just $ \_ f y -> pure $ DerivedFunctionFunctionNoun (Just $ const $ F.constant1 y) (Just $ const $ F.constant2 y) Nothing dex f y
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (Just $ callMonad g) (Just $ callDyad g) Nothing dex f g }
forkA = PrimitiveConjunction
  { conjRepr = [G.forkA]
  , conjContext = Nothing
  , conjOnNounNoun = Just $ \_ x y -> pure $ DerivedFunctionNounNoun (Just $ \_ _ -> message) (Just $ \_ _ _ -> message) Nothing forkA x y
  , conjOnNounFunction = Just $ \_ x g -> pure $ DerivedFunctionNounFunction (Just $ \_ _ -> message) (Just $ \_ _ _ -> message) Nothing forkA x g
  , conjOnFunctionNoun = Just $ \_ f y -> pure $ DerivedFunctionFunctionNoun (Just $ \_ _ -> message) (Just $ \_ _ _ -> message) Nothing forkA f y
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (Just $ \_ _ -> message) (Just $ \_ _ _ -> message) Nothing forkA f g }
  where message = throwError $ DomainError $ [G.forkA] ++ " must be used in conjunction with " ++ [G.forkB]
forkB = PrimitiveConjunction
  { conjRepr = [G.forkB]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Just $ \_ left z -> case left of
    DerivedFunctionNounNoun _ _ _ op x y | op == forkA -> pure $ DerivedFunctionFunctionNoun (Just $ const $ F.fork1 (F.constant1 x) (F.constant2 y) (F.constant1 z)) (Just $ const $ F.fork2 (F.constant2 x) (F.constant2 y) (F.constant2 z)) Nothing forkB left z
    DerivedFunctionNounFunction _ _ _ op x g | op == forkA -> pure $ DerivedFunctionFunctionNoun (Just $ \ea -> F.fork1 (F.constant1 x) (callDyad g ea) (F.constant1 z)) (Just $ \ea -> F.fork2 (F.constant2 x) (callDyad g ea) (F.constant2 z)) Nothing forkB left z
    DerivedFunctionFunctionNoun _ _ _ op f y | op == forkA -> pure $ DerivedFunctionFunctionNoun (Just $ \ea -> F.fork1 (callMonad f ea) (F.constant2 y) (F.constant1 z)) (Just $ \ea -> F.fork2 (callDyad f ea) (F.constant2 y) (F.constant2 z)) Nothing forkB left z
    DerivedFunctionFunctionFunction _ _ _ op f g | op == forkA -> pure $ DerivedFunctionFunctionNoun (Just $ \ea -> F.fork1 (callMonad f ea) (callDyad g ea) (F.constant1 z)) (Just $ \ea -> F.fork2 (callDyad f ea) (callDyad g ea) (F.constant2 z)) Nothing forkB left z
    _ -> message
  , conjOnFunctionFunction = Just $ \_ left h -> case left of
    DerivedFunctionNounNoun _ _ _ op x y | op == forkA -> pure $ DerivedFunctionFunctionFunction (Just $ \ea -> F.fork1 (F.constant1 x) (F.constant2 y) (callMonad h ea)) (Just $ \ea -> F.fork2 (F.constant2 x) (F.constant2 y) (callDyad h ea)) Nothing forkB left h
    DerivedFunctionNounFunction _ _ _ op x g | op == forkA -> pure $ DerivedFunctionFunctionFunction (Just $ \ea -> F.fork1 (F.constant1 x) (callDyad g ea) (callMonad h ea)) (Just $ \ea -> F.fork2 (F.constant2 x) (callDyad g ea) (callDyad h ea)) Nothing forkB left h
    DerivedFunctionFunctionNoun _ _ _ op f y | op == forkA -> pure $ DerivedFunctionFunctionFunction (Just $ \ea ->  F.fork1 (callMonad f ea) (F.constant2 y) (callMonad h ea)) (Just $ \ea -> F.fork2 (callDyad f ea) (F.constant2 y) (callDyad h ea)) Nothing forkB left h
    DerivedFunctionFunctionFunction _ _ _ op f g | op == forkA -> pure $ DerivedFunctionFunctionFunction (Just $ \ea -> F.fork1 (callMonad f ea) (callDyad g ea) (callMonad h ea)) (Just $ \ea -> F.fork2 (callDyad f ea) (callDyad g ea) (callDyad h ea)) Nothing forkB left h
    _ -> message }
  where message = throwError $ DomainError $ [G.forkA] ++ " must be used in conjunction with " ++ [G.forkB]

conjunctions = (\x -> (headPromise $ conjRepr x, x)) <$>
  [ TinyAPL.Primitives.atop
  , TinyAPL.Primitives.over
  , TinyAPL.Primitives.after
  , TinyAPL.Primitives.before
  , TinyAPL.Primitives.leftHook
  , TinyAPL.Primitives.rightHook
  , TinyAPL.Primitives.mirror
  , TinyAPL.Primitives.leftFork
  , TinyAPL.Primitives.rightFork
  , TinyAPL.Primitives.repeat
  , TinyAPL.Primitives.valences
  , TinyAPL.Primitives.under
  , TinyAPL.Primitives.innerProduct
  , TinyAPL.Primitives.lev
  , TinyAPL.Primitives.dex
  , TinyAPL.Primitives.forkA
  , TinyAPL.Primitives.forkB ]