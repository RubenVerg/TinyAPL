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

plus = PrimitiveFunction (Just F.conjugate') (Just F.add') [G.plus] Nothing
minus = PrimitiveFunction (Just F.neg') (Just F.sub') [G.minus] Nothing
times = PrimitiveFunction (Just F.sign') (Just F.times') [G.times] Nothing
divide = PrimitiveFunction (Just F.reciprocal') (Just F.divide') [G.divide] Nothing
power = PrimitiveFunction (Just F.ePow') (Just F.pow') [G.power] Nothing
logarithm = PrimitiveFunction (Just F.ln') (Just F.log') [G.logarithm] Nothing
root = PrimitiveFunction (Just F.squareRoot') (Just F.root') [G.root] Nothing
floor = PrimitiveFunction (Just F.floor') (Just F.min') [G.floor] Nothing
ceil = PrimitiveFunction (Just F.ceil') (Just F.max') [G.ceil] Nothing
round = PrimitiveFunction (Just F.round') (Just F.roundTo') [G.round] Nothing
less = PrimitiveFunction Nothing (Just F.less') [G.less] Nothing
lessEqual = PrimitiveFunction Nothing (Just F.lessEqual') [G.lessEqual] Nothing
equal = PrimitiveFunction Nothing (Just F.equal') [G.equal] Nothing
greaterEqual = PrimitiveFunction Nothing (Just F.greaterEqual') [G.greaterEqual] Nothing
greater = PrimitiveFunction Nothing (Just F.greater') [G.greater] Nothing
notEqual = PrimitiveFunction (Just F.nubSieve') (Just F.notEqual') [G.notEqual] Nothing
and = PrimitiveFunction (Just F.promote) (Just F.lcm') [G.and] Nothing
or = PrimitiveFunction (Just F.demote) (Just F.gcd') [G.or] Nothing
nand = PrimitiveFunction Nothing (Just F.nand') [G.nand] Nothing
nor = PrimitiveFunction Nothing (Just F.nor') [G.nor] Nothing
cartesian = PrimitiveFunction (Just F.imaginary') (Just F.cartesian') [G.cartesian] Nothing
polar = PrimitiveFunction (Just F.unitPolar') (Just F.polar') [G.polar] Nothing
identical = PrimitiveFunction (Just F.depth') (Just F.identical') [G.identical] Nothing
notIdentical = PrimitiveFunction (Just F.tally') (Just F.notIdentical') [G.notIdentical] Nothing
rho = PrimitiveFunction (Just F.shape') (Just F.reshape') [G.rho] Nothing
ravel = PrimitiveFunction (Just F.ravel') (Just F.laminate) [G.ravel] Nothing
reverse = PrimitiveFunction (Just F.reverse') (Just F.rotate') [G.reverse] Nothing
pair = PrimitiveFunction (Just F.singleton) (Just F.pair) [G.pair] Nothing
enclose = PrimitiveFunction (Just F.enclose') (Just F.partitionEnclose') [G.enclose] Nothing
first = PrimitiveFunction (Just F.first) Nothing [G.first] Nothing
last = PrimitiveFunction (Just F.last) (Just F.from) [G.last] Nothing
take = PrimitiveFunction (Just F.mix) (Just F.take') [G.take] Nothing
drop = PrimitiveFunction (Just F.majorCells') (Just F.drop') [G.drop] Nothing
left = PrimitiveFunction (Just $ \x -> pure x) (Just $ \x _ -> pure x) [G.left] Nothing
right = PrimitiveFunction (Just $ \x -> pure x) (Just $ \_ y -> pure y) [G.right] Nothing
iota = PrimitiveFunction (Just F.indexGenerator') (Just F.indexOf) [G.iota] Nothing
indices = PrimitiveFunction (Just F.indices) (Just F.intervalIndex) [G.indices] Nothing
replicate = PrimitiveFunction Nothing (Just F.replicate') [G.replicate] Nothing
abs = PrimitiveFunction (Just F.abs') (Just F.remainder') [G.abs] Nothing
phase = PrimitiveFunction (Just F.phase') (Just F.arctan') [G.phase] Nothing
real = PrimitiveFunction (Just F.real') Nothing [G.real] Nothing
imag = PrimitiveFunction (Just F.imag') Nothing [G.imag] Nothing
union = PrimitiveFunction (Just F.unique') (Just F.union') [G.union] Nothing
intersection = PrimitiveFunction Nothing (Just F.intersection') [G.intersection] Nothing
difference = PrimitiveFunction (Just F.not') (Just F.difference') [G.difference] Nothing
symdiff = PrimitiveFunction Nothing (Just F.symmetricDifference') [G.symdiff] Nothing
element = PrimitiveFunction (Just F.enlist') (Just $ F.elementOf) [G.element] Nothing
roll = PrimitiveFunction (Just F.roll') (Just F.deal') [G.roll] Nothing
squad = PrimitiveFunction Nothing (Just $ F.squad) [G.squad] Nothing
rank = PrimitiveFunction (Just F.rank') (Just F.rerank') [G.rank] Nothing
catenate = PrimitiveFunction (Just F.join') (Just F.catenate) [G.catenate] Nothing
gradeUp = PrimitiveFunction (Just F.gradeUp') (Just F.sortByUp') [G.gradeUp] Nothing
gradeDown = PrimitiveFunction (Just F.gradeDown') (Just F.sortByDown') [G.gradeDown] Nothing
precedes = PrimitiveFunction Nothing (Just F.precedes') [G.precedes] Nothing
precedesOrIdentical = PrimitiveFunction (Just $ F.sortUp') (Just F.precedesOrIdentical') [G.precedesOrIdentical] Nothing
succeedsOrIdentical = PrimitiveFunction (Just $ F.sortDown') (Just F.succeedsOrIdentical') [G.succeedsOrIdentical] Nothing
succeeds = PrimitiveFunction Nothing (Just F.succeeds') [G.succeeds] Nothing
minimal = PrimitiveFunction Nothing (Just F.minimal) [G.minimal] Nothing
maximal = PrimitiveFunction Nothing (Just F.maximal) [G.maximal] Nothing
transpose = PrimitiveFunction (Just F.transpose) (Just F.reorderAxes') [G.transpose] Nothing
matrixInverse = PrimitiveFunction (Just F.matrixInverse') (Just F.matrixDivide') [G.matrixInverse] Nothing
factorial = PrimitiveFunction (Just F.factorial') (Just F.binomial') [G.factorial] Nothing
raise = PrimitiveFunction (Just F.raise1) (Just F.raise') [G.raise] Nothing
decode = PrimitiveFunction (Just F.decodeBase2) (Just F.decode') [G.decode] Nothing
encode = PrimitiveFunction (Just F.encodeBase2) (Just F.encode') [G.encode] Nothing
histogram = PrimitiveFunction Nothing (Just F.count) [G.histogram] Nothing
increment = PrimitiveFunction (Just F.increment') Nothing [G.increment] Nothing
decrement = PrimitiveFunction (Just F.decrement') (Just F.span') [G.decrement] Nothing
range = PrimitiveFunction (Just F.oneRange) (Just F.range) [G.range] Nothing
keyValue = PrimitiveFunction (Just F.fromPairs) (Just F.keyValuePair) [G.keyValue] Nothing
invertedTable = PrimitiveFunction (Just F.fromInvertedTable) (Just F.fromKeysAndValues') [G.invertedTable] Nothing
group = PrimitiveFunction Nothing (Just F.group') [G.group] Nothing
partition = PrimitiveFunction Nothing (Just F.partition') [G.partition] Nothing
execute = PrimitiveFunction (Just F.execute') Nothing [G.execute] Nothing
format = PrimitiveFunction (Just F.format') Nothing [G.format] Nothing

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
  , adverbOnNoun = Just $ \x -> pure $ DerivedFunctionNoun (Just $ F.constant1 x) (Just $ F.constant2 x) Nothing selfie x
  , adverbOnFunction = Just $ \f -> pure $ DerivedFunctionFunction (Just $ F.duplicate $ callDyad f) (Just $ F.commute $ callDyad f) Nothing selfie f }
reduce = PrimitiveAdverb
  { adverbRepr = [G.reduce]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \f -> pure $ DerivedFunctionFunction (Just $ F.reduce' $ callDyad f) (Just $ F.fold' $ callDyad f) Nothing reduce f }
reduceBack = PrimitiveAdverb
  { adverbRepr = [G.reduceBack]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \f -> pure $ DerivedFunctionFunction (Just $ F.reduceBack' $ callDyad f) (Just $ F.foldBack' $ callDyad f) Nothing reduceBack f }
onPrefixes = PrimitiveAdverb
  { adverbRepr = [G.onPrefixes]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \f -> pure $ DerivedFunctionFunction (Just $ F.onPrefixes' $ callMonad f) Nothing Nothing onPrefixes f }
onSuffixes = PrimitiveAdverb
  { adverbRepr = [G.onSuffixes]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \f -> pure $ DerivedFunctionFunction (Just $ F.onSuffixes' $ callMonad f) (Just $ F.onInfixes' $ callMonad f) Nothing onSuffixes f }
each = PrimitiveAdverb
  { adverbRepr = [G.each]
  , adverbContext = Nothing
  , adverbOnNoun = Just $ \x -> pure $ DerivedFunctionNoun (Just $ F.each1 $ F.constant1 x) (Just $ F.each2 $ F.constant2 x) Nothing each x
  , adverbOnFunction = Just $ \f -> pure $ DerivedFunctionFunction (Just $ F.each1 $ callMonad f) (Just $ F.each2 $ callDyad f) Nothing each f }
eachLeft = PrimitiveAdverb
  { adverbRepr = [G.eachLeft]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \f -> pure $ DerivedFunctionFunction Nothing (Just $ F.eachLeft $ callDyad f) Nothing eachLeft f }
eachRight = PrimitiveAdverb
  { adverbRepr = [G.eachRight]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \f -> pure $ DerivedFunctionFunction Nothing (Just $ F.eachRight $ callDyad f) Nothing eachRight f }
key = PrimitiveAdverb
  { adverbRepr = [G.key]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \f -> pure $ DerivedFunctionFunction (Just $ F.keyMonad $ callDyad f) (Just $ F.key' $ callDyad f) Nothing key f }
onCells = PrimitiveAdverb
  { adverbRepr = [G.onCells]
  , adverbContext = Nothing
  , adverbOnNoun = Just $ \x -> pure $ DerivedFunctionNoun (Just $ F.onCells1 $ F.constant1 x) (Just $ F.onCells2 $ F.constant2 x) Nothing onCells x
  , adverbOnFunction = Just $ \f -> pure $ DerivedFunctionFunction (Just $ F.onCells1 $ callMonad f) (Just $ F.onCells2 $ callDyad f) Nothing onCells f }
onScalars = PrimitiveAdverb
  { adverbRepr = [G.onScalars]
  , adverbContext = Nothing
  , adverbOnNoun = Just $ \x -> pure $ DerivedFunctionNoun (Just $ F.onScalars1 $ F.constant1 x) (Just $ F.onScalars2 $ F.constant2 x) Nothing onScalars x
  , adverbOnFunction = Just $ \f -> pure $ DerivedFunctionFunction (Just $ F.onScalars1 $ callMonad f) (Just $ F.onScalars2 $ callDyad f) Nothing onScalars f }
boxed = PrimitiveAdverb
  { adverbRepr = [G.boxed]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \f -> pure $ DerivedFunctionFunction (Just $ F.boxed1 $ callMonad f) (Just $ F.boxed2 $ callDyad f) Nothing boxed f }
onContents = PrimitiveAdverb
  { adverbRepr = [G.onContents]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \f -> pure $ DerivedFunctionFunction (Just $ F.onContents1 $ callMonad f) (Just $ F.onContents2 $ callDyad f) Nothing onContents f }
table = PrimitiveAdverb
  { adverbRepr = [G.table]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \f -> pure $ DerivedFunctionFunction Nothing (Just $ F.table (callDyad f)) Nothing table f }
ident = PrimitiveAdverb
  { adverbRepr = [G.ident]
  , adverbContext = Nothing
  , adverbOnNoun = Just $ \x -> pure $ DerivedFunctionNoun (Just $ F.constant1 x) (Just $ F.constant2 x) Nothing ident x
  , adverbOnFunction = Just $ \f -> pure $ DerivedFunctionFunction (Just $ callMonad f) (Just $ callDyad f) Nothing ident f }
onSimpleScalars = PrimitiveAdverb
  { adverbRepr = [G.onSimpleScalars]
  , adverbContext = Nothing
  , adverbOnNoun = Just $ \x -> pure $ DerivedFunctionNoun (Just $ F.onSimpleScalars1 (F.constant1 x)) (Just $ F.onSimpleScalars2 (F.constant2 x)) Nothing onSimpleScalars x
  , adverbOnFunction = Just $ \f -> pure $ DerivedFunctionFunction (Just $ F.onSimpleScalars1 (callMonad f)) (Just $ F.onSimpleScalars2 (callDyad f)) Nothing onSimpleScalars f }

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
  , conjOnFunctionNoun = Just $ \f r -> pure $ DerivedFunctionFunctionNoun (Just $ F.atRank1' (callMonad f) r) (Just $ F.atRank2' (callDyad f) r) Nothing atop f r
  , conjOnFunctionFunction = Just $ \f g -> pure $ DerivedFunctionFunctionFunction (Just $ F.compose (callMonad f) (callMonad g)) (Just $ F.atop (callMonad f) (callDyad g)) Nothing atop f g }
over = PrimitiveConjunction
  { conjRepr = [G.over]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Just $ \f d -> pure $ DerivedFunctionFunctionNoun (Just $ F.atDepth1' (callMonad f) d) (Just $ F.atDepth2' (callDyad f) d) Nothing over f d
  , conjOnFunctionFunction = Just $ \f g -> pure $ DerivedFunctionFunctionFunction (Just $ F.compose (callMonad f) (callMonad g)) (Just $ F.over (callDyad f) (callMonad g)) Nothing over f g }
after = PrimitiveConjunction
  { conjRepr = [G.after]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Just $ \x g -> pure $ DerivedFunctionNounFunction (Just $ \y -> callDyad g x y) Nothing Nothing after x g
  , conjOnFunctionNoun = Just $ \f y -> pure $ DerivedFunctionFunctionNoun (Just $ \x -> callDyad f x y) Nothing Nothing after f y
  , conjOnFunctionFunction = Just $ \f g -> pure $ DerivedFunctionFunctionFunction (Just $ F.compose (callMonad f) (callMonad g)) (Just $ F.after (callDyad f) (callMonad g)) Nothing after f g }
before = PrimitiveConjunction
  { conjRepr = [G.before]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Just $ \x g -> pure $ DerivedFunctionNounFunction (Just $ \y -> callDyad g x y) (Just $ \x' y -> callDyad g x' y) Nothing before x g
  , conjOnFunctionNoun = Just $ \f y -> pure $ DerivedFunctionFunctionNoun (Just $ \x -> callDyad f x y) (Just $ \x y' -> callDyad f x y') Nothing before f y
  , conjOnFunctionFunction = Just $ \f g -> pure $ DerivedFunctionFunctionFunction (Just $ F.reverseCompose (callMonad f) (callMonad g)) (Just $ F.before (callMonad f) (callDyad g)) Nothing before f g }
leftHook = PrimitiveConjunction
  { conjRepr = [G.leftHook]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Nothing
  , conjOnFunctionFunction = Just $ \f g -> pure $ DerivedFunctionFunctionFunction (Just $ F.leftHook (callMonad f) (callDyad g)) (Just $ F.before (callMonad f) (callDyad g)) Nothing leftHook f g }
rightHook = PrimitiveConjunction
  { conjRepr = [G.rightHook]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Nothing
  , conjOnFunctionFunction = Just $ \f g -> pure $ DerivedFunctionFunctionFunction (Just $ F.rightHook (callDyad f) (callMonad g)) (Just $ F.after (callDyad f) (callMonad g)) Nothing rightHook f g }
mirror = PrimitiveConjunction
  { conjRepr = [G.mirror]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Nothing
  , conjOnFunctionFunction = Just $ \f g -> pure $ DerivedFunctionFunctionFunction Nothing (Just $ F.mirror (callDyad f) (callDyad g)) Nothing mirror f g }
leftFork = PrimitiveConjunction
  { conjRepr = [G.leftFork]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Nothing
  , conjOnFunctionFunction = Just $ \f g -> pure $ DerivedFunctionFunctionFunction (Just $ F.leftHook (callMonad f) (callDyad g)) (Just $ F.leftFork (callDyad f) (callDyad g)) Nothing leftFork f g }
rightFork = PrimitiveConjunction
  { conjRepr = [G.rightFork]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Nothing
  , conjOnFunctionFunction = Just $ \f g -> pure $ DerivedFunctionFunctionFunction (Just $ F.rightHook (callDyad f) (callMonad g)) (Just $ F.rightFork (callDyad f) (callDyad g)) Nothing rightFork f g }
repeat = PrimitiveConjunction
  { conjRepr = [G.repeat]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Just $ \f y -> pure $ DerivedFunctionFunctionNoun (Just $ F.repeat1 (callMonad f) y) (Just $ F.repeat2 (callDyad f) y) Nothing TinyAPL.Primitives.repeat f y
  , conjOnFunctionFunction = Just $ \f g -> pure $ DerivedFunctionFunctionFunction (Just $ F.until1 (callMonad f) (callDyad g)) (Just $ F.until2 (callDyad f) (callDyad g)) Nothing TinyAPL.Primitives.repeat f g }
valences = PrimitiveConjunction
  { conjRepr = [G.valences]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Nothing
  , conjOnFunctionFunction = Just $ \f g -> pure $ DerivedFunctionFunctionFunction (Just $ callMonad f) (Just $ callDyad g) Nothing valences f g }
under = PrimitiveConjunction
  { conjRepr = [G.under]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Just $ \x g -> pure $ DerivedFunctionNounFunction (Just $ F.underK x (callMonad g)) Nothing Nothing under x g
  , conjOnFunctionNoun = Nothing
  , conjOnFunctionFunction = Just $ \f g -> pure $ DerivedFunctionFunctionFunction (Just $ F.under (callMonad f) (callMonad g)) (Just $ F.under2 (callDyad f) (callMonad g)) Nothing under f g }
innerProduct = PrimitiveConjunction
  { conjRepr = [G.innerProduct]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Nothing
  , conjOnFunctionFunction = Just $ \f g -> pure $ DerivedFunctionFunctionFunction Nothing (Just $ F.innerProduct (callMonad f) (callDyad g)) Nothing innerProduct f g }
lev = PrimitiveConjunction
  { conjRepr = [G.lev]
  , conjContext = Nothing
  , conjOnNounNoun = Just $ \x y -> pure $ DerivedFunctionNounNoun (Just $ F.constant1 x) (Just $ F.constant2 x) Nothing lev x y
  , conjOnNounFunction = Just $ \x g -> pure $ DerivedFunctionNounFunction (Just $ F.constant1 x) (Just $ F.constant2 x) Nothing lev x g
  , conjOnFunctionNoun = Just $ \f y -> pure $ DerivedFunctionFunctionNoun (Just $ callMonad f) (Just $ callDyad f) Nothing lev f y
  , conjOnFunctionFunction = Just $ \f g -> pure $ DerivedFunctionFunctionFunction (Just $ callMonad f) (Just $ callDyad f) Nothing lev f g }
dex = PrimitiveConjunction
  { conjRepr = [G.dex]
  , conjContext = Nothing
  , conjOnNounNoun = Just $ \x y -> pure $ DerivedFunctionNounNoun (Just $ F.constant1 y) (Just $ F.constant2 y) Nothing dex x y
  , conjOnNounFunction = Just $ \x g -> pure $ DerivedFunctionNounFunction (Just $ callMonad g) (Just $ callDyad g) Nothing dex x g
  , conjOnFunctionNoun = Just $ \f y -> pure $ DerivedFunctionFunctionNoun (Just $ F.constant1 y) (Just $ F.constant2 y) Nothing dex f y
  , conjOnFunctionFunction = Just $ \f g -> pure $ DerivedFunctionFunctionFunction (Just $ callMonad g) (Just $ callDyad g) Nothing dex f g }
forkA = PrimitiveConjunction
  { conjRepr = [G.forkA]
  , conjContext = Nothing
  , conjOnNounNoun = Just $ \x y -> pure $ DerivedFunctionNounNoun (Just $ \_ -> message) (Just $ \_ _ -> message) Nothing forkA x y
  , conjOnNounFunction = Just $ \x g -> pure $ DerivedFunctionNounFunction (Just $ \_ -> message) (Just $ \_ _ -> message) Nothing forkA x g
  , conjOnFunctionNoun = Just $ \f y -> pure $ DerivedFunctionFunctionNoun (Just $ \_ -> message) (Just $ \_ _ -> message) Nothing forkA f y
  , conjOnFunctionFunction = Just $ \f g -> pure $ DerivedFunctionFunctionFunction (Just $ \_ -> message) (Just $ \_ _ -> message) Nothing forkA f g }
  where message = throwError $ DomainError $ [G.forkA] ++ " must be used in conjunction with " ++ [G.forkB]
forkB = PrimitiveConjunction
  { conjRepr = [G.forkB]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Just $ \left z -> case left of
    DerivedFunctionNounNoun _ _ _ op x y | op == forkA -> pure $ DerivedFunctionFunctionNoun (Just $ F.fork1 (F.constant1 x) (F.constant2 y) (F.constant1 z)) (Just $ F.fork2 (F.constant2 x) (F.constant2 y) (F.constant2 z)) Nothing forkB left z
    DerivedFunctionNounFunction _ _ _ op x g | op == forkA -> pure $ DerivedFunctionFunctionNoun (Just $ F.fork1 (F.constant1 x) (callDyad g) (F.constant1 z)) (Just $ F.fork2 (F.constant2 x) (callDyad g) (F.constant2 z)) Nothing forkB left z
    DerivedFunctionFunctionNoun _ _ _ op f y | op == forkA -> pure $ DerivedFunctionFunctionNoun (Just $ F.fork1 (callMonad f) (F.constant2 y) (F.constant1 z)) (Just $ F.fork2 (callDyad f) (F.constant2 y) (F.constant2 z)) Nothing forkB left z
    DerivedFunctionFunctionFunction _ _ _ op f g | op == forkA -> pure $ DerivedFunctionFunctionNoun (Just $ F.fork1 (callMonad f) (callDyad g) (F.constant1 z)) (Just $ F.fork2 (callDyad f) (callDyad g) (F.constant2 z)) Nothing forkB left z
    _ -> message
  , conjOnFunctionFunction = Just $ \left h -> case left of
    DerivedFunctionNounNoun _ _ _ op x y | op == forkA -> pure $ DerivedFunctionFunctionFunction (Just $ F.fork1 (F.constant1 x) (F.constant2 y) (callMonad h)) (Just $ F.fork2 (F.constant2 x) (F.constant2 y) (callDyad h)) Nothing forkB left h
    DerivedFunctionNounFunction _ _ _ op x g | op == forkA -> pure $ DerivedFunctionFunctionFunction (Just $ F.fork1 (F.constant1 x) (callDyad g) (callMonad h)) (Just $ F.fork2 (F.constant2 x) (callDyad g) (callDyad h)) Nothing forkB left h
    DerivedFunctionFunctionNoun _ _ _ op f y | op == forkA -> pure $ DerivedFunctionFunctionFunction (Just $ F.fork1 (callMonad f) (F.constant2 y) (callMonad h)) (Just $ F.fork2 (callDyad f) (F.constant2 y) (callDyad h)) Nothing forkB left h
    DerivedFunctionFunctionFunction _ _ _ op f g | op == forkA -> pure $ DerivedFunctionFunctionFunction (Just $ F.fork1 (callMonad f) (callDyad g) (callMonad h)) (Just $ F.fork2 (callDyad f) (callDyad g) (callDyad h)) Nothing forkB left h
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