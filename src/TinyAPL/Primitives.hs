module TinyAPL.Primitives where

import TinyAPL.ArrayFunctionOperator
import qualified TinyAPL.Functions as F
import qualified TinyAPL.Glyphs as G
import TinyAPL.Error
import TinyAPL.Util (headPromise, rollR)
import TinyAPL.Complex (Complex((:+)))

import Data.Tuple (swap)

withCoreExtraArgs1 :: (CoreExtraArgs -> a -> St r) -> ExtraArgs -> a -> St r
withCoreExtraArgs1 f ea a = parseCoreExtraArgs ea >>= (\cea -> f cea a)

withCoreExtraArgs2 :: (CoreExtraArgs -> a -> b -> St r) -> ExtraArgs -> a -> b -> St r
withCoreExtraArgs2 f ea a b = parseCoreExtraArgs ea >>= (\cea -> f cea a b)

-- * Primitive arrays


zilde = vector []
cilde = dictionary []

arrays =
  [ (G.zilde, zilde)
  , (G.cilde, cilde) ]

-- * Primitive functions

plus = PrimitiveFunction (Just $ const F.conjugate') (Just $ const F.add') (Just $ const F.conjugate') (Just $ const $ F.commute F.sub') (Just $ const F.sub') (Just $ withCoreExtraArgs1 F.floorAndFrac') (Just $ const F.halve') (Just $ const F.addAna') [G.plus] Nothing
minus = PrimitiveFunction (Just $ const F.neg') (Just $ const F.sub') Nothing Nothing Nothing Nothing Nothing Nothing [G.minus] Nothing
times = PrimitiveFunction (Just $ const F.sign') (Just $ const F.times') Nothing (Just $ const $ F.commute F.divide') (Just $ const F.divide') (Just $ const F.signAndAbs') (Just $ const F.squareRoot') (Just $ const F.timesAna') [G.times] Nothing
divide = PrimitiveFunction (Just $ const F.reciprocal') (Just $ const F.divide') Nothing Nothing Nothing Nothing Nothing Nothing [G.divide] Nothing
power = PrimitiveFunction (Just $ const F.ePow') (Just $ const F.pow') Nothing Nothing Nothing Nothing Nothing Nothing [G.power] Nothing
logarithm = PrimitiveFunction (Just $ const F.ln') (Just $ const F.log') Nothing Nothing Nothing Nothing Nothing Nothing [G.logarithm] Nothing
root = PrimitiveFunction (Just $ const F.squareRoot') (Just $ const F.root') Nothing Nothing Nothing Nothing Nothing Nothing [G.root] Nothing
floor = PrimitiveFunction (Just $ withCoreExtraArgs1 F.floor') (Just $ const F.min') Nothing Nothing Nothing Nothing Nothing Nothing [G.floor] Nothing
ceil = PrimitiveFunction (Just $ withCoreExtraArgs1 F.ceil') (Just $ const F.max') Nothing Nothing Nothing Nothing Nothing Nothing [G.ceil] Nothing
round = PrimitiveFunction (Just $ const F.round') (Just $ const F.roundTo') Nothing Nothing Nothing Nothing Nothing Nothing [G.round] Nothing
less = PrimitiveFunction Nothing (Just $ withCoreExtraArgs2 F.less') Nothing Nothing Nothing Nothing Nothing Nothing [G.less] Nothing
lessEqual = PrimitiveFunction Nothing (Just $ withCoreExtraArgs2 F.lessEqual') Nothing Nothing Nothing Nothing Nothing Nothing [G.lessEqual] Nothing
equal = PrimitiveFunction Nothing (Just $ withCoreExtraArgs2 F.equal') Nothing Nothing Nothing Nothing Nothing Nothing [G.equal] Nothing
greaterEqual = PrimitiveFunction (Just $ withCoreExtraArgs1 F.lastCell) (Just $ withCoreExtraArgs2 F.greaterEqual') Nothing Nothing Nothing Nothing Nothing Nothing [G.greaterEqual] Nothing
greater = PrimitiveFunction (Just $ withCoreExtraArgs1 F.firstCell) (Just $ withCoreExtraArgs2 F.greater') Nothing Nothing Nothing Nothing Nothing Nothing [G.greater] Nothing
notEqual = PrimitiveFunction (Just $ withCoreExtraArgs1 F.nubSieve') (Just $ withCoreExtraArgs2 F.notEqual') Nothing Nothing Nothing Nothing Nothing Nothing [G.notEqual] Nothing
and = PrimitiveFunction (Just $ const F.promote) (Just $ withCoreExtraArgs2 F.lcm') Nothing Nothing Nothing Nothing Nothing Nothing [G.and] Nothing
or = PrimitiveFunction (Just $ const F.demote) (Just $ withCoreExtraArgs2 F.gcd') Nothing Nothing Nothing Nothing Nothing Nothing [G.or] Nothing
nand = PrimitiveFunction Nothing (Just $ const F.nand') Nothing Nothing Nothing Nothing Nothing Nothing [G.nand] Nothing
nor = PrimitiveFunction Nothing (Just $ const F.nor') Nothing Nothing Nothing Nothing Nothing Nothing [G.nor] Nothing
cartesian = PrimitiveFunction (Just $ const F.imaginary') (Just $ const F.cartesian') Nothing Nothing Nothing (Just $ const F.realAndImag') Nothing Nothing [G.cartesian] Nothing
polar = PrimitiveFunction (Just $ const F.unitPolar') (Just $ const F.polar') Nothing Nothing Nothing (Just $ const F.absAndPhase') Nothing Nothing [G.polar] Nothing
identical = PrimitiveFunction (Just $ const F.depth') (Just $ withCoreExtraArgs2 F.identical') Nothing Nothing Nothing Nothing Nothing Nothing [G.identical] Nothing
notIdentical = PrimitiveFunction (Just $ const F.tally') (Just $ withCoreExtraArgs2 F.notIdentical') Nothing Nothing Nothing Nothing Nothing Nothing [G.notIdentical] Nothing
rho = PrimitiveFunction (Just $ const F.shape') (Just $ const F.reshape') Nothing Nothing Nothing Nothing Nothing Nothing [G.rho] Nothing
ravel = PrimitiveFunction (Just $ const F.ravel') (Just $ withCoreExtraArgs2 F.laminate) Nothing Nothing Nothing Nothing Nothing Nothing [G.ravel] Nothing
reverse = PrimitiveFunction (Just $ const F.reverse') (Just $ withCoreExtraArgs2 F.rotate') Nothing Nothing Nothing Nothing Nothing Nothing [G.reverse] Nothing
pair = PrimitiveFunction (Just $ const F.singleton) (Just $ const F.pair) Nothing Nothing Nothing Nothing Nothing Nothing [G.pair] Nothing
enclose = PrimitiveFunction (Just $ const F.enclose') (Just $ const F.partitionEnclose') Nothing Nothing Nothing Nothing Nothing Nothing [G.enclose] Nothing
first = PrimitiveFunction (Just $ withCoreExtraArgs1 F.first) Nothing Nothing Nothing Nothing Nothing Nothing Nothing [G.first] Nothing
last = PrimitiveFunction (Just $ withCoreExtraArgs1 F.last) (Just $ withCoreExtraArgs2 F.from) Nothing Nothing Nothing Nothing Nothing Nothing [G.last] Nothing
take = PrimitiveFunction (Just $ withCoreExtraArgs1 F.mix) (Just $ withCoreExtraArgs2 F.take') Nothing Nothing Nothing Nothing Nothing Nothing [G.take] Nothing
drop = PrimitiveFunction (Just $ const F.majorCells') (Just $ withCoreExtraArgs2 F.drop') Nothing Nothing Nothing Nothing Nothing Nothing [G.drop] Nothing
left = PrimitiveFunction (Just $ \_ x -> pure x) (Just $ \_ x _ -> pure x) Nothing Nothing Nothing Nothing Nothing Nothing [G.left] Nothing
right = PrimitiveFunction (Just $ \_ x -> pure x) (Just $ \_ _ y -> pure y) Nothing Nothing Nothing Nothing Nothing Nothing [G.right] Nothing
iota = PrimitiveFunction (Just $ withCoreExtraArgs1 F.indexGenerator') (Just $ withCoreExtraArgs2 F.indexOf) Nothing Nothing Nothing Nothing Nothing Nothing [G.iota] Nothing
indices = PrimitiveFunction (Just $ withCoreExtraArgs1 F.indices) (Just $ withCoreExtraArgs2 F.intervalIndex) Nothing Nothing Nothing Nothing Nothing Nothing [G.indices] Nothing
replicate = PrimitiveFunction Nothing (Just $ const F.replicate') Nothing Nothing Nothing Nothing Nothing Nothing [G.replicate] Nothing
abs = PrimitiveFunction (Just $ const F.abs') (Just $ withCoreExtraArgs2 F.remainder') Nothing Nothing Nothing Nothing Nothing Nothing [G.abs] Nothing
phase = PrimitiveFunction (Just $ const F.phase') (Just $ const F.arctan') Nothing Nothing Nothing Nothing Nothing Nothing [G.phase] Nothing
real = PrimitiveFunction (Just $ const F.real') Nothing Nothing Nothing Nothing Nothing Nothing Nothing [G.real] Nothing
imag = PrimitiveFunction (Just $ const F.imag') Nothing Nothing Nothing Nothing Nothing Nothing Nothing [G.imag] Nothing
union = PrimitiveFunction (Just $ withCoreExtraArgs1 F.unique') (Just $ withCoreExtraArgs2 F.union') Nothing Nothing Nothing Nothing Nothing Nothing [G.union] Nothing
intersection = PrimitiveFunction Nothing (Just $ withCoreExtraArgs2 F.intersection') Nothing Nothing Nothing Nothing Nothing Nothing [G.intersection] Nothing
difference = PrimitiveFunction (Just $ const F.not') (Just $ withCoreExtraArgs2 F.difference') Nothing Nothing Nothing Nothing Nothing Nothing [G.difference] Nothing
symdiff = PrimitiveFunction Nothing (Just $ withCoreExtraArgs2 F.symmetricDifference') Nothing Nothing Nothing Nothing Nothing Nothing [G.symdiff] Nothing
element = PrimitiveFunction (Just $ const F.enlist') (Just $ withCoreExtraArgs2 F.elementOf) Nothing Nothing Nothing Nothing Nothing Nothing [G.element] Nothing
roll = PrimitiveFunction (Just $ withCoreExtraArgs1 F.roll') (Just $ withCoreExtraArgs2 F.deal') Nothing Nothing Nothing Nothing Nothing Nothing [G.roll] Nothing
squad = PrimitiveFunction Nothing (Just $ withCoreExtraArgs2 F.squad) Nothing Nothing Nothing Nothing Nothing Nothing [G.squad] Nothing
rank = PrimitiveFunction (Just $ const F.rank') (Just $ const F.rerank') Nothing Nothing Nothing Nothing Nothing Nothing [G.rank] Nothing
catenate = PrimitiveFunction (Just $ withCoreExtraArgs1 F.join') (Just $ withCoreExtraArgs2 F.catenate) Nothing Nothing Nothing Nothing Nothing Nothing [G.catenate] Nothing
gradeUp = PrimitiveFunction (Just $ withCoreExtraArgs1 F.gradeUp') (Just $ withCoreExtraArgs2 F.sortByUp') Nothing Nothing Nothing Nothing Nothing Nothing [G.gradeUp] Nothing
gradeDown = PrimitiveFunction (Just $ withCoreExtraArgs1 F.gradeDown') (Just $ withCoreExtraArgs2 F.sortByDown') Nothing Nothing Nothing Nothing Nothing Nothing [G.gradeDown] Nothing
precedes = PrimitiveFunction Nothing (Just $ withCoreExtraArgs2 F.precedes') Nothing Nothing Nothing Nothing Nothing Nothing [G.precedes] Nothing
precedesOrIdentical = PrimitiveFunction (Just $ withCoreExtraArgs1 F.sortUp') (Just $ withCoreExtraArgs2 F.precedesOrIdentical') Nothing Nothing Nothing Nothing Nothing Nothing [G.precedesOrIdentical] Nothing
succeedsOrIdentical = PrimitiveFunction (Just $ withCoreExtraArgs1 F.sortDown') (Just $ withCoreExtraArgs2 F.succeedsOrIdentical') Nothing Nothing Nothing Nothing Nothing Nothing [G.succeedsOrIdentical] Nothing
succeeds = PrimitiveFunction Nothing (Just $ withCoreExtraArgs2 F.succeeds') Nothing Nothing Nothing Nothing Nothing Nothing [G.succeeds] Nothing
minimal = PrimitiveFunction Nothing (Just $ const F.minimal) Nothing Nothing Nothing Nothing Nothing Nothing [G.minimal] Nothing
maximal = PrimitiveFunction Nothing (Just $ const F.maximal) Nothing Nothing Nothing Nothing Nothing Nothing [G.maximal] Nothing
transpose = PrimitiveFunction (Just $ withCoreExtraArgs1 F.transpose) (Just $ withCoreExtraArgs2 F.dyadicTranspose') Nothing Nothing Nothing Nothing Nothing Nothing [G.transpose] Nothing
matrixInverse = PrimitiveFunction (Just $ const F.matrixInverse') (Just $ const F.matrixDivide') Nothing Nothing Nothing Nothing Nothing Nothing [G.matrixInverse] Nothing
factorial = PrimitiveFunction (Just $ const F.factorial') (Just $ const F.binomial') Nothing Nothing Nothing Nothing Nothing Nothing [G.factorial] Nothing
raise = PrimitiveFunction (Just $ const F.raise1) (Just $ const F.raise') Nothing Nothing Nothing Nothing Nothing Nothing [G.raise] Nothing
decode = PrimitiveFunction (Just $ withCoreExtraArgs1 F.decodeBase2) (Just $ withCoreExtraArgs2 F.decode') Nothing Nothing Nothing Nothing Nothing Nothing [G.decode] Nothing
encode = PrimitiveFunction (Just $ withCoreExtraArgs1 F.encodeBase2) (Just $ withCoreExtraArgs2 F.encode') Nothing Nothing Nothing Nothing Nothing Nothing [G.encode] Nothing
histogram = PrimitiveFunction (Just $ withCoreExtraArgs1 F.histogram) (Just $ withCoreExtraArgs2 F.count) Nothing Nothing Nothing Nothing Nothing Nothing [G.histogram] Nothing
increment = PrimitiveFunction (Just $ const F.increment') Nothing Nothing Nothing Nothing Nothing Nothing Nothing [G.increment] Nothing
decrement = PrimitiveFunction (Just $ const F.decrement') (Just $ const F.span') Nothing Nothing Nothing Nothing Nothing Nothing [G.decrement] Nothing
range = PrimitiveFunction (Just $ withCoreExtraArgs1 F.oneRange) (Just $ withCoreExtraArgs2 F.range) Nothing Nothing Nothing Nothing Nothing Nothing [G.range] Nothing
keyValue = PrimitiveFunction (Just $ const F.fromPairs) (Just $ const F.keyValuePair) Nothing Nothing Nothing Nothing Nothing Nothing [G.keyValue] Nothing
invertedTable = PrimitiveFunction (Just $ const F.fromInvertedTable) (Just $ const F.fromKeysAndValues') Nothing Nothing Nothing Nothing Nothing Nothing [G.invertedTable] Nothing
group = PrimitiveFunction Nothing (Just $ withCoreExtraArgs2 F.group') Nothing Nothing Nothing Nothing Nothing Nothing [G.group] Nothing
partition = PrimitiveFunction Nothing (Just $ const F.partition') Nothing Nothing Nothing Nothing Nothing Nothing [G.partition] Nothing
execute = PrimitiveFunction (Just $ const F.execute') Nothing Nothing Nothing Nothing Nothing Nothing Nothing [G.execute] Nothing
format = PrimitiveFunction (Just $ const F.format') Nothing Nothing Nothing Nothing Nothing Nothing Nothing [G.format] Nothing
find = PrimitiveFunction Nothing (Just $ withCoreExtraArgs2 F.find') Nothing Nothing Nothing Nothing Nothing Nothing [G.find] Nothing
mask = PrimitiveFunction Nothing (Just $ withCoreExtraArgs2 F.mask') Nothing Nothing Nothing Nothing Nothing Nothing [G.mask] Nothing
raises = PrimitiveFunction (Just $ const F.square') (Just $ const F.raises') Nothing Nothing Nothing Nothing Nothing Nothing [G.raises] Nothing

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
  , TinyAPL.Primitives.format
  , TinyAPL.Primitives.find
  , TinyAPL.Primitives.mask
  , TinyAPL.Primitives.raises ]

-- * Primitive adverbs

selfie = PrimitiveAdverb
  { adverbRepr = [G.selfie]
  , adverbContext = Nothing
  , adverbOnNoun = Just $ \_ x -> pure $ DerivedFunctionNoun (Just $ const $ F.constant1 x) (Just $ const $ F.constant2 x) Nothing Nothing Nothing Nothing Nothing Nothing Nothing selfie x
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction (Just $ const $ F.duplicate $ callDyad f []) (Just $ const $ F.commute $ callDyad f []) (Just $ const $ callBi f []) (Just $ const $ F.commute $ callContra f []) (Just $ const $ F.commute $ callAnti f []) (Just $ const $ fmap swap . callDis f []) Nothing Nothing Nothing selfie f }
reduce = PrimitiveAdverb
  { adverbRepr = [G.reduce]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \ea f -> pure $ DerivedFunctionFunction (Just $ \ea' -> withCoreExtraArgs1 (flip F.reduce' $ callDyad f []) (ea' ++ ea)) (Just $ \ea' -> withCoreExtraArgs2 (flip F.fold' $ callDyad f []) (ea' ++ ea)) (Just $ const $ \y -> case f of
    DerivedFunctionFunction{ derivedFunctionFunctionLeft = f', derivedFunctionAdverb = adv } | adv == onContents -> callDis f' [] y >>= uncurry F.pair
    _ -> callDis f [] y >>= uncurry (F.laminate defaultCoreExtraArgs)) (Just $ const $ \x y -> case f of
    DerivedFunctionFunction{ derivedFunctionFunctionLeft = f', derivedFunctionAdverb = adv } | adv == onContents -> vector . fmap box <$> callAna f' [] x y
    _ -> fromMajorCells <$> callAna f [] x y) Nothing Nothing Nothing Nothing Nothing reduce f }
onPrefixes = PrimitiveAdverb
  { adverbRepr = [G.onPrefixes]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction (Just $ const $ F.onPrefixes' $ callMonad f []) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing onPrefixes f }
onSuffixes = PrimitiveAdverb
  { adverbRepr = [G.onSuffixes]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction (Just $ const $ F.onSuffixes' $ callMonad f []) (Just $ const $ F.onInfixes' $ callMonad f []) Nothing Nothing Nothing Nothing Nothing Nothing Nothing onSuffixes f }
each = PrimitiveAdverb
  { adverbRepr = [G.each]
  , adverbContext = Nothing
  , adverbOnNoun = Just $ \_ x -> pure $ DerivedFunctionNoun (Just $ const $ F.each1 $ F.constant1 x) (Just $ const $ F.each2 $ F.constant2 x) Nothing Nothing Nothing Nothing Nothing Nothing Nothing each x
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction (Just $ const $ F.each1 $ callMonad f []) (Just $ const $ F.each2 $ callDyad f []) Nothing Nothing Nothing Nothing Nothing Nothing Nothing each f }
eachLeft = PrimitiveAdverb
  { adverbRepr = [G.eachLeft]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction Nothing (Just $ const $ F.eachLeft $ callDyad f []) Nothing Nothing Nothing Nothing Nothing Nothing Nothing eachLeft f }
eachRight = PrimitiveAdverb
  { adverbRepr = [G.eachRight]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction Nothing (Just $ const $ F.eachRight $ callDyad f []) Nothing Nothing Nothing Nothing Nothing Nothing Nothing eachRight f }
key = PrimitiveAdverb
  { adverbRepr = [G.key]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \ea f -> pure $ DerivedFunctionFunction (Just $ \ea' -> withCoreExtraArgs1 (flip F.keyMonad $ callDyad f []) (ea' ++ ea)) (Just $ const $ F.key' $ callDyad f []) Nothing Nothing Nothing Nothing Nothing Nothing Nothing key f }
onCells = PrimitiveAdverb
  { adverbRepr = [G.onCells]
  , adverbContext = Nothing
  , adverbOnNoun = Just $ \ea x -> pure $ DerivedFunctionNoun (Just $ \ea' -> withCoreExtraArgs1 (flip F.onCells1 $ F.constant1 x) (ea' ++ ea)) (Just $ \ea' -> withCoreExtraArgs2 (flip F.onCells2 $ F.constant2 x) (ea' ++ ea)) Nothing Nothing Nothing Nothing Nothing Nothing Nothing onCells x
  , adverbOnFunction = Just $ \ea f -> pure $ DerivedFunctionFunction (Just $ \ea' -> withCoreExtraArgs1 (flip F.onCells1 $ callMonad f []) (ea' ++ ea)) (Just $ \ea' -> withCoreExtraArgs2 (flip F.onCells2 $ callDyad f []) (ea' ++ ea)) Nothing Nothing Nothing Nothing Nothing Nothing Nothing onCells f }
onScalars = PrimitiveAdverb
  { adverbRepr = [G.onScalars]
  , adverbContext = Nothing
  , adverbOnNoun = Just $ \ea x -> pure $ DerivedFunctionNoun (Just $ \ea' -> withCoreExtraArgs1 (flip F.onScalars1 $ F.constant1 x) (ea' ++ ea)) (Just $ \ea' -> withCoreExtraArgs2 (flip F.onScalars2 $ F.constant2 x) (ea' ++ ea)) Nothing Nothing Nothing Nothing Nothing Nothing Nothing onScalars x
  , adverbOnFunction = Just $ \ea f -> pure $ DerivedFunctionFunction (Just $ \ea' -> withCoreExtraArgs1 (flip F.onScalars1 $ callMonad f []) (ea' ++ ea)) (Just $ \ea' -> withCoreExtraArgs2 (flip F.onScalars2 $ callDyad f []) (ea' ++ ea)) Nothing Nothing Nothing Nothing Nothing Nothing Nothing onScalars f }
boxed = PrimitiveAdverb
  { adverbRepr = [G.boxed]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction (Just $ const $ F.boxed1 $ callMonad f []) (Just $ const $ F.boxed2 $ callDyad f []) Nothing Nothing Nothing Nothing Nothing Nothing Nothing boxed f }
onContents = PrimitiveAdverb
  { adverbRepr = [G.onContents]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction (Just $ const $ F.onContents1 $ callMonad f []) (Just $ const $ F.onContents2 $ callDyad f []) Nothing Nothing Nothing Nothing Nothing Nothing Nothing onContents f }
table = PrimitiveAdverb
  { adverbRepr = [G.table]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction Nothing (Just $ const $ F.table $ callDyad f []) Nothing Nothing Nothing Nothing Nothing Nothing Nothing table f }
ident = PrimitiveAdverb
  { adverbRepr = [G.ident]
  , adverbContext = Nothing
  , adverbOnNoun = Just $ \_ x -> pure $ DerivedFunctionNoun (Just $ const $ F.constant1 x) (Just $ const $ F.constant2 x) Nothing Nothing Nothing Nothing Nothing Nothing Nothing ident x
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction (Just $ callMonad f) (Just $ callDyad f) Nothing Nothing Nothing Nothing Nothing Nothing Nothing ident f }
onSimpleScalars = PrimitiveAdverb
  { adverbRepr = [G.onSimpleScalars]
  , adverbContext = Nothing
  , adverbOnNoun = Just $ \_ x -> pure $ DerivedFunctionNoun (Just $ const $ F.onSimpleScalars1 $ F.constant1 x) (Just $ const $ F.onSimpleScalars2 $ F.constant2 x) Nothing Nothing Nothing Nothing Nothing Nothing Nothing onSimpleScalars x
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction (Just $ const $ F.onSimpleScalars1 $ callMonad f []) (Just $ const $ F.onSimpleScalars2 $ callDyad f []) Nothing Nothing Nothing Nothing Nothing Nothing Nothing onSimpleScalars f }
originOne = PrimitiveAdverb
  { adverbRepr = [G.originOne]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction (Just $ \ea' -> callMonad f $ (box $ vector $ Character <$> coreExtraArgsOriginKey, Number 1) : ea') (Just $ \ea' -> callDyad f $ (box $ vector $ Character <$> coreExtraArgsOriginKey, Number 1) : ea') Nothing Nothing Nothing Nothing Nothing Nothing Nothing originOne f }
backward = PrimitiveAdverb
  { adverbRepr = [G.backward]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction (Just $ \ea' -> callMonad f $ (box $ vector $ Character <$> coreExtraArgsBackwardKey, Number 1) : ea') (Just $ \ea' -> callDyad f $ (box $ vector $ Character <$> coreExtraArgsBackwardKey, Number 1) : ea') Nothing Nothing Nothing Nothing Nothing Nothing Nothing backward f }
bitwise = PrimitiveAdverb
  { adverbRepr = [G.bitwise]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \ea f -> pure $ DerivedFunctionFunction (Just $ \ea' -> withCoreExtraArgs1 (flip F.bitwise1' $ callMonad f []) (ea' ++ ea)) (Just $ \ea' -> withCoreExtraArgs2 (flip F.bitwise2' $ callDyad f []) (ea' ++ ea)) Nothing Nothing Nothing Nothing Nothing Nothing Nothing onCells f }
cellsLeft = PrimitiveAdverb
  { adverbRepr = [G.cellsLeft]
  , adverbContext = Nothing
  , adverbOnNoun = Just $ \ea x -> pure $ DerivedFunctionNoun Nothing (Just $ \ea' -> withCoreExtraArgs2 (flip F.cellsLeft $ F.constant2 x) (ea' ++ ea)) Nothing Nothing Nothing Nothing Nothing Nothing Nothing cellsLeft x
  , adverbOnFunction = Just $ \ea f -> pure $ DerivedFunctionFunction Nothing (Just $ \ea' -> withCoreExtraArgs2 (flip F.cellsLeft $ callDyad f []) (ea' ++ ea)) Nothing Nothing Nothing Nothing Nothing Nothing Nothing cellsLeft f }
cellsRight = PrimitiveAdverb
  { adverbRepr = [G.cellsRight]
  , adverbContext = Nothing
  , adverbOnNoun = Just $ \ea x -> pure $ DerivedFunctionNoun Nothing (Just $ \ea' -> withCoreExtraArgs2 (flip F.cellsRight $ F.constant2 x) (ea' ++ ea)) Nothing Nothing Nothing Nothing Nothing Nothing Nothing cellsRight x
  , adverbOnFunction = Just $ \ea f -> pure $ DerivedFunctionFunction Nothing (Just $ \ea' -> withCoreExtraArgs2 (flip F.cellsRight $ callDyad f []) (ea' ++ ea)) Nothing Nothing Nothing Nothing Nothing Nothing Nothing cellsRight f }
inverse = PrimitiveAdverb
  { adverbRepr = [G.inverse]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ const $ \f -> pure $ DerivedFunctionFunction (Just $ callUn f) (Just $ callAnti f) (Just $ callMonad f) (Just $ callDyad f) Nothing Nothing Nothing Nothing Nothing inverse f }

adverbs = (\x -> (headPromise $ adverbRepr x, x)) <$>
  [ TinyAPL.Primitives.selfie
  , TinyAPL.Primitives.reduce
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
  , TinyAPL.Primitives.onSimpleScalars
  , TinyAPL.Primitives.originOne
  , TinyAPL.Primitives.backward
  , TinyAPL.Primitives.bitwise
  , TinyAPL.Primitives.cellsLeft
  , TinyAPL.Primitives.cellsRight
  , TinyAPL.Primitives.inverse ]

-- * Primitive conjunctions

atop = PrimitiveConjunction
  { conjRepr = [G.atop]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Just $ \_ x g -> pure $ DerivedFunctionNounFunction (Just $ \_ y -> callDyad g [] x y) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing atop x g
  , conjOnFunctionNoun = Just $ \_ f y -> pure $ DerivedFunctionFunctionNoun (Just $ \_ x -> callDyad f [] x y) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing atop f y
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (Just $ const $ F.compose (callMonad f []) (callMonad g [])) (Just $ const $ F.atop (callMonad f []) (callDyad g [])) (Just $ const $ F.compose (callUn g []) (callUn f [])) (Just $ const $ F.after (callAnti g []) (callUn f [])) (Just $ const $ F.before (callUn f []) (callContra g [])) (Just $ const $ F.compose (callDis g []) (callUn f [])) Nothing Nothing Nothing atop f g }
over = PrimitiveConjunction
  { conjRepr = [G.over]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Nothing
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (Just $ const $ F.compose (callMonad f []) (callMonad g [])) (Just $ const $ F.over (callDyad f []) (callMonad g [])) Nothing Nothing Nothing Nothing Nothing Nothing Nothing over f g }
reverseAtop = PrimitiveConjunction
  { conjRepr = [G.reverseAtop]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Just $ \_ x g -> pure $ DerivedFunctionNounFunction (Just $ \_ y -> callDyad g [] x y) (Just $ \_ x' y -> callDyad g [] x' y) Nothing Nothing Nothing Nothing Nothing Nothing Nothing reverseAtop x g
  , conjOnFunctionNoun = Just $ \_ f y -> pure $ DerivedFunctionFunctionNoun (Just $ \_ x -> callDyad f [] x y) (Just $ \_ x y' -> callDyad f [] x y') Nothing Nothing Nothing Nothing Nothing Nothing Nothing reverseAtop f y
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (Just $ const $ F.reverseCompose (callMonad f []) (callMonad g [])) (Just $ const $ F.reverseAtop (callDyad f []) (callMonad g [])) Nothing Nothing Nothing Nothing Nothing Nothing Nothing reverseAtop f g }
reverseOver = PrimitiveConjunction
  { conjRepr = [G.reverseOver]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Nothing
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (Just $ const $ F.reverseCompose (callMonad f []) (callMonad g [])) (Just $ const $ F.reverseOver (callMonad f []) (callDyad g [])) Nothing Nothing Nothing Nothing Nothing Nothing Nothing reverseOver f g }
leftHook = PrimitiveConjunction
  { conjRepr = [G.leftHook]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Nothing
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (Just $ const $ F.leftHook (callMonad f []) (callDyad g [])) (Just $ const $ F.before (callMonad f []) (callDyad g [])) Nothing Nothing Nothing Nothing Nothing Nothing Nothing leftHook f g }
rightHook = PrimitiveConjunction
  { conjRepr = [G.rightHook]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Nothing
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (Just $ const $ F.rightHook (callDyad f []) (callMonad g [])) (Just $ const $ F.after (callDyad f []) (callMonad g [])) Nothing Nothing Nothing Nothing Nothing Nothing Nothing rightHook f g }
mirror = PrimitiveConjunction
  { conjRepr = [G.mirror]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Nothing
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction Nothing (Just $ const $ F.mirror (callDyad f []) (callDyad g [])) Nothing Nothing Nothing Nothing Nothing Nothing Nothing mirror f g }
leftFork = PrimitiveConjunction
  { conjRepr = [G.leftFork]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Nothing
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (Just $ const $ F.leftHook (callMonad f []) (callDyad g [])) (Just $ const $ F.leftFork (callDyad f []) (callDyad g [])) Nothing Nothing Nothing Nothing Nothing Nothing Nothing leftFork f g }
rightFork = PrimitiveConjunction
  { conjRepr = [G.rightFork]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Nothing
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (Just $ const $ F.rightHook (callDyad f []) (callMonad g [])) (Just $ const $ F.rightFork (callDyad f []) (callDyad g [])) Nothing Nothing Nothing Nothing Nothing Nothing Nothing rightFork f g }
atRank = PrimitiveConjunction
  { conjRepr = [G.atRank]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Just $ \ea f r -> pure $ DerivedFunctionFunctionNoun (Just $ \ea' -> withCoreExtraArgs1 (rollR F.atRank1' (callMonad f []) r) (ea' ++ ea)) (Just $ \ea' -> withCoreExtraArgs2 (rollR F.atRank2' (callDyad f []) r) (ea' ++ ea)) Nothing Nothing Nothing Nothing Nothing Nothing Nothing atop f r
  , conjOnFunctionFunction = Nothing }
atDepth = PrimitiveConjunction
  { conjRepr = [G.atDepth]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Just $ \_ f d -> pure $ DerivedFunctionFunctionNoun (Just $ const $ F.atDepth1' (callMonad f []) d) (Just $ const $ F.atDepth2' (callDyad f []) d) Nothing Nothing Nothing Nothing Nothing Nothing Nothing over f d
  , conjOnFunctionFunction = Nothing }
repeat = PrimitiveConjunction
  { conjRepr = [G.repeat]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Just $ \_ f y -> pure $ DerivedFunctionFunctionNoun (Just $ const $ F.repeat1 (callMonad f []) y) (Just $ const $ F.repeat2 (callDyad f []) y) Nothing Nothing Nothing Nothing Nothing Nothing Nothing TinyAPL.Primitives.repeat f y
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (Just $ const $ F.until1 (callMonad f []) (callDyad g [])) (Just $ const $ F.until2 (callDyad f []) (callDyad g [])) Nothing Nothing Nothing Nothing Nothing Nothing Nothing TinyAPL.Primitives.repeat f g }
valences = PrimitiveConjunction
  { conjRepr = [G.valences]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Nothing
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (Just $ callMonad f) (Just $ callDyad g) Nothing Nothing Nothing Nothing Nothing Nothing Nothing valences f g }
under = PrimitiveConjunction
  { conjRepr = [G.under]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Just $ \_ x g -> pure $ DerivedFunctionNounFunction (Just $ const $ F.underK x (callMonad g [])) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing under x g
  , conjOnFunctionNoun = Nothing
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (Just $ const $ F.under (callMonad f []) (callMonad g [])) (Just $ const $ F.under2 (callDyad f []) (callMonad g [])) Nothing Nothing Nothing Nothing Nothing Nothing Nothing under f g }
innerProduct = PrimitiveConjunction
  { conjRepr = [G.innerProduct]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Nothing
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction Nothing (Just $ const $ F.innerProduct (callMonad f []) (callDyad g [])) Nothing Nothing Nothing Nothing Nothing Nothing Nothing innerProduct f g }
lev = PrimitiveConjunction
  { conjRepr = [G.lev]
  , conjContext = Nothing
  , conjOnNounNoun = Just $ \_ x y -> pure $ DerivedFunctionNounNoun (Just $ const $ F.constant1 x) (Just $ const $ F.constant2 x) Nothing Nothing Nothing Nothing Nothing Nothing Nothing lev x y
  , conjOnNounFunction = Just $ \_ x g -> pure $ DerivedFunctionNounFunction (Just $ const $ F.constant1 x) (Just $ const $ F.constant2 x) Nothing Nothing Nothing Nothing Nothing Nothing Nothing lev x g
  , conjOnFunctionNoun = Just $ \_ f y -> pure $ DerivedFunctionFunctionNoun (Just $ callMonad f) (Just $ callDyad f) Nothing Nothing Nothing Nothing Nothing Nothing Nothing lev f y
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (Just $ callMonad f) (Just $ callDyad f) Nothing Nothing Nothing Nothing Nothing Nothing Nothing lev f g }
dex = PrimitiveConjunction
  { conjRepr = [G.dex]
  , conjContext = Nothing
  , conjOnNounNoun = Just $ \_ x y -> pure $ DerivedFunctionNounNoun (Just $ const $ F.constant1 y) (Just $ const $ F.constant2 y) Nothing Nothing Nothing Nothing Nothing Nothing Nothing dex x y
  , conjOnNounFunction = Just $ \_ x g -> pure $ DerivedFunctionNounFunction (Just $ callMonad g) (Just $ callDyad g) Nothing Nothing Nothing Nothing Nothing Nothing Nothing dex x g
  , conjOnFunctionNoun = Just $ \_ f y -> pure $ DerivedFunctionFunctionNoun (Just $ const $ F.constant1 y) (Just $ const $ F.constant2 y) Nothing Nothing Nothing Nothing Nothing Nothing Nothing dex f y
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (Just $ callMonad g) (Just $ callDyad g) Nothing Nothing Nothing Nothing Nothing Nothing Nothing dex f g }
forkA = PrimitiveConjunction
  { conjRepr = [G.forkA]
  , conjContext = Nothing
  , conjOnNounNoun = Just $ \_ x y -> pure $ DerivedFunctionNounNoun (Just $ \_ _ -> message) (Just $ \_ _ _ -> message) Nothing Nothing Nothing Nothing Nothing Nothing Nothing forkA x y
  , conjOnNounFunction = Just $ \_ x g -> pure $ DerivedFunctionNounFunction (Just $ \_ _ -> message) (Just $ \_ _ _ -> message) Nothing Nothing Nothing Nothing Nothing Nothing Nothing forkA x g
  , conjOnFunctionNoun = Just $ \_ f y -> pure $ DerivedFunctionFunctionNoun (Just $ \_ _ -> message) (Just $ \_ _ _ -> message) Nothing Nothing Nothing Nothing Nothing Nothing Nothing forkA f y
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (Just $ \_ _ -> message) (Just $ \_ _ _ -> message) Nothing Nothing Nothing Nothing Nothing Nothing Nothing forkA f g }
  where message = throwError $ DomainError $ [G.forkA] ++ " must be used in conjunction with " ++ [G.forkB]
forkB = PrimitiveConjunction
  { conjRepr = [G.forkB]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Just $ \_ left z -> case left of
    DerivedFunctionNounNoun _ _ _ _ _ _ _ _ _ op x y | op == forkA -> pure $ DerivedFunctionFunctionNoun (Just $ const $ F.fork1 (F.constant1 x) (F.constant2 y) (F.constant1 z)) (Just $ const $ F.fork2 (F.constant2 x) (F.constant2 y) (F.constant2 z)) Nothing Nothing Nothing Nothing Nothing Nothing Nothing forkB left z
    DerivedFunctionNounFunction _ _ _ _ _ _ _ _ _ op x g | op == forkA -> pure $ DerivedFunctionFunctionNoun (Just $ const $ F.fork1 (F.constant1 x) (callDyad g []) (F.constant1 z)) (Just $ const $ F.fork2 (F.constant2 x) (callDyad g []) (F.constant2 z)) Nothing Nothing Nothing Nothing Nothing Nothing Nothing forkB left z
    DerivedFunctionFunctionNoun _ _ _ _ _ _ _ _ _ op f y | op == forkA -> pure $ DerivedFunctionFunctionNoun (Just $ const $ F.fork1 (callMonad f []) (F.constant2 y) (F.constant1 z)) (Just $ const $ F.fork2 (callDyad f []) (F.constant2 y) (F.constant2 z)) Nothing Nothing Nothing Nothing Nothing Nothing Nothing forkB left z
    DerivedFunctionFunctionFunction _ _ _ _ _ _ _ _ _ op f g | op == forkA -> pure $ DerivedFunctionFunctionNoun (Just $ const $ F.fork1 (callMonad f []) (callDyad g []) (F.constant1 z)) (Just $ const $ F.fork2 (callDyad f []) (callDyad g []) (F.constant2 z)) Nothing Nothing Nothing Nothing Nothing Nothing Nothing forkB left z
    _ -> message
  , conjOnFunctionFunction = Just $ \_ left h -> case left of
    DerivedFunctionNounNoun _ _ _ _ _ _ _ _ _ op x y | op == forkA -> pure $ DerivedFunctionFunctionFunction (Just $ const $ F.fork1 (F.constant1 x) (F.constant2 y) (callMonad h [])) (Just $ const $ F.fork2 (F.constant2 x) (F.constant2 y) (callDyad h [])) Nothing Nothing Nothing Nothing Nothing Nothing Nothing forkB left h
    DerivedFunctionNounFunction _ _ _ _ _ _ _ _ _ op x g | op == forkA -> pure $ DerivedFunctionFunctionFunction (Just $ const $ F.fork1 (F.constant1 x) (callDyad g []) (callMonad h [])) (Just $ const $ F.fork2 (F.constant2 x) (callDyad g []) (callDyad h [])) Nothing Nothing Nothing Nothing Nothing Nothing Nothing forkB left h
    DerivedFunctionFunctionNoun _ _ _ _ _ _ _ _ _ op f y | op == forkA -> pure $ DerivedFunctionFunctionFunction (Just $ const $  F.fork1 (callMonad f []) (F.constant2 y) (callMonad h [])) (Just $ const $ F.fork2 (callDyad f []) (F.constant2 y) (callDyad h [])) Nothing Nothing Nothing Nothing Nothing Nothing Nothing forkB left h
    DerivedFunctionFunctionFunction _ _ _ _ _ _ _ _ _ op f g | op == forkA -> pure $ DerivedFunctionFunctionFunction (Just $ const $ F.fork1 (callMonad f []) (callDyad g []) (callMonad h [])) (Just $ const $ F.fork2 (callDyad f []) (callDyad g []) (callDyad h [])) Nothing Nothing Nothing Nothing Nothing Nothing Nothing forkB left h
    _ -> message }
  where message = throwError $ DomainError $ [G.forkA] ++ " must be used in conjunction with " ++ [G.forkB]
approximate = PrimitiveConjunction
  { conjRepr = [G.approximate]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Just $ \_ f v -> do
    let err = DomainError $ "Approximate right operand must be a scalar real or function returning a scalar real"
    tolerance <- asScalar err v >>= asNumber err >>= asReal err
    pure $ DerivedFunctionFunctionNoun (Just $ \ea' -> callMonad f $ (box $ vector $ Character <$> coreExtraArgsToleranceKey, Number $ tolerance :+ 0) : ea') (Just $ \ea' -> callDyad f $ (box $ vector $ Character <$> coreExtraArgsToleranceKey, Number $ tolerance :+ 0) : ea') Nothing Nothing Nothing Nothing Nothing Nothing Nothing approximate f v
  , conjOnFunctionFunction = Just $ \_ f g -> do
    let err = DomainError $ "Approximate right operand must be a scalar real or function returning a scalar real"
    pure $ DerivedFunctionFunctionFunction (Just $ \ea' y -> do
      tolerance <- callMonad g [] y >>= asScalar err >>= asNumber err >>= asReal err
      callMonad f ((box $ vector $ Character <$> coreExtraArgsToleranceKey, Number $ tolerance :+ 0) : ea') y) (Just $ \ea' x y -> do
      tolerance <- callDyad g [] x y >>= asScalar err >>= asNumber err >>= asReal err
      callDyad f ((box $ vector $ Character <$> coreExtraArgsToleranceKey, Number $ tolerance :+ 0) : ea') x y) Nothing Nothing Nothing Nothing Nothing Nothing Nothing approximate f g }
fill = PrimitiveConjunction
  { conjRepr = [G.fill]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Just $ \_ f v -> do
    let fl = toScalar v
    pure $ DerivedFunctionFunctionNoun (Just $ \ea' -> callMonad f $ (box $ vector $ Character <$> coreExtraArgsFillKey, fl) : ea') (Just $ \ea' -> callDyad f $ (box $ vector $ Character <$> coreExtraArgsFillKey, fl) : ea') Nothing Nothing Nothing Nothing Nothing Nothing Nothing fill f v
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (Just $ \ea' y -> do
      fl <- toScalar <$> callMonad g [] y
      callMonad f ((box $ vector $ Character <$> coreExtraArgsFillKey, fl) : ea') y) (Just $ \ea' x y -> do
      fl <- toScalar <$> callDyad g [] x y
      callDyad f ((box $ vector $ Character <$> coreExtraArgsFillKey, fl) : ea') x y) Nothing Nothing Nothing Nothing Nothing Nothing Nothing fill f g }

conjunctions = (\x -> (headPromise $ conjRepr x, x)) <$>
  [ TinyAPL.Primitives.atop
  , TinyAPL.Primitives.over
  , TinyAPL.Primitives.reverseAtop
  , TinyAPL.Primitives.reverseOver
  , TinyAPL.Primitives.leftHook
  , TinyAPL.Primitives.rightHook
  , TinyAPL.Primitives.mirror
  , TinyAPL.Primitives.leftFork
  , TinyAPL.Primitives.rightFork
  , TinyAPL.Primitives.atRank
  , TinyAPL.Primitives.atDepth
  , TinyAPL.Primitives.repeat
  , TinyAPL.Primitives.valences
  , TinyAPL.Primitives.under
  , TinyAPL.Primitives.innerProduct
  , TinyAPL.Primitives.lev
  , TinyAPL.Primitives.dex
  , TinyAPL.Primitives.forkA
  , TinyAPL.Primitives.forkB
  , TinyAPL.Primitives.approximate
  , TinyAPL.Primitives.fill ]