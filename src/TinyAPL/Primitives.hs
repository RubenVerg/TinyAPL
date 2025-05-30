module TinyAPL.Primitives where

import TinyAPL.Noun
import TinyAPL.Function
import TinyAPL.Adverb
import TinyAPL.Conjunction
import TinyAPL.Context
import TinyAPL.CoreExtraArgs
import qualified TinyAPL.Functions as F
import qualified TinyAPL.Glyphs as G
import TinyAPL.Error
import TinyAPL.Util (rollR, bothM, showM)
import TinyAPL.Complex (Complex((:+)))

import Data.Tuple (swap)
import Control.Monad ((>=>))

withCoreExtraArgs1 :: (CoreExtraArgs -> a -> St r) -> ExtraArgs -> a -> St r
withCoreExtraArgs1 f ea a = parseCoreExtraArgs ea >>= (\cea -> f cea a)

withCoreExtraArgs2 :: (CoreExtraArgs -> a -> b -> St r) -> ExtraArgs -> a -> b -> St r
withCoreExtraArgs2 f ea a b = parseCoreExtraArgs ea >>= (\cea -> f cea a b)

-- * Primitive arrays


zilde = vector []
cilde = dictionary []

arrays :: [([Char], Noun)]
arrays =
  [ ([G.zilde], zilde)
  , ([G.cilde], cilde) ]

-- * Primitive functions

plus = PrimitiveFunction (FunctionCalls (Just $ const F.conjugate') (Just $ const F.add') (Just $ const F.conjugate') (Just $ const $ F.commute F.sub') (Just $ const F.sub') (Just $ withCoreExtraArgs1 F.floorAndFrac') (Just $ const F.halve') (Just $ const F.addAna') Nothing Nothing) [G.plus] Nothing
minus = PrimitiveFunction (FunctionCalls (Just $ const F.neg') (Just $ const F.sub') (Just $ const F.neg') (Just $ const F.sub') (Just $ const F.add') Nothing Nothing Nothing Nothing Nothing) [G.minus] Nothing
times = PrimitiveFunction (FunctionCalls (Just $ const F.sign') (Just $ const F.times') Nothing (Just $ const $ F.commute F.divide') (Just $ const F.divide') (Just $ const F.signAndAbs') (Just $ const F.squareRoot') (Just $ const F.timesAna') Nothing Nothing) [G.times] Nothing
divide = PrimitiveFunction (FunctionCalls (Just $ const F.reciprocal') (Just $ const F.divide') (Just $ const F.reciprocal') (Just $ const F.divide') (Just $ const F.times') Nothing Nothing Nothing Nothing Nothing) [G.divide] Nothing
power = PrimitiveFunction (FunctionCalls (Just $ const F.ePow') (Just $ const F.pow') (Just $ const F.ln') (Just $ const F.log') (Just $ const $ F.commute F.root') Nothing Nothing Nothing Nothing Nothing) [G.power] Nothing
logarithm = PrimitiveFunction (FunctionCalls (Just $ const F.ln') (Just $ const F.log') (Just $ const F.ePow') (Just $ const F.pow') (Just $ const $ F.root') Nothing Nothing Nothing Nothing Nothing) [G.logarithm] Nothing
root = PrimitiveFunction (FunctionCalls (Just $ const F.squareRoot') (Just $ const F.root') (Just $ const F.square') (Just $ const F.raises') (Just $ const F.log') Nothing Nothing Nothing Nothing Nothing) [G.root] Nothing
floor = PrimitiveFunction (FunctionCalls (Just $ withCoreExtraArgs1 F.floor') (Just $ const F.min') Nothing Nothing Nothing Nothing Nothing Nothing (Just $ withCoreExtraArgs1 F.underFloorForward) (Just $ const F.underFloorBack)) [G.floor] Nothing
ceil = PrimitiveFunction (FunctionCalls (Just $ withCoreExtraArgs1 F.ceil') (Just $ const F.max') Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.ceil] Nothing
round = PrimitiveFunction (FunctionCalls (Just $ const F.round') (Just $ const F.roundTo') Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.round] Nothing
less = PrimitiveFunction (FunctionCalls Nothing (Just $ withCoreExtraArgs2 F.less') Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.less] Nothing
lessEqual = PrimitiveFunction (FunctionCalls Nothing (Just $ withCoreExtraArgs2 F.lessEqual') Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.lessEqual] Nothing
equal = PrimitiveFunction (FunctionCalls Nothing (Just $ withCoreExtraArgs2 F.equal') Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.equal] Nothing
greaterEqual = PrimitiveFunction (FunctionCalls (Just $ withCoreExtraArgs1 F.lastCell) (Just $ withCoreExtraArgs2 F.greaterEqual') (Just $ const F.promote) Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.greaterEqual] Nothing
greater = PrimitiveFunction (FunctionCalls (Just $ withCoreExtraArgs1 F.firstCell) (Just $ withCoreExtraArgs2 F.greater') (Just $ const F.promote) Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.greater] Nothing
notEqual = PrimitiveFunction (FunctionCalls (Just $ withCoreExtraArgs1 F.nubSieve') (Just $ withCoreExtraArgs2 F.notEqual') Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.notEqual] Nothing
and = PrimitiveFunction (FunctionCalls (Just $ const F.promote) (Just $ const F.and') (Just $ const F.demote) Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.and] Nothing
or = PrimitiveFunction (FunctionCalls (Just $ const F.demote) (Just $ const F.or') (Just $ const F.promote) Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.or] Nothing
nand = PrimitiveFunction (FunctionCalls Nothing (Just $ const F.nand') Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.nand] Nothing
nor = PrimitiveFunction (FunctionCalls Nothing (Just $ const F.nor') Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.nor] Nothing
cartesian = PrimitiveFunction (FunctionCalls (Just $ const F.imaginary') (Just $ const F.cartesian') Nothing Nothing Nothing (Just $ const F.realAndImag') Nothing Nothing Nothing Nothing) [G.cartesian] Nothing
polar = PrimitiveFunction (FunctionCalls (Just $ const F.unitPolar') (Just $ const F.polar') Nothing Nothing Nothing (Just $ const F.absAndPhase') Nothing Nothing Nothing Nothing) [G.polar] Nothing
identical = PrimitiveFunction (FunctionCalls (Just $ const F.depth') (Just $ withCoreExtraArgs2 F.identical') Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.identical] Nothing
notIdentical = PrimitiveFunction (FunctionCalls (Just $ const F.tally') (Just $ withCoreExtraArgs2 F.notIdentical') (Just $ const F.unTally') Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.notIdentical] Nothing
rho = PrimitiveFunction (FunctionCalls (Just $ const F.shape') (Just $ const F.reshape') (Just $ withCoreExtraArgs1 F.unShape') Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.rho] Nothing
ravel = PrimitiveFunction (FunctionCalls (Just $ const F.ravel') (Just $ withCoreExtraArgs2 F.laminate) Nothing Nothing Nothing (Just $ withCoreExtraArgs1 F.firstAndLastCells) Nothing Nothing Nothing Nothing) [G.ravel] Nothing
reverse = PrimitiveFunction (FunctionCalls (Just $ const F.reverse') (Just $ withCoreExtraArgs2 F.rotate') (Just $ const F.reverse') (Just $ withCoreExtraArgs2 $ \cea -> F.rotate' cea{ coreExtraArgsBackward = not $ coreExtraArgsBackward cea }) Nothing Nothing Nothing Nothing Nothing Nothing) [G.reverse] Nothing
pair = PrimitiveFunction (FunctionCalls (Just $ const F.singleton) (Just $ const F.pair) (Just $ withCoreExtraArgs1 F.first) Nothing Nothing (Just $ withCoreExtraArgs1 F.firstAndLast) Nothing Nothing Nothing Nothing) [G.pair] Nothing
enclose = PrimitiveFunction (FunctionCalls (Just $ const F.enclose') (Just $ const F.partitionEnclose') (Just $ withCoreExtraArgs1 F.first) Nothing Nothing (Just $ const F.disPartitionEnclose') Nothing Nothing Nothing Nothing) [G.enclose] Nothing
first = PrimitiveFunction (FunctionCalls (Just $ withCoreExtraArgs1 F.first) Nothing (Just $ const F.enclose') Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.first] Nothing
last = PrimitiveFunction (FunctionCalls (Just $ withCoreExtraArgs1 F.last) (Just $ withCoreExtraArgs2 F.from) (Just $ const F.enclose') Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.last] Nothing
take = PrimitiveFunction (FunctionCalls (Just $ withCoreExtraArgs1 F.mix) (Just $ withCoreExtraArgs2 F.take') (Just $ const F.majorCells') Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.take] Nothing
drop = PrimitiveFunction (FunctionCalls (Just $ const F.majorCells') (Just $ withCoreExtraArgs2 F.drop') (Just $ withCoreExtraArgs1 F.mix) Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.drop] Nothing
left = PrimitiveFunction (FunctionCalls (Just $ \_ x -> pure x) (Just $ \_ x _ -> pure x) (Just $ \_ x -> pure x) Nothing (Just $ \_ x _ -> pure x) Nothing Nothing Nothing Nothing Nothing) [G.left] Nothing
right = PrimitiveFunction (FunctionCalls (Just $ \_ x -> pure x) (Just $ \_ _ y -> pure y) (Just $ \_ x -> pure x) (Just $ \_ _ y -> pure y) Nothing Nothing Nothing Nothing Nothing Nothing) [G.right] Nothing
iota = PrimitiveFunction (FunctionCalls (Just $ withCoreExtraArgs1 F.indexGenerator') (Just $ withCoreExtraArgs2 F.indexOf) Nothing (Just $ withCoreExtraArgs2 $ F.commute . F.from) Nothing Nothing Nothing Nothing Nothing Nothing) [G.iota] Nothing
indices = PrimitiveFunction (FunctionCalls (Just $ withCoreExtraArgs1 F.indices) (Just $ withCoreExtraArgs2 F.intervalIndex) (Just $ withCoreExtraArgs1 F.histogram) Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.indices] Nothing
replicate = PrimitiveFunction (FunctionCalls Nothing (Just $ const F.replicate') Nothing Nothing Nothing (Just $ const F.runLengthEncode') Nothing Nothing Nothing Nothing) [G.replicate] Nothing
abs = PrimitiveFunction (FunctionCalls (Just $ const F.abs') (Just $ withCoreExtraArgs2 F.remainder') Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.abs] Nothing
phase = PrimitiveFunction (FunctionCalls (Just $ const F.phase') (Just $ const F.arctan') (Just $ const F.unitPolar') Nothing Nothing (Just $ const F.sineAndCosine') Nothing Nothing Nothing Nothing) [G.phase] Nothing
real = PrimitiveFunction (FunctionCalls (Just $ const F.real') Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.real] Nothing
imag = PrimitiveFunction (FunctionCalls (Just $ const F.imag') Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.imag] Nothing
union = PrimitiveFunction (FunctionCalls (Just $ withCoreExtraArgs1 F.unique') (Just $ withCoreExtraArgs2 F.union') Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.union] Nothing
intersection = PrimitiveFunction (FunctionCalls Nothing (Just $ withCoreExtraArgs2 F.intersection') Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.intersection] Nothing
difference = PrimitiveFunction (FunctionCalls (Just $ const F.not') (Just $ withCoreExtraArgs2 F.difference') (Just $ const F.not') Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.difference] Nothing
symdiff = PrimitiveFunction (FunctionCalls Nothing (Just $ withCoreExtraArgs2 F.symmetricDifference') Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.symdiff] Nothing
element = PrimitiveFunction (FunctionCalls (Just $ const F.enlist') (Just $ withCoreExtraArgs2 F.elementOf) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.element] Nothing
roll = PrimitiveFunction (FunctionCalls (Just $ withCoreExtraArgs1 F.roll') (Just $ withCoreExtraArgs2 F.deal') Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.roll] Nothing
squad = PrimitiveFunction (FunctionCalls Nothing (Just $ withCoreExtraArgs2 F.squad) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.squad] Nothing
rank = PrimitiveFunction (FunctionCalls (Just $ const F.rank') (Just $ const F.rerank') (Just $ withCoreExtraArgs1 F.unRank') Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.rank] Nothing
catenate = PrimitiveFunction (FunctionCalls (Just $ withCoreExtraArgs1 F.join') (Just $ withCoreExtraArgs2 F.catenate) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.catenate] Nothing
gradeUp = PrimitiveFunction (FunctionCalls (Just $ withCoreExtraArgs1 F.gradeUp') (Just $ withCoreExtraArgs2 F.sortByUp') Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.gradeUp] Nothing
gradeDown = PrimitiveFunction (FunctionCalls (Just $ withCoreExtraArgs1 F.gradeDown') (Just $ withCoreExtraArgs2 F.sortByDown') Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.gradeDown] Nothing
precedes = PrimitiveFunction (FunctionCalls Nothing (Just $ withCoreExtraArgs2 F.precedes') Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.precedes] Nothing
precedesOrIdentical = PrimitiveFunction (FunctionCalls (Just $ withCoreExtraArgs1 F.sortUp') (Just $ withCoreExtraArgs2 F.precedesOrIdentical') Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.precedesOrIdentical] Nothing
succeedsOrIdentical = PrimitiveFunction (FunctionCalls (Just $ withCoreExtraArgs1 F.sortDown') (Just $ withCoreExtraArgs2 F.succeedsOrIdentical') Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.succeedsOrIdentical] Nothing
succeeds = PrimitiveFunction (FunctionCalls Nothing (Just $ withCoreExtraArgs2 F.succeeds') Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.succeeds] Nothing
minimal = PrimitiveFunction (FunctionCalls Nothing (Just $ const F.minimal) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.minimal] Nothing
maximal = PrimitiveFunction (FunctionCalls Nothing (Just $ const F.maximal) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.maximal] Nothing
transpose = PrimitiveFunction (FunctionCalls (Just $ withCoreExtraArgs1 F.transpose) (Just $ withCoreExtraArgs2 F.dyadicTranspose') (Just $ withCoreExtraArgs1 $ \cea -> F.transpose cea{ coreExtraArgsBackward = not $ coreExtraArgsBackward cea }) (Just $ withCoreExtraArgs2 $ \cea -> F.dyadicTranspose' cea{ coreExtraArgsBackward = not $ coreExtraArgsBackward cea }) Nothing Nothing Nothing Nothing Nothing Nothing) [G.transpose] Nothing
matrixInverse = PrimitiveFunction (FunctionCalls (Just $ const F.matrixInverse') (Just $ const F.matrixDivide') (Just $ const F.matrixInverse') (Just $ const F.matrixDivide') (Just $ const $ F.commute $ F.innerProduct (F.reduce' defaultCoreExtraArgs F.add') F.times') Nothing Nothing Nothing Nothing Nothing) [G.matrixInverse] Nothing
factorial = PrimitiveFunction (FunctionCalls (Just $ const F.factorial') (Just $ const F.binomial') Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.factorial] Nothing
raise = PrimitiveFunction (FunctionCalls (Just $ const F.raise1) (Just $ const F.raise') Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.raise] Nothing
decode = PrimitiveFunction (FunctionCalls (Just $ withCoreExtraArgs1 F.decodeBase2) (Just $ withCoreExtraArgs2 F.decode') (Just $ withCoreExtraArgs1 F.encodeBase2) (Just $ withCoreExtraArgs2 F.encode') Nothing Nothing Nothing Nothing Nothing Nothing) [G.decode] Nothing
encode = PrimitiveFunction (FunctionCalls (Just $ withCoreExtraArgs1 F.encodeBase2) (Just $ withCoreExtraArgs2 F.encode') (Just $ withCoreExtraArgs1 F.decodeBase2) (Just $ withCoreExtraArgs2 F.decode') Nothing Nothing Nothing Nothing Nothing Nothing) [G.encode] Nothing
histogram = PrimitiveFunction (FunctionCalls (Just $ withCoreExtraArgs1 F.histogram) (Just $ withCoreExtraArgs2 F.count) (Just $ withCoreExtraArgs1 F.indices) Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.histogram] Nothing
increment = PrimitiveFunction (FunctionCalls (Just $ const F.increment') Nothing (Just $ const F.decrement') Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.increment] Nothing
decrement = PrimitiveFunction (FunctionCalls (Just $ const F.decrement') (Just $ const F.span') (Just $ const F.increment') (Just $ const F.span') (Just $ const $ F.atop F.decrement' F.add') Nothing Nothing Nothing Nothing Nothing) [G.decrement] Nothing
range = PrimitiveFunction (FunctionCalls (Just $ withCoreExtraArgs1 F.oneRange) (Just $ withCoreExtraArgs2 F.range) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.range] Nothing
keyValue = PrimitiveFunction (FunctionCalls (Just $ const F.fromPairs) (Just $ const F.keyValuePair) (Just $ const F.majorCells') Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.keyValue] Nothing
invertedTable = PrimitiveFunction (FunctionCalls (Just $ const F.fromInvertedTable) (Just $ const F.fromKeysAndValues') (Just $ const F.invertedTable') Nothing Nothing (Just $ withCoreExtraArgs1 F.firstAndLast) Nothing Nothing Nothing Nothing) [G.invertedTable] Nothing
group = PrimitiveFunction (FunctionCalls Nothing (Just $ withCoreExtraArgs2 F.group') Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.group] Nothing
partition = PrimitiveFunction (FunctionCalls Nothing (Just $ const F.partition') Nothing Nothing Nothing (Just $ withCoreExtraArgs1 F.disPartition') Nothing Nothing Nothing Nothing) [G.partition] Nothing
execute = PrimitiveFunction (FunctionCalls (Just $ const F.execute') (Just $ const F.executeWith) (Just $ const $ \y -> vector . fmap Character <$> showM (Repr y)) Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.execute] Nothing
format = PrimitiveFunction (FunctionCalls (Just $ const F.format') Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.format] Nothing
find = PrimitiveFunction (FunctionCalls Nothing (Just $ withCoreExtraArgs2 F.find') Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.find] Nothing
mask = PrimitiveFunction (FunctionCalls Nothing (Just $ withCoreExtraArgs2 F.mask') Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.mask] Nothing
raises = PrimitiveFunction (FunctionCalls (Just $ const F.square') (Just $ const F.raises') (Just $ const F.squareRoot') (Just $ const F.root') (Just $ const $ F.commute F.log') Nothing Nothing Nothing Nothing Nothing) [G.raises] Nothing
lcm = PrimitiveFunction (FunctionCalls (Just $ withCoreExtraArgs1 F.numerator') (Just $ withCoreExtraArgs2 F.lcm') Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.lcm] Nothing
gcd = PrimitiveFunction (FunctionCalls (Just $ withCoreExtraArgs1 F.denominator') (Just $ withCoreExtraArgs2 F.gcd') Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [G.gcd] Nothing

functions :: [(String, Function)]
functions = (\x -> (functionRepr x, x)) <$>
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
  , TinyAPL.Primitives.raises
  , TinyAPL.Primitives.lcm
  , TinyAPL.Primitives.gcd ]

-- * Primitive adverbs

selfie = PrimitiveAdverb
  { adverbRepr = [G.selfie]
  , adverbContext = Nothing
  , adverbOnNoun = Just $ \_ x -> pure $ DerivedFunctionNoun (FunctionCalls (Just $ const $ F.constant1 x) (Just $ const $ F.constant2 x) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing selfie x
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction (FunctionCalls (Just $ const $ F.duplicate $ callDyad f []) (Just $ const $ F.commute $ callDyad f []) (Just $ const $ callBi f []) (Just $ const $ F.commute $ callContra f []) (Just $ const $ F.commute $ callAnti f []) (Just $ const $ fmap swap . callDis f []) Nothing Nothing Nothing Nothing) Nothing selfie f }
reduce = PrimitiveAdverb
  { adverbRepr = [G.reduce]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \ea f -> pure $ DerivedFunctionFunction (FunctionCalls (Just $ \ea' -> withCoreExtraArgs1 (flip F.reduce' $ callDyad f []) (ea' ++ ea)) (Just $ \ea' -> withCoreExtraArgs2 (flip F.fold' $ callDyad f []) (ea' ++ ea)) (Just $ \ea' y -> case f of
    DerivedFunctionFunction{ derivedFunctionFunctionLeft = f', derivedFunctionAdverb = adv } | adv == onContents -> callDis f' (ea' ++ ea) y >>= uncurry F.pair
    _ -> callDis f (ea' ++ ea) y >>= uncurry (F.laminate defaultCoreExtraArgs)) (Just $ \ea' x y -> case f of
    DerivedFunctionFunction{ derivedFunctionFunctionLeft = f', derivedFunctionAdverb = adv } | adv == onContents -> vector . fmap box <$> callAna f' (ea' ++ ea) x y
    _ -> do
      CoreExtraArgs{ coreExtraArgsFill = fill } <- parseCoreExtraArgs $ ea' ++ ea
      callAna f (ea' ++ ea) x y >>= fromMajorCellsMaybeFilled fill) Nothing Nothing Nothing Nothing Nothing Nothing) Nothing reduce f }
onPrefixes = PrimitiveAdverb
  { adverbRepr = [G.onPrefixes]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \ea f -> pure $ DerivedFunctionFunction (FunctionCalls (Just $ \ea' -> withCoreExtraArgs1 (flip F.onPrefixes' $ callMonad f []) (ea' ++ ea)) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing onPrefixes f }
onInfixes = PrimitiveAdverb
  { adverbRepr = [G.onInfixes]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction (FunctionCalls (Just $ const $ F.onPairs' $ callDyad f []) (Just $ const $ F.onInfixes' $ callMonad f []) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing onInfixes f }
each = PrimitiveAdverb
  { adverbRepr = [G.each]
  , adverbContext = Nothing
  , adverbOnNoun = Just $ \_ x -> pure $ DerivedFunctionNoun (FunctionCalls (Just $ const $ F.each1 $ F.constant1 x) (Just $ const $ F.each2 $ F.constant2 x) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing each x
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction (FunctionCalls (Just $ const $ F.each1 $ callMonad f []) (Just $ const $ F.each2 $ callDyad f []) (Just $ const $ F.each1 $ callUn f []) (Just $ const $ F.each2 $ callAnti f []) (Just $ const $ F.each2 $ callContra f []) Nothing (Just $ const $ F.each1 $ callBi f []) Nothing Nothing Nothing) Nothing each f }
eachLeft = PrimitiveAdverb
  { adverbRepr = [G.eachLeft]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction (FunctionCalls Nothing (Just $ const $ F.eachLeft $ callDyad f []) Nothing (Just $ const $ F.atop (F.first defaultCoreExtraArgs) $ F.each2 $ callAnti f []) (Just $ const $ F.eachLeft $ callContra f []) Nothing Nothing Nothing Nothing Nothing) Nothing eachLeft f }
eachRight = PrimitiveAdverb
  { adverbRepr = [G.eachRight]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction (FunctionCalls Nothing (Just $ const $ F.eachRight $ callDyad f []) Nothing (Just $ const $ F.eachRight $ callAnti f []) (Just $ const $ F.atop (F.first defaultCoreExtraArgs) $ F.each2 $ callContra f []) Nothing Nothing Nothing Nothing Nothing) Nothing eachRight f }
key = PrimitiveAdverb
  { adverbRepr = [G.key]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \ea f -> pure $ DerivedFunctionFunction (FunctionCalls (Just $ \ea' -> withCoreExtraArgs1 (flip F.keyMonad $ callDyad f []) (ea' ++ ea)) (Just $ const $ F.key' $ callDyad f []) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing key f }
onCells = PrimitiveAdverb
  { adverbRepr = [G.onCells]
  , adverbContext = Nothing
  , adverbOnNoun = Just $ \ea x -> pure $ DerivedFunctionNoun (FunctionCalls (Just $ \ea' -> withCoreExtraArgs1 (flip F.onCells1 $ F.constant1 x) (ea' ++ ea)) (Just $ \ea' -> withCoreExtraArgs2 (flip F.onCells2 $ F.constant2 x) (ea' ++ ea)) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing onCells x
  , adverbOnFunction = Just $ \ea f -> pure $ DerivedFunctionFunction (FunctionCalls (Just $ \ea' -> withCoreExtraArgs1 (flip F.onCells1 $ callMonad f []) (ea' ++ ea)) (Just $ \ea' -> withCoreExtraArgs2 (flip F.onCells2 $ callDyad f []) (ea' ++ ea)) (Just $ \ea' -> withCoreExtraArgs1 (flip F.onCells1 $ callUn f []) (ea' ++ ea)) (Just $ \ea' -> withCoreExtraArgs2 (flip F.onCells2 $ callAnti f []) (ea' ++ ea)) (Just $ \ea' -> withCoreExtraArgs2 (flip F.onCells2 $ callContra f []) (ea' ++ ea)) Nothing (Just $ \ea' -> withCoreExtraArgs1 (flip F.onCells1 $ callBi f []) (ea' ++ ea)) Nothing Nothing Nothing) Nothing onCells f }
onScalars = PrimitiveAdverb
  { adverbRepr = [G.onScalars]
  , adverbContext = Nothing
  , adverbOnNoun = Just $ \ea x -> pure $ DerivedFunctionNoun (FunctionCalls (Just $ \ea' -> withCoreExtraArgs1 (flip F.onScalars1 $ F.constant1 x) (ea' ++ ea)) (Just $ \ea' -> withCoreExtraArgs2 (flip F.onScalars2 $ F.constant2 x) (ea' ++ ea)) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing onScalars x
  , adverbOnFunction = Just $ \ea f -> pure $ DerivedFunctionFunction (FunctionCalls (Just $ \ea' -> withCoreExtraArgs1 (flip F.onScalars1 $ callMonad f []) (ea' ++ ea)) (Just $ \ea' -> withCoreExtraArgs2 (flip F.onScalars2 $ callDyad f []) (ea' ++ ea)) (Just $ \ea' -> withCoreExtraArgs1 (flip F.onScalars1 $ callUn f []) (ea' ++ ea)) (Just $ \ea' -> withCoreExtraArgs2 (flip F.onScalars2 $ callAnti f []) (ea' ++ ea)) (Just $ \ea' -> withCoreExtraArgs2 (flip F.onScalars2 $ callContra f []) (ea' ++ ea)) Nothing (Just $ \ea' -> withCoreExtraArgs1 (flip F.onScalars1 $ callBi f []) (ea' ++ ea)) Nothing Nothing Nothing) Nothing onScalars f }
boxed = PrimitiveAdverb
  { adverbRepr = [G.boxed]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction (FunctionCalls (Just $ const $ F.boxed1 $ callMonad f []) (Just $ const $ F.boxed2 $ callDyad f []) (Just $ const $ F.onContents1 $ callUn f []) (Just $ const $ F.after (callAnti f []) F.discloseIfScalar) (Just $ const $ F.before F.discloseIfScalar (callContra f [])) Nothing (Just $ const $ F.onContents1 $ callBi f []) Nothing Nothing Nothing) Nothing boxed f }
onContents = PrimitiveAdverb
  { adverbRepr = [G.onContents]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction (FunctionCalls (Just $ const $ F.onContents1 $ callMonad f []) (Just $ const $ F.onContents2 $ callDyad f []) (Just $ const $ F.boxed1 $ callUn f []) (Just $ const $ F.boxed2 $ callAnti f []) (Just $ const $ F.boxed2 $ callContra f []) Nothing (Just $ const $ F.boxed1 $ callBi f []) Nothing Nothing Nothing) Nothing onContents f }
table = PrimitiveAdverb
  { adverbRepr = [G.table]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction (FunctionCalls Nothing (Just $ const $ F.table $ callDyad f []) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing table f }
ident = PrimitiveAdverb
  { adverbRepr = [G.ident]
  , adverbContext = Nothing
  , adverbOnNoun = Just $ \_ x -> pure $ DerivedFunctionNoun (FunctionCalls (Just $ const $ F.constant1 x) (Just $ const $ F.constant2 x) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing ident x
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction (FunctionCalls (Just $ callMonad f) (Just $ callDyad f) (Just $ callUn f) (Just $ callAnti f) (Just $ callContra f) (Just $ callDis f) (Just $ callBi f) (Just $ callAna f) Nothing Nothing) Nothing ident f }
onSimpleScalars = PrimitiveAdverb
  { adverbRepr = [G.onSimpleScalars]
  , adverbContext = Nothing
  , adverbOnNoun = Just $ \_ x -> pure $ DerivedFunctionNoun (FunctionCalls (Just $ const $ F.onSimpleScalars1 $ F.constant1 x) (Just $ const $ F.onSimpleScalars2 $ F.constant2 x) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing onSimpleScalars x
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction (FunctionCalls (Just $ const $ F.onSimpleScalars1 $ callMonad f []) (Just $ const $ F.onSimpleScalars2 $ callDyad f []) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing onSimpleScalars f }
originOne = PrimitiveAdverb
  { adverbRepr = [G.originOne]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction (FunctionCalls (Just $ \ea' -> callMonad f $ (box $ vector $ Character <$> coreExtraArgsOriginKey, Number 1) : ea') (Just $ \ea' -> callDyad f $ (box $ vector $ Character <$> coreExtraArgsOriginKey, Number 1) : ea') (Just $ \ea' -> callUn f $ (box $ vector $ Character <$> coreExtraArgsOriginKey, Number 1) : ea') (Just $ \ea' -> callAnti f $ (box $ vector $ Character <$> coreExtraArgsOriginKey, Number 1) : ea') (Just $ \ea' -> callContra f $ (box $ vector $ Character <$> coreExtraArgsOriginKey, Number 1) : ea') (Just $ \ea' -> callDis f $ (box $ vector $ Character <$> coreExtraArgsOriginKey, Number 1) : ea') (Just $ \ea' -> callBi f $ (box $ vector $ Character <$> coreExtraArgsOriginKey, Number 1) : ea') (Just $ \ea' -> callAna f $ (box $ vector $ Character <$> coreExtraArgsOriginKey, Number 1) : ea') Nothing Nothing) Nothing originOne f }
backward = PrimitiveAdverb
  { adverbRepr = [G.backward]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \_ f -> pure $ DerivedFunctionFunction (FunctionCalls (Just $ \ea' -> callMonad f $ (box $ vector $ Character <$> coreExtraArgsBackwardKey, Number 1) : ea') (Just $ \ea' -> callDyad f $ (box $ vector $ Character <$> coreExtraArgsBackwardKey, Number 1) : ea') (Just $ \ea' -> callUn f $ (box $ vector $ Character <$> coreExtraArgsBackwardKey, Number 1) : ea') (Just $ \ea' -> callAnti f $ (box $ vector $ Character <$> coreExtraArgsBackwardKey, Number 1) : ea') (Just $ \ea' -> callContra f $ (box $ vector $ Character <$> coreExtraArgsBackwardKey, Number 1) : ea') (Just $ \ea' -> callDis f $ (box $ vector $ Character <$> coreExtraArgsBackwardKey, Number 1) : ea') (Just $ \ea' -> callBi f $ (box $ vector $ Character <$> coreExtraArgsBackwardKey, Number 1) : ea') (Just $ \ea' -> callAna f $ (box $ vector $ Character <$> coreExtraArgsBackwardKey, Number 1) : ea') Nothing Nothing) Nothing backward f }
bitwise = PrimitiveAdverb
  { adverbRepr = [G.bitwise]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \ea f -> pure $ DerivedFunctionFunction (FunctionCalls (Just $ \ea' -> withCoreExtraArgs1 (flip F.bitwise1' $ callMonad f []) (ea' ++ ea)) (Just $ \ea' -> withCoreExtraArgs2 (flip F.bitwise2' $ callDyad f []) (ea' ++ ea)) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing onCells f }
cellsLeft = PrimitiveAdverb
  { adverbRepr = [G.cellsLeft]
  , adverbContext = Nothing
  , adverbOnNoun = Just $ \ea x -> pure $ DerivedFunctionNoun (FunctionCalls Nothing (Just $ \ea' -> withCoreExtraArgs2 (flip F.cellsLeft $ F.constant2 x) (ea' ++ ea)) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing cellsLeft x
  , adverbOnFunction = Just $ \ea f -> pure $ DerivedFunctionFunction (FunctionCalls Nothing (Just $ \ea' -> withCoreExtraArgs2 (flip F.cellsLeft $ callDyad f []) (ea' ++ ea)) Nothing (Just $ \ea' -> F.atop (F.firstCell defaultCoreExtraArgs) $ withCoreExtraArgs2 (flip F.onCells2 $ callAnti f []) (ea' ++ ea)) (Just $ \ea' -> withCoreExtraArgs2 (flip F.cellsLeft $ callContra f []) (ea' ++ ea)) Nothing Nothing Nothing Nothing Nothing) Nothing cellsLeft f }
cellsRight = PrimitiveAdverb
  { adverbRepr = [G.cellsRight]
  , adverbContext = Nothing
  , adverbOnNoun = Just $ \ea x -> pure $ DerivedFunctionNoun (FunctionCalls Nothing (Just $ \ea' -> withCoreExtraArgs2 (flip F.cellsRight $ F.constant2 x) (ea' ++ ea)) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing cellsRight x
  , adverbOnFunction = Just $ \ea f -> pure $ DerivedFunctionFunction (FunctionCalls Nothing (Just $ \ea' -> withCoreExtraArgs2 (flip F.cellsRight $ callDyad f []) (ea' ++ ea)) Nothing (Just $ \ea' -> withCoreExtraArgs2 (flip F.cellsRight $ callAnti f []) (ea' ++ ea)) (Just $ \ea' -> F.atop (F.firstCell defaultCoreExtraArgs) $ withCoreExtraArgs2 (flip F.onCells2 $ callContra f []) (ea' ++ ea)) Nothing Nothing Nothing Nothing Nothing) Nothing cellsRight f }
inverse = PrimitiveAdverb
  { adverbRepr = [G.inverse]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ const $ \f -> pure $ DerivedFunctionFunction (FunctionCalls (Just $ callUn f) (Just $ callAnti f) (Just $ callMonad f) (Just $ callDyad f) Nothing Nothing Nothing Nothing Nothing Nothing) Nothing inverse f }
onLeft = PrimitiveAdverb
  { adverbRepr = [G.onLeft]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \ea f -> pure $ DerivedFunctionFunction (FunctionCalls (Just $ \ea' y -> callMonad f (ea' ++ ea) y) (Just $ \ea' x _ -> callMonad f (ea' ++ ea) x) (Just $ \ea' y -> callUn f (ea' ++ ea) y) Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing onLeft f }
onRight = PrimitiveAdverb
  { adverbRepr = [G.onRight]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ \ea f -> pure $ DerivedFunctionFunction (FunctionCalls (Just $ \ea' y -> callMonad f (ea' ++ ea) y) (Just $ \ea' _ y -> callMonad f (ea' ++ ea) y) (Just $ \ea' y -> callUn f (ea' ++ ea) y) Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing onRight f }
multisets = PrimitiveAdverb
  { adverbRepr = [G.multisets]
  , adverbContext = Nothing
  , adverbOnNoun = Nothing
  , adverbOnFunction = Just $ const $ \f -> pure $ DerivedFunctionFunction (FunctionCalls (Just $ const $ F.onCounts $ callMonad f []) (Just $ const $ F.multisets $ callDyad f []) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing multisets f }

adverbs :: [(String, Adverb)]
adverbs = (\x -> (adverbRepr x, x)) <$>
  [ TinyAPL.Primitives.selfie
  , TinyAPL.Primitives.reduce
  , TinyAPL.Primitives.onPrefixes
  , TinyAPL.Primitives.onInfixes
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
  , TinyAPL.Primitives.inverse
  , TinyAPL.Primitives.onLeft
  , TinyAPL.Primitives.onRight
  , TinyAPL.Primitives.multisets ]

-- * Primitive conjunctions

atop = PrimitiveConjunction
  { conjRepr = [G.atop]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Just $ \_ x g -> pure $ DerivedFunctionNounFunction (FunctionCalls (Just $ \_ y -> callDyad g [] x y) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing atop x g
  , conjOnFunctionNoun = Just $ \_ f y -> pure $ DerivedFunctionFunctionNoun (FunctionCalls (Just $ \_ x -> callDyad f [] x y) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing atop f y
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (FunctionCalls (Just $ const $ F.compose (callMonad f []) (callMonad g [])) (Just $ const $ F.atop (callMonad f []) (callDyad g [])) (Just $ const $ F.compose (callUn g []) (callUn f [])) (Just $ const $ F.after (callAnti g []) (callUn f [])) (Just $ const $ F.before (callUn f []) (callContra g [])) (Just $ const $ F.compose (callDis g []) (callUn f [])) Nothing Nothing Nothing Nothing) Nothing atop f g }
over = PrimitiveConjunction
  { conjRepr = [G.over]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Just $ \_ x g -> pure $ DerivedFunctionNounFunction (FunctionCalls (Just $ \_ y -> callDyad g [] x y) (Just $ \_ x' y -> callDyad g [] x' y) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing over x g
  , conjOnFunctionNoun = Just $ \_ f y -> pure $ DerivedFunctionFunctionNoun (FunctionCalls (Just $ \_ x -> callDyad f [] x y) (Just $ \_ x y' -> callDyad f [] x y') Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing over f y
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (FunctionCalls (Just $ const $ F.compose (callMonad f []) (callMonad g [])) (Just $ const $ F.over (callDyad f []) (callMonad g [])) (Just $ const $ F.compose (callUn g []) (callUn f [])) (Just $ const $ F.before (callMonad f []) (F.atop (callUn f []) (callAnti g []))) (Just $ const $ F.after (F.atop (callUn f []) (callContra g [])) (callMonad f [])) (Just $ const $ (callDis f []) >=> bothM (callUn g [])) (Just $ const $ F.compose (callUn g []) (callBi f [])) Nothing Nothing Nothing) Nothing over f g }
reverseAtop = PrimitiveConjunction
  { conjRepr = [G.reverseAtop]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Just $ \_ y g -> pure $ DerivedFunctionNounFunction (FunctionCalls (Just $ \_ x -> callDyad g [] x y) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing reverseAtop y g
  , conjOnFunctionNoun = Just $ \_ f x -> pure $ DerivedFunctionFunctionNoun (FunctionCalls (Just $ \_ y -> callDyad f [] x y) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing reverseAtop f x
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (FunctionCalls (Just $ const $ F.reverseCompose (callMonad f []) (callMonad g [])) (Just $ const $ F.reverseAtop (callDyad f []) (callMonad g [])) (Just $ const $ F.compose (callUn f []) (callUn g [])) (Just $ const $ F.after (callAnti f []) (callUn g [])) (Just $ const $ F.before (callUn g []) (callContra f [])) (Just $ const $ F.compose (callDis f []) (callUn g [])) Nothing Nothing Nothing Nothing) Nothing reverseAtop f g }
reverseOver = PrimitiveConjunction
  { conjRepr = [G.reverseOver]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Just $ \_ y g -> pure $ DerivedFunctionNounFunction (FunctionCalls (Just $ \_ x -> callDyad g [] x y) (Just $ \_ x y' -> callDyad g [] x y') Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing reverseOver y g
  , conjOnFunctionNoun = Just $ \_ f x -> pure $ DerivedFunctionFunctionNoun (FunctionCalls (Just $ \_ y -> callDyad f [] x y) (Just $ \_ x' y -> callDyad f [] x' y) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing reverseOver f x
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (FunctionCalls (Just $ const $ F.reverseCompose (callMonad f []) (callMonad g [])) (Just $ const $ F.reverseOver (callMonad f []) (callDyad g [])) (Just $ const $ F.compose (callUn f []) (callUn g [])) (Just $ const $ F.before (callMonad g []) (F.atop (callUn g []) (callAnti f []))) (Just $ const $ F.after (F.atop (callUn g []) (callContra f [])) (callMonad g [])) (Just $ const $ (callDis g []) >=> bothM (callUn f [])) (Just $ const $ F.compose (callUn f []) (callBi g [])) Nothing Nothing Nothing) Nothing reverseOver f g }
leftHook = PrimitiveConjunction
  { conjRepr = [G.leftHook]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Nothing
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (FunctionCalls (Just $ const $ F.leftHook (callMonad f []) (callDyad g [])) (Just $ const $ F.before (callMonad f []) (callDyad g [])) Nothing (Just $ const $ F.before (callMonad f []) (callAnti g [])) (Just $ const $ F.atop (callUn f []) (callContra g [])) Nothing Nothing Nothing Nothing Nothing) Nothing leftHook f g }
rightHook = PrimitiveConjunction
  { conjRepr = [G.rightHook]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Nothing
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (FunctionCalls (Just $ const $ F.rightHook (callDyad f []) (callMonad g [])) (Just $ const $ F.after (callDyad f []) (callMonad g [])) Nothing (Just $ const $ F.atop (callUn g []) (callAnti f [])) (Just $ const $ F.after (callContra f []) (callMonad g [])) Nothing Nothing Nothing Nothing Nothing) Nothing rightHook f g }
mirror = PrimitiveConjunction
  { conjRepr = [G.mirror]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Nothing
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (FunctionCalls Nothing (Just $ const $ F.mirror (callDyad f []) (callDyad g [])) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing mirror f g }
leftFork = PrimitiveConjunction
  { conjRepr = [G.leftFork]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Nothing
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (FunctionCalls (Just $ const $ F.leftHook (callMonad f []) (callDyad g [])) (Just $ const $ F.leftFork (callDyad f []) (callDyad g [])) Nothing Nothing (Just $ const $ F.leftFork (callContra f []) (callContra g [])) Nothing Nothing Nothing Nothing Nothing) Nothing leftFork f g }
rightFork = PrimitiveConjunction
  { conjRepr = [G.rightFork]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Nothing
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (FunctionCalls (Just $ const $ F.rightHook (callDyad f []) (callMonad g [])) (Just $ const $ F.rightFork (callDyad f []) (callDyad g [])) Nothing (Just $ const $ F.rightFork (callAnti g []) (callAnti f [])) Nothing Nothing Nothing Nothing Nothing Nothing) Nothing rightFork f g }
atRank = PrimitiveConjunction
  { conjRepr = [G.atRank]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Just $ \ea f r -> pure $ DerivedFunctionFunctionNoun (FunctionCalls (Just $ \ea' -> withCoreExtraArgs1 (rollR F.atRank1' (callMonad f []) r) (ea' ++ ea)) (Just $ \ea' -> withCoreExtraArgs2 (rollR F.atRank2' (callDyad f []) r) (ea' ++ ea)) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing atop f r
  , conjOnFunctionFunction = Nothing }
atDepth = PrimitiveConjunction
  { conjRepr = [G.atDepth]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Just $ \_ f d -> pure $ DerivedFunctionFunctionNoun (FunctionCalls (Just $ const $ F.atDepth1' (callMonad f []) d) (Just $ const $ F.atDepth2' (callDyad f []) d) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing over f d
  , conjOnFunctionFunction = Nothing }
repeat = PrimitiveConjunction
  { conjRepr = [G.repeat]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Just $ \_ f y -> pure $ DerivedFunctionFunctionNoun (FunctionCalls (Just $ const $ F.repeat1 (callMonad f []) (callUn f []) y) (Just $ const $ F.repeat2 (callDyad f []) (callAnti f []) y) (Just $ const $ F.repeat1 (callUn f []) (callMonad f []) y) (Just $ const $ F.repeat2 (callAnti f []) (callDyad f []) y) (Just $ const $ F.commute $ F.repeat2 (F.commute $ callContra f []) (F.commute $ callDyad f []) y) Nothing Nothing Nothing Nothing Nothing) Nothing TinyAPL.Primitives.repeat f y
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (FunctionCalls (Just $ const $ F.until1 (callMonad f []) (callDyad g [])) (Just $ const $ F.until2 (callDyad f []) (callDyad g [])) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing TinyAPL.Primitives.repeat f g }
valences = PrimitiveConjunction
  { conjRepr = [G.valences]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Nothing
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (FunctionCalls (Just $ callMonad f) (Just $ callDyad g) (Just $ callUn f) (Just $ callAnti g) (Just $ callContra g) (Just $ callDis g) (Just $ callBi g) (Just $ callAna g) Nothing Nothing) Nothing valences f g }
under = PrimitiveConjunction
  { conjRepr = [G.under]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Just $ \_ x g -> pure $ DerivedFunctionNounFunction (FunctionCalls (Just $ const $ F.underK x (callMonad g [])) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing under x g
  , conjOnFunctionNoun = Nothing
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (FunctionCalls (Just $ const $ F.under (callMonad f []) (callMonad g [])) (Just $ const $ F.under2 (callDyad f []) (callMonad g [])) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing under f g }
innerProduct = PrimitiveConjunction
  { conjRepr = [G.innerProduct]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Nothing
  , conjOnFunctionFunction = Just $ \ea f g -> pure $ DerivedFunctionFunctionFunction (FunctionCalls (Just $ \ea' -> withCoreExtraArgs1 (rollR F.alternant (callMonad f []) (callDyad g [])) (ea' ++ ea)) (Just $ const $ F.innerProduct (callMonad f []) (callDyad g [])) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing innerProduct f g }
lev = PrimitiveConjunction
  { conjRepr = [G.lev]
  , conjContext = Nothing
  , conjOnNounNoun = Just $ \_ x y -> pure $ DerivedFunctionNounNoun (FunctionCalls (Just $ const $ F.constant1 x) (Just $ const $ F.constant2 x) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing lev x y
  , conjOnNounFunction = Just $ \_ x g -> pure $ DerivedFunctionNounFunction (FunctionCalls (Just $ const $ F.constant1 x) (Just $ const $ F.constant2 x) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing lev x g
  , conjOnFunctionNoun = Just $ \_ f y -> pure $ DerivedFunctionFunctionNoun (FunctionCalls (Just $ callMonad f) (Just $ callDyad f) (Just $ callUn f) (Just $ callAnti f) (Just $ callContra f) (Just $ callDis f) (Just $ callBi f) (Just $ callAna f) (Just $ callUnderForward f) (Just $ callUnderBack f)) Nothing lev f y
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (FunctionCalls (Just $ callMonad f) (Just $ callDyad f) (Just $ callUn f) (Just $ callAnti f) (Just $ callContra f) (Just $ callDis f) (Just $ callBi f) (Just $ callAna f) (Just $ callUnderForward f) (Just $ callUnderBack f)) Nothing lev f g }
dex = PrimitiveConjunction
  { conjRepr = [G.dex]
  , conjContext = Nothing
  , conjOnNounNoun = Just $ \_ x y -> pure $ DerivedFunctionNounNoun (FunctionCalls (Just $ const $ F.constant1 y) (Just $ const $ F.constant2 y) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing dex x y
  , conjOnNounFunction = Just $ \_ x g -> pure $ DerivedFunctionNounFunction (FunctionCalls (Just $ callMonad g) (Just $ callDyad g) (Just $ callUn g) (Just $ callAnti g) (Just $ callContra g) (Just $ callDis g) (Just $ callBi g) (Just $ callAna g) (Just $ callUnderForward g) (Just $ callUnderBack g)) Nothing dex x g
  , conjOnFunctionNoun = Just $ \_ f y -> pure $ DerivedFunctionFunctionNoun (FunctionCalls (Just $ const $ F.constant1 y) (Just $ const $ F.constant2 y) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing dex f y
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (FunctionCalls (Just $ callMonad g) (Just $ callDyad g) (Just $ callUn g) (Just $ callAnti g) (Just $ callContra g) (Just $ callDis g) (Just $ callBi g) (Just $ callAna g) (Just $ callUnderForward g) (Just $ callUnderBack g)) Nothing dex f g }
forkA = PrimitiveConjunction
  { conjRepr = [G.forkA]
  , conjContext = Nothing
  , conjOnNounNoun = Just $ \_ x y -> pure $ DerivedFunctionNounNoun (FunctionCalls (Just $ \_ _ -> message) (Just $ \_ _ _ -> message) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing forkA x y
  , conjOnNounFunction = Just $ \_ x g -> pure $ DerivedFunctionNounFunction (FunctionCalls (Just $ \_ _ -> message) (Just $ \_ _ _ -> message) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing forkA x g
  , conjOnFunctionNoun = Just $ \_ f y -> pure $ DerivedFunctionFunctionNoun (FunctionCalls (Just $ \_ _ -> message) (Just $ \_ _ _ -> message) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing forkA f y
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (FunctionCalls (Just $ \_ _ -> message) (Just $ \_ _ _ -> message) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing forkA f g }
  where message = throwError $ DomainError $ [G.forkA] ++ " must be used in conjunction with " ++ [G.forkB]
forkB = PrimitiveConjunction
  { conjRepr = [G.forkB]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Just $ \_ left z -> case left of
    DerivedFunctionNounNoun _ _ op x y | op == forkA -> pure $ DerivedFunctionFunctionNoun (FunctionCalls (Just $ const $ F.fork1 (F.constant1 x) (F.constant2 y) (F.constant1 z)) (Just $ const $ F.fork2 (F.constant2 x) (F.constant2 y) (F.constant2 z)) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing forkB left z
    DerivedFunctionNounFunction _ _ op x g | op == forkA -> pure $ DerivedFunctionFunctionNoun (FunctionCalls (Just $ const $ F.fork1 (F.constant1 x) (callDyad g []) (F.constant1 z)) (Just $ const $ F.fork2 (F.constant2 x) (callDyad g []) (F.constant2 z)) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing forkB left z
    DerivedFunctionFunctionNoun _ _ op f y | op == forkA -> pure $ DerivedFunctionFunctionNoun (FunctionCalls (Just $ const $ F.fork1 (callMonad f []) (F.constant2 y) (F.constant1 z)) (Just $ const $ F.fork2 (callDyad f []) (F.constant2 y) (F.constant2 z)) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing forkB left z
    DerivedFunctionFunctionFunction _ _ op f g | op == forkA -> pure $ DerivedFunctionFunctionNoun (FunctionCalls (Just $ const $ F.fork1 (callMonad f []) (callDyad g []) (F.constant1 z)) (Just $ const $ F.fork2 (callDyad f []) (callDyad g []) (F.constant2 z)) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing forkB left z
    _ -> message
  , conjOnFunctionFunction = Just $ \_ left h -> case left of
    DerivedFunctionNounNoun _ _ op x y | op == forkA -> pure $ DerivedFunctionFunctionFunction (FunctionCalls (Just $ const $ F.fork1 (F.constant1 x) (F.constant2 y) (callMonad h [])) (Just $ const $ F.fork2 (F.constant2 x) (F.constant2 y) (callDyad h [])) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing forkB left h
    DerivedFunctionNounFunction _ _ op x g | op == forkA -> pure $ DerivedFunctionFunctionFunction (FunctionCalls (Just $ const $ F.fork1 (F.constant1 x) (callDyad g []) (callMonad h [])) (Just $ const $ F.fork2 (F.constant2 x) (callDyad g []) (callDyad h [])) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing forkB left h
    DerivedFunctionFunctionNoun _ _ op f y | op == forkA -> pure $ DerivedFunctionFunctionFunction (FunctionCalls (Just $ const $  F.fork1 (callMonad f []) (F.constant2 y) (callMonad h [])) (Just $ const $ F.fork2 (callDyad f []) (F.constant2 y) (callDyad h [])) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing forkB left h
    DerivedFunctionFunctionFunction _ _ op f g | op == forkA -> pure $ DerivedFunctionFunctionFunction (FunctionCalls (Just $ const $ F.fork1 (callMonad f []) (callDyad g []) (callMonad h [])) (Just $ const $ F.fork2 (callDyad f []) (callDyad g []) (callDyad h [])) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing forkB left h
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
    pure $ DerivedFunctionFunctionNoun (FunctionCalls (Just $ \ea' -> callMonad f $ (box $ vector $ Character <$> coreExtraArgsToleranceKey, Number $ tolerance :+ 0) : ea') (Just $ \ea' -> callDyad f $ (box $ vector $ Character <$> coreExtraArgsToleranceKey, Number $ tolerance :+ 0) : ea') (Just $ \ea' -> callUn f $ (box $ vector $ Character <$> coreExtraArgsToleranceKey, Number $ tolerance :+ 0) : ea') (Just $ \ea' -> callAnti f $ (box $ vector $ Character <$> coreExtraArgsToleranceKey, Number $ tolerance :+ 0) : ea') (Just $ \ea' -> callContra f $ (box $ vector $ Character <$> coreExtraArgsToleranceKey, Number $ tolerance :+ 0) : ea') (Just $ \ea' -> callDis f $ (box $ vector $ Character <$> coreExtraArgsToleranceKey, Number $ tolerance :+ 0) : ea') (Just $ \ea' -> callBi f $ (box $ vector $ Character <$> coreExtraArgsToleranceKey, Number $ tolerance :+ 0) : ea') (Just $ \ea' -> callAna f $ (box $ vector $ Character <$> coreExtraArgsToleranceKey, Number $ tolerance :+ 0) : ea') Nothing Nothing) Nothing approximate f v
  , conjOnFunctionFunction = Just $ \_ f g -> do
    let err = DomainError $ "Approximate right operand must be a scalar real or function returning a scalar real"
    pure $ DerivedFunctionFunctionFunction (FunctionCalls (Just $ \ea' y -> do
      tolerance <- callMonad g [] y >>= asScalar err >>= asNumber err >>= asReal err
      callMonad f ((box $ vector $ Character <$> coreExtraArgsToleranceKey, Number $ tolerance :+ 0) : ea') y) (Just $ \ea' x y -> do
      tolerance <- callDyad g [] x y >>= asScalar err >>= asNumber err >>= asReal err
      callDyad f ((box $ vector $ Character <$> coreExtraArgsToleranceKey, Number $ tolerance :+ 0) : ea') x y) (Just $ \ea' y -> do
      tolerance <- callMonad g [] y >>= asScalar err >>= asNumber err >>= asReal err
      callUn f ((box $ vector $ Character <$> coreExtraArgsToleranceKey, Number $ tolerance :+ 0) : ea') y) (Just $ \ea' x y -> do
      tolerance <- callDyad g [] x y >>= asScalar err >>= asNumber err >>= asReal err
      callAnti f ((box $ vector $ Character <$> coreExtraArgsToleranceKey, Number $ tolerance :+ 0) : ea') x y) (Just $ \ea' x y -> do
      tolerance <- callDyad g [] x y >>= asScalar err >>= asNumber err >>= asReal err
      callContra f ((box $ vector $ Character <$> coreExtraArgsToleranceKey, Number $ tolerance :+ 0) : ea') x y) (Just $ \ea' y -> do
      tolerance <- callMonad g [] y >>= asScalar err >>= asNumber err >>= asReal err
      callDis f ((box $ vector $ Character <$> coreExtraArgsToleranceKey, Number $ tolerance :+ 0) : ea') y) (Just $ \ea' y -> do
      tolerance <- callMonad g [] y >>= asScalar err >>= asNumber err >>= asReal err
      callBi f ((box $ vector $ Character <$> coreExtraArgsToleranceKey, Number $ tolerance :+ 0) : ea') y) (Just $ \ea' x y -> do
      tolerance <- callDyad g [] x y >>= asScalar err >>= asNumber err >>= asReal err
      callAna f ((box $ vector $ Character <$> coreExtraArgsToleranceKey, Number $ tolerance :+ 0) : ea') x y) Nothing Nothing) Nothing approximate f g }
fill = PrimitiveConjunction
  { conjRepr = [G.fill]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Just $ \_ f v -> do
    let fl = toScalar v
    pure $ DerivedFunctionFunctionNoun (FunctionCalls (Just $ \ea' -> callMonad f $ (box $ vector $ Character <$> coreExtraArgsFillKey, fl) : ea') (Just $ \ea' -> callDyad f $ (box $ vector $ Character <$> coreExtraArgsFillKey, fl) : ea') (Just $ \ea' -> callUn f $ (box $ vector $ Character <$> coreExtraArgsFillKey, fl) : ea') (Just $ \ea' -> callAnti f $ (box $ vector $ Character <$> coreExtraArgsFillKey, fl) : ea') (Just $ \ea' -> callContra f $ (box $ vector $ Character <$> coreExtraArgsFillKey, fl) : ea') (Just $ \ea' -> callDis f $ (box $ vector $ Character <$> coreExtraArgsFillKey, fl) : ea') (Just $ \ea' -> callBi f $ (box $ vector $ Character <$> coreExtraArgsFillKey, fl) : ea') (Just $ \ea' -> callAna f $ (box $ vector $ Character <$> coreExtraArgsFillKey, fl) : ea') Nothing Nothing) Nothing fill f v
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (FunctionCalls (Just $ \ea' y -> do
      fl <- toScalar <$> callMonad g [] y
      callMonad f ((box $ vector $ Character <$> coreExtraArgsFillKey, fl) : ea') y) (Just $ \ea' x y -> do
      fl <- toScalar <$> callDyad g [] x y
      callDyad f ((box $ vector $ Character <$> coreExtraArgsFillKey, fl) : ea') x y) (Just $ \ea' y -> do
      fl <- toScalar <$> callMonad g [] y
      callUn f ((box $ vector $ Character <$> coreExtraArgsFillKey, fl) : ea') y) (Just $ \ea' x y -> do
      fl <- toScalar <$> callDyad g [] x y
      callAnti f ((box $ vector $ Character <$> coreExtraArgsFillKey, fl) : ea') x y) (Just $ \ea' x y -> do
      fl <- toScalar <$> callDyad g [] x y
      callContra f ((box $ vector $ Character <$> coreExtraArgsFillKey, fl) : ea') x y) (Just $ \ea' y -> do
      fl <- toScalar <$> callMonad g [] y
      callDis f ((box $ vector $ Character <$> coreExtraArgsFillKey, fl) : ea') y) (Just $ \ea' y -> do
      fl <- toScalar <$> callMonad g [] y
      callBi f ((box $ vector $ Character <$> coreExtraArgsFillKey, fl) : ea') y) (Just $ \ea' x y -> do
      fl <- toScalar <$> callDyad g [] x y
      callAna f ((box $ vector $ Character <$> coreExtraArgsFillKey, fl) : ea') x y) Nothing Nothing) Nothing fill f g }
catch = PrimitiveConjunction
  { conjRepr = [G.catch]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Just $ \_ f v -> pure $ DerivedFunctionFunctionNoun (FunctionCalls (Just $ const $ F.catch1 (callMonad f []) (F.constant1 v)) (Just $ const $ F.catch2 (callDyad f []) (F.constant2 v)) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing catch f v
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (FunctionCalls (Just $ const $ F.catch1 (callMonad f []) (callMonad g [])) (Just $ const $ F.catch2 (callDyad f []) (callDyad g [])) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing catch f g }
at = PrimitiveConjunction
  { conjRepr = [G.at]
  , conjContext = Nothing
  , conjOnNounNoun = Just $ \_ u v -> pure $ DerivedFunctionNounNoun (FunctionCalls (Just $ const $ F.atArr u v) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing at u v
  , conjOnNounFunction = Nothing
  , conjOnFunctionNoun = Just $ \_ f v -> pure $ DerivedFunctionFunctionNoun (FunctionCalls (Just $ const $ F.at (callMonad f []) v) (Just $ const $ \x -> F.at (callDyad f [] x) v) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing at f v
  , conjOnFunctionFunction = Nothing }
contextualUnder = PrimitiveConjunction
  { conjRepr = [G.contextualUnder]
  , conjContext = Nothing
  , conjOnNounNoun = Nothing
  , conjOnNounFunction = Just $ \_ x g -> pure $ DerivedFunctionNounFunction (FunctionCalls (Just $ const $ F.contextualUnderK (callUnderForward g []) x (callUnderBack g [])) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing contextualUnder x g
  , conjOnFunctionNoun = Nothing
  , conjOnFunctionFunction = Just $ \_ f g -> pure $ DerivedFunctionFunctionFunction (FunctionCalls (Just $ const $ F.contextualUnder (callUnderForward g []) (callMonad f []) (callUnderBack g [])) (Just $ const $ F.contextualUnder2 (callUnderForward g []) (callDyad f []) (callUnderBack g [])) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) Nothing contextualUnder f g }

conjunctions :: [(String, Conjunction)]
conjunctions = (\x -> (conjRepr x, x)) <$>
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
  , TinyAPL.Primitives.fill
  , TinyAPL.Primitives.catch
  , TinyAPL.Primitives.at
  , TinyAPL.Primitives.contextualUnder ]

primitives :: Primitives
primitives = (arrays, functions, adverbs, conjunctions)
