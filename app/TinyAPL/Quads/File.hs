module TinyAPL.Quads.File where

import TinyAPL.ArrayFunctionOperator
import TinyAPL.Error
import qualified TinyAPL.Files as F
import qualified TinyAPL.Glyphs as G

read' :: Noun -> St Noun
read' s = do
  let err = DomainError "read: argument must be a string path"
  path <- asString err s
  vector . fmap Character <$> F.read path

write' :: Noun -> Noun -> St Noun
write' s d = do
  let err = DomainError "write: arguments must be a string path and string data"
  path <- asString err s
  da <- asString err d
  F.write path da
  pure $ vector []

file :: Nilad
file = Nilad (Just $ do
  scope <- createRef $ Scope [] ((\n -> (functionRepr n, (VariableConstant, n))) <$>
    [ PrimitiveFunction (Just $ const read') Nothing "Read" Nothing
    , PrimitiveFunction Nothing (Just $ const write') "Write" Nothing]) [] [] Nothing
  ctx <- getContext
  pure $ scalar $ Struct $ ctx{ contextScope = scope }) Nothing (G.quad : "file") Nothing