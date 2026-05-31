{-# LANGUAGE NegativeLiterals, OverloadedLists #-}

module TinyAPL.ParserSpec where

import TinyAPL.Complex
import qualified TinyAPL.Glyphs as G
import TinyAPL.Parser
import TinyAPL.Util
import TinyAPL.Primitives (primitives)
import TinyAPL.Context
import TinyAPL.Pretty (defaultConfig)
import TinyAPL.CoreQuads (core)
import TinyAPL.Error

import Test.Hspec
import Data.List (singleton)
import Data.List.NonEmpty ()
import System.IO.Unsafe
import Data.IORef

pars :: ParsingInfo
pars = case primitives of (n, f, a, c) -> ParsingInfo n f a c Nothing

scope = unsafePerformIO $ newIORef $ Scope [] [] [] [] Nothing True
{-# NOINLINE scope #-}
idRef = unsafePerformIO $ newIORef (0 :: Integer)
{-# NOINLINE idRef #-}
prettyRef = unsafePerformIO $ newIORef defaultConfig
{-# NOINLINE prettyRef #-}
ctx = Context scope core undefined undefined undefined idRef "" pars prettyRef False

spec :: Spec
spec = do
  describe "tokenize" $ do
    let tok x = runResult $ fst <$> runSt (tokenize pars "<test>" x) ctx

    it "ignores comments" $ do
      tok "⟃abc⟄ 1" `shouldReturn` pure [[TokenNumber 1 emptyPos]]
      tok "2 ⍝ abc" `shouldReturn` pure [[TokenNumber 2 emptyPos]]
      tok "1 ⍝ abc\n10" `shouldReturn` pure [[TokenNumber 1 emptyPos, TokenNumber 10 emptyPos]]

    it "parses numbers" $ do
      tok "1" `shouldReturn` pure [[TokenNumber 1 emptyPos]]
      tok "¯2" `shouldReturn` pure [[TokenNumber -2 emptyPos]]
      tok "1.5" `shouldReturn` pure [[TokenNumber 1.5 emptyPos]]
      tok "¯3.25" `shouldReturn` pure [[TokenNumber -3.25 emptyPos]]
      tok "3⏨2" `shouldReturn` pure [[TokenNumber 300 emptyPos]]
      tok "2.4⏨¯3" `shouldReturn` pure [[TokenNumber 0.0024 emptyPos]]
      tok "3ᴊ2" `shouldReturn` pure [[TokenNumber (3 :+ 2) emptyPos]]
      tok "¯2ᴊ1.5⏨2" `shouldReturn` pure [[TokenNumber (-2 :+ 150) emptyPos]]
      tok "∞" `shouldReturn` pure [[TokenNumber (inf :+ 0) emptyPos]]
      tok "¯∞" `shouldReturn` pure [[TokenNumber (ninf :+ 0) emptyPos]]
      tok "0ᴊ∞" `shouldReturn` pure [[TokenNumber (0 :+ inf) emptyPos]]
    
    it "parses character vectors" $ do
      tok "'abc'" `shouldReturn` pure [[TokenChar "abc" emptyPos]]
      tok "''" `shouldReturn` pure [[TokenChar "" emptyPos]]
    
    it "parses strings" $ do
      tok "\"abc\"" `shouldReturn` pure [[TokenString "abc" emptyPos]]
      tok "\"\"" `shouldReturn` pure [[TokenString "" emptyPos]]
      tok "\"a⍘nb⍘\"c⍘⍘d⍘re⍘tf\"" `shouldReturn` pure [[TokenString "a\nb\"c⍘d\re\tf" emptyPos]]

    it "parses vector notation" $ do
      tok "⟨1⋄2⟩" `shouldReturn` pure [[TokenVector [[TokenNumber 1 emptyPos], [TokenNumber 2 emptyPos]] emptyPos]]
      tok "⟨⟩" `shouldReturn` pure [[TokenVector [] emptyPos]]

    it "parses high rank notation" $ do
      tok "[1⋄2]" `shouldReturn` pure [[TokenHighRank [[TokenNumber 1 emptyPos], [TokenNumber 2 emptyPos]] emptyPos]]
      tok "[]" `shouldReturn` pure [[TokenHighRank [] emptyPos]]

    it "parses primitives" $ do
      mapM_ (\n -> tok n `shouldReturn` pure [[TokenPrimArray n emptyPos]]) $ singleton <$> G.arrays
      mapM_ (\n -> tok n `shouldReturn` pure [[TokenPrimFunction n emptyPos]]) $ singleton <$> G.functions
      mapM_ (\n -> tok n `shouldReturn` pure [[TokenPrimAdverb n emptyPos]]) $ singleton <$> G.adverbs
      mapM_ (\n -> tok n `shouldReturn` pure [[TokenPrimConjunction n emptyPos]]) $ singleton <$> G.conjunctions

    it "parses array names" $ do
      tok "abc ∆x" `shouldReturn` pure [[TokenArrayName "abc" emptyPos, TokenArrayName "∆x" emptyPos]]
      tok "⍺ ⍺⍺ ⍵ ⍵⍵ ⎕ ⍞ ɛ" `shouldReturn` pure [[TokenArrayName "⍺" emptyPos, TokenArrayName "⍺⍺" emptyPos, TokenArrayName "⍵" emptyPos, TokenArrayName "⍵⍵" emptyPos, TokenArrayName "⎕" emptyPos, TokenArrayName "⍞" emptyPos, TokenArrayName "ɛ" emptyPos]]
      tok "⎕io" `shouldReturn` pure [[TokenArrayName "⎕io" emptyPos]]

    it "parses function names" $ do
      tok "Abc ⍙y" `shouldReturn` pure [[TokenFunctionName "Abc" emptyPos, TokenFunctionName "⍙y" emptyPos]]
      tok "⍶⍶ ⍹⍹ ∇" `shouldReturn` pure [[TokenFunctionName "⍶⍶" emptyPos, TokenFunctionName "⍹⍹" emptyPos, TokenFunctionName "∇" emptyPos]]
      tok "⎕C" `shouldReturn` pure [[TokenFunctionName "⎕C" emptyPos]]
      tok "⍞100" `shouldReturn` pure [[TokenFunctionName "⍞100" emptyPos]]
  
    it "parses adverb names" $ do
      tok "_Abc _abc" `shouldReturn` pure [[TokenAdverbName "_Abc" emptyPos, TokenAdverbName "_abc" emptyPos]]
      tok "_∇" `shouldReturn` pure [[TokenAdverbName "_∇" emptyPos]]
      tok "⎕_BinFile" `shouldReturn` pure [[TokenAdverbName "⎕_BinFile" emptyPos]]
      tok "⍞_100" `shouldReturn` pure [[TokenAdverbName "⍞_100" emptyPos]]

    it "parses conjunction names" $ do
      tok "_Abc_ _abc_" `shouldReturn` pure [[TokenConjunctionName "_Abc_" emptyPos, TokenConjunctionName "_abc_" emptyPos]]
      tok "_∇_" `shouldReturn` pure [[TokenConjunctionName "_∇_" emptyPos]]
      tok "⎕_Whatever_" `shouldReturn` pure [[TokenConjunctionName "⎕_Whatever_" emptyPos]]
      tok "⍞_100_" `shouldReturn` pure [[TokenConjunctionName "⍞_100_" emptyPos]]

    it "parses qualified names" $ do
      tok "a→b→c" `shouldReturn` pure [[TokenQualifiedArrayName (TokenArrayName "a" emptyPos) ["b", "c"] emptyPos]]
      tok "a→b→C" `shouldReturn` pure [[TokenQualifiedFunctionName (TokenArrayName "a" emptyPos) ["b", "C"] emptyPos]]
      tok "a→b→_C" `shouldReturn` pure [[TokenQualifiedAdverbName (TokenArrayName "a" emptyPos) ["b", "_C"] emptyPos]]
      tok "a→b→_C_" `shouldReturn` pure [[TokenQualifiedConjunctionName (TokenArrayName "a" emptyPos) ["b", "_C_"] emptyPos]]

    it "parses assignment" $ do
      tok "abc←3" `shouldReturn` pure [[TokenArrayAssign "abc" AssignNormal [TokenNumber 3 emptyPos] emptyPos]]
      tok "Abc←3" `shouldReturn` pure [[TokenFunctionAssign "Abc" AssignNormal [TokenNumber 3 emptyPos] emptyPos]]
      tok "_Abc←3" `shouldReturn` pure [[TokenAdverbAssign "_Abc" AssignNormal [TokenNumber 3 emptyPos] emptyPos]]
      tok "_Abc_←3" `shouldReturn` pure [[TokenConjunctionAssign "_Abc_" AssignNormal [TokenNumber 3 emptyPos] emptyPos]]
      tok "⎕seed←3" `shouldReturn` pure [[TokenArrayAssign "⎕seed" AssignNormal [TokenNumber 3 emptyPos] emptyPos]]
      tok "abc↩3" `shouldReturn` pure [[TokenArrayAssign "abc" AssignModify [TokenNumber 3 emptyPos] emptyPos]]
      tok "Abc↩3" `shouldReturn` pure [[TokenFunctionAssign "Abc" AssignModify [TokenNumber 3 emptyPos] emptyPos]]
      tok "_Abc↩3" `shouldReturn` pure [[TokenAdverbAssign "_Abc" AssignModify [TokenNumber 3 emptyPos] emptyPos]]
      tok "_Abc_↩3" `shouldReturn` pure [[TokenConjunctionAssign "_Abc_" AssignModify [TokenNumber 3 emptyPos] emptyPos]]
      tok "abc⇇3" `shouldReturn` pure [[TokenArrayAssign "abc" AssignConstant [TokenNumber 3 emptyPos] emptyPos]]
      tok "Abc⇇3" `shouldReturn` pure [[TokenFunctionAssign "Abc" AssignConstant [TokenNumber 3 emptyPos] emptyPos]]
      tok "_Abc⇇3" `shouldReturn` pure [[TokenAdverbAssign "_Abc" AssignConstant [TokenNumber 3 emptyPos] emptyPos]]
      tok "_Abc_⇇3" `shouldReturn` pure [[TokenConjunctionAssign "_Abc_" AssignConstant [TokenNumber 3 emptyPos] emptyPos]]
      tok "abc↚3" `shouldReturn` pure [[TokenArrayAssign "abc" AssignPrivate [TokenNumber 3 emptyPos] emptyPos]]
      tok "Abc↚3" `shouldReturn` pure [[TokenFunctionAssign "Abc" AssignPrivate [TokenNumber 3 emptyPos] emptyPos]]
      tok "_Abc↚3" `shouldReturn` pure [[TokenAdverbAssign "_Abc" AssignPrivate [TokenNumber 3 emptyPos] emptyPos]]
      tok "_Abc_↚3" `shouldReturn` pure [[TokenConjunctionAssign "_Abc_" AssignPrivate [TokenNumber 3 emptyPos] emptyPos]]

    it "parses qualified assignment" $ do
      tok "a→b→c←3" `shouldReturn` pure [[TokenQualifiedArrayAssign (TokenArrayName "a" emptyPos) ["b", "c"] AssignNormal [TokenNumber 3 emptyPos] emptyPos]]
      tok "a→b→c↩3" `shouldReturn` pure [[TokenQualifiedArrayAssign (TokenArrayName "a" emptyPos) ["b", "c"] AssignModify [TokenNumber 3 emptyPos] emptyPos]]
      tok "a→b→c⇇3" `shouldReturn` pure [[TokenQualifiedArrayAssign (TokenArrayName "a" emptyPos) ["b", "c"] AssignConstant [TokenNumber 3 emptyPos] emptyPos]]
      tok "a→b→C↚3" `shouldReturn` pure [[TokenQualifiedFunctionAssign (TokenArrayName "a" emptyPos) ["b", "C"] AssignPrivate [TokenNumber 3 emptyPos] emptyPos]]
      tok "a→b→C←3" `shouldReturn` pure [[TokenQualifiedFunctionAssign (TokenArrayName "a" emptyPos) ["b", "C"] AssignNormal [TokenNumber 3 emptyPos] emptyPos]]
      tok "a→b→C↩3" `shouldReturn` pure [[TokenQualifiedFunctionAssign (TokenArrayName "a" emptyPos) ["b", "C"] AssignModify [TokenNumber 3 emptyPos] emptyPos]]
      tok "a→b→C⇇3" `shouldReturn` pure [[TokenQualifiedFunctionAssign (TokenArrayName "a" emptyPos) ["b", "C"] AssignConstant [TokenNumber 3 emptyPos] emptyPos]]
      tok "a→b→_C↚3" `shouldReturn` pure [[TokenQualifiedAdverbAssign (TokenArrayName "a" emptyPos) ["b", "_C"] AssignPrivate [TokenNumber 3 emptyPos] emptyPos]]
      tok "a→b→_C←3" `shouldReturn` pure [[TokenQualifiedAdverbAssign (TokenArrayName "a" emptyPos) ["b", "_C"] AssignNormal [TokenNumber 3 emptyPos] emptyPos]]
      tok "a→b→_C↩3" `shouldReturn` pure [[TokenQualifiedAdverbAssign (TokenArrayName "a" emptyPos) ["b", "_C"] AssignModify [TokenNumber 3 emptyPos] emptyPos]]
      tok "a→b→_C⇇3" `shouldReturn` pure [[TokenQualifiedAdverbAssign (TokenArrayName "a" emptyPos) ["b", "_C"] AssignConstant [TokenNumber 3 emptyPos] emptyPos]]
      tok "a→b→_C_↚3" `shouldReturn` pure [[TokenQualifiedConjunctionAssign (TokenArrayName "a" emptyPos) ["b", "_C_"] AssignPrivate [TokenNumber 3 emptyPos] emptyPos]]
      tok "a→b→_C_←3" `shouldReturn` pure [[TokenQualifiedConjunctionAssign (TokenArrayName "a" emptyPos) ["b", "_C_"] AssignNormal [TokenNumber 3 emptyPos] emptyPos]]
      tok "a→b→_C_↩3" `shouldReturn` pure [[TokenQualifiedConjunctionAssign (TokenArrayName "a" emptyPos) ["b", "_C_"] AssignModify [TokenNumber 3 emptyPos] emptyPos]]
      tok "a→b→_C_⇇3" `shouldReturn` pure [[TokenQualifiedConjunctionAssign (TokenArrayName "a" emptyPos) ["b", "_C_"] AssignConstant [TokenNumber 3 emptyPos] emptyPos]]
      tok "a→b→_C_↚3" `shouldReturn` pure [[TokenQualifiedConjunctionAssign (TokenArrayName "a" emptyPos) ["b", "_C_"] AssignPrivate [TokenNumber 3 emptyPos] emptyPos]]

    it "parses dfns and dops" $ do
      tok "{3⋄1}" `shouldReturn` pure [[TokenDfn [[TokenNumber 3 emptyPos], [TokenNumber 1 emptyPos]] emptyPos]]
      tok "_{3⋄1}" `shouldReturn` pure [[TokenDadv [[TokenNumber 3 emptyPos], [TokenNumber 1 emptyPos]] emptyPos]]
      tok "_{3⋄1}_" `shouldReturn` pure [[TokenDconj [[TokenNumber 3 emptyPos], [TokenNumber 1 emptyPos]] emptyPos]]

    it "parses wraps" $ do
      tok "⊏+" `shouldReturn` pure [[TokenWrap (TokenPrimFunction "+" emptyPos) emptyPos]]

    it "parses unwraps" $ do
      tok "⊐3" `shouldReturn` pure [[TokenUnwrap (TokenNumber 3 emptyPos) emptyPos]]
      tok "_⊐3" `shouldReturn` pure [[TokenUnwrapAdverb (TokenNumber 3 emptyPos) emptyPos]]
      tok "_⊐_3" `shouldReturn` pure [[TokenUnwrapConjunction (TokenNumber 3 emptyPos) emptyPos]]
    
    it "parses guards" $ do
      tok "{1:2}" `shouldReturn` pure [[TokenDfn [[TokenGuard [TokenNumber 1 emptyPos] [TokenNumber 2 emptyPos] emptyPos]] emptyPos]]

    it "parses exit statements" $ do
      tok "{■5}" `shouldReturn` pure [[TokenDfn [[TokenExit [TokenNumber 5 emptyPos] emptyPos]] emptyPos]]

    it "parses separator-separated statements" $ do
      tok "1⋄2" `shouldReturn` pure [[TokenNumber 1 emptyPos], [TokenNumber 2 emptyPos]]

    it "ignores spaces and newlines" $ do
      tok "1\n2" `shouldReturn` pure [[TokenNumber 1 emptyPos, TokenNumber 2 emptyPos]]
      tok "1     2" `shouldReturn` pure [[TokenNumber 1 emptyPos, TokenNumber 2 emptyPos]]

    it "treats multiple newlines as a separator" $ do
      tok "1\n\n2" `shouldReturn` pure [[TokenNumber 1 emptyPos], [TokenNumber 2 emptyPos]]
      tok "1\n\n\n2" `shouldReturn` pure [[TokenNumber 1 emptyPos], [TokenNumber 2 emptyPos]]
      tok "1\n\n\n\n2" `shouldReturn` pure [[TokenNumber 1 emptyPos], [TokenNumber 2 emptyPos]]
    
    it "parses parens" $ do
      tok "(1 2)" `shouldReturn` pure [[TokenParens [TokenNumber 1 emptyPos, TokenNumber 2 emptyPos] emptyPos]]

    it "parses trains and modifier trains" $ do
      tok "⦅1⋄2⦆" `shouldReturn` pure [[TokenTrain [[TokenNumber 1 emptyPos], [TokenNumber 2 emptyPos]] emptyPos]]
      tok "⦅1⋄2⋄3⦆" `shouldReturn` pure [[TokenTrain [[TokenNumber 1 emptyPos], [TokenNumber 2 emptyPos], [TokenNumber 3 emptyPos]] emptyPos]]
      tok "⦅1⋄⋄2⋄3⦆" `shouldReturn` pure [[TokenTrain [[TokenNumber 1 emptyPos], [], [TokenNumber 2 emptyPos], [TokenNumber 3 emptyPos]] emptyPos]]
      tok "_⦅1⋄2⋄3⦆" `shouldReturn` pure [[TokenAdverbTrain [[TokenNumber 1 emptyPos], [TokenNumber 2 emptyPos], [TokenNumber 3 emptyPos]] emptyPos]]
      tok "_⦅1⋄2⋄3⦆_" `shouldReturn` pure [[TokenConjunctionTrain [[TokenNumber 1 emptyPos], [TokenNumber 2 emptyPos], [TokenNumber 3 emptyPos]] emptyPos]]

    it "parses destructuring assignment" $ do
      tok "⟨a⋄b⟩←9" `shouldReturn` pure [[TokenVectorAssign ["a", "b"] AssignNormal [TokenNumber 9 emptyPos] emptyPos]]
      tok "⟨a⋄b⟩↩9" `shouldReturn` pure [[TokenVectorAssign ["a", "b"] AssignModify [TokenNumber 9 emptyPos] emptyPos]]
      tok "⟨a⋄b⟩⇇9" `shouldReturn` pure [[TokenVectorAssign ["a", "b"] AssignConstant [TokenNumber 9 emptyPos] emptyPos]]
      tok "⟨a⋄b⟩↚9" `shouldReturn` pure [[TokenVectorAssign ["a", "b"] AssignPrivate [TokenNumber 9 emptyPos] emptyPos]]
      tok "[a⋄b]←7" `shouldReturn` pure [[TokenHighRankAssign ["a", "b"] AssignNormal [TokenNumber 7 emptyPos] emptyPos]]
      tok "[a⋄b]↩7" `shouldReturn` pure [[TokenHighRankAssign ["a", "b"] AssignModify [TokenNumber 7 emptyPos] emptyPos]]
      tok "[a⋄b]⇇7" `shouldReturn` pure [[TokenHighRankAssign ["a", "b"] AssignConstant [TokenNumber 7 emptyPos] emptyPos]]
      tok "[a⋄b]↚7" `shouldReturn` pure [[TokenHighRankAssign ["a", "b"] AssignPrivate [TokenNumber 7 emptyPos] emptyPos]]
      tok "a‿b←11" `shouldReturn` pure [[TokenTieAssign ["a", "b"] AssignNormal [TokenNumber 11 emptyPos] emptyPos]]
      tok "a‿b↩11" `shouldReturn` pure [[TokenTieAssign ["a", "b"] AssignModify [TokenNumber 11 emptyPos] emptyPos]]
      tok "a‿b⇇11" `shouldReturn` pure [[TokenTieAssign ["a", "b"] AssignConstant [TokenNumber 11 emptyPos] emptyPos]]
      tok "a‿b↚11" `shouldReturn` pure [[TokenTieAssign ["a", "b"] AssignPrivate [TokenNumber 11 emptyPos] emptyPos]]

    it "parses struct assignment" $ do
      tok "⦃a⦄←3" `shouldReturn` pure [[TokenStructAssign [("a", Nothing)] AssignNormal [TokenNumber 3 emptyPos] emptyPos]]
      tok "⦃a←b⦄←3" `shouldReturn` pure [[TokenStructAssign [("a", Just (AssignNormal, "b"))] AssignNormal [TokenNumber 3 emptyPos] emptyPos]]
      tok "⦃a↩b⦄⇇3" `shouldReturn` pure [[TokenStructAssign [("a", Just (AssignModify, "b"))] AssignConstant [TokenNumber 3 emptyPos] emptyPos]]
      tok "⦃a⋄b←c⦄↚3" `shouldReturn` pure [[TokenStructAssign [("a", Nothing), ("b", Just (AssignNormal, "c"))] AssignPrivate [TokenNumber 3 emptyPos] emptyPos]]

    it "parses structs" $ do
      tok "⦃1⋄2⋄3⦄" `shouldReturn` pure [[TokenStruct [[TokenNumber 1 emptyPos], [TokenNumber 2 emptyPos], [TokenNumber 3 emptyPos]] emptyPos]]

    it "parses ties" $ do
      tok "1‿2‿3" `shouldReturn` pure [[TokenTie [TokenNumber 1 emptyPos, TokenNumber 2 emptyPos, TokenNumber 3 emptyPos] emptyPos]]
      tok "+‿-‿×" `shouldReturn` pure [[TokenTie [TokenPrimFunction "+" emptyPos, TokenPrimFunction "-" emptyPos, TokenPrimFunction "×" emptyPos] emptyPos]]

    it "parses ternaries" $ do
      tok "1⍰2⍠3" `shouldReturn` pure [[TokenTernary [TokenNumber 1 emptyPos] [TokenNumber 2 emptyPos] [TokenNumber 3 emptyPos] emptyPos]]

    it "parses extra arguments" $ do
      tok "⦋⦌" `shouldReturn` pure [[TokenExtraArgs [] emptyPos]]
      tok "⦋1:2⦌" `shouldReturn` pure [[TokenExtraArgs [([TokenNumber 1 emptyPos], [TokenNumber 2 emptyPos])] emptyPos]]
      tok "⦋1:2⋄3:4⦌" `shouldReturn` pure [[TokenExtraArgs [([TokenNumber 1 emptyPos], [TokenNumber 2 emptyPos]), ([TokenNumber 3 emptyPos], [TokenNumber 4 emptyPos])] emptyPos]]
      tok "⦋1:2⋄3:4⋄5:6⦌" `shouldReturn` pure [[TokenExtraArgs [([TokenNumber 1 emptyPos], [TokenNumber 2 emptyPos]), ([TokenNumber 3 emptyPos], [TokenNumber 4 emptyPos]), ([TokenNumber 5 emptyPos], [TokenNumber 6 emptyPos])] emptyPos]]
      tok "⦋hi⦌" `shouldReturn` pure [[TokenSpreadExtraArgs [TokenArrayName "hi" emptyPos] emptyPos]]
    
    it "parses nothing" $ do
      tok "·" `shouldReturn` pure [[TokenNothing emptyPos]]

  describe "binder" $ do
    let e2m (Right x) = Just x
        e2m (Left _)  = Nothing
    let par x = fmap e2m $ runResult $ fst <$> runSt (parse pars "<test>" x) ctx
    
    it "parses leaves" $ do
      par "1" `shouldReturn` pure [Just $ Leaf CatArray (TokenNumber 1 emptyPos)]
      par "'abc'" `shouldReturn` pure [Just $ Leaf CatArray (TokenChar "abc" emptyPos)]
      par "\"abc\"" `shouldReturn` pure [Just $ Leaf CatArray (TokenString "abc" emptyPos)]
      par "⍬" `shouldReturn` pure [Just $ Leaf CatArray (TokenPrimArray "⍬" emptyPos)]
      par "+" `shouldReturn` pure [Just $ Leaf CatFunction (TokenPrimFunction "+" emptyPos)]
      par "⍨" `shouldReturn` pure [Just $ Leaf CatAdverb (TokenPrimAdverb "⍨" emptyPos)]
      par "∘" `shouldReturn` pure [Just $ Leaf CatConjunction (TokenPrimConjunction "∘" emptyPos)]
      par "abc" `shouldReturn` pure [Just $ Leaf CatArray (TokenArrayName "abc" emptyPos)]
      par "Abc" `shouldReturn` pure [Just $ Leaf CatFunction (TokenFunctionName "Abc" emptyPos)]
      par "_Abc" `shouldReturn` pure [Just $ Leaf CatAdverb (TokenAdverbName "_Abc" emptyPos)]
      par "_Abc_" `shouldReturn` pure [Just $ Leaf CatConjunction (TokenConjunctionName "_Abc_" emptyPos)]
    
    it "parses parens" $ do
      par "(1)" `shouldReturn` pure [Just $ Leaf CatArray (TokenNumber 1 emptyPos)]

    describe "application" $ do
      it "parses monad application" $ do
        par "+1" `shouldReturn` pure [Just $ MonadCallBranch (Leaf CatFunction (TokenPrimFunction "+" emptyPos)) (Leaf CatArray (TokenNumber 1 emptyPos))]
      
      it "parses dyad application" $ do
        par "1+" `shouldReturn` pure [Just $ DyadCallBranch (Leaf CatArray (TokenNumber 1 emptyPos)) (Leaf CatFunction (TokenPrimFunction "+" emptyPos))]
        par "1+2" `shouldReturn` pure [Just $ MonadCallBranch (DyadCallBranch (Leaf CatArray (TokenNumber 1 emptyPos)) (Leaf CatFunction (TokenPrimFunction "+" emptyPos))) (Leaf CatArray (TokenNumber 2 emptyPos))]

      it "parses adverb application" $ do
        par "1⍨" `shouldReturn` pure [Just $ AdverbCallBranch (Leaf CatArray (TokenNumber 1 emptyPos)) (Leaf CatAdverb (TokenPrimAdverb "⍨" emptyPos))]
        par "+⍨" `shouldReturn` pure [Just $ AdverbCallBranch (Leaf CatFunction (TokenPrimFunction "+" emptyPos)) (Leaf CatAdverb (TokenPrimAdverb "⍨" emptyPos))]

      it "parses conjunction application" $ do
        par "∘1" `shouldReturn` pure [Just $ ConjunctionCallBranch (Leaf CatConjunction (TokenPrimConjunction "∘" emptyPos)) (Leaf CatArray (TokenNumber 1 emptyPos))]
        par "∘+" `shouldReturn` pure [Just $ ConjunctionCallBranch (Leaf CatConjunction (TokenPrimConjunction "∘" emptyPos)) (Leaf CatFunction (TokenPrimFunction "+" emptyPos))]

      it "parses extra arguments application" $ do
        par "+⦋⦌" `shouldReturn` pure [Just $ ExtraArgsBranch CatFunction (Leaf CatFunction (TokenPrimFunction "+" emptyPos)) (UnboundExtraArgsBranch (DictionaryBranch []))]
        par "⍨⦋⦌" `shouldReturn` pure [Just $ ExtraArgsBranch CatAdverb (Leaf CatAdverb (TokenPrimAdverb "⍨" emptyPos)) (UnboundExtraArgsBranch (DictionaryBranch []))]
        par "∘⦋⦌" `shouldReturn` pure [Just $ ExtraArgsBranch CatConjunction (Leaf CatConjunction (TokenPrimConjunction "∘" emptyPos)) (UnboundExtraArgsBranch (DictionaryBranch []))]

    describe "dfns" $ do
      it "parses dfns and dops" $ do
        par "{1}" `shouldReturn` pure [Just $ DefinedBranch CatFunction [Leaf CatArray (TokenNumber 1 emptyPos)]]
        par "_{1}" `shouldReturn` pure [Just $ DefinedBranch CatAdverb [Leaf CatArray (TokenNumber 1 emptyPos)]]
        par "_{1}_" `shouldReturn` pure [Just $ DefinedBranch CatConjunction [Leaf CatArray (TokenNumber 1 emptyPos)]]
      
      it "requires the last statement to be an array" $ do
        par "{+}" `shouldReturn` Nothing
    
    describe "assignment" $ do
      it "parses assignment to variables of the correct type" $ do
        par "a←b" `shouldReturn` pure [Just $ AssignBranch CatArray "a" AssignNormal (Leaf CatArray (TokenArrayName "b" emptyPos))]
        par "A←B" `shouldReturn` pure [Just $ AssignBranch CatFunction "A" AssignNormal (Leaf CatFunction (TokenFunctionName "B" emptyPos))]
        par "_A←_B" `shouldReturn` pure [Just $ AssignBranch CatAdverb "_A" AssignNormal (Leaf CatAdverb (TokenAdverbName "_B" emptyPos))]
        par "_A_←_B_" `shouldReturn` pure [Just $ AssignBranch CatConjunction "_A_" AssignNormal (Leaf CatConjunction (TokenConjunctionName "_B_" emptyPos))]

      it "fails on assignment to variables of the wrong type" $ do
        par "a←B" `shouldReturn` Nothing
        par "A←_B_" `shouldReturn` Nothing
        par "_A←b" `shouldReturn` Nothing
        par "_A_←_B" `shouldReturn` Nothing
    
    describe "guards" $ do
      it "parses guards with array conditions" $ do
        par "{a:b}" `shouldReturn` pure [Just $ DefinedBranch CatFunction [GuardBranch (Leaf CatArray (TokenArrayName "a" emptyPos)) (Leaf CatArray (TokenArrayName "b" emptyPos))]]
      
      it "fails on guards with non-array conditions" $ do
        par "{A:b}" `shouldReturn` Nothing

    describe "exit statements" $ do
      it "parses exit statements with array results" $ do
        par "{■3}" `shouldReturn` pure [Just $ DefinedBranch CatFunction [ExitBranch (Leaf CatArray (TokenNumber 3 emptyPos))]]

      it "fails on exit statements with non-array results" $ do
        par "{■+}" `shouldReturn` Nothing

    describe "vector notation" $ do
      it "parses arrays with any contents" $ do
        par "⟨1⋄2⟩" `shouldReturn` pure [Just $ VectorBranch [Leaf CatArray (TokenNumber 1 emptyPos), Leaf CatArray (TokenNumber 2 emptyPos)]]
        par "⟨+⟩" `shouldReturn` pure [Just $ VectorBranch [Leaf CatFunction (TokenPrimFunction "+" emptyPos)]]
        par "⟨⍨⟩" `shouldReturn` pure [Just $ VectorBranch [Leaf CatAdverb (TokenPrimAdverb "⍨" emptyPos)]]
        par "⟨⍥⟩" `shouldReturn` pure [Just $ VectorBranch [Leaf CatConjunction (TokenPrimConjunction "⍥" emptyPos)]]
        par "⟨⟩" `shouldReturn` pure [Just $ VectorBranch []]

    describe "high rank notation" $ do
      it "parses arrays with array contents" $ do
        par "[1⋄2]" `shouldReturn` pure [Just $ HighRankBranch [Leaf CatArray (TokenNumber 1 emptyPos), Leaf CatArray (TokenNumber 2 emptyPos)]]
        par "[]" `shouldReturn` pure [Just $ HighRankBranch []]
      
      it "fails on arrays with non-array contents" $ do
        par "[+]" `shouldReturn` Nothing

    describe "wraps" $ do
      it "parses wraps of functions and modifiers" $ do
        par "⊏+" `shouldReturn` pure [Just $ WrapBranch (Leaf CatFunction (TokenPrimFunction "+" emptyPos))]
        par "⊏⍨" `shouldReturn` pure [Just $ WrapBranch (Leaf CatAdverb (TokenPrimAdverb "⍨" emptyPos))]
        par "⊏∘" `shouldReturn` pure [Just $ WrapBranch (Leaf CatConjunction (TokenPrimConjunction "∘" emptyPos))]

      it "fails on wraps of non-functions" $ do
        par "⊏3" `shouldReturn` Nothing

    describe "unwraps" $ do
      it "parses unwraps of arrays" $ do
        par "⊐3" `shouldReturn` pure [Just $ UnwrapBranch CatFunction (Leaf CatArray (TokenNumber 3 emptyPos))]
        par "_⊐3" `shouldReturn` pure [Just $ UnwrapBranch CatAdverb (Leaf CatArray (TokenNumber 3 emptyPos))]
        par "_⊐_3" `shouldReturn` pure [Just $ UnwrapBranch CatConjunction (Leaf CatArray (TokenNumber 3 emptyPos))]

      it "fails on unwraps of non-arrays" $ do
        par "⊐+" `shouldReturn` Nothing
        par "_⊐+" `shouldReturn` Nothing
        par "_⊐_+" `shouldReturn` Nothing

    describe "structs" $ do
      it "parses structs" $ do
        par "⦃1⋄2⋄3⦄" `shouldReturn` pure [Just $ StructBranch [Leaf CatArray (TokenNumber 1 emptyPos), Leaf CatArray (TokenNumber 2 emptyPos), Leaf CatArray (TokenNumber 3 emptyPos)]]

    describe "empty statements" $ do
      it "parses empty statements" $ do
        par "1⋄⋄2" `shouldReturn` pure [Just $ Leaf CatArray (TokenNumber 1 emptyPos), Nothing, Just $ Leaf CatArray (TokenNumber 2 emptyPos)]

    describe "ternaries" $ do
      it "parses ternaries" $ do
        par "1⍰2⍠3" `shouldReturn` pure [Just $ TernaryBranch (Leaf CatArray (TokenNumber 1 emptyPos)) (Leaf CatArray (TokenNumber 2 emptyPos)) (Leaf CatArray (TokenNumber 3 emptyPos))]

    describe "trains" $ do
      it "parses trains" $ do
        par "⦅1⋄2⦆" `shouldReturn` pure [Just $ TrainBranch CatFunction [Just $ Leaf CatArray (TokenNumber 1 emptyPos), Just $ Leaf CatArray (TokenNumber 2 emptyPos)]]
        par "⦅1⋄2⋄3⦆" `shouldReturn` pure [Just $ TrainBranch CatFunction [Just $ Leaf CatArray (TokenNumber 1 emptyPos), Just $ Leaf CatArray (TokenNumber 2 emptyPos), Just $ Leaf CatArray (TokenNumber 3 emptyPos)]]
        par "⦅1⋄⋄2⋄3⦆" `shouldReturn` pure [Just $ TrainBranch CatFunction [Just $ Leaf CatArray (TokenNumber 1 emptyPos), Nothing, Just $ Leaf CatArray (TokenNumber 2 emptyPos), Just $ Leaf CatArray (TokenNumber 3 emptyPos)]]
        par "_⦅1⋄2⋄3⦆" `shouldReturn` pure [Just $ TrainBranch CatAdverb [Just $ Leaf CatArray (TokenNumber 1 emptyPos), Just $ Leaf CatArray (TokenNumber 2 emptyPos), Just $ Leaf CatArray (TokenNumber 3 emptyPos)]]
      it "parses compact trains" $ do
        par "⦅1 2 3⦆" `shouldReturn` pure [Just $ TrainBranch CatFunction [Just $ Leaf CatArray (TokenNumber 1 emptyPos), Just $ Leaf CatArray (TokenNumber 2 emptyPos), Just $ Leaf CatArray (TokenNumber 3 emptyPos)]]
        par "⦅+ 1⦆" `shouldReturn` pure [Just $ TrainBranch CatFunction [Just $ Leaf CatFunction (TokenPrimFunction "+" emptyPos), Just $ Leaf CatArray (TokenNumber 1 emptyPos)]]
        par "⦅+⍨ 1⦆" `shouldReturn` pure [Just $ TrainBranch CatFunction [Just $ AdverbCallBranch (Leaf CatFunction (TokenPrimFunction "+" emptyPos)) (Leaf CatAdverb (TokenPrimAdverb "⍨" emptyPos)), Just $ Leaf CatArray (TokenNumber 1 emptyPos)]]
        par "⦅+·-1⦆" `shouldReturn` pure [Just $ TrainBranch CatFunction [Just $ Leaf CatFunction (TokenPrimFunction "+" emptyPos), Just $ Leaf CatNothing (TokenNothing emptyPos), Just $ Leaf CatFunction (TokenPrimFunction "-" emptyPos), Just $ Leaf CatArray (TokenNumber 1 emptyPos)]]
