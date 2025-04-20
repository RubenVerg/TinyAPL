{-# LANGUAGE NegativeLiterals, OverloadedLists #-}

module TinyAPL.ParserSpec where

import TinyAPL.Complex
import qualified TinyAPL.Glyphs as G
import TinyAPL.Parser
import TinyAPL.Util
import TinyAPL.Primitives (arrays, functions, adverbs, conjunctions)

import Test.Hspec
import Data.List (singleton)

import Data.List.NonEmpty ()

spec :: Spec
spec = do
  describe "tokenize" $ do
    let tok = tokenize (fst <$> arrays, fst <$> functions, fst <$> adverbs, fst <$> conjunctions) "<test>"

    it "ignores comments" $ do
      tok "⟃abc⟄ 1" `shouldBe` pure [[TokenNumber 1 emptyPos]]
      tok "2 ⍝ abc" `shouldBe` pure [[TokenNumber 2 emptyPos]]
      tok "1 ⍝ abc\n10" `shouldBe` pure [[TokenNumber 1 emptyPos, TokenNumber 10 emptyPos]]

    it "parses numbers" $ do
      tok "1" `shouldBe` pure [[TokenNumber 1 emptyPos]]
      tok "¯2" `shouldBe` pure [[TokenNumber -2 emptyPos]]
      tok "1.5" `shouldBe` pure [[TokenNumber 1.5 emptyPos]]
      tok "¯3.25" `shouldBe` pure [[TokenNumber -3.25 emptyPos]]
      tok "3⏨2" `shouldBe` pure [[TokenNumber 300 emptyPos]]
      tok "2.4⏨¯3" `shouldBe` pure [[TokenNumber 0.0024 emptyPos]]
      tok "3ᴊ2" `shouldBe` pure [[TokenNumber (3 :+ 2) emptyPos]]
      tok "¯2ᴊ1.5⏨2" `shouldBe` pure [[TokenNumber (-2 :+ 150) emptyPos]]
      tok "∞" `shouldBe` pure [[TokenNumber (inf :+ 0) emptyPos]]
      tok "¯∞" `shouldBe` pure [[TokenNumber (ninf :+ 0) emptyPos]]
      tok "0ᴊ∞" `shouldBe` pure [[TokenNumber (0 :+ inf) emptyPos]]
    
    it "parses character vectors" $ do
      tok "'abc'" `shouldBe` pure [[TokenChar "abc" emptyPos]]
      tok "''" `shouldBe` pure [[TokenChar "" emptyPos]]
    
    it "parses strings" $ do
      tok "\"abc\"" `shouldBe` pure [[TokenString "abc" emptyPos]]
      tok "\"\"" `shouldBe` pure [[TokenString "" emptyPos]]
      tok "\"a⍘nb⍘\"c⍘⍘d⍘re⍘tf\"" `shouldBe` pure [[TokenString "a\nb\"c⍘d\re\tf" emptyPos]]

    it "parses vector notation" $ do
      tok "⟨1⋄2⟩" `shouldBe` pure [[TokenVector [[TokenNumber 1 emptyPos], [TokenNumber 2 emptyPos]] emptyPos]]
      tok "⟨⟩" `shouldBe` pure [[TokenVector [] emptyPos]]

    it "parses high rank notation" $ do
      tok "[1⋄2]" `shouldBe` pure [[TokenHighRank [[TokenNumber 1 emptyPos], [TokenNumber 2 emptyPos]] emptyPos]]
      tok "[]" `shouldBe` pure [[TokenHighRank [] emptyPos]]

    it "parses primitives" $ do
      mapM_ (\n -> tok n `shouldBe` pure [[TokenPrimArray n emptyPos]]) $ singleton <$> G.arrays
      mapM_ (\n -> tok n `shouldBe` pure [[TokenPrimFunction n emptyPos]]) $ singleton <$> G.functions
      mapM_ (\n -> tok n `shouldBe` pure [[TokenPrimAdverb n emptyPos]]) $ singleton <$> G.adverbs
      mapM_ (\n -> tok n `shouldBe` pure [[TokenPrimConjunction n emptyPos]]) $ singleton <$> G.conjunctions

    it "parses array names" $ do
      tok "abc ∆x" `shouldBe` pure [[TokenArrayName "abc" emptyPos, TokenArrayName "∆x" emptyPos]]
      tok "⍺ ⍺⍺ ⍵ ⍵⍵ ⎕ ⍞ ɛ" `shouldBe` pure [[TokenArrayName "⍺" emptyPos, TokenArrayName "⍺⍺" emptyPos, TokenArrayName "⍵" emptyPos, TokenArrayName "⍵⍵" emptyPos, TokenArrayName "⎕" emptyPos, TokenArrayName "⍞" emptyPos, TokenArrayName "ɛ" emptyPos]]
      tok "⎕io" `shouldBe` pure [[TokenArrayName "⎕io" emptyPos]]

    it "parses function names" $ do
      tok "Abc ⍙y" `shouldBe` pure [[TokenFunctionName "Abc" emptyPos, TokenFunctionName "⍙y" emptyPos]]
      tok "⍶⍶ ⍹⍹ ∇" `shouldBe` pure [[TokenFunctionName "⍶⍶" emptyPos, TokenFunctionName "⍹⍹" emptyPos, TokenFunctionName "∇" emptyPos]]
      tok "⎕C" `shouldBe` pure [[TokenFunctionName "⎕C" emptyPos]]
      tok "⍞100" `shouldBe` pure [[TokenFunctionName "⍞100" emptyPos]]
  
    it "parses adverb names" $ do
      tok "_Abc _abc" `shouldBe` pure [[TokenAdverbName "_Abc" emptyPos, TokenAdverbName "_abc" emptyPos]]
      tok "_∇" `shouldBe` pure [[TokenAdverbName "_∇" emptyPos]]
      tok "⎕_BinFile" `shouldBe` pure [[TokenAdverbName "⎕_BinFile" emptyPos]]
      tok "⍞_100" `shouldBe` pure [[TokenAdverbName "⍞_100" emptyPos]]

    it "parses conjunction names" $ do
      tok "_Abc_ _abc_" `shouldBe` pure [[TokenConjunctionName "_Abc_" emptyPos, TokenConjunctionName "_abc_" emptyPos]]
      tok "_∇_" `shouldBe` pure [[TokenConjunctionName "_∇_" emptyPos]]
      tok "⎕_Whatever_" `shouldBe` pure [[TokenConjunctionName "⎕_Whatever_" emptyPos]]
      tok "⍞_100_" `shouldBe` pure [[TokenConjunctionName "⍞_100_" emptyPos]]

    it "parses qualified names" $ do
      tok "a→b→c" `shouldBe` pure [[TokenQualifiedArrayName (TokenArrayName "a" emptyPos) ["b", "c"] emptyPos]]
      tok "a→b→C" `shouldBe` pure [[TokenQualifiedFunctionName (TokenArrayName "a" emptyPos) ["b", "C"] emptyPos]]
      tok "a→b→_C" `shouldBe` pure [[TokenQualifiedAdverbName (TokenArrayName "a" emptyPos) ["b", "_C"] emptyPos]]
      tok "a→b→_C_" `shouldBe` pure [[TokenQualifiedConjunctionName (TokenArrayName "a" emptyPos) ["b", "_C_"] emptyPos]]

    it "parses assignment" $ do
      tok "abc←3" `shouldBe` pure [[TokenArrayAssign "abc" AssignNormal [TokenNumber 3 emptyPos] emptyPos]]
      tok "Abc←3" `shouldBe` pure [[TokenFunctionAssign "Abc" AssignNormal [TokenNumber 3 emptyPos] emptyPos]]
      tok "_Abc←3" `shouldBe` pure [[TokenAdverbAssign "_Abc" AssignNormal [TokenNumber 3 emptyPos] emptyPos]]
      tok "_Abc_←3" `shouldBe` pure [[TokenConjunctionAssign "_Abc_" AssignNormal [TokenNumber 3 emptyPos] emptyPos]]
      tok "⎕seed←3" `shouldBe` pure [[TokenArrayAssign "⎕seed" AssignNormal [TokenNumber 3 emptyPos] emptyPos]]
      tok "abc↩3" `shouldBe` pure [[TokenArrayAssign "abc" AssignModify [TokenNumber 3 emptyPos] emptyPos]]
      tok "Abc↩3" `shouldBe` pure [[TokenFunctionAssign "Abc" AssignModify [TokenNumber 3 emptyPos] emptyPos]]
      tok "_Abc↩3" `shouldBe` pure [[TokenAdverbAssign "_Abc" AssignModify [TokenNumber 3 emptyPos] emptyPos]]
      tok "_Abc_↩3" `shouldBe` pure [[TokenConjunctionAssign "_Abc_" AssignModify [TokenNumber 3 emptyPos] emptyPos]]
      tok "abc⇇3" `shouldBe` pure [[TokenArrayAssign "abc" AssignConstant [TokenNumber 3 emptyPos] emptyPos]]
      tok "Abc⇇3" `shouldBe` pure [[TokenFunctionAssign "Abc" AssignConstant [TokenNumber 3 emptyPos] emptyPos]]
      tok "_Abc⇇3" `shouldBe` pure [[TokenAdverbAssign "_Abc" AssignConstant [TokenNumber 3 emptyPos] emptyPos]]
      tok "_Abc_⇇3" `shouldBe` pure [[TokenConjunctionAssign "_Abc_" AssignConstant [TokenNumber 3 emptyPos] emptyPos]]
      tok "abc↚3" `shouldBe` pure [[TokenArrayAssign "abc" AssignPrivate [TokenNumber 3 emptyPos] emptyPos]]
      tok "Abc↚3" `shouldBe` pure [[TokenFunctionAssign "Abc" AssignPrivate [TokenNumber 3 emptyPos] emptyPos]]
      tok "_Abc↚3" `shouldBe` pure [[TokenAdverbAssign "_Abc" AssignPrivate [TokenNumber 3 emptyPos] emptyPos]]
      tok "_Abc_↚3" `shouldBe` pure [[TokenConjunctionAssign "_Abc_" AssignPrivate [TokenNumber 3 emptyPos] emptyPos]]

    it "parses qualified assignment" $ do
      tok "a→b→c←3" `shouldBe` pure [[TokenQualifiedArrayAssign (TokenArrayName "a" emptyPos) ["b", "c"] AssignNormal [TokenNumber 3 emptyPos] emptyPos]]
      tok "a→b→c↩3" `shouldBe` pure [[TokenQualifiedArrayAssign (TokenArrayName "a" emptyPos) ["b", "c"] AssignModify [TokenNumber 3 emptyPos] emptyPos]]
      tok "a→b→c⇇3" `shouldBe` pure [[TokenQualifiedArrayAssign (TokenArrayName "a" emptyPos) ["b", "c"] AssignConstant [TokenNumber 3 emptyPos] emptyPos]]
      tok "a→b→C↚3" `shouldBe` pure [[TokenQualifiedFunctionAssign (TokenArrayName "a" emptyPos) ["b", "C"] AssignPrivate [TokenNumber 3 emptyPos] emptyPos]]
      tok "a→b→C←3" `shouldBe` pure [[TokenQualifiedFunctionAssign (TokenArrayName "a" emptyPos) ["b", "C"] AssignNormal [TokenNumber 3 emptyPos] emptyPos]]
      tok "a→b→C↩3" `shouldBe` pure [[TokenQualifiedFunctionAssign (TokenArrayName "a" emptyPos) ["b", "C"] AssignModify [TokenNumber 3 emptyPos] emptyPos]]
      tok "a→b→C⇇3" `shouldBe` pure [[TokenQualifiedFunctionAssign (TokenArrayName "a" emptyPos) ["b", "C"] AssignConstant [TokenNumber 3 emptyPos] emptyPos]]
      tok "a→b→_C↚3" `shouldBe` pure [[TokenQualifiedAdverbAssign (TokenArrayName "a" emptyPos) ["b", "_C"] AssignPrivate [TokenNumber 3 emptyPos] emptyPos]]
      tok "a→b→_C←3" `shouldBe` pure [[TokenQualifiedAdverbAssign (TokenArrayName "a" emptyPos) ["b", "_C"] AssignNormal [TokenNumber 3 emptyPos] emptyPos]]
      tok "a→b→_C↩3" `shouldBe` pure [[TokenQualifiedAdverbAssign (TokenArrayName "a" emptyPos) ["b", "_C"] AssignModify [TokenNumber 3 emptyPos] emptyPos]]
      tok "a→b→_C⇇3" `shouldBe` pure [[TokenQualifiedAdverbAssign (TokenArrayName "a" emptyPos) ["b", "_C"] AssignConstant [TokenNumber 3 emptyPos] emptyPos]]
      tok "a→b→_C_↚3" `shouldBe` pure [[TokenQualifiedConjunctionAssign (TokenArrayName "a" emptyPos) ["b", "_C_"] AssignPrivate [TokenNumber 3 emptyPos] emptyPos]]
      tok "a→b→_C_←3" `shouldBe` pure [[TokenQualifiedConjunctionAssign (TokenArrayName "a" emptyPos) ["b", "_C_"] AssignNormal [TokenNumber 3 emptyPos] emptyPos]]
      tok "a→b→_C_↩3" `shouldBe` pure [[TokenQualifiedConjunctionAssign (TokenArrayName "a" emptyPos) ["b", "_C_"] AssignModify [TokenNumber 3 emptyPos] emptyPos]]
      tok "a→b→_C_⇇3" `shouldBe` pure [[TokenQualifiedConjunctionAssign (TokenArrayName "a" emptyPos) ["b", "_C_"] AssignConstant [TokenNumber 3 emptyPos] emptyPos]]
      tok "a→b→_C_↚3" `shouldBe` pure [[TokenQualifiedConjunctionAssign (TokenArrayName "a" emptyPos) ["b", "_C_"] AssignPrivate [TokenNumber 3 emptyPos] emptyPos]]

    it "parses dfns and dops" $ do
      tok "{3⋄1}" `shouldBe` pure [[TokenDfn [[TokenNumber 3 emptyPos], [TokenNumber 1 emptyPos]] emptyPos]]
      tok "_{3⋄1}" `shouldBe` pure [[TokenDadv [[TokenNumber 3 emptyPos], [TokenNumber 1 emptyPos]] emptyPos]]
      tok "_{3⋄1}_" `shouldBe` pure [[TokenDconj [[TokenNumber 3 emptyPos], [TokenNumber 1 emptyPos]] emptyPos]]

    it "parses wraps" $ do
      tok "⊏+" `shouldBe` pure [[TokenWrap (TokenPrimFunction "+" emptyPos) emptyPos]]

    it "parses unwraps" $ do
      tok "⊐3" `shouldBe` pure [[TokenUnwrap (TokenNumber 3 emptyPos) emptyPos]]
      tok "_⊐3" `shouldBe` pure [[TokenUnwrapAdverb (TokenNumber 3 emptyPos) emptyPos]]
      tok "_⊐_3" `shouldBe` pure [[TokenUnwrapConjunction (TokenNumber 3 emptyPos) emptyPos]]
    
    it "parses guards" $ do
      tok "{1:2}" `shouldBe` pure [[TokenDfn [[TokenGuard [TokenNumber 1 emptyPos] [TokenNumber 2 emptyPos] emptyPos]] emptyPos]]

    it "parses exit statements" $ do
      tok "{■5}" `shouldBe` pure [[TokenDfn [[TokenExit [TokenNumber 5 emptyPos] emptyPos]] emptyPos]]

    it "parses separator-separated statements" $ do
      tok "1⋄2" `shouldBe` pure [[TokenNumber 1 emptyPos], [TokenNumber 2 emptyPos]]

    it "ignores spaces and newlines" $ do
      tok "1\n2" `shouldBe` pure [[TokenNumber 1 emptyPos, TokenNumber 2 emptyPos]]
      tok "1     2" `shouldBe` pure [[TokenNumber 1 emptyPos, TokenNumber 2 emptyPos]]

    it "treats multiple newlines as a separator" $ do
      tok "1\n\n2" `shouldBe` pure [[TokenNumber 1 emptyPos], [TokenNumber 2 emptyPos]]
      tok "1\n\n\n2" `shouldBe` pure [[TokenNumber 1 emptyPos], [TokenNumber 2 emptyPos]]
      tok "1\n\n\n\n2" `shouldBe` pure [[TokenNumber 1 emptyPos], [TokenNumber 2 emptyPos]]
    
    it "parses parens" $ do
      tok "(1 2)" `shouldBe` pure [[TokenParens [TokenNumber 1 emptyPos, TokenNumber 2 emptyPos] emptyPos]]

    it "parses trains and modifier trains" $ do
      tok "⦅1⋄2⦆" `shouldBe` pure [[TokenTrain [[TokenNumber 1 emptyPos], [TokenNumber 2 emptyPos]] emptyPos]]
      tok "⦅1⋄2⋄3⦆" `shouldBe` pure [[TokenTrain [[TokenNumber 1 emptyPos], [TokenNumber 2 emptyPos], [TokenNumber 3 emptyPos]] emptyPos]]
      tok "⦅1⋄⋄2⋄3⦆" `shouldBe` pure [[TokenTrain [[TokenNumber 1 emptyPos], [], [TokenNumber 2 emptyPos], [TokenNumber 3 emptyPos]] emptyPos]]
      tok "_⦅1⋄2⋄3⦆" `shouldBe` pure [[TokenAdverbTrain [[TokenNumber 1 emptyPos], [TokenNumber 2 emptyPos], [TokenNumber 3 emptyPos]] emptyPos]]
      tok "_⦅1⋄2⋄3⦆_" `shouldBe` pure [[TokenConjunctionTrain [[TokenNumber 1 emptyPos], [TokenNumber 2 emptyPos], [TokenNumber 3 emptyPos]] emptyPos]]

    it "parses destructuring assignment" $ do
      tok "⟨a⋄b⟩←9" `shouldBe` pure [[TokenVectorAssign ["a", "b"] AssignNormal [TokenNumber 9 emptyPos] emptyPos]]
      tok "⟨a⋄b⟩↩9" `shouldBe` pure [[TokenVectorAssign ["a", "b"] AssignModify [TokenNumber 9 emptyPos] emptyPos]]
      tok "⟨a⋄b⟩⇇9" `shouldBe` pure [[TokenVectorAssign ["a", "b"] AssignConstant [TokenNumber 9 emptyPos] emptyPos]]
      tok "⟨a⋄b⟩↚9" `shouldBe` pure [[TokenVectorAssign ["a", "b"] AssignPrivate [TokenNumber 9 emptyPos] emptyPos]]
      tok "[a⋄b]←7" `shouldBe` pure [[TokenHighRankAssign ["a", "b"] AssignNormal [TokenNumber 7 emptyPos] emptyPos]]
      tok "[a⋄b]↩7" `shouldBe` pure [[TokenHighRankAssign ["a", "b"] AssignModify [TokenNumber 7 emptyPos] emptyPos]]
      tok "[a⋄b]⇇7" `shouldBe` pure [[TokenHighRankAssign ["a", "b"] AssignConstant [TokenNumber 7 emptyPos] emptyPos]]
      tok "[a⋄b]↚7" `shouldBe` pure [[TokenHighRankAssign ["a", "b"] AssignPrivate [TokenNumber 7 emptyPos] emptyPos]]
      tok "a‿b←11" `shouldBe` pure [[TokenTieAssign ["a", "b"] AssignNormal [TokenNumber 11 emptyPos] emptyPos]]
      tok "a‿b↩11" `shouldBe` pure [[TokenTieAssign ["a", "b"] AssignModify [TokenNumber 11 emptyPos] emptyPos]]
      tok "a‿b⇇11" `shouldBe` pure [[TokenTieAssign ["a", "b"] AssignConstant [TokenNumber 11 emptyPos] emptyPos]]
      tok "a‿b↚11" `shouldBe` pure [[TokenTieAssign ["a", "b"] AssignPrivate [TokenNumber 11 emptyPos] emptyPos]]

    it "parses struct assignment" $ do
      tok "⦃a⦄←3" `shouldBe` pure [[TokenStructAssign [("a", Nothing)] AssignNormal [TokenNumber 3 emptyPos] emptyPos]]
      tok "⦃a←b⦄←3" `shouldBe` pure [[TokenStructAssign [("a", Just (AssignNormal, "b"))] AssignNormal [TokenNumber 3 emptyPos] emptyPos]]
      tok "⦃a↩b⦄⇇3" `shouldBe` pure [[TokenStructAssign [("a", Just (AssignModify, "b"))] AssignConstant [TokenNumber 3 emptyPos] emptyPos]]
      tok "⦃a⋄b←c⦄↚3" `shouldBe` pure [[TokenStructAssign [("a", Nothing), ("b", Just (AssignNormal, "c"))] AssignPrivate [TokenNumber 3 emptyPos] emptyPos]]

    it "parses structs" $ do
      tok "⦃1⋄2⋄3⦄" `shouldBe` pure [[TokenStruct [[TokenNumber 1 emptyPos], [TokenNumber 2 emptyPos], [TokenNumber 3 emptyPos]] emptyPos]]

    it "parses ties" $ do
      tok "1‿2‿3" `shouldBe` pure [[TokenTie [TokenNumber 1 emptyPos, TokenNumber 2 emptyPos, TokenNumber 3 emptyPos] emptyPos]]
      tok "+‿-‿×" `shouldBe` pure [[TokenTie [TokenPrimFunction "+" emptyPos, TokenPrimFunction "-" emptyPos, TokenPrimFunction "×" emptyPos] emptyPos]]

    it "parses ternaries" $ do
      tok "1⍰2⍠3" `shouldBe` pure [[TokenTernary [TokenNumber 1 emptyPos] [TokenNumber 2 emptyPos] [TokenNumber 3 emptyPos] emptyPos]]

    it "parses extra arguments" $ do
      tok "⦋⦌" `shouldBe` pure [[TokenExtraArgs [] emptyPos]]
      tok "⦋1:2⦌" `shouldBe` pure [[TokenExtraArgs [([TokenNumber 1 emptyPos], [TokenNumber 2 emptyPos])] emptyPos]]
      tok "⦋1:2⋄3:4⦌" `shouldBe` pure [[TokenExtraArgs [([TokenNumber 1 emptyPos], [TokenNumber 2 emptyPos]), ([TokenNumber 3 emptyPos], [TokenNumber 4 emptyPos])] emptyPos]]
      tok "⦋1:2⋄3:4⋄5:6⦌" `shouldBe` pure [[TokenExtraArgs [([TokenNumber 1 emptyPos], [TokenNumber 2 emptyPos]), ([TokenNumber 3 emptyPos], [TokenNumber 4 emptyPos]), ([TokenNumber 5 emptyPos], [TokenNumber 6 emptyPos])] emptyPos]]
      tok "⦋hi⦌" `shouldBe` pure [[TokenSpreadExtraArgs [TokenArrayName "hi" emptyPos] emptyPos]]
    
    it "parses nothing" $ do
      tok "·" `shouldBe` pure [[TokenNothing emptyPos]]

  describe "binder" $ do
    let e2m (Right x) = Just x
        e2m (Left _)  = Nothing
    let par = e2m . parse (fst <$> arrays, fst <$> functions, fst <$> adverbs, fst <$> conjunctions) "<test>"
    
    it "parses leaves" $ do
      par "1" `shouldBe` pure [Just $ Leaf CatArray (TokenNumber 1 emptyPos)]
      par "'abc'" `shouldBe` pure [Just $ Leaf CatArray (TokenChar "abc" emptyPos)]
      par "\"abc\"" `shouldBe` pure [Just $ Leaf CatArray (TokenString "abc" emptyPos)]
      par "⍬" `shouldBe` pure [Just $ Leaf CatArray (TokenPrimArray "⍬" emptyPos)]
      par "+" `shouldBe` pure [Just $ Leaf CatFunction (TokenPrimFunction "+" emptyPos)]
      par "⍨" `shouldBe` pure [Just $ Leaf CatAdverb (TokenPrimAdverb "⍨" emptyPos)]
      par "∘" `shouldBe` pure [Just $ Leaf CatConjunction (TokenPrimConjunction "∘" emptyPos)]
      par "abc" `shouldBe` pure [Just $ Leaf CatArray (TokenArrayName "abc" emptyPos)]
      par "Abc" `shouldBe` pure [Just $ Leaf CatFunction (TokenFunctionName "Abc" emptyPos)]
      par "_Abc" `shouldBe` pure [Just $ Leaf CatAdverb (TokenAdverbName "_Abc" emptyPos)]
      par "_Abc_" `shouldBe` pure [Just $ Leaf CatConjunction (TokenConjunctionName "_Abc_" emptyPos)]
    
    it "parses parens" $ do
      par "(1)" `shouldBe` pure [Just $ Leaf CatArray (TokenNumber 1 emptyPos)]

    describe "application" $ do
      it "parses monad application" $ do
        par "+1" `shouldBe` pure [Just $ MonadCallBranch (Leaf CatFunction (TokenPrimFunction "+" emptyPos)) (Leaf CatArray (TokenNumber 1 emptyPos))]
      
      it "parses dyad application" $ do
        par "1+" `shouldBe` pure [Just $ DyadCallBranch (Leaf CatArray (TokenNumber 1 emptyPos)) (Leaf CatFunction (TokenPrimFunction "+" emptyPos))]
        par "1+2" `shouldBe` pure [Just $ MonadCallBranch (DyadCallBranch (Leaf CatArray (TokenNumber 1 emptyPos)) (Leaf CatFunction (TokenPrimFunction "+" emptyPos))) (Leaf CatArray (TokenNumber 2 emptyPos))]

      it "parses adverb application" $ do
        par "1⍨" `shouldBe` pure [Just $ AdverbCallBranch (Leaf CatArray (TokenNumber 1 emptyPos)) (Leaf CatAdverb (TokenPrimAdverb "⍨" emptyPos))]
        par "+⍨" `shouldBe` pure [Just $ AdverbCallBranch (Leaf CatFunction (TokenPrimFunction "+" emptyPos)) (Leaf CatAdverb (TokenPrimAdverb "⍨" emptyPos))]

      it "parses conjunction application" $ do
        par "∘1" `shouldBe` pure [Just $ ConjunctionCallBranch (Leaf CatConjunction (TokenPrimConjunction "∘" emptyPos)) (Leaf CatArray (TokenNumber 1 emptyPos))]
        par "∘+" `shouldBe` pure [Just $ ConjunctionCallBranch (Leaf CatConjunction (TokenPrimConjunction "∘" emptyPos)) (Leaf CatFunction (TokenPrimFunction "+" emptyPos))]

      it "parses extra arguments application" $ do
        par "+⦋⦌" `shouldBe` pure [Just $ ExtraArgsBranch CatFunction (Leaf CatFunction (TokenPrimFunction "+" emptyPos)) (UnboundExtraArgsBranch (DictionaryBranch []))]
        par "⍨⦋⦌" `shouldBe` pure [Just $ ExtraArgsBranch CatAdverb (Leaf CatAdverb (TokenPrimAdverb "⍨" emptyPos)) (UnboundExtraArgsBranch (DictionaryBranch []))]
        par "∘⦋⦌" `shouldBe` pure [Just $ ExtraArgsBranch CatConjunction (Leaf CatConjunction (TokenPrimConjunction "∘" emptyPos)) (UnboundExtraArgsBranch (DictionaryBranch []))]

    describe "dfns" $ do
      it "parses dfns and dops" $ do
        par "{1}" `shouldBe` pure [Just $ DefinedBranch CatFunction [Leaf CatArray (TokenNumber 1 emptyPos)]]
        par "_{1}" `shouldBe` pure [Just $ DefinedBranch CatAdverb [Leaf CatArray (TokenNumber 1 emptyPos)]]
        par "_{1}_" `shouldBe` pure [Just $ DefinedBranch CatConjunction [Leaf CatArray (TokenNumber 1 emptyPos)]]
      
      it "requires the last statement to be an array" $ do
        par "{+}" `shouldBe` Nothing
    
    describe "assignment" $ do
      it "parses assignment to variables of the correct type" $ do
        par "a←b" `shouldBe` pure [Just $ AssignBranch CatArray "a" AssignNormal (Leaf CatArray (TokenArrayName "b" emptyPos))]
        par "A←B" `shouldBe` pure [Just $ AssignBranch CatFunction "A" AssignNormal (Leaf CatFunction (TokenFunctionName "B" emptyPos))]
        par "_A←_B" `shouldBe` pure [Just $ AssignBranch CatAdverb "_A" AssignNormal (Leaf CatAdverb (TokenAdverbName "_B" emptyPos))]
        par "_A_←_B_" `shouldBe` pure [Just $ AssignBranch CatConjunction "_A_" AssignNormal (Leaf CatConjunction (TokenConjunctionName "_B_" emptyPos))]

      it "fails on assignment to variables of the wrong type" $ do
        par "a←B" `shouldBe` Nothing
        par "A←_B_" `shouldBe` Nothing
        par "_A←b" `shouldBe` Nothing
        par "_A_←_B" `shouldBe` Nothing
    
    describe "guards" $ do
      it "parses guards with array conditions" $ do
        par "{a:b}" `shouldBe` pure [Just $ DefinedBranch CatFunction [GuardBranch (Leaf CatArray (TokenArrayName "a" emptyPos)) (Leaf CatArray (TokenArrayName "b" emptyPos))]]
      
      it "fails on guards with non-array conditions" $ do
        par "{A:b}" `shouldBe` Nothing

    describe "exit statements" $ do
      it "parses exit statements with array results" $ do
        par "{■3}" `shouldBe` pure [Just $ DefinedBranch CatFunction [ExitBranch (Leaf CatArray (TokenNumber 3 emptyPos))]]

      it "fails on exit statements with non-array results" $ do
        par "{■+}" `shouldBe` Nothing

    describe "vector notation" $ do
      it "parses arrays with any contents" $ do
        par "⟨1⋄2⟩" `shouldBe` pure [Just $ VectorBranch [Leaf CatArray (TokenNumber 1 emptyPos), Leaf CatArray (TokenNumber 2 emptyPos)]]
        par "⟨+⟩" `shouldBe` pure [Just $ VectorBranch [Leaf CatFunction (TokenPrimFunction "+" emptyPos)]]
        par "⟨⍨⟩" `shouldBe` pure [Just $ VectorBranch [Leaf CatAdverb (TokenPrimAdverb "⍨" emptyPos)]]
        par "⟨⍥⟩" `shouldBe` pure [Just $ VectorBranch [Leaf CatConjunction (TokenPrimConjunction "⍥" emptyPos)]]
        par "⟨⟩" `shouldBe` pure [Just $ VectorBranch []]

    describe "high rank notation" $ do
      it "parses arrays with array contents" $ do
        par "[1⋄2]" `shouldBe` pure [Just $ HighRankBranch [Leaf CatArray (TokenNumber 1 emptyPos), Leaf CatArray (TokenNumber 2 emptyPos)]]
        par "[]" `shouldBe` pure [Just $ HighRankBranch []]
      
      it "fails on arrays with non-array contents" $ do
        par "[+]" `shouldBe` Nothing

    describe "wraps" $ do
      it "parses wraps of functions and modifiers" $ do
        par "⊏+" `shouldBe` pure [Just $ WrapBranch (Leaf CatFunction (TokenPrimFunction "+" emptyPos))]
        par "⊏⍨" `shouldBe` pure [Just $ WrapBranch (Leaf CatAdverb (TokenPrimAdverb "⍨" emptyPos))]
        par "⊏∘" `shouldBe` pure [Just $ WrapBranch (Leaf CatConjunction (TokenPrimConjunction "∘" emptyPos))]

      it "fails on wraps of non-functions" $ do
        par "⊏3" `shouldBe` Nothing

    describe "unwraps" $ do
      it "parses unwraps of arrays" $ do
        par "⊐3" `shouldBe` pure [Just $ UnwrapBranch CatFunction (Leaf CatArray (TokenNumber 3 emptyPos))]
        par "_⊐3" `shouldBe` pure [Just $ UnwrapBranch CatAdverb (Leaf CatArray (TokenNumber 3 emptyPos))]
        par "_⊐_3" `shouldBe` pure [Just $ UnwrapBranch CatConjunction (Leaf CatArray (TokenNumber 3 emptyPos))]

      it "fails on unwraps of non-arrays" $ do
        par "⊐+" `shouldBe` Nothing
        par "_⊐+" `shouldBe` Nothing
        par "_⊐_+" `shouldBe` Nothing

    describe "structs" $ do
      it "parses structs" $ do
        par "⦃1⋄2⋄3⦄" `shouldBe` pure [Just $ StructBranch [Leaf CatArray (TokenNumber 1 emptyPos), Leaf CatArray (TokenNumber 2 emptyPos), Leaf CatArray (TokenNumber 3 emptyPos)]]

    describe "empty statements" $ do
      it "parses empty statements" $ do
        par "1⋄⋄2" `shouldBe` pure [Just $ Leaf CatArray (TokenNumber 1 emptyPos), Nothing, Just $ Leaf CatArray (TokenNumber 2 emptyPos)]

    describe "ternaries" $ do
      it "parses ternaries" $ do
        par "1⍰2⍠3" `shouldBe` pure [Just $ TernaryBranch (Leaf CatArray (TokenNumber 1 emptyPos)) (Leaf CatArray (TokenNumber 2 emptyPos)) (Leaf CatArray (TokenNumber 3 emptyPos))]

    describe "trains" $ do
      it "parses trains" $ do
        par "⦅1⋄2⦆" `shouldBe` pure [Just $ TrainBranch CatFunction [Just $ Leaf CatArray (TokenNumber 1 emptyPos), Just $ Leaf CatArray (TokenNumber 2 emptyPos)]]
        par "⦅1⋄2⋄3⦆" `shouldBe` pure [Just $ TrainBranch CatFunction [Just $ Leaf CatArray (TokenNumber 1 emptyPos), Just $ Leaf CatArray (TokenNumber 2 emptyPos), Just $ Leaf CatArray (TokenNumber 3 emptyPos)]]
        par "⦅1⋄⋄2⋄3⦆" `shouldBe` pure [Just $ TrainBranch CatFunction [Just $ Leaf CatArray (TokenNumber 1 emptyPos), Nothing, Just $ Leaf CatArray (TokenNumber 2 emptyPos), Just $ Leaf CatArray (TokenNumber 3 emptyPos)]]
        par "_⦅1⋄2⋄3⦆" `shouldBe` pure [Just $ TrainBranch CatAdverb [Just $ Leaf CatArray (TokenNumber 1 emptyPos), Just $ Leaf CatArray (TokenNumber 2 emptyPos), Just $ Leaf CatArray (TokenNumber 3 emptyPos)]]
      it "parses compact trains" $ do
        par "⦅1 2 3⦆" `shouldBe` pure [Just $ TrainBranch CatFunction [Just $ Leaf CatArray (TokenNumber 1 emptyPos), Just $ Leaf CatArray (TokenNumber 2 emptyPos), Just $ Leaf CatArray (TokenNumber 3 emptyPos)]]
        par "⦅+ 1⦆" `shouldBe` pure [Just $ TrainBranch CatFunction [Just $ Leaf CatFunction (TokenPrimFunction "+" emptyPos), Just $ Leaf CatArray (TokenNumber 1 emptyPos)]]
        par "⦅+⍨ 1⦆" `shouldBe` pure [Just $ TrainBranch CatFunction [Just $ AdverbCallBranch (Leaf CatFunction (TokenPrimFunction "+" emptyPos)) (Leaf CatAdverb (TokenPrimAdverb "⍨" emptyPos)), Just $ Leaf CatArray (TokenNumber 1 emptyPos)]]
        par "⦅+·-1⦆" `shouldBe` pure [Just $ TrainBranch CatFunction [Just $ Leaf CatFunction (TokenPrimFunction "+" emptyPos), Just $ Leaf CatNothing (TokenNothing emptyPos), Just $ Leaf CatFunction (TokenPrimFunction "-" emptyPos), Just $ Leaf CatArray (TokenNumber 1 emptyPos)]]
