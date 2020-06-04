{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | A representation where all bindings are annotated with range
-- information.
module Futhark.Representation.Ranges
       ( -- * The Lore definition
         Ranges
       , module Futhark.Representation.AST.Attributes.Ranges
         -- * Module re-exports
       , module Futhark.Representation.AST.Attributes
       , module Futhark.Representation.AST.Traversals
       , module Futhark.Representation.AST.Pretty
       , module Futhark.Representation.AST.Syntax
         -- * Adding ranges
       , addRangesToPattern
       , mkRangedBody
       , mkPatternRanges
       , mkBodyRanges
         -- * Removing ranges
       , removeExpRanges
       , removeBodyRanges
       , removeStmRanges
       , removeLambdaRanges
       , removePatternRanges
       )
where

import Control.Monad.Identity
import Control.Monad.Reader

import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Attributes
import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Representation.AST.Attributes.Ranges
import Futhark.Representation.AST.Traversals
import Futhark.Representation.AST.Pretty
import Futhark.Analysis.Rephrase
import qualified Futhark.Util.Pretty as PP

-- | The lore for the basic representation.
data Ranges lore

instance (Annotations lore, CanBeRanged (Op lore)) =>
         Annotations (Ranges lore) where
  type LetAttr (Ranges lore) = (Range, LetAttr lore)
  type ExpAttr (Ranges lore) = ExpAttr lore
  type BodyAttr (Ranges lore) = ([Range], BodyAttr lore)
  type FParamAttr (Ranges lore) = FParamAttr lore
  type LParamAttr (Ranges lore) = LParamAttr lore
  type RetType (Ranges lore) = RetType lore
  type BranchType (Ranges lore) = BranchType lore
  type Op (Ranges lore) = OpWithRanges (Op lore)

withoutRanges :: (HasScope (Ranges lore) m, Monad m) =>
                 ReaderT (Scope lore) m a ->
                 m a
withoutRanges m = do
  scope <- asksScope $ fmap unRange
  runReaderT m scope
    where unRange :: NameInfo (Ranges lore) -> NameInfo lore
          unRange (LetInfo (_, x)) = LetInfo x
          unRange (FParamInfo x) = FParamInfo x
          unRange (LParamInfo x) = LParamInfo x
          unRange (IndexInfo x) = IndexInfo x

instance (Attributes lore, CanBeRanged (Op lore)) =>
         Attributes (Ranges lore) where
  expTypesFromPattern =
    withoutRanges . expTypesFromPattern . removePatternRanges

instance RangeOf (Range, attr) where
  rangeOf = fst

instance RangesOf ([Range], attr) where
  rangesOf = fst

instance PrettyAnnot (PatElemT attr) =>
  PrettyAnnot (PatElemT (Range, attr)) where

  ppAnnot patelem =
    range_annot <> inner_annot
    where range_annot =
            case fst . patElemAttr $ patelem of
              (Nothing, Nothing) -> Nothing
              range ->
                Just $ PP.oneLine $
                PP.text "-- " <> PP.ppr (patElemName patelem) <> PP.text " range: " <>
                PP.ppr range
          inner_annot = ppAnnot $ fmap snd patelem


instance (PrettyLore lore, CanBeRanged (Op lore)) => PrettyLore (Ranges lore) where
  ppExpLore attr = ppExpLore attr . removeExpRanges

removeRanges :: CanBeRanged (Op lore) => Rephraser Identity (Ranges lore) lore
removeRanges = Rephraser { rephraseExpLore = return
                         , rephraseLetBoundLore = return . snd
                         , rephraseBodyLore = return . snd
                         , rephraseFParamLore = return
                         , rephraseLParamLore = return
                         , rephraseRetType = return
                         , rephraseBranchType = return
                         , rephraseOp = return . removeOpRanges
                         }

removeExpRanges :: CanBeRanged (Op lore) =>
                   Exp (Ranges lore) -> Exp lore
removeExpRanges = runIdentity . rephraseExp removeRanges

removeBodyRanges :: CanBeRanged (Op lore) =>
                    Body (Ranges lore) -> Body lore
removeBodyRanges = runIdentity . rephraseBody removeRanges

removeStmRanges :: CanBeRanged (Op lore) =>
                       Stm (Ranges lore) -> Stm lore
removeStmRanges = runIdentity . rephraseStm removeRanges

removeLambdaRanges :: CanBeRanged (Op lore) =>
                      Lambda (Ranges lore) -> Lambda lore
removeLambdaRanges = runIdentity . rephraseLambda removeRanges

removePatternRanges :: PatternT (Range, a)
                    -> PatternT a
removePatternRanges = runIdentity . rephrasePattern (return . snd)

addRangesToPattern :: (Attributes lore, CanBeRanged (Op lore)) =>
                      Pattern lore -> Exp (Ranges lore)
                   -> Pattern (Ranges lore)
addRangesToPattern pat e =
  uncurry Pattern $ mkPatternRanges pat e

mkRangedBody :: BodyAttr lore -> Stms (Ranges lore) -> Result
             -> Body (Ranges lore)
mkRangedBody innerlore bnds res =
  Body (mkBodyRanges bnds res, innerlore) bnds res

mkPatternRanges :: (Attributes lore, CanBeRanged (Op lore)) =>
                   Pattern lore
                -> Exp (Ranges lore)
                -> ([PatElemT (Range, LetAttr lore)],
                    [PatElemT (Range, LetAttr lore)])
mkPatternRanges pat e =
  (map (`addRanges` unknownRange) $ patternContextElements pat,
   zipWith addRanges (patternValueElements pat) ranges)
  where addRanges patElem range =
          let innerlore = patElemAttr patElem
          in patElem `setPatElemLore` (range, innerlore)
        ranges = expRanges e

mkBodyRanges :: Stms lore -> Result -> [Range]
mkBodyRanges bnds = map $ removeUnknownBounds . rangeOf
  where boundInBnds =
          foldMap (namesFromList . patternNames . stmPattern) bnds
        removeUnknownBounds (lower,upper) =
          (removeUnknownBound lower,
           removeUnknownBound upper)
        removeUnknownBound (Just bound)
          | freeIn bound `namesIntersect` boundInBnds = Nothing
          | otherwise                                 = Just bound
        removeUnknownBound Nothing =
          Nothing

-- It is convenient for a wrapped aliased lore to also be aliased.

instance AliasesOf attr => AliasesOf ([Range], attr) where
  aliasesOf = aliasesOf . snd

instance AliasesOf attr => AliasesOf (Range, attr) where
  aliasesOf = aliasesOf . snd

instance (Aliased lore, CanBeRanged (Op lore),
          AliasedOp (OpWithRanges (Op lore))) => Aliased (Ranges lore) where
  bodyAliases = bodyAliases . removeBodyRanges
  consumedInBody = consumedInBody . removeBodyRanges