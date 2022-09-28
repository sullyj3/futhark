{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Futhark.Internalise.Automap (transformProg) where

import Control.Monad.State
import qualified Data.Map.Strict as M
import Data.Maybe
import Debug.Trace
import Futhark.MonadFreshNames
import Language.Futhark
import Language.Futhark.Traversals

-- addValBind :: ValBind -> LiftM ()
-- addValBind vb = modify $ \s ->
--  s
--    { stateValBinds = vb : stateValBinds s,
--      stateGlobal = foldl' (flip S.insert) (stateGlobal s) (valBindBound vb)
--    }

data SEnv = SEnv
  { sEnvNameSource :: VNameSource,
    sEnvAutoMapIntrinsics :: M.Map VName ValBind,
    sEnvDecs :: [Dec]
  }

newtype AutoMapM a = AutoMapM (State SEnv a)
  deriving (Functor, Applicative, Monad, MonadState SEnv)

instance MonadFreshNames AutoMapM where
  getNameSource = gets sEnvNameSource
  putNameSource src = modify (\senv -> senv {sEnvNameSource = src})

initialState :: VNameSource -> SEnv
initialState src = SEnv src mempty mempty

runAutoMapM :: VNameSource -> AutoMapM () -> ([Dec], VNameSource)
runAutoMapM src (AutoMapM m) =
  let s = execState m (initialState src)
   in (map ValDec (M.elems (sEnvAutoMapIntrinsics s)) ++ sEnvDecs s, sEnvNameSource s)

transformExp :: Exp -> AutoMapM Exp
transformExp (AppExp bop@(BinOp (fname, op_loc) (Info t) (e1, Info (t1, d1, a1)) (e2, Info (t2, d2, a2)) loc) (Info (res@(AppRes ret ext)))) = do
  traceM $ "am1: " <> show a1
  traceM $ "am2: " <> show a2
  traceM $ "bop: " <> pretty bop
  traceM $ "bop: " <> show bop
  fname' <-
    if a1 <> a2 /= mempty
      then lookupAutoMapIntrinsic fname
      else pure (qualLeaf fname)
  let fname'' = QualName [] fname'
  pure $ AppExp (BinOp (fname'', op_loc) (Info t) (e1, Info (t1, d1, a1)) (e2, Info (t2, d2, a2)) loc) (Info res)
transformExp e = pure e

lookupAutoMapIntrinsic :: QualName VName -> AutoMapM VName
lookupAutoMapIntrinsic f = do
  m <- gets sEnvAutoMapIntrinsics
  case M.lookup (qualLeaf f) m of
    Just vb -> pure $ valBindName vb
    Nothing -> removeIntrinsicFun $ qualLeaf f

removeIntrinsicFun :: VName -> AutoMapM (VName)
removeIntrinsicFun f
  | baseTag f <= maxIntrinsicTag = do
      (tps, var_params, ret) <- makeRetParamTypes f
      -- f' <- newName f
      f' <- newVName "blah"
      let fun_ts =
            scanr
              ( \(_, pt) ft ->
                  let (p, t) = patternParam pt
                   in Scalar (Arrow mempty p t (RetType [] ft))
              )
              (retType ret)
              var_params
          f_e = Var (QualName [] f) (Info $ fromStruct $ last fun_ts) mempty
          body =
            foldl
              ( \fn ((x, pt), t) ->
                  AppExp (Apply fn x (Info (Observe, Nothing, mempty)) mempty) (Info $ AppRes (fromStruct t) mempty)
              )
              f_e
              (zip var_params fun_ts)

      let vb =
            ValBind
              { valBindEntryPoint = Nothing,
                valBindName = f',
                valBindRetDecl = Nothing,
                valBindRetType = Info ret,
                valBindTypeParams = tps,
                valBindParams = map snd var_params,
                valBindBody = body,
                valBindDoc = Nothing,
                valBindAttrs = mempty,
                valBindLocation = mempty
              }

      traceM $ "fun_ts: " <> pretty fun_ts
      traceM $ "var_params: " <> pretty var_params
      traceM $ "body: " <> pretty body

      modify $ \senv -> senv {sEnvAutoMapIntrinsics = M.insert f vb (sEnvAutoMapIntrinsics senv)}
      -- env <- transformValBind vb
      -- modify (env <>)
      pure f'
  | otherwise = pure f
  where
    primToType = (Scalar . Prim)
    primToTypes = map (Scalar . Prim)

    mPrimToType _ (Just pt) = primToType pt
    mPrimToType tv Nothing = tv

    mkParams :: [StructType] -> AutoMapM [(Exp, Pat)]
    mkParams pts =
      forM pts $ \t -> do
        x <- newNameFromString "x"
        pure
          ( Var (qualName x) (Info $ fromStruct t) mempty,
            Id x (Info $ fromStruct t) mempty
          )

    makeRetParamTypes :: VName -> AutoMapM ([TypeParam], [(Exp, Pat)], StructRetType)
    makeRetParamTypes fn =
      case intrinsics M.! fn of
        IntrinsicMonoFun pts rt -> do
          var_params <- mkParams $ primToTypes pts
          pure (mempty, var_params, RetType [] $ primToType rt)
        IntrinsicOverloadedFun _ pts rt -> do
          vn <- newNameFromString "t"
          let tv = Scalar $ TypeVar mempty Nonunique (QualName [] vn) []
              pts' = map (mPrimToType tv) pts
              rt' = RetType [] $ toStruct $ mPrimToType tv rt
          var_params <- mkParams pts'
          let tps
                | any isNothing pts = [TypeParamType Unlifted vn mempty]
                | otherwise = []
          pure (tps, var_params, rt')
        IntrinsicPolyFun tps pts ret -> do
          var_params <- mkParams pts
          pure (tps, var_params, ret)
        IntrinsicType {} -> error $ "makeRetParamTypes: " <> pretty f
        IntrinsicEquality -> do
          vn <- newNameFromString "t"
          let tv = Scalar $ TypeVar mempty Nonunique (QualName [] vn) []
              tp = TypeParamType Unlifted vn mempty
          var_params <- mkParams [tv, tv]
          pure ([tp], var_params, RetType [] $ Scalar $ Prim $ Bool)

addDec :: Dec -> AutoMapM ()
addDec dec = modify $ \s ->
  s
    { sEnvDecs = sEnvDecs s ++ [dec]
    }

transformDecs :: [Dec] -> AutoMapM ()
transformDecs [] = pure ()
transformDecs (ValDec valbind : ds) = do
  e <- transformExp $ valBindBody valbind
  addDec $ ValDec $ valbind {valBindBody = e}
  transformDecs ds
transformDecs (td@TypeDec {} : ds) = do
  addDec td
  transformDecs ds

transformProg :: MonadFreshNames m => [Dec] -> m [Dec]
transformProg decs =
  modifyNameSource $ \namesrc ->
    runAutoMapM namesrc $ transformDecs decs
