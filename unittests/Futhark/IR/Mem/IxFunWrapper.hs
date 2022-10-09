-- | Perform index function operations in both algebraic and LMAD
-- representations.
module Futhark.IR.Mem.IxFunWrapper
  ( IxFun,
    iota,
    permute,
    reshape,
    coerce,
    slice,
    flatSlice,
    rebase,
  )
where

import Futhark.IR.Mem.IxFun qualified as I
import Futhark.IR.Mem.IxFun.Alg qualified as IA
import Futhark.IR.Syntax (FlatSlice, Slice)
import Futhark.Util.IntegralExp

type Shape num = [num]

type Permutation = [Int]

type IxFun num = (I.IxFun num, IA.IxFun num)

iota ::
  IntegralExp num =>
  Shape num ->
  IxFun num
iota x = (I.iota x, IA.iota x)

permute ::
  IntegralExp num =>
  IxFun num ->
  Permutation ->
  IxFun num
permute (l, a) x = (I.permute l x, IA.permute a x)

rotate ::
  IntegralExp num =>
  IxFun num ->
  Indices num ->
  IxFun num
rotate (l, a) x = (I.rotate l x, IA.rotate a x)

reshape ::
  IntegralExp num =>
  IxFun num ->
  Shape num ->
  IxFun num
reshape (l, a) x = (I.reshape l x, IA.reshape a x)

coerce ::
  IntegralExp num =>
  IxFun num ->
  Shape num ->
  IxFun num
coerce (l, a) x = (I.coerce l x, IA.coerce a x)

slice ::
  IntegralExp num =>
  IxFun num ->
  Slice num ->
  IxFun num
slice (l, a) x = (I.slice l x, IA.slice a x)

flatSlice ::
  IntegralExp num =>
  IxFun num ->
  FlatSlice num ->
  IxFun num
flatSlice (l, a) x = (I.flatSlice l x, IA.flatSlice a x)

rebase ::
  IntegralExp num =>
  IxFun num ->
  IxFun num ->
  IxFun num
rebase (l, a) (l1, a1) = (I.rebase l l1, IA.rebase a a1)
