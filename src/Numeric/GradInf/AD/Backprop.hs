{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Numeric.GradInf.AD.Backprop (Reifies, BVar, W, module Numeric.GradInf.AD.Backprop) where

import Prelude hiding (flip, (<>))

import Numeric.Backprop
import Numeric.Backprop.Class
import Numeric.Backprop.Internal qualified as Internal (
    BRef (..),
    BVar (..),
    _bvVal,
 )
import Numeric.LinearAlgebra hiding (fromInt, toInt)
import Data.Vector.Storable qualified as VS

import Numeric.GradInf.Coupling
import Numeric.GradInf.Primitives.DeterministicPrimitives

-- Base type transform --

-- BVar type defined by backprop
-- TODO: avoid orphans, via e.g. a wrapper type

instance Backprop Bool where
    zero = const False
    add = const (const False)
    one = error "Cannot construct non-zero gradient for booleans"

instance (Show a) => Show (BVar s a) where
    -- TODO: add primal information
    show (Internal.BV Internal.BRC x) = "BVar (BRC) " ++ show x
    show (Internal.BV _ x) = "BVar " ++ show x

instance
    (Reifies s W) =>
    DeterministicPrimitives (BVar s Double) (BVar s Int) (BVar s Bool) (BVar s (Matrix Double))

-- Helper functions for below definitions --

pureBVar :: t -> BVar s t
pureBVar = auto

extractBVar :: forall s t. (Reifies s W) => BVar s t -> t
extractBVar = Internal._bvVal

mapBVar1 ::
    (Reifies s W, Backprop t1, Backprop t2) => (t1 -> t2) -> BVar s t1 -> BVar s t2
mapBVar1 f = liftOp1 . op1 $ \x -> (f x, const (zero x))

mapBVar2 ::
    (Reifies s W, Backprop t1, Backprop t2) =>
    (t1 -> t2 -> t) ->
    BVar s t1 ->
    BVar s t2 ->
    BVar s t
mapBVar2 f = liftOp2 . op2 $ \x1 x2 -> (f x1 x2, const (zero x1, zero x2))

instance (Real a, Reifies s W) => Real (BVar s a) where
    toRational = toRational . extractBVar

instance (Reifies s W) => Enum (BVar s Int) where
    succ = mapBVar1 succ
    pred = mapBVar1 pred
    toEnum = pureBVar . toEnum
    fromEnum = fromEnum . extractBVar

instance (Reifies s W) => Integral (BVar s Int) where
    toInteger = toInteger . extractBVar
    quot = mapBVar2 quot
    rem = mapBVar2 rem
    div = mapBVar2 div
    mod = mapBVar2 mod

    quotRem x y = error "quotRem: not implemented for BVar"

instance FromBool (BVar s Bool) where
    fromBool = pureBVar

instance (Reifies s W) => ExtractBool (BVar s Bool) where
    extractBool = extractBVar

instance (Reifies s W) => ExtractDouble (BVar s Double) where
    extractDouble = extractBVar

instance FromDouble (BVar s Double) where
    fromDouble = pureBVar

instance FromInt (BVar s Int) where
    fromInt = pureBVar

instance FromMatrix (BVar s (Matrix Double)) where
    fromMatrix = pureBVar

instance (Reifies s W) => ToInt (BVar s Bool) (BVar s Int) where
    toInt = mapBVar1 toInt

instance (Reifies s W) => ToInt (BVar s Double) (BVar s Int) where
    toInt = mapBVar1 toInt

instance (Reifies s W) => ToDouble (BVar s Int) (BVar s Double) where
    toDouble = mapBVar1 toDouble

instance (Reifies s W) => Pow (BVar s Double) (BVar s Int) (BVar s Double) where
    pow = mapBVar2 pow

instance (Reifies s W) => GetIndex tt (BVar s Int) where
    getIndex xs i = getIndex xs (extractBVar i)

instance (Reifies s W) => IsGreater (BVar s Double) (BVar s Bool) where
    isGreater xBVar yBVar = do
        let x = extractBVar xBVar
        let y = extractBVar yBVar
        if x /= y
            then
                pureBVar (isGreater x y)
            else case xBVar of
                (Internal.BV Internal.BRC _) -> pureBVar False
                _ -> pureBVar True

instance (Reifies s W) => IsGreater (BVar s Int) (BVar s Bool) where
    isGreater = mapBVar2 isGreater

instance (Reifies s W) => SafeDiv (BVar s Double) where
    safeDiv = mapBVar2 safeDiv

instance (Reifies s W) => SafeMul (BVar s Double) where
    safeMul xBVar yBVar = do
        if extractBVar xBVar == 0 && extractBVar yBVar == 0
            then pureBVar 0
            -- Use regular multiply here so that we can rely on the already defined rule.
            else xBVar * yBVar

instance Backprop (Matrix Double) where
    zero = zeroNum
    add = addNum
    one = oneNum

instance (Reifies s W) => MatrixToList (BVar s (Matrix Double)) (BVar s Double) where
    matrixToList bVarMat = do
       let bVarX :: BVar s [Double] = (liftOp1 . op1 $ \mat ->
            (matrixToList mat, listToMatrix)) bVarMat
       sequenceVar bVarX

instance (Reifies s W) => ListToMatrix (BVar s (Matrix Double)) (BVar s Double) where
    listToMatrix listBVar = do
        let bVarList = collectVar listBVar
        let bVarMat :: BVar s (Matrix Double) = (liftOp1 . op1 $ \list ->
                (listToMatrix list, matrixToList)) bVarList
        bVarMat

-- based on https://github.com/mstksg/hmatrix-backprop/blob/91661e0f02012146aa7be323d05c27921fb16c72/src/Numeric/LinearAlgebra/Static/Backprop.hs#L752
instance (Reifies s W) => MatrixMultiply (BVar s (Matrix Double))  where
    matrixMultiply = liftOp2 . op2 $ \x y ->
        ( x <> y
        , \d -> (d <> tr y, tr x <> d)
        )

-- based on https://github.com/mstksg/hmatrix-backprop/blob/91661e0f02012146aa7be323d05c27921fb16c72/src/Numeric/LinearAlgebra/Static/Backprop.hs#L1165
instance (Reifies s W) => MatrixLength (BVar s (Matrix Double)) where
    matrixLength = matrixLength . extractBVar

instance (Reifies s W) => MatrixSum (BVar s (Matrix Double)) (BVar s Double) where
    matrixSum = liftOp1 . op1 $ \x ->
        ( sumElements x
        , \d -> konst d (size x)
        )

-- based on https://github.com/mstksg/hmatrix-backprop/blob/91661e0f02012146aa7be323d05c27921fb16c72/src/Numeric/LinearAlgebra/Static/Backprop.hs#L901C1-L901C5
instance (Reifies s W) => MatrixMap (BVar s (Matrix Double)) where
    matrixMap f = do
        let fDeriv :: Double -> Double = snd . backprop f
        (liftOp1 . op1 $ \mat -> do
                let matOut = cmap f mat
                let matDerivs = cmap fDeriv mat
                ( matOut
                    , \d -> reshape (cols mat) (VS.zipWith (*) (flatten d) (flatten matDerivs))
                    )
            )

-- based on https://github.com/mstksg/hmatrix-backprop/blob/91661e0f02012146aa7be323d05c27921fb16c72/src/Numeric/LinearAlgebra/Static/Backprop.hs#L979
instance (Reifies s W) => MatrixZipWith (BVar s (Matrix Double)) where
    matrixZipWith f = do
        let fDeriv :: Double -> Double -> (Double, Double) = \x y -> snd (backprop2 f x y)
        (liftOp2 . op2 $ \mat1 mat2 -> do
                let matOut = reshape (cols mat1) (VS.zipWith f (flatten mat1) (flatten mat2))
                let matDerivs1 = reshape (cols mat1) (VS.zipWith (\x y -> fst (fDeriv x y)) (flatten mat1) (flatten mat2))
                let matDerivs2 = reshape (cols mat1) (VS.zipWith (\x y -> snd (fDeriv x y)) (flatten mat1) (flatten mat2))
                ( matOut
                    , \d -> (reshape (cols mat1) (VS.zipWith (*) (flatten d) (flatten matDerivs1)), reshape (cols mat2) (VS.zipWith (*) (flatten d) (flatten matDerivs2)))
                    )
            )

-- TODO: reimplement more simply by extracting the boolean via extractBVar and then calling the standard ifThenElse
instance (Reifies s W, Backprop t) => BranchingConstructs (BVar s Bool) (BVar s t) where
    ifThenElse = liftOp3 . op3 $ \x1 x2 x3 ->
        ( ifThenElse x1 x2 x3
        , \dy -> (zero x1, if x1 then dy else (zero x2), if x1 then (zero x3) else dy)
        )

instance
    (Reifies s W, Backprop t) =>
    BranchingConstructs (BVar s Bool) (Coupled (BVar s t))
    where
    ifThenElse b (Coupled (t1A, t1B)) (Coupled (t2A, t2B)) = Coupled (ifThenElse b t1A t2A, ifThenElse b t1B t2B)

instance
    (BranchingConstructs (BVar s Bool) tt) =>
    BranchingConstructs (BVar s Bool) [tt]
    where
    ifThenElse b xs ys = zipWith (ifThenElse b) xs ys
