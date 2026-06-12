{-# LANGUAGE RebindableSyntax #-}

module Numeric.GradInf.Inference.Base where

import Foreign.Storable (Storable)
import Prelude hiding (flip)

import Numeric.GradInf.Coupling
import Numeric.GradInf.Primitives.DeterministicPrimitives

-- Base type transform --

newtype Base d t = Base t
    deriving (Show, Storable)

instance
    (DeterministicPrimitives d i b mat, forall t. GetIndex t i) =>
    DeterministicPrimitives (Base d d) (Base d i) (Base d b) (Base mat mat)

-- Helper functions for below definitions --

mapBase1 :: (t1 -> t2) -> Base d1 t1 -> Base d2 t2
mapBase1 f (Base x) = Base (f x)

mapBase2 ::
    (t1 -> t2 -> t) -> Base d1 t1 -> Base d2 t2 -> Base d t
mapBase2 f (Base x) (Base y) = Base (f x y)

-- Support deterministic ops --

instance (Num a) => Num (Base d a) where
    (+) = mapBase2 (+)
    (-) = mapBase2 (-)
    (*) = mapBase2 (*)
    negate = mapBase1 negate
    abs = mapBase1 abs
    signum = mapBase1 signum
    fromInteger x = (Base (fromIntegral x))

instance (Eq a) => Eq (Base d a) where
    (Base x) == (Base x') = (x == x')

instance (Ord a) => Ord (Base d a) where
    compare (Base x) (Base x') = compare x x'

instance (Real a) => Real (Base d a) where
    toRational (Base x) = toRational x

instance (Enum a) => Enum (Base d a) where
    succ = mapBase1 succ
    pred = mapBase1 pred
    toEnum = Base . toEnum
    fromEnum (Base x) = fromEnum x

instance (Integral a) => Integral (Base d a) where
    toInteger (Base x) = toInteger x
    quot = mapBase2 quot
    rem = mapBase2 rem
    div = mapBase2 div
    mod = mapBase2 mod
    quotRem (Base x) (Base x') = (Base q, Base r)
        where
            (q, r) = quotRem x x'

instance (Fractional a) => Fractional (Base d a) where
    recip = mapBase1 recip
    fromRational = Base . fromRational
    (/) = mapBase2 (/)

instance (Floating a) => Floating (Base d a) where
    pi = Base pi
    exp = mapBase1 exp
    log = mapBase1 log
    sqrt = mapBase1 sqrt
    (**) = mapBase2 (**)
    sin = mapBase1 sin
    cos = mapBase1 cos
    tan = mapBase1 tan
    asin = mapBase1 asin
    acos = mapBase1 acos
    atan = mapBase1 atan
    sinh = mapBase1 sinh
    cosh = mapBase1 cosh
    tanh = mapBase1 tanh
    asinh = mapBase1 asinh
    acosh = mapBase1 acosh
    atanh = mapBase1 atanh

instance (ToInt t1 t2) => ToInt (Base d t1) (Base d t2) where
    toInt = mapBase1 toInt

instance (ToDouble t1 t2) => ToDouble (Base d t1) (Base d t2) where
    toDouble = mapBase1 toDouble

instance (Pow t1 t2 t3) => Pow (Base d t1) (Base d t2) (Base d t3) where
    pow = mapBase2 pow

instance (GetIndex t i) => GetIndex t (Base d i) where
    getIndex xs (Base i) = getIndex xs i

instance (IsGreater t b) => IsGreater (Base d t) (Base d b) where
    isGreater = mapBase2 isGreater

instance (FromDouble d) => FromDouble (Base d d) where
    fromDouble = Base . fromDouble

instance (FromInt i) => FromInt (Base d i) where
    fromInt = Base . fromInt

instance (FromBool b) => FromBool (Base d b) where
    fromBool = Base . fromBool

instance (FromMatrix mat) => FromMatrix (Base mat mat) where
    fromMatrix = Base . fromMatrix

instance (ExtractBool b) => ExtractBool (Base d b) where
    extractBool (Base x) = extractBool x

instance (ExtractDouble d) => ExtractDouble (Base d d) where
    extractDouble (Base x) = extractDouble x

instance (SafeDiv d) => SafeDiv (Base d d) where
    safeDiv = mapBase2 safeDiv

instance (SafeMul d) => SafeMul (Base d d) where
    safeMul = mapBase2 safeMul

instance (MatrixToList mat d) => MatrixToList (Base mat mat) (Base d d) where
    matrixToList (Base mat) = map Base (matrixToList mat)

instance (ListToMatrix mat d) => ListToMatrix (Base mat mat) (Base d d) where
    listToMatrix ds = Base (listToMatrix (map (\(Base x) -> x) ds))

instance (MatrixMultiply mat) => MatrixMultiply (Base mat mat) where
    matrixMultiply = mapBase2 matrixMultiply

instance (MatrixSum mat d) => MatrixSum (Base mat mat) (Base d d) where
    matrixSum = mapBase1 matrixSum

instance (MatrixLength mat) => MatrixLength (Base mat mat) where
    matrixLength (Base mat) = matrixLength mat

instance (MatrixMap mat) => MatrixMap (Base mat mat) where
    matrixMap f = mapBase1 (matrixMap f)

instance (MatrixZipWith mat) => MatrixZipWith (Base mat mat) where
    matrixZipWith f = mapBase2 (matrixZipWith f)

instance (BranchingConstructs b t) =>
    BranchingConstructs (Base d b) (Base d t)
    where
    ifThenElse (Base b) (Base t1) (Base t2) = Base (ifThenElse b t1 t2)

instance
    (BranchingConstructs (Base d b) tt) =>
    BranchingConstructs (Base d b) [tt]
    where
    ifThenElse b ts1 ts2 = zipWith (ifThenElse b) ts1 ts2

instance (BranchingConstructs b t) => BranchingConstructs (Base d b) (Coupled (Base d t))
    where
    ifThenElse b (Coupled (t1A, t1B)) (Coupled (t2A, t2B)) = Coupled (ifThenElse b t1A t2A, ifThenElse b t1B t2B)

