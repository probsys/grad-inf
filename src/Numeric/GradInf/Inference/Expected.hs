{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE InstanceSigs #-}

module Numeric.GradInf.Inference.Expected where

import Foreign.Storable (Storable)
import Prelude hiding (flip)

import Numeric.GradInf.Coupling
import Numeric.GradInf.Inference.SMC.SMCTypes
import Numeric.GradInf.Primitives.DeterministicPrimitives

-- Base type transform --

newtype Expected d t = Expected d
    deriving (Show, Storable)

instance
    (DeterministicPrimitives d i b mat) =>
    DeterministicPrimitives (Expected d d) (Expected d i) (Expected d b) (Expected mat mat)

-- Helper functions for below definitions --

mapExpected1 :: (d1 -> d2) -> Expected d1 t1 -> Expected d2 t2
mapExpected1 f (Expected x) = Expected (f x)

mapExpected2 ::
    (d1 -> d2 -> d) -> Expected d1 t1 -> Expected d2 t2 -> Expected d t
mapExpected2 f (Expected x) (Expected y) = Expected (f x y)

-- Support deterministic ops --

instance (Num d) => Num (Expected d a) where
    (+) = mapExpected2 (+)
    (-) = mapExpected2 (-)
    (*) = mapExpected2 (*)
    negate = mapExpected1 negate
    abs = mapExpected1 abs
    signum = mapExpected1 signum
    fromInteger x = (Expected (fromIntegral x))

instance (Eq d) => Eq (Expected d a) where
    -- TODO: we should probably error more aggressively here,
    -- but for now I've written the below to ensure the right
    -- behaviour in the special case where x and x' are "exact"
    -- approximations, e.g. created via pureExpected
    (Expected x) == (Expected x') = (x == x')

instance (Ord d) => Ord (Expected d a) where
    compare (Expected x) (Expected x') = compare x x'

instance (Real d) => Real (Expected d a) where
    toRational (Expected x) = error "toRational: unsupported for Expected"

instance Enum (Expected d a) where
    succ = error "succ: unsupported for Expected"
    pred = error "pred: unsupported for Expected"
    toEnum = error "toEnum: unsupported for Expected"
    fromEnum (Expected x) = error "fromEnum: unsupported for Expected"

instance (Real d) => Integral (Expected d a) where
    toInteger _ = error "toInteger: unsupported for Expected"
    quot = error "quot: unsupported for Expected"
    rem = error "rem: unsupported for Expected"
    div = error "div: unsupported for Expected"
    mod = error "mod: unsupported for Expected"
    quotRem (Expected x) (Expected x') = error "quotRem: unsupported for Expected"

instance (Fractional d) => Fractional (Expected d a) where
    recip = mapExpected1 recip
    fromRational = Expected . fromRational
    (/) = mapExpected2 (/)

instance (Floating d) => Floating (Expected d a) where
    pi = Expected pi
    exp = mapExpected1 exp
    log = mapExpected1 log
    sqrt = mapExpected1 sqrt
    (**) = mapExpected2 (**)
    sin = mapExpected1 sin
    cos = mapExpected1 cos
    tan = mapExpected1 tan
    asin = mapExpected1 asin
    acos = mapExpected1 acos
    atan = mapExpected1 atan
    sinh = mapExpected1 sinh
    cosh = mapExpected1 cosh
    tanh = mapExpected1 tanh
    asinh = mapExpected1 asinh
    acosh = mapExpected1 acosh
    atanh = mapExpected1 atanh

instance (ToInt t1 t2) => ToInt (Expected d t1) (Expected d t2) where
    toInt = mapExpected1 id

instance (ToDouble t1 t2) => ToDouble (Expected d t1) (Expected d t2) where
    toDouble = mapExpected1 id

instance
    (Floating d, Pow t1 t2 t3) =>
    Pow (Expected d t1) (Expected d t2) (Expected d t3)
    where
    -- even though pow represents integer exponentiation,
    -- the mean-field version uses floating point exponentation.
    pow = mapExpected2 (**)

instance (ExtractDouble d, Show d) => GetIndex t (Expected d i) where
    -- TODO: Handle more general cases. may need to add "round" to DeterministicPrimitives
    getIndex xs (Expected i) = do
        let iDouble = extractDouble i
        if iDouble == fromInteger (round iDouble)
            then do
                getIndex xs ((fromIntegral (round iDouble)) :: Int)
            else
                error ("getIndex: unsupported by Expected (i = " ++ (show i) ++ ")")

instance
    (Num d, IsGreater d b, BranchingConstructs b d) =>
    IsGreater (Expected d t) (Expected d b)
    where
    isGreater (Expected x) (Expected y) = Expected (if ((isGreater x y) :: b) then 1 else 0)

instance (FromDouble d) => FromDouble (Expected d d) where
    fromDouble = Expected . fromDouble

instance (DeterministicPrimitives d i b mat) => FromInt (Expected d i) where
    fromInt x = Expected (toDouble (fromInt x :: i))

instance (DeterministicPrimitives d i b mat) => FromBool (Expected d b) where
    fromBool b = if b then (Expected 1) else (Expected 0)

instance (FromMatrix mat) => FromMatrix (Expected mat mat) where
    fromMatrix = Expected . fromMatrix

instance (Num d, Eq d) => ExtractBool (Expected d b) where
    extractBool (Expected x) =
        if x == 1
            then True
            else
                ( if x == 0
                    then False
                    else error ("extractBool: unsupported by Expected for x not 0 or 1")
                )

instance (ExtractDouble d) => ExtractDouble (Expected d b) where
    extractDouble (Expected x) = extractDouble x

instance (SafeDiv d) => SafeDiv (Expected d d) where
    safeDiv = mapExpected2 safeDiv

instance (SafeMul d) => SafeMul (Expected d d) where
    safeMul = mapExpected2 safeMul

instance (MatrixToList mat d) => MatrixToList (Expected mat mat) (Expected d d) where
    matrixToList (Expected mat) = map Expected (matrixToList mat)

instance (ListToMatrix mat d) => ListToMatrix (Expected mat mat) (Expected d d) where
    listToMatrix ds = Expected (listToMatrix (map (\(Expected x) -> x) ds))

instance (MatrixMultiply mat) => MatrixMultiply (Expected mat mat) where
    matrixMultiply = mapExpected2 matrixMultiply

instance (MatrixSum mat d) => MatrixSum (Expected mat mat) (Expected d d) where
    matrixSum = mapExpected1 matrixSum

instance (MatrixLength mat) => MatrixLength (Expected mat mat) where
    matrixLength (Expected mat) = matrixLength mat

instance (MatrixMap mat) => MatrixMap (Expected mat mat) where
    matrixMap f = mapExpected1 (matrixMap f)

instance (MatrixZipWith mat) => MatrixZipWith (Expected mat mat) where
    matrixZipWith f = mapExpected2 (matrixZipWith f)

instance
    (DeterministicPrimitives d i b mat) =>
    BranchingConstructs (Expected d b) (Expected d t)
    where
    ifThenElse (Expected b) (Expected t1) (Expected t2) = do
        let safeEquals :: d -> d -> b
            safeEquals v c =
                ( if (isGreater v c) :: b
                    then (fromBool False)
                    else (if (isGreater c v) :: b then (fromBool False) else (fromBool True))
                ) ::
                    b
        if (extractBool (safeEquals b 1))
            then
                Expected t1
            else
                if (extractBool (safeEquals b 0))
                    then
                        Expected t2
                    else
                        Expected (b * t1 + (1 - b) * t2)

instance
    (BranchingConstructs (Expected d b) tt) =>
    BranchingConstructs (Expected d b) [tt]
    where
    ifThenElse b ts1 ts2 = zipWith (ifThenElse b) ts1 ts2

instance
    (DeterministicPrimitives d i b mat) =>
    BranchingConstructs (Expected d b) (Coupled (Expected d t))
    where
    ifThenElse b (Coupled (t1A, t1B)) (Coupled (t2A, t2B)) = Coupled (ifThenElse b t1A t2A, ifThenElse b t1B t2B)

