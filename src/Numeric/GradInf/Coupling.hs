module Numeric.GradInf.Coupling where

import Control.Applicative (ZipList (..))
import Control.Monad.Bayes.Sampler.Lazy (Sampler (..))
import Control.Monad.Identity (Identity (..))
import Foreign.Storable (Storable)

import Numeric.GradInf.Primitives.DeterministicPrimitives

-- Core type --

newtype Coupled t = Coupled (t, t)
    deriving (Show, Storable)

left :: Coupled t -> t
left (Coupled (xL, _)) = xL

right :: Coupled t -> t
right (Coupled (_, xR)) = xR

instance
    ( DeterministicPrimitives d i b mat
    , BranchingConstructs (Coupled b) (Coupled i)
    , BranchingConstructs (Coupled b) (Coupled d)
    , BranchingConstructs (Coupled b) (Coupled b)
    , BranchingConstructs (Coupled b) [Coupled d]
    , GetIndex (Coupled i) i
    , GetIndex (Coupled d) i
    , Show i
    , Show d
    , Show b
    ) =>
    DeterministicPrimitives (Coupled d) (Coupled i) (Coupled b) (Coupled mat)

-- Helper functions for below definitions --

pureCoupled :: t -> Coupled t
pureCoupled x = Coupled (x, x)

mapCoupled1 :: (t1 -> t2) -> Coupled t1 -> Coupled t2
mapCoupled1 f (Coupled (x, y)) = Coupled (f x, f y)

mapCoupled2 ::
    (t1 -> t2 -> t) -> Coupled t1 -> Coupled t2 -> Coupled t
mapCoupled2 f (Coupled (x, y)) (Coupled (x', y')) =
    Coupled (f x x', f y y')

-- Support deterministic ops --

instance (Num a) => Num (Coupled a) where
    (+) = mapCoupled2 (+)
    (-) = mapCoupled2 (-)
    (*) = mapCoupled2 (*)
    negate = mapCoupled1 negate
    abs = mapCoupled1 abs
    signum = mapCoupled1 signum
    fromInteger = pureCoupled . fromInteger

instance (Eq a) => Eq (Coupled a) where
    (Coupled (x, y)) == (Coupled (x', y')) = if (x == x') == (x' == y') then x == y else error "(==): multiple values"

instance (Ord a) => Ord (Coupled a) where
    compare (Coupled (x, y)) (Coupled (x', y')) =
        if (compare x x') == (compare y y')
            then compare x x'
            else error "compare: multiple values"

instance (Real a) => Real (Coupled a) where
    toRational (Coupled (x, y)) =
        if x == y
            then toRational x
            else error "toRational: multiple values"

instance (Enum a, Eq a) => Enum (Coupled a) where
    succ = mapCoupled1 succ
    pred = mapCoupled1 pred
    toEnum = pureCoupled . toEnum
    fromEnum (Coupled (x, y)) = if x == y then fromEnum x else error "fromEnum: multiple values"

instance (Integral a) => Integral (Coupled a) where
    toInteger (Coupled (x, y)) =
        if x == y
            then toInteger x
            else error "toInteger: multiple values"
    quot = mapCoupled2 quot
    rem = mapCoupled2 rem
    div = mapCoupled2 div
    mod = mapCoupled2 mod
    quotRem (Coupled (x, y)) (Coupled (x', y')) = (Coupled (q, q'), Coupled (r, r'))
      where
        (q, r) = quotRem x x'
        (q', r') = quotRem y y'

instance (Fractional a) => Fractional (Coupled a) where
    recip = mapCoupled1 recip
    fromRational = pureCoupled . fromRational
    (/) = mapCoupled2 (/)

instance (Floating a) => Floating (Coupled a) where
    pi = pureCoupled pi
    exp = mapCoupled1 exp
    log = mapCoupled1 log
    sqrt = mapCoupled1 sqrt
    (**) = mapCoupled2 (**)
    sin = mapCoupled1 sin
    cos = mapCoupled1 cos
    tan = mapCoupled1 tan
    asin = mapCoupled1 asin
    acos = mapCoupled1 acos
    atan = mapCoupled1 atan
    sinh = mapCoupled1 sinh
    cosh = mapCoupled1 cosh
    tanh = mapCoupled1 tanh
    asinh = mapCoupled1 asinh
    acosh = mapCoupled1 acosh
    atanh = mapCoupled1 atanh

instance (ToInt t1 t2) => ToInt (Coupled t1) (Coupled t2) where
    toInt = mapCoupled1 toInt

instance (ToDouble t1 t2) => ToDouble (Coupled t1) (Coupled t2) where
    toDouble = mapCoupled1 toDouble

instance (Pow t1 t2 t3) => Pow (Coupled t1) (Coupled t2) (Coupled t3) where
    pow = mapCoupled2 pow

instance
    (GetIndex (Coupled t) i, Num i, Show i, Ord i) =>
    GetIndex (Coupled t) (Coupled i)
    where
    getIndex xs (Coupled (iA, iB)) =
        if (iA < 0)
            then error ("Negative index iA = " ++ (show iA))
            else
                if (iB < 0)
                    then error ("Negative index iB = " ++ (show iB))
                    else
                        let
                            (Coupled (xA, _)) = getIndex xs iA
                            (Coupled (_, xB)) = getIndex xs iB
                         in
                            Coupled (xA, xB)

instance (IsGreater t b) => IsGreater (Coupled t) (Coupled b) where
    isGreater = mapCoupled2 isGreater

instance (FromDouble d) => FromDouble (Coupled d) where
    fromDouble = pureCoupled . fromDouble

instance (FromInt i) => FromInt (Coupled i) where
    fromInt = pureCoupled . fromInt

instance (FromBool b) => FromBool (Coupled b) where
    fromBool = pureCoupled . fromBool

instance (FromMatrix mat) => FromMatrix (Coupled mat) where
    fromMatrix = pureCoupled . fromMatrix

instance (ExtractBool b) => ExtractBool (Coupled b) where
    extractBool (Coupled (x, y)) =
        if (extractBool x) == (extractBool y)
            then extractBool x
            else error "extractBool: multiple values"

instance (ExtractDouble d) => ExtractDouble (Coupled d) where
    extractDouble (Coupled (x, y)) =
        if (extractDouble x) == (extractDouble y)
            then extractDouble x
            -- This is unsafe in the general case; should warn users about correct usage of this construct.
            else extractDouble x

instance (SafeDiv d) => SafeDiv (Coupled d) where
    safeDiv = mapCoupled2 safeDiv

instance (SafeMul d) => SafeMul (Coupled d) where
    safeMul = mapCoupled2 safeMul

instance (MatrixToList mat d) => MatrixToList (Coupled mat) (Coupled d) where
    matrixToList (Coupled (matA, matB)) = map Coupled (zip (matrixToList matA) (matrixToList matB))

instance (ListToMatrix mat d) => ListToMatrix (Coupled mat) (Coupled d) where
    listToMatrix listCoupled = do
        let listA = map (\(Coupled (xA, _)) -> xA) listCoupled
        let listB = map (\(Coupled (_, xB)) -> xB) listCoupled
        Coupled (listToMatrix listA, listToMatrix listB)

instance (MatrixMultiply mat) => MatrixMultiply (Coupled mat) where
    matrixMultiply = mapCoupled2 matrixMultiply

instance (MatrixSum mat d) => MatrixSum (Coupled mat) (Coupled d) where
    matrixSum = mapCoupled1 matrixSum

instance (MatrixLength mat) => MatrixLength (Coupled mat) where
    matrixLength (Coupled (matA, matB)) =
        if lenA == lenB then lenA else error "matrixLength: multiple values"
      where
        lenA = matrixLength matA
        lenB = matrixLength matB

instance (MatrixMap mat) => MatrixMap (Coupled mat) where
    matrixMap f (Coupled (matA, matB)) = do
        let matA' = matrixMap f matA
        let matB' = matrixMap f matB
        Coupled (matA', matB')

instance (MatrixZipWith mat) => MatrixZipWith (Coupled mat) where
    matrixZipWith f (Coupled (mat1A, mat1B)) (Coupled (mat2A, mat2B)) = do
        let matA' = matrixZipWith f mat1A mat2A
        let matB' = matrixZipWith f mat1B mat2B
        Coupled (matA', matB')

-- Merge helper --

instance Mergeable a (Coupled a) where
    merge (Coupled (x, _)) (Coupled (_, y)) = Coupled (x, y)

instance (Mergeable a aa, Mergeable b bb) => Mergeable (a -> b) (aa -> bb) where
    merge f g x = merge (f x) (g x)

-- The Sampler restriction is necessary because type constructors match general MonadDistribution's,
-- causing conflict with the Mergeable (Coupled a) instance.
instance (Mergeable a aa) => Mergeable (Sampler a) (Sampler aa) where
    merge mx my = merge <$> mx <*> my

instance (Mergeable a aa) => Mergeable (Identity a) (Identity aa) where
    merge mx my = merge <$> mx <*> my

instance (Mergeable a aa, Mergeable b bb) => Mergeable (a, b) (aa, bb) where
    merge (x1, x2) (y1, y2) = (merge x1 y1, merge x2 y2)

instance (Mergeable a aa, Mergeable b bb, Mergeable c cc) => Mergeable (a, b, c) (aa, bb, cc) where
    merge (x1, x2, x3) (y1, y2, y3) = (merge x1 y1, merge x2 y2, merge x3 y3)

instance (Mergeable a aa) => Mergeable [a] [aa] where
    merge xs ys = do
        if length xs /= length ys
            then
                error "merge: cannot merge lists of unequal lengths"
            else
                map (\(x, y) -> merge x y) (zip xs ys)

instance (Mergeable a aa) => Mergeable (ZipList a) (ZipList aa) where
    merge (ZipList xs) (ZipList ys) = ZipList (merge xs ys)

instance (Mergeable a aa, BranchingConstructs b aa) => BranchingConstructs (Coupled b) aa where
    ifThenElse (Coupled (bA, bB)) t1 t2 = ifThenElse bA
        (ifThenElse bB t1 (merge t1 t2))
        (ifThenElse bB (merge t2 t1) t2)