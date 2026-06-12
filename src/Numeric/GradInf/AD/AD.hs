{-# OPTIONS_GHC -Wno-orphans #-}

module Numeric.GradInf.AD.AD (Forward, AD (..), module Numeric.GradInf.AD.AD) where

import Prelude hiding (flip)

import Numeric.AD.Internal.Forward (Forward (..), primal)
import Numeric.AD.Internal.Type (AD (..))
import Numeric.LinearAlgebra hiding (fromInt, toInt)

import Data.Vector.Storable qualified as VS

import Numeric.GradInf.Primitives.DeterministicPrimitives

-- Base type transform --

-- Forward type defined by AD
-- TODO: avoid orphans, via e.g. a wrapper type

instance DeterministicPrimitives (Forward Double) (Forward Int) (Forward Bool) (Forward (Matrix Double))
instance
    DeterministicPrimitives
        (AD s (Forward Double))
        (AD s (Forward Int))
        (AD s (Forward Bool))
        (AD s (Forward (Matrix Double)))

-- Helper functions for below definitions --

pureForward :: t -> Forward t
pureForward = Lift

extractForward :: Forward t -> t
extractForward (Lift x) = x
extractForward _ = error "extractForward: non-zero tangent not supported"

-- A version of extractForward that also handles Dual numbers
-- whose dual component is floating point zero.
extractForwardNum :: (Num t, Eq t, Show t) => Forward t -> t
extractForwardNum (Lift x) = x
extractForwardNum (Forward x x') = if x' == (x * 0) then x else error ("x': " ++ show x' ++ ", x * 0: " ++ show (x * 0))
extractForwardNum Zero = 0

mapForward1 :: (t1 -> t2) -> Forward t1 -> Forward t2
mapForward1 f (Lift x) = Lift (f x)
mapForward1 f _ = error "mapForward1: non-zero tangent not supported"

mapForward2 ::
    (t1 -> t2 -> t) -> Forward t1 -> Forward t2 -> Forward t
mapForward2 f (Lift x) (Lift y) = Lift (f x y)
mapForward2 f _ _ = error "mapForward2: non-zero tangent not supported"

tangentNum :: (Num t) => Forward t -> t
tangentNum (Lift x) = x * 0
tangentNum (Forward _ x') = x'
tangentNum Zero = 0

-- Support deterministic ops --

instance Integral (Forward Int) where
    toInteger = toInteger . extractForward
    quot = mapForward2 quot
    rem = mapForward2 rem
    div = mapForward2 div
    mod = mapForward2 mod
    quotRem x y = do
        let (a, b) = quotRem (extractForward x) (extractForward y)
        (Lift a, Lift b)

deriving instance (Integral i) => Integral (AD s i)

instance FromBool (Forward Bool) where
    fromBool = pureForward

instance (FromBool b) => FromBool (AD s b) where
    fromBool = AD . fromBool

instance ExtractBool (Forward Bool) where
    extractBool = extractForward

instance (ExtractBool b) => ExtractBool (AD s b) where
    extractBool = extractBool . runAD

instance ExtractDouble (Forward Double) where
    -- A user using extractDouble should be aware that it discards derivative info.
    extractDouble xForward = primal xForward

instance (ExtractDouble d) => ExtractDouble (AD s d) where
    extractDouble = extractDouble . runAD

instance SafeDiv (Forward Double) where
    safeDiv xForward yForward = do
        let (x, x') = (primal xForward, tangentNum xForward)
        let (y, y') = (primal yForward, tangentNum yForward)
        if x == 0 && y == 0
            then
                Forward (x' / y') 0
            else
                -- Use the direct dual-number rule since ad uses the indirect definition x * recip y
                -- (see https://github.com/ekmett/ad/blob/9b92002e7e111301055c9b062e33cde694755595/include/instances.h#L34)
                -- which introduces unnecessary floating point error.
                Forward (x / y) ((x' * y - x * y') / (y * y))

instance (SafeDiv d) => SafeDiv (AD s d) where
    safeDiv x y = AD (safeDiv (runAD x) (runAD y))

instance SafeMul (Forward Double) where
    safeMul (Forward x x') (Forward y y') = do
        if x == 0 && y == 0
            then
                Lift 0
            else
                (Forward x x') * (Forward y y')
    safeMul x y = x * y

instance (SafeMul d) => SafeMul (AD s d) where
    safeMul x y = AD (safeMul (runAD x) (runAD y))

instance FromDouble (Forward Double) where
    fromDouble = pureForward

instance (FromDouble d) => FromDouble (AD s d) where
    fromDouble = AD . fromDouble

instance FromInt (Forward Int) where
    fromInt = pureForward

instance (FromInt i) => FromInt (AD s i) where
    fromInt = AD . fromInt

instance FromMatrix (Forward (Matrix Double)) where
    fromMatrix = pureForward

instance (FromMatrix mat) => FromMatrix (AD s mat) where
    fromMatrix = AD . fromMatrix

instance ToInt (Forward Bool) (Forward Int) where
    toInt = mapForward1 toInt

instance ToInt (Forward Double) (Forward Int) where
    toInt = mapForward1 toInt

instance (ToInt b i) => ToInt (AD s b) (AD s i) where
    toInt = AD . toInt . runAD

instance ToDouble (Forward Int) (Forward Double) where
    toDouble = Lift . toDouble . extractForwardNum

instance (ToDouble i d) => ToDouble (AD s i) (AD s d) where
    toDouble = AD . toDouble . runAD

instance Pow (Forward Double) (Forward Int) (Forward Double) where
    -- TODO: make this definition safe for general inputs
    pow = mapForward2 pow

instance (Pow d i d) => Pow (AD s d) (AD s i) (AD s d) where
    pow x y = AD (pow (runAD x) (runAD y))

instance GetIndex tt (Forward Int) where
    getIndex xs i = getIndex xs (extractForward i)

instance (GetIndex d i) => GetIndex d (AD s i) where
    getIndex xs i = getIndex xs (runAD i)

-- When primal value is 0, we compare the dual value against 0.
-- Useful when combined with SafeDiv, see e.g. dual-number-friendly
-- SMC multinomial resampling.
floatTolerance :: Double = 1e-15
instance IsGreater (Forward Double) (Forward Bool) where
    isGreater (Forward x x') (Forward y y') = Lift (if x /= y then isGreater x y else isGreater x' y')
    isGreater (Forward x x') (Lift y) = Lift (if x /= y then isGreater x y else isGreater x' floatTolerance)
    isGreater (Forward x x') (Zero) = Lift (if x /= 0 then isGreater x floatTolerance else isGreater x' floatTolerance)
    isGreater Zero (Forward y y') = Lift (if y /= 0 then isGreater (-floatTolerance) y else isGreater (-floatTolerance) y')
    isGreater Zero (Lift y) = Lift (isGreater (-floatTolerance) y)
    isGreater Zero Zero = Lift False
    isGreater (Lift x) (Lift y) = Lift (isGreater x y)
    isGreater (Lift x) Zero = Lift (isGreater x floatTolerance)
    isGreater (Lift x) (Forward y y') = Lift (if x /= y then isGreater x y else isGreater (-floatTolerance) y')

instance IsGreater (Forward Int) (Forward Bool) where
    isGreater (Lift x) (Lift y) = Lift (isGreater x y)
    isGreater (Lift x) Zero = Lift (isGreater x 0)
    isGreater Zero (Lift y) = Lift (isGreater 0 y)
    isGreater x y = error "isGreater: non-zero tangent not supported"

instance (IsGreater a b) => IsGreater (AD s a) (AD s b) where
    isGreater x y = AD (isGreater (runAD x) (runAD y))

instance MatrixToList (Forward (Matrix Double)) (Forward Double) where
    matrixToList forwardMat = do
        let mat = primal forwardMat
        let mat' = tangentNum forwardMat
        let vec = matrixToList mat
        let vec' = matrixToList mat'
        if length vec /= length vec'
            then error ("MatrixToList: primal and tangent vectors have different lengths (" ++ show (length vec) ++ " /= " ++ show (length vec') ++ ")")
        else
            map (\(x, x') -> if x' == 0 then Lift x else Forward x x') (zip vec vec')

instance (MatrixToList mat d) => MatrixToList (AD s mat) (AD s d) where
    matrixToList matAD = map AD (matrixToList (runAD matAD))

instance ListToMatrix (Forward (Matrix Double)) (Forward Double) where
    listToMatrix forwardList = do
        let list = map primal forwardList
        let list' = map tangentNum forwardList
        let mat :: Matrix Double = listToMatrix list
        let mat' :: Matrix Double = listToMatrix list'
        Forward mat mat'

instance (ListToMatrix mat d) => ListToMatrix (AD s mat) (AD s d) where
    listToMatrix listAD = AD (listToMatrix (map runAD listAD))

instance MatrixLength (Forward (Matrix Double)) where
    matrixLength = matrixLength . primal

instance (MatrixLength mat) => MatrixLength (AD s mat) where
    matrixLength = matrixLength . runAD

instance MatrixMultiply (Forward (Matrix Double)) where
    matrixMultiply forwardMat1 forwardMat2 = do
        let (mat1, mat1') = (primal forwardMat1, tangentNum forwardMat1)
        let (mat2, mat2') = (primal forwardMat2, tangentNum forwardMat2)
        Forward (matrixMultiply mat1 mat2) (matrixMultiply mat1 mat2' + matrixMultiply mat1' mat2)

instance (MatrixMultiply mat) => MatrixMultiply (AD s mat) where
    matrixMultiply mat1AD mat2AD = AD (matrixMultiply (runAD mat1AD) (runAD mat2AD))

instance MatrixSum (Forward (Matrix Double)) (Forward Double) where
    matrixSum forwardMat = do
        let mat = primal forwardMat
        let mat' = tangentNum forwardMat
        Forward (matrixSum mat) (matrixSum mat')

instance (MatrixSum mat d) => MatrixSum (AD s mat) (AD s d) where
    matrixSum matAD = AD (matrixSum (runAD matAD))

instance MatrixMap (Forward (Matrix Double)) where
    matrixMap f forwardMat = do
        let mat = primal forwardMat
        let mat' = tangentNum forwardMat
        let matOut = cmap f mat
        let matOut' = reshape (cols matOut) $ VS.zipWith (\x x' -> tangentNum (f (Forward x x'))) (flatten mat) (flatten mat')
        Forward matOut matOut'

instance (MatrixMap mat) => MatrixMap (AD s mat) where
    matrixMap f matAD = AD (matrixMap f (runAD matAD))

instance MatrixZipWith (Forward (Matrix Double)) where
    matrixZipWith f forwardMat1 forwardMat2 = do
        let (mat1, mat1') = (primal forwardMat1, tangentNum forwardMat1)
        let (mat2, mat2') = (primal forwardMat2, tangentNum forwardMat2)
        let matOut = reshape (cols mat1) $ VS.zipWith f (flatten mat1) (flatten mat2)
        let matOut' = reshape (cols mat1) $ VS.zipWith4 (\x1 x1' x2 x2' -> tangentNum (f (Forward x1 x1') (Forward x2 x2'))) (flatten mat1) (flatten mat1') (flatten mat2) (flatten mat2')
        Forward matOut matOut'

instance (MatrixZipWith mat) => MatrixZipWith (AD s mat) where
    matrixZipWith f matAD1 matAD2 = AD (matrixZipWith f (runAD matAD1) (runAD matAD2))

instance BranchingConstructs (Forward Bool) tt where
    ifThenElse b x y = if (extractForward b) then x else y

instance (BranchingConstructs b tt) => BranchingConstructs (AD s b) tt where
    ifThenElse b x y = ifThenElse (runAD b) x y
