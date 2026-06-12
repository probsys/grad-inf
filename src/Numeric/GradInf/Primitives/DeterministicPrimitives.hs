module Numeric.GradInf.Primitives.DeterministicPrimitives where

import Prelude hiding ((<>))
import Numeric.LinearAlgebra
import qualified Data.Vector.Storable as VS

-- Class definition

class BranchingConstructs b a where
    ifThenElse :: b -> a -> a -> a

class ToInt a b | a -> b where
    toInt :: a -> b

class ToDouble a b | a -> b where
    toDouble :: a -> b

class Pow a b c | a b -> c where
    pow :: a -> b -> c

class GetIndex t i where
    getIndex :: [t] -> i -> t

class IsGreater t b where
    isGreater :: t -> t -> b

class FromDouble d where
    fromDouble :: Double -> d

class FromInt i where
    fromInt :: Int -> i

class FromBool b where
    fromBool :: Bool -> b

class FromMatrix mat where
    fromMatrix :: Matrix Double -> mat

-- For extracting an ordinary Boolean for situations where we really need
-- a normal Boolean, and expect to have one.
class ExtractBool b where
    extractBool :: b -> Bool

class ExtractDouble d where
    extractDouble :: d -> Double

class SafeDiv d where
    safeDiv :: d -> d -> d

class SafeMul d where
    safeMul :: d -> d -> d

class MatrixToList mat d where
    matrixToList :: mat -> [d]

class ListToMatrix mat d where
    listToMatrix :: [d] -> mat

class MatrixMultiply mat where
    matrixMultiply :: mat -> mat -> mat

class MatrixSum mat d where
    matrixSum :: mat -> d

class MatrixLength mat where
    matrixLength :: mat -> Int

class MatrixMap mat where
    matrixMap :: (forall d i b mat. (DeterministicPrimitives d i b mat) => d -> d) -> mat -> mat

class MatrixZipWith mat where
    matrixZipWith :: (forall d i b mat. (DeterministicPrimitives d i b mat) => d -> d -> d) -> mat -> mat -> mat

class Mergeable a aa | aa -> a where
    merge :: aa -> aa -> aa

-- By making mat an explicit argument, instances can choose
-- whether to e.g. adopt a struct of arrays or array of structs
-- representation.
type DeterministicPrimitivesConstraint d i b mat =
    ( Floating d
    , Real d
    , Integral i
    , BranchingConstructs b i
    , BranchingConstructs b d
    , BranchingConstructs b b
    , BranchingConstructs b [d]
    , ToInt b i
    , ToInt d i
    , ToDouble i d
    , Pow d i d
    , Ord d
    , FromDouble d
    , FromInt i
    , FromBool b
    , FromMatrix mat
    , ExtractBool b
    , ExtractDouble d
    , GetIndex i i
    , GetIndex d i
    , IsGreater d b
    , IsGreater i b
    , SafeDiv d
    , SafeMul d
    , MatrixToList mat d
    , ListToMatrix mat d
    , MatrixMultiply mat
    , MatrixLength mat
    , MatrixMap mat
    , MatrixSum mat d
    , MatrixZipWith mat
    , Show d
    , Show i
    , Show b
    , Show mat
    )

-- List of deterministic functions to overload can be expanded. See:

-- * https://github.com/ekmett/ad/blob/56a2ef5220c48d116dd5970c7d7a58ceae6f2f01/include/instances.h

-- * https://hackage.haskell.org/blob/56a2ef5220c48d116dd5970c7d7a58ceae6f2f01/include/instances.h

-- * https://hackage.haskell.org/package/log-domain-0.13.2/docs/src/Numeric.Log.html

class
    (DeterministicPrimitivesConstraint d i b mat) =>
    DeterministicPrimitives d i b mat
        | d -> i b mat
        , i -> d b mat
        , b -> d i mat
        , mat -> d i b

-- Core semantics

instance BranchingConstructs Bool a where
    ifThenElse bool t s = if bool then t else s

instance ToInt Bool Int where
    toInt b = if b then 1 else 0

instance ToInt Double Int where
    toInt r = do
        let i = round r
        if abs (r - fromIntegral i) > 1e-8
            then
                error "toInt: double to int conversion failed"
            else
                i

instance ToDouble Int Double where
    toDouble = fromIntegral

instance Pow Double Int Double where
    pow = (^)

instance Pow Int Int Int where
    pow = (^)

instance GetIndex t Int where
    getIndex xs i = if i < 0 then error ("Negative index i = " ++ (show i)) else (xs !! i)

instance IsGreater Int Bool where
    isGreater x1 x2 = x1 > x2

instance IsGreater Double Bool where
    isGreater d1 d2 = d1 > d2

instance FromDouble Double where
    fromDouble = id

instance FromInt Int where
    fromInt = id

instance FromBool Bool where
    fromBool = id

instance FromMatrix (Matrix Double) where
    fromMatrix = id

instance ExtractBool Bool where
    extractBool = id

instance ExtractDouble Double where
    extractDouble = id

instance SafeDiv Double where
    safeDiv a b = if a == 0 && b == 0 then 0 else a / b

instance SafeMul Double where
    safeMul a b = a * b

instance MatrixToList (Matrix Double) Double where
    matrixToList mat = toList (flatten mat)

instance ListToMatrix (Matrix Double) Double where
    listToMatrix xs = reshape 1 (VS.fromList xs)

instance MatrixMultiply (Matrix Double) where
    matrixMultiply mat1 mat2 = mat1 <> mat2

instance MatrixLength (Matrix Double) where
    matrixLength mat = rows mat * cols mat

instance MatrixSum (Matrix Double) Double where
    matrixSum mat = sumElements mat

instance MatrixMap (Matrix Double) where
    matrixMap = cmap

instance MatrixZipWith (Matrix Double) where
    matrixZipWith f mat1 mat2 = do
        if size mat1 /= size mat2 then
            error "matrixZipWith: matrices must have the same size"
        else
            reshape (cols mat1) (VS.zipWith f (flatten mat1) (flatten mat2))

instance DeterministicPrimitives Double Int Bool (Matrix Double) where
