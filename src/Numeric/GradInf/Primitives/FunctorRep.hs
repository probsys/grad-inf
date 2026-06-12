module Numeric.GradInf.Primitives.FunctorRep where

import Prelude
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Functor.Product
import Control.Monad.State
import Control.Applicative
import Numeric.GradInf.Coupling
import Numeric.GradInf.Inference.Expected
import Numeric.GradInf.AD.AD

class (Traversable f, Applicative f) => FunctorRep a d f | a -> d f where
    toFunctor :: a -> f d
    fromFunctor :: f d -> a

withIndices :: (Traversable f) => f a -> (f (a, Int), Int)
withIndices structure =
    runState (traverse assignNext structure) 0
  where
    -- Assign the next index and increment the counter
    assignNext :: a -> State Int (a, Int)
    assignNext x = do
        current <- get
        put (current + 1)
        return (x, current)

instance FunctorRep Double Double Identity where
    toFunctor x = Identity x
    fromFunctor (Identity x) = x

instance FunctorRep (AD s (Forward Double)) (AD s (Forward Double)) Identity where
    toFunctor x = Identity x
    fromFunctor (Identity x) = x

instance FunctorRep (Expected d a) (Expected d d) Identity where
    toFunctor (Expected x) = Identity (Expected x)
    fromFunctor (Identity (Expected x)) = Expected x

instance (FunctorRep a1 d f1, FunctorRep a2 d f2) => FunctorRep (a1, a2) d (Product f1 f2) where
    toFunctor (x1, x2) = Pair (toFunctor x1) (toFunctor x2)
    fromFunctor (Pair f1 f2) = (fromFunctor f1, fromFunctor f2)

instance (FunctorRep a1 d f1, FunctorRep a2 d f2, FunctorRep a3 d f3) => FunctorRep (a1, a2, a3) d (Product (Product f1 f2) f3) where
    toFunctor (x1, x2, x3) = Pair (toFunctor (x1, x2)) (toFunctor x3)
    fromFunctor (Pair f12 f3) = let (x1, x2) = fromFunctor f12 in (x1, x2, fromFunctor f3)

instance (FunctorRep a d f) => FunctorRep [a] d (Compose ZipList f) where
    toFunctor xs = Compose (ZipList (map toFunctor xs))
    fromFunctor (Compose (ZipList fs)) = map fromFunctor fs

instance (FunctorRep a d f) => FunctorRep (Coupled a) d (Product f f) where
    toFunctor (Coupled (xL, xR)) = Pair (toFunctor xL) (toFunctor xR)
    fromFunctor (Pair fL fR) = Coupled (fromFunctor fL, fromFunctor fR)
