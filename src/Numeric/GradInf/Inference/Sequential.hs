module Numeric.GradInf.Inference.Sequential where

import Control.Monad (ap)
import Data.Vector (Vector, fromList)
import Data.Vector.Generic qualified as Vector
import Control.Monad.Bayes.Class (MonadDistribution)
import Control.Monad.Bayes.Class qualified as Bayes (
    bernoulli,
    categorical,
    random,
 )
import Numeric.GradInf.Inference.SMC.SMCTypes

data Sequential susp a = SequentialNonEmpty {getOriginalValue :: () -> a, getFirstSuspended :: () -> susp, sequentialFuncs :: Vector (susp -> susp), curFunc :: susp -> a}
                        | SequentialEmpty {getOriginalValue :: () -> a}

instance Monad (Sequential susp) where
    return x = SequentialEmpty (const x)
    m >>= f = do
        case m of
            SequentialEmpty getOriginalValue -> f (getOriginalValue ())
            SequentialNonEmpty getOriginalValue getFirstSuspended sequentialFuncs curFunc -> do
                let m' = f (getOriginalValue ())
                case m' of
                    SequentialEmpty getOriginalValue' -> do
                        let curFunc' = (\a -> let SequentialEmpty {getOriginalValue = getOriginalValue'} = f a in getOriginalValue' ()) . curFunc
                        SequentialNonEmpty getOriginalValue' getFirstSuspended sequentialFuncs curFunc'
                    SequentialNonEmpty getOriginalValue' getFirstSuspended' sequentialFuncs' curFunc' -> do
                        let sequentialFuncMiddle :: susp -> susp = (\a -> let SequentialNonEmpty {getFirstSuspended = getFirstSuspended'} = f a in getFirstSuspended' ()) . curFunc
                        SequentialNonEmpty getOriginalValue' getFirstSuspended (sequentialFuncs Vector.++ (fromList [sequentialFuncMiddle]) Vector.++ sequentialFuncs') curFunc'

instance Applicative (Sequential susp) where
    pure = return
    (<*>) = ap

instance Functor (Sequential susp) where
    fmap f x = x >>= (return . f)

instance MonadFactor d (Sequential susp) where
    score w = error "score unsupported in Sequential monad"

instance MonadDistribution (Sequential susp) where
    random = error "random unsupported in Sequential monad"
    bernoulli _ = error "bernoulli unsupported in Sequential monad"
    categorical _ = error "categorical unsupported in Sequential monad"

instance MonadMeasure d (Sequential susp)

suspend :: Sequential susp susp -> Sequential susp susp
suspend (SequentialEmpty getOriginalValue) = SequentialNonEmpty getOriginalValue getOriginalValue (fromList []) id
suspend (SequentialNonEmpty getOriginalValue getFirstSuspended sequentialFuncs curFunc) = do
    SequentialNonEmpty getOriginalValue getFirstSuspended (sequentialFuncs Vector.++ (fromList [curFunc])) id