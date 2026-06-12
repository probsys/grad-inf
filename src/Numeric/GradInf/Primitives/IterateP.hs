module Numeric.GradInf.Primitives.IterateP (IterateP (..)) where

import Numeric.GradInf.Primitives.Resample (Resample (..))

class Resample m t => IterateP m t where
    iterateP :: (t -> m t) -> t -> [m t]

instance (Resample m t) => IterateP m t where
    iterateP f x0 = iterate (\mu -> resample (mu >>= f)) (return x0)
