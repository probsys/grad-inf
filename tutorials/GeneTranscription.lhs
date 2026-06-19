In this example, we apply GradInf to a
[chemical reaction network (RN)](https://en.wikipedia.org/wiki/Chemical_reaction_network_theory)
model of gene transcription.
The considered model has four reactions:

$$
\emptyset \to^\alpha M, \quad
M \to^{\beta} M + P, \quad
M \to^{\gamma} \emptyset, \quad
P \to^{\delta} \emptyset
$$

corresponding to gene transcription, mRNA translation, mRNA degradation,
and protein degradation, respectively.
We simulate the reaction network using the
[Gillespie algorithm](https://en.wikipedia.org/wiki/Gillespie_algorithm),
and apply a loss function that measures the relative error between
the simulated mean copy numbers of mRNA and protein and ground truth
reference values.

Writing the Model
-----------------

\begin{code}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

import Prelude
import Control.Applicative (ZipList(..), getZipList)
import Control.Monad
import Control.Monad.Bayes.Sampler.Lazy
import System.Random (setStdGen, mkStdGen)
import Data.List (transpose)
import Numeric.GradInf.Primitives.DeterministicPrimitives
import Numeric.GradInf.Primitives.Uniform
import Numeric.GradInf.Primitives.CategoricalMaxIndep
import Numeric.GradInf.Primitives.FoldP
import Numeric.GradInf.Primitives.IterateP
import Numeric.GradInf.Inference
import Numeric.GradInf.Inference.Base (Base(..))
import Numeric.GradInf

-- $setup (use a fixed seed)
-- >>> setStdGen (mkStdGen 42)

numObs :: Int
numObs = 5 -- number of independent traces to simulate; reduced in the tutorial for faster execution

n :: Int
n = 1000

capT :: Double
capT = 2.5

geneTranscriptionKernel ::
    forall m d i b mat.
    (DeterministicPrimitives d i b mat,
    CategoricalMaxIndep m d [i], Uniform m d)
    => [d] -> ([i], d, [d]) -> m ([i], d, [d])
geneTranscriptionKernel theta (x, t, acc) = do

    if extractDouble t > capT then
        return (x, t, acc)
    else do
        let alpha = exp (theta !! 0)
        let beta = exp (theta !! 1)
        let gamma = exp (theta !! 2)
        let delta = exp (theta !! 3)

        let m :: i = x !! 0
        let p :: i = x !! 1

        let rates :: [d] = [alpha, beta * (toDouble m), gamma * (toDouble m), delta * (toDouble p)]
        let totalRate :: d = sum rates
        let probs :: [d] = map (\q -> q / totalRate) rates

        x' <- categoricalMaxIndep (probs, [[m + 1, p], [m, p + 1], [m - 1, p], [m, p - 1]])

        u <- uniform
        let t' = t + log (1 / u) / totalRate

        let tEnd = if extractDouble t' > capT then fromDouble capT else t'
        let dt = tEnd - t
        let acc' = [acc !! 0 + toDouble m * dt, acc !! 1 + toDouble p * dt]

        return (x', t', acc')

geneTranscriptionModel ::
    forall m d i b mat.
    (DeterministicPrimitives d i b mat, IterateP m ([i], d, [d]),
    FoldrP m ([i], d, [d]) ([d], d),
    CategoricalMaxIndep m d [i], Uniform m d)
    => Int -> Int -> [d] -> m d
geneTranscriptionModel numObs n theta = do
    let x0 = [5, 40]

    let simulateTrace :: m ([i], d, [d]) =
            iterateP (geneTranscriptionKernel theta) (x0, 0.0, [0.0, 0.0]) !! n

    let foldF (_, t, timeInt) (accX, accT) = do
            if extractDouble t < capT then
                error ("Did not simulate the full time period (t = " ++ (show t) ++ " < " ++ (show capT) ++ ")")
            else do
                ([accX !! 0 + timeInt !! 0, accX !! 1 + timeInt !! 1], accT + t)
    (traceSumX, traceSumT) <- foldrP foldF ([0.0, 0.0], 0.0) (replicate numObs simulateTrace)

    let mAvg = traceSumX !! 0 / fromIntegral numObs / fromDouble capT
    let pAvg = traceSumX !! 1 / fromIntegral numObs / fromDouble capT

    let mExp = 10.4
    let pExp = 22.3
    let loss = (mAvg - mExp) * (mAvg - mExp) / (mExp * mExp) + (pAvg - pExp) * (pAvg - pExp) / (pExp * pExp)
    return loss
\end{code}

We make use of a `categoricalMaxIndep` primitive, which tells GradInf
to employ a maximal coupling with independent residuals when
differentiating the gene transcription model.

\begin{code}
getSamples :: Sampler [Double]
getSamples =
    replicateM 5 (geneTranscriptionModel numObs n
                    (map log [18, 8, 1.5, 4]))
\end{code}

Differentiating the Model
-------------------------

We now apply GradInf to compute gradient estimates with respect
to all four parameters simultaneously using the `ZipList` functor.
This computes the full gradient in a single call to `gradInfAD`.

\begin{code}
getGradientEstimates :: Sampler [ZipList Double]
getGradientEstimates =
    let twistFunc ::
            forall d i b mat. (Num d, Fractional d, Floating d, FromDouble d, ToDouble i d, ExtractDouble d) =>
            Int ->
            ([Coupled i], Coupled d, [Coupled d]) ->
            d
        twistFunc _ (x, t, acc) = do
            let Coupled (mA, mB) = x !! 0
            let Coupled (pA, pB) = x !! 1
            let Coupled (tA, tB) = t
            let Coupled (accMA, accMB) = acc !! 0
            let Coupled (accPA, accPB) = acc !! 1
            let horizon = fromDouble $ min 0.5 (capT - extractDouble tA)
            let timeScale = 1 / 20
            abs ((accMB - accMA) + (toDouble mB - toDouble mA) * horizon) / fromDouble 10.4
                + abs ((accPB - accPA) + (toDouble pB - toDouble pA) * horizon) / fromDouble 22.3
                + abs (tB - tA) * timeScale
     in replicateM 10
            ( gradInfAD
                (geneTranscriptionModel numObs n . getZipList)
                ( TwistedSMCInference 1 twistFunc ::
                    forall d i b mat. (DeterministicPrimitives d i b mat) =>
                    TwistedSMCInference Base CRN d i b
                )
                (ZipList (map log [18, 8, 1.5, 4]))
            )

-- |
-- >>> fmap (map getZipList) (sampler getGradientEstimates)
-- [[0.14409024236638188,0.11626475191080754,-0.3959782466319224,-0.33542844390957105],[-0.19389663177549918,-8.764469447850404e-2,0.14729457653949962,3.047766914693925e-2],[0.2097233270616201,-0.5420223962041146,-0.9989225686730319,-0.14908300886071865],[-8.578939748394014e-2,-0.13719902372690249,5.3826963222109524e-2,-0.11435708776253722],[9.07704924279736e-2,0.11441913800866106,-1.936587936373914e-2,-8.527810690679449e-2],[-1.4101513738978218e-2,-0.10367086023403566,3.5507579198788726e-2,2.4173278724310077e-2],[4.96847450171267e-2,8.898381879440269e-2,-9.77076205342738e-3,-1.5242867343302848e-2],[7.160380585293154e-2,5.5657611194625485e-2,-3.522902896384387e-2,-5.078994328063205e-2],[8.518867942215486e-2,2.2521171867356336e-2,-0.12402426950342152,-0.10932506166065545],[-4.071094367489457e-2,0.16894068362793094,6.62798544875896e-2,0.16195007965891323]]
\end{code}

We can now calculate the component-wise empirical mean and
variance of the gradient estimator with respect to each
parameter.
\begin{code}
meanAndVariance :: [Double] -> (Double, Double)
meanAndVariance xs = do
    let n = length xs
    let mu = (sum xs) / (fromIntegral n)
    let var = (sum (map (\x -> (x - mu) * (x - mu)) xs))
              / (fromIntegral (n - 1))
    (mu, var)

printMeanAndVariance :: IO ()
printMeanAndVariance = do
    samples <- fmap (map getZipList)
                 (sampler getGradientEstimates)
    let byComponent = transpose samples
    mapM_ (print . meanAndVariance) byComponent

-- |
-- >>> printMeanAndVariance
-- (3.1656280547487654e-2,1.3847633481611804e-2)
-- (-3.0374979923977273e-2,4.3165330546016206e-2)
-- (-0.1280381781741399,0.11533864109220437)
-- (-6.429034921940491e-2,1.7362265555445507e-2)
\end{code}
