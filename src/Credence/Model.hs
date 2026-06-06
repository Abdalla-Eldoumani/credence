{-# LANGUAGE GADTs #-}

{- | The model monad: an operational representation of a probabilistic program.

A model is a monadic computation over two primitives. 'sample' draws a value
from a distribution; 'observe' factors the execution by a likelihood, supplied as
a log-likelihood increment. Everything else is the 'Monad' instance threading
those two together.

The representation is operational and explicit: a 'Model' is a GADT whose
constructors reify each instruction rather than hiding it behind a typeclass or a
transformer stack. This is the deliberate, load-bearing choice of the project.
Reifying the program as an inert structure is what lets one model be run by
several interpreters (importance sampling here, trace-based Metropolis-Hastings
and a particle filter later) and be inspected by the visualizer, at the cost of
more explicit plumbing than the typeclass style of current monad-bayes. That
trade favors transparency, which is the point of a language built to teach. The
design follows Scibior, Ghahramani, and Gordon, "Practical Probabilistic
Programming with Monads," Haskell Symposium 2015.

Building a 'Model' performs no sampling and no IO. It allocates a structure that
an interpreter walks. The interpreter is where a generator is threaded and where
the log-weight is accumulated; none of that lives in the model.
-}
module Credence.Model (
    Model (..),
    sample,
    observe,
    coinModel,
) where

import Credence.Distribution (Dist, bernoulliDist, betaDist, logDensityDist)

-- The free-monad bind @Sample d k >>= f = Sample d (\x -> k x >>= f)@ is the
-- canonical, textbook form. Rewriting it to @k >=> f@ would define this very
-- '>>=' through '>=>', which is itself defined in terms of '>>=': clever but
-- circular-looking and less clear than the structural definition.
{-# ANN module ("HLint: ignore Use >=>" :: String) #-}

{- | A probabilistic program returning an @a@, as an inert syntax tree.

Three instructions, in continuation-passing form so that bind is a structural
rewrite and never forces a distribution or a likelihood:

* @Pure x@ is the finished computation returning @x@.
* @Sample d k@ draws from distribution @d@ and continues with @k@ applied to the
  drawn value. The drawn value's type is existential: an interpreter learns it
  only by supplying the generator, which is what keeps one model runnable by
  interpreters that draw in different ways.
* @Observe w m@ factors the execution by a likelihood whose logarithm is @w@ (a
  log-likelihood increment, per the @observe :: Double -> Model ()@ contract) and
  continues with @m@. The increment is already in log-space; an interpreter sums
  these into the execution's log-weight.
-}
data Model a where
    Pure :: a -> Model a
    Sample :: Dist a -> (a -> Model b) -> Model b
    Observe :: Double -> Model b -> Model b

{- | @fmap@ over the result, by recursion into the continuation. Defined via
'>>=' on the Monad instance so the three instances cannot drift apart.
-}
instance Functor Model where
    fmap f m = m >>= (Pure . f)

{- | @pure@ is 'Pure'; @<*>@ runs the function model, then the argument model,
then applies. Sequencing is bind, so observes from both sides accumulate in the
left-to-right order they are written.
-}
instance Applicative Model where
    pure = Pure
    mf <*> mx = mf >>= \f -> fmap f mx

{- | Bind is a structural rewrite over the instruction tree, the heart of the
operational representation.

* On @Pure x@, hand @x@ to the continuation: @f x@. This is left identity by
  construction.
* On @Sample d k@, keep the draw and push the bind under the continuation:
  @Sample d (\x -> k x >>= f)@. The distribution is not touched.
* On @Observe w m@, keep the factor and push the bind under it:
  @Observe w (m >>= f)@. The increment is not touched.

Because bind only rewrites structure and never evaluates a 'Dist' or a weight,
the right identity and associativity laws hold by the same recursion, and an
interpreter is free to assign meaning to Sample and Observe however it likes.
-}
instance Monad Model where
    Pure x >>= f = f x
    Sample d k >>= f = Sample d (\x -> k x >>= f)
    Observe w m >>= f = Observe w (m >>= f)

{- | Draw one value from a distribution. The primitive form of 'Sample': draw,
then return the value with no further instructions.
-}
sample :: Dist a -> Model a
sample d = Sample d Pure

{- | Factor the current execution by a likelihood, given as a log-likelihood
increment.

@observe w@ multiplies the execution's likelihood by @exp w@, equivalently adds
@w@ to its log-weight. The increment is a plain 'Double' computed by the caller,
typically from a distribution's 'logDensityDist' at an observed datum, which
keeps 'observe' agnostic to where the likelihood comes from (the spec).
-}
observe :: Double -> Model ()
observe w = Observe w (Pure ())

{- | The coin model, the canonical example and the fixture the conjugate gate
runs on.

Given a Beta(@a@, @b@) prior on the coin's bias and a list of observed flips
(@True@ for heads), draw the bias @theta@ once, then for each flip add that
flip's Bernoulli log-likelihood under @theta@ via 'observe', and return @theta@.
The posterior on @theta@ is conjugate: Beta(@a@ + heads, @b@ + tails). Each
flip's likelihood is scored by 'logDensityDist' of @bernoulliDist theta@, so the
model carries the same parameterization the distribution library was proven on.

The model is structure only: building it draws nothing. Importance sampling, and
later MH, interpret this exact value unchanged, which is the operational
representation paying off.
-}
coinModel :: Double -> Double -> [Bool] -> Model Double
coinModel a b flips = do
    theta <- sample (betaDist a b)
    mapM_ (observe . logDensityDist (bernoulliDist theta)) flips
    pure theta
