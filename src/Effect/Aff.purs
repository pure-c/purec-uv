module Effect.Aff where

import Prelude

import Control.Monad (ap)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Partial.Unsafe (unsafeCrashWith)

foreign import data Aff :: Type -> Type
foreign import data Fiber :: Type -> Type

foreign import runFiber :: ∀ a. Fiber a -> Effect Unit
foreign import makeFiberImpl
  :: ∀ a
   . (∀ u v. Either u v -> Boolean)
  -> (∀ u v. Either u v -> Boolean)
  -> (∀ u v. Either u v -> u)
  -> (∀ u v. Either u v -> v)
  -> (∀ u v. u -> Either u v)
  -> (∀ u v. v -> Either u v)
  -> Aff a
  -> Effect (Fiber a)

makeFiber :: ∀ a. Aff a -> Effect (Fiber a)
makeFiber =
  makeFiberImpl
    isLeft
    isRight
    unsafeFromLeft
    unsafeFromRight
    Left
    Right

  where
  isLeft :: ∀ u v. Either u v -> Boolean
  isLeft = case _ of
    Left _ -> true
    Right _ -> false

  isRight :: ∀ u v. Either u v -> Boolean
  isRight = case _ of
    Left _ -> false
    Right _ -> true

  unsafeFromLeft :: ∀ u v. Either u v -> u
  unsafeFromLeft = case _ of
    Left a  -> a
    Right _ -> unsafeCrashWith "unsafeFromLeft: Right"

  unsafeFromRight :: ∀ u v. Either u v -> v
  unsafeFromRight = case _ of
    Right a -> a
    Left  _ -> unsafeCrashWith "unsafeFromRight: Left"

-- | Forks an `Aff` from an `Effect` context, returning the `Fiber`.
launchAff :: ∀ a. Aff a -> Effect (Fiber a)
launchAff aff = do
  fiber <- makeFiber aff
  fiber <$ runFiber fiber

instance functorAff :: Functor Aff where
  map = _map

instance applicativeAff :: Applicative Aff where
  pure = _pure

instance bindAff :: Bind Aff where
  bind = _bind

instance applyAff :: Apply Aff where
  apply = ap

instance monadEffectAff :: MonadEffect Aff where
  liftEffect = _liftEffect

instance monadAff :: Monad Aff

foreign import _liftEffect :: ∀ a. Effect a -> Aff a
foreign import _pure :: ∀ a. a → Aff a
foreign import _map :: ∀ a b. (a -> b) -> Aff a -> Aff b
foreign import _bind :: ∀ a b. Aff a -> (a -> Aff b) -> Aff b
