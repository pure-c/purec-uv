module Effect.Aff where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Partial.Unsafe (unsafeCrashWith)

foreign import data Aff :: Type -> Type -> Type
foreign import data Fiber :: Type -> Type -> Type

-- | A cancellation effect for actions run via `makeAff`. If a `Fiber` is
-- | killed, and an async action is pending, the canceler will be called to
-- | clean it up.
newtype Canceler e = Canceler (e -> Aff e Unit)

-- | Constructs an `Aff` from low-level `Effect` effects using a callback. A
-- | `Canceler` effect should be returned to cancel the pending action. The
-- | supplied callback may be invoked only once. Subsequent invocation are
-- | ignored.
foreign import makeAff
  :: ∀ e a
   . ((Either e a -> Effect Unit) -> Effect (Canceler e))
  -> Aff e a

foreign import runFiber :: ∀ e a. Fiber e a -> Effect Unit

foreign import makeFiberImpl
  :: ∀ e a
   . (∀ u v. Either u v -> Boolean)
  -> (∀ u v. Either u v -> Boolean)
  -> (∀ u v. Either u v -> u)
  -> (∀ u v. Either u v -> v)
  -> (∀ u v. u -> Either u v)
  -> (∀ u v. v -> Either u v)
  -> Aff e a
  -> Effect (Fiber e a)

makeFiber :: ∀ e a. Aff e a -> Effect (Fiber e a)
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
launchAff :: ∀ e a. Aff e a -> Effect (Fiber e a)
launchAff aff = do
  fiber <- makeFiber aff
  fiber <$ runFiber fiber

instance functorAff :: Functor (Aff e) where
  map = _map

instance applicativeAff :: Applicative (Aff e) where
  pure = _pure

instance bindAff :: Bind (Aff e) where
  bind = _bind

instance applyAff :: Apply (Aff e) where
  apply = ap

instance monadEffectAff :: MonadEffect (Aff e) where
  liftEffect = _liftEffect

instance monadAff :: Monad (Aff e)

foreign import _liftEffect :: ∀ e a. Effect a -> Aff e a
foreign import _pure :: ∀ e a. a → Aff e a
foreign import _map :: ∀ e a b. (a -> b) -> Aff e a -> Aff e b
foreign import _bind :: ∀ e a b. Aff e a -> (a -> Aff e b) -> Aff e b
