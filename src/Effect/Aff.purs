module Effect.Aff where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Rec.Class (class MonadRec, Step(..))
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Partial.Unsafe (unsafeCrashWith)

foreign import data Aff :: Type -> Type -> Type

foreign import data Fiber :: Type -> Type -> Type

-- | Blocks until the fiber completes, yielding the result. If the fiber
-- | throws an exception, it is rethrown in the current fiber.
joinFiber :: ∀ e. Fiber e ~> Aff e
joinFiber fiber =
  makeAff \k ->
    effectCanceler <$>
      _joinFiber fiber k

foreign import _joinFiber
  :: ∀ e a
   . Fiber e a
  -> (Either e a -> Effect Unit)
  -> Effect (Effect Unit)

-- | A cancellation effect for actions run via `makeAff`. If a `Fiber` is
-- | killed, and an async action is pending, the canceler will be called to
-- | clean it up.
newtype Canceler e = Canceler (e -> Aff e Unit)

-- | A canceler which does not cancel anything.
nonCanceler :: ∀ e. Canceler e
nonCanceler = Canceler (const (pure unit))

-- | A canceler from an Effect action.
effectCanceler :: ∀ e. Effect Unit -> Canceler e
effectCanceler = Canceler <<< const <<< liftEffect

-- | Constructs an `Aff` from low-level `Effect` effects using a callback. A
-- | `Canceler` effect should be returned to cancel the pending action. The
-- | supplied callback may be invoked only once. Subsequent invocation are
-- | ignored.
foreign import makeAff
  :: ∀ e a
   . ((Either e a -> Effect Unit) -> Effect (Canceler e))
  -> Aff e a

foreign import runFiber :: ∀ e a. Fiber e a -> Effect Unit

type SetTimeoutFn =
  Int -> Effect Unit -> Effect Unit

type UncaughtErrorHandler e =
   e -> Effect Unit

foreign import makeFiberImpl
  :: ∀ e a
   . (∀ u v. Either u v -> Boolean)
  -> (∀ u v. Either u v -> Boolean)
  -> (∀ u v. Either u v -> u)
  -> (∀ u v. Either u v -> v)
  -> (∀ u v. u -> Either u v)
  -> (∀ u v. v -> Either u v)
  -> SetTimeoutFn
  -> UncaughtErrorHandler e
  -> Aff e a
  -> Effect (Fiber e a)

makeFiber
  :: ∀ e a
   . SetTimeoutFn
  -> UncaughtErrorHandler e
  -> Aff e a
  -> Effect (Fiber e a)
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
launchAff
  :: ∀ e a
   . SetTimeoutFn
  -> UncaughtErrorHandler e
  -> Aff e a
  -> Effect (Fiber e a)
launchAff setTimeout onUncaughtError aff = do
  fiber <- makeFiber setTimeout onUncaughtError aff
  fiber <$ runFiber fiber

-- | Forks an `Aff` from within a parent `Aff` context, returning the `Fiber`.
forkAff :: ∀ e a. Aff e a -> Aff e (Fiber e a)
forkAff = _fork true

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

-- | This instance is provided for compatibility. `Aff` is always stack-safe
-- | within a given fiber. This instance will just result in unnecessary
-- | bind overhead.
instance monadRecAff :: MonadRec (Aff e) where
  tailRecM k = go
    where
    go a = do
      res <- k a
      case res of
        Done r -> pure r
        Loop b -> go b

instance monadThrowAff :: MonadThrow e (Aff e) where
  throwError = _throwError

instance monadErrorAff :: MonadError e (Aff e) where
  catchError = _catchError

foreign import _fork :: ∀ e a. Boolean -> Aff e a -> Aff e (Fiber e a)
foreign import _throwError :: ∀ e a. e -> Aff e a
foreign import _catchError :: ∀ e a. Aff e a -> (e -> Aff e a) -> Aff e a
foreign import _liftEffect :: ∀ e a. Effect a -> Aff e a
foreign import _pure :: ∀ e a. a -> Aff e a
foreign import _map :: ∀ e a b. (a -> b) -> Aff e a -> Aff e b
foreign import _bind :: ∀ e a b. Aff e a -> (a -> Aff e b) -> Aff e b
