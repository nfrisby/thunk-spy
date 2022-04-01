{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS -funbox-strict-fields #-}

-- | The pair of 'Spy' and its 'Handler' is a simple, lightweight, and robust
-- mechanism for instrumenting even pure code and later/concurrently observing
-- its evaluation order from within an 'IO' action.
--
-- See 'Message' for the entrypoint Haddock.
--
-- TODO: add options in 'Config' that trade completeness of reporting and
-- listening for less time/space usage (eg bounded queues)
--
-- TODO: more than one 'Spy' per 'Handler'? More than one 'Handler' per 'Spy'?
-- IE 'TChan'?

module Control.Thunk.Spy (
  -- * Types
  Config,
  Handler,
  Message (..),
  Spy,
  primitiveInsideHandler,
  primitiveInsideSpy,
  -- * Construction
  defaultConfig,
  newIO,
  -- * 'Spy'
  report,
  reportIO,
  -- * 'Handler'
  isAliveIO,
  isAliveSTM,
  listenIO,
  listenSTM,
  tryListenIO,
  tryListenSTM,
  -- * Sneaking
  Sneaking,
  sneak,
  sneakIO,
  sneakingInto,
  ) where

import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TVar
import           Control.Monad.STM (STM, atomically)
import           Data.Functor.Contravariant
import           Data.IORef (IORef, newIORef)
import           Data.Kind (Type)
import           Data.Maybe (isJust)
import           Data.Reflection (Given, given, give)
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           System.IO.Unsafe (unsafePerformIO)
import           System.Mem.Weak (Weak, deRefWeak)

import           GHC.Clock (getMonotonicTimeNSec)
import           GHC.Conc.Sync (STM (..))
import           GHC.Exts (mkWeak#, touch#)
import           GHC.IO (IO (..))
import           GHC.IORef (IORef (..))
import           GHC.STRef (STRef (..))
import           GHC.Stack (CallStack, HasCallStack, callStack)
import           GHC.Weak (Weak (..))

----- low-level internals

-- | Not exported; a generalization of 'Data.IORef.mkWeakIORef'
mkWeakIORef_ :: IORef a -> v -> IO () -> IO (Weak v)
mkWeakIORef_ (IORef (STRef r#)) v (IO finalizer) = IO $ \s0 ->
    case mkWeak# r# v finalizer s0 of (# s1, w #) -> (# s1, Weak w #)

-- | Keep the 'IORef' alive during the action
--
-- See <https://www.haskell.org/ghc/blog/20210607-the-keepAlive-story.html> for
-- how to safely use this function.
unsafeWithIO :: IORef () -> IO a -> IO a
unsafeWithIO r (IO f) = IO $ \s0 ->
  case f s0 of
    (# s1, a #) -> case touch# r s1 of
      s2 -> (# s2, a #)

-- | Keep the 'IORef' alive during the action
--
-- See <https://www.haskell.org/ghc/blog/20210607-the-keepAlive-story.html> for
-- how to safely use this function.
unsafeWithSTM :: IORef () -> STM a -> STM a
unsafeWithSTM r (STM f) = STM $ \s0 ->
  case f s0 of
    (# s1, a #) -> case touch# r s1 of
      s2 -> (# s2, a #)

----- construction

-- | How to customize the behavior of the 'Spy' and its 'Handler'
data Config = Config

instance Monoid Config where
  mempty = Config

instance Semigroup Config where
  Config <> Config = Config

-- | A 'Spy' configuration suitable for most use cases
defaultConfig :: Config
defaultConfig = Config

-- | Create a new 'Spy' and its 'Handler'
newIO :: Config -> IO (Spy a, Handler a)
newIO _config = do
    -- These IORefs are merely keys for the weak pointers (see the Haddock on
    -- 'Data.IORef.mkWeakIORef')
    spyKey     <- newIORef ()
    handlerKey <- newIORef ()   -- TODO wouldn't need this one if we had @primitiveInsideTQueue@

    q <- newTQueueIO

    -- Set to 'True' only by the spy's finalizer
    tombstone <- newTVarIO False

    -- There's no point in reporting/listening if the listener/reporter is
    -- already dead. In particular, if the handler confirms its spy is dead (or
    -- retired on a beach because they completed their mission, they should
    -- pack up their things and return to headquarters (ie release any
    -- resources etc).
    spyWeak <- mkWeakIORef_ spyKey     () (atomically $ writeTVar tombstone True)
    qWeak   <- mkWeakIORef_ handlerKey q  (pure ())

    pure (
        Spy spyKey qWeak id
      ,
        Handler handlerKey spyWeak tombstone id q
      )

----- Message

-- | Information the 'Spy' puts in each 'report'
--
-- Its 'Handler' can 'listenIO's these 'Message's.
data Message a = Message {
    messageCallStack :: !CallStack
    -- ^ where
  ,
    messageMonotonicTimeNSec :: !Word64
    -- ^ when
  ,
    messagePayload :: !a
    -- ^ what
  }
  deriving (Generic, Show)

instance Functor Message where
  fmap f msg = msg { messagePayload = f (messagePayload msg) }

----- Spy

-- | Who 'report's 'Message's
data Spy :: Type -> Type where
  Spy ::
       !(IORef ())
       -- ^ hold this because our handler monitors it to know if we're is still
       -- alive
    ->
       !(Weak (TQueue (Message x)))
       -- ^ kept alive by our handler
       --
       -- we only need to keep our key alive during actions that use this
       -- queue, since we don't want our 'Handler' to think we're dead /before/
       -- we write our last message to this queue
    ->
       !(a -> x)
    ->
       Spy a

-- | If the 'Spy' is alive, this 'IORef' will be too
primitiveInsideSpy :: Spy a -> IORef ()
primitiveInsideSpy (Spy k _qWeak _pre) = k

instance Contravariant Spy where
  contramap f (Spy k q pre) = Spy k q (pre . f)

-- | Report from within 'IO'
--
-- It's a noop if the 'Handler' is no longer alive.
reportIO :: HasCallStack => Spy a -> a -> IO ()
reportIO (Spy k qWeak pre) a = deRefWeak qWeak >>= \case
    Nothing -> pure ()
    Just q  -> do

      now <- getMonotonicTimeNSec

      -- The only part of this action that GHC could possible conclude will
      -- definitely diverge is @pre@. But if so, then the message will never be
      -- sent, and so its fine for our 'Handler' to have already concluded
      -- we're dead.
      unsafeWithIO k $ atomically $ writeTQueue q Message {
          messageCallStack = callStack
        ,
          messageMonotonicTimeNSec = now
        ,
          messagePayload = pre a
        }

-- | Report from within pure code
--
-- Use 'seq' and 'Control.Concurrent.pseq' to carefully control when a 'Spy'
-- 'report's.
report :: HasCallStack => Spy a -> a -> ()
report spy a = unsafePerformIO $ reportIO spy a

----- sneaking

-- | A synonym for 'Given' from the @reflection@ package, for
-- self-containedness of our type signatures
type Sneaking = Given

-- | This method lets a 'Spy' vanish into the type context
sneakingInto :: x -> (Sneaking x => r) -> r
sneakingInto = give

-- | 'report' using whichever 'Spy' has snuck in through the type context
sneak :: (HasCallStack, Sneaking x) => a -> (x -> Spy a) -> ()
sneak a f = report (f given) a

-- | 'reportIO' using whichever 'Spy' has snuck in through the type context
sneakIO :: (HasCallStack, Sneaking x) => a -> (x -> Spy a) -> IO ()
sneakIO a f = reportIO (f given) a

----- Handler

-- | Who 'listenIO's to 'Message's
data Handler :: Type -> Type where
  Handler ::
       !(IORef ())
       -- ^ hold this because our 'Spy' monitors it to know if we're still alive
    ->
       !(Weak ())
       -- ^ kept alive by our 'Spy' (TODO redundant with the @'TVar' Bool@?)
    ->
       !(TVar Bool)
       -- ^ set to 'True' only by the finalizer of our 'Spy'
    ->
       !(x -> a)
    ->
       !(TQueue (Message x))
       -- ^ we only need to keep our key alive during actions that use this
       -- queue, since this queue is the only thing affected by living spies
    ->
       Handler a

-- | If the 'Handler' is alive, this 'IORef' will be too
primitiveInsideHandler :: Handler a -> IORef ()
primitiveInsideHandler (Handler k _spyWeak _tombstone _post _q) = k

instance Functor Handler where
  fmap f (Handler k spyWeak tombstone post q) =
      Handler k spyWeak tombstone (f . post) q

-- | Listen for the latest message from the 'Spy', if any
tryListenIO :: Handler a -> IO (Maybe (Message a))
tryListenIO = atomically . tryListenSTM

-- | Listen for the latest message from the 'Spy', if any
tryListenSTM :: Handler a -> STM (Maybe (Message a))
tryListenSTM (Handler k _spyWeak _tombstone post q) = do
    fmap (fmap post) <$> unsafeWithSTM k (tryReadTQueue q)

-- | Listen for the latest message from the 'Spy'
--
-- This will never unblock if the 'Spy' is no longer alive, so consider racing
-- with 'isAliveSTM'.
listenIO :: Handler a -> IO (Message a)
listenIO = atomically . listenSTM

-- | Listen for the latest or next message from the 'Spy'
listenSTM :: Handler a -> STM (Message a)
listenSTM (Handler k _spyWeak _tombstone post q) =
    fmap post <$> unsafeWithSTM k (readTQueue q)

-- | Check if the 'Spy' is still alive
--
-- This will return a false positive when the finalizer of the 'Spy' is
-- imminent but still pending. (TODO: confirm?)
isAliveIO :: Handler a -> IO Bool
isAliveIO (Handler _k spyWeak _tombstone _post _q) =
    isJust <$> deRefWeak spyWeak

-- | Check if the 'Spy' is still alive
--
-- This will return a false positive when the finalizer of the 'Spy' is
-- imminent but still pending.
isAliveSTM :: Handler a -> STM Bool
isAliveSTM (Handler _k _spyWeak tombstone _post _q) =
    not <$> readTVar tombstone
