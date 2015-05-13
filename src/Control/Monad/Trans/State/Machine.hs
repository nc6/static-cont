{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StaticPointers             #-}

module Control.Monad.Trans.State.Machine (
    SM
  , stepSM
  , continue
  , fork
  , start
  , stop
  ) where

import Control.Monad.Trans.State.Lazy

import Data.Binary (Binary)
import qualified Data.Binary as B
import Data.Typeable (Typeable)

import GHC.Fingerprint.Type (Fingerprint(..))
import GHC.Generics (Generic)
import GHC.StaticPtr

import System.IO.Unsafe (unsafePerformIO)

-- | Stored step in a state machine with no final result type
data StoredStep s = StoredStep s StaticKey
  deriving (Generic, Typeable)

instance Binary s => Binary (StoredStep s) where
  put (StoredStep s (Fingerprint w1 w2)) = do
    B.put s >> B.put w1 >> B.put w2
  get = do
    s <- B.get
    w1 <- B.get
    w2 <- B.get
    return $ StoredStep s (Fingerprint w1 w2)

-- | Run a stored step, yielding the next step.
runStep :: forall s m. (Monad m, Typeable s)
        => StoredStep s
        -> m (SM s m)
runStep (StoredStep s key) =
  case (unsafePerformIO $ unsafeLookupStaticPtr key) of
    Just sp -> evalStateT act s
      where
        act :: StateT s m (SM s m)
        act = deRefStaticPtr sp
    Nothing -> evalStateT stop s

-- | State machine parametrized by:
--
-- * @s@ The state threading through the computation
-- * @m@ The underlying monad.
data SM s (m :: * -> *) =
    Continue (StoredStep s)
  | Fork (SM s m)
  | Stop
  deriving (Generic, Typeable)

instance Binary s => Binary (SM s m)

-- | Executes a single step of a state machine, returning the next step(s).
stepSM :: (Typeable s, Monad m) => SM s m -> m [SM s m]
stepSM (Continue m) = runStep m >>= \x -> return [x]
stepSM (Fork a) = stepSM a >>= \b -> return $ a:b
stepSM Stop = return []

-- | Begin a state machine
start :: forall m s. Monad m
      => StaticPtr (StateT s m (SM s m))
      -> s
      -> SM s m
start sp s = Continue $ StoredStep s (staticKey sp)

-- | Terminate a state machine
stop :: forall m s. Monad m => StateT s m (SM s m)
stop = return Stop

-- | Suspend a state machine computation, giving a pointer to the
--   next computation.
continue :: (Monad m, Typeable s)
         => StaticPtr (StateT s m (SM s m))
         -> StateT s m (SM s m)
continue sp = do
  s <- get
  let ss = StoredStep s (staticKey sp)
  return $ Continue ss

-- | Fork the computation, returning both the current step and
--   the next steps.
fork :: forall m s. Monad m => SM s m -> StateT s m (SM s m)
fork = return . Fork
