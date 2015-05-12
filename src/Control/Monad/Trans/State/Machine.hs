{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StaticPointers             #-}

module Control.Monad.Trans.State.Machine where

import Control.Monad.State.Class (MonadState, get)
import Control.Monad.Trans.State.Lazy hiding (get)

import Data.Typeable (Typeable)

import GHC.Generics (Generic)
import GHC.StaticPtr

import System.IO.Unsafe (unsafePerformIO)

-- | Stored step in a state machine with no final result type
data StoredStep s = StoredStep s StaticKey
  deriving (Generic, Typeable)

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
    Start (StateT s m (SM s m)) s
  | Continue (StoredStep s)
  | Fork (SM s m)
  | Stop

-- | Executes a single step of a state machine, returning the next step(s).
stepSM :: (Typeable s, Monad m) => SM s m -> m [SM s m]
stepSM (Start a s) = evalStateT a s >>= \x -> return [x]
stepSM (Continue m) = runStep m >>= \x -> return [x]
stepSM (Fork a) = stepSM a >>= \b -> return $ a:b
stepSM Stop = return []

-- | Begin a state machine
start :: forall m s. Monad m => StateT s m (SM s m) -> s -> SM s m
start = Start

-- | Terminate a state machine
stop :: forall m s. Monad m => StateT s m (SM s m)
stop = return Stop

-- | Suspend a state machine computation, giving a pointer to the
--   next computation.
continue :: (Typeable s, MonadState s (StateT s m))
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
