{-# LANGUAGE AutoDeriveTypeable         #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StaticPointers             #-}

import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.State.Machine

import qualified Data.Binary as B
import Data.Typeable (Typeable)

import GHC.StaticPtr

first :: StateT Int IO (SM Int IO)
first = do
  n <- get
  put $ n + 3
  continue ptr2
middle :: StateT Int IO (SM Int IO)
middle =  do
  n <- get
  put $ n*2
  continue ptr3
end :: StateT Int IO (SM Int IO)
end = do
  n <- get
  liftIO $ putStrLn . show $ n
  stop
ptr1 = static first
ptr2 = static middle
ptr3 = static end

stateMachine :: StateT Int IO (SM Int IO)
stateMachine = continue ptr1 >>= fork where

runMachine :: (B.Binary a, Typeable a)
           => [SM a IO] -> IO ()
runMachine [] = return ()
runMachine (x:xs) = do
  sm' <- stepSM x
  B.encodeFile "/tmp/static-cont-stored" sm'
  sm'' <- B.decodeFile "/tmp/static-cont-stored"
  runMachine (xs ++ sm'')

main :: IO ()
main = runMachine [start sm1 0, start sm1 1]
  where
    sm1 = static stateMachine
