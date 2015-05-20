{-# LANGUAGE AutoDeriveTypeable         #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StaticPointers             #-}

import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.State.Machine

import Data.Typeable (Typeable)

first :: StateT Int IO (SM Int IO)
first = do
  b1 <- continue (static br1)
  b2 <- continue (static br2)
  switch [b1,b2]
br1 :: StateT Int IO (SM Int IO)
br1 = do
  liftIO $ putStrLn "Awaking br1"
  n <- get
  if n > 10 then
    continue (static end)
  else do
    liftIO $ putStrLn "Suspending br1"
    suspend
br2 :: StateT Int IO (SM Int IO)
br2 = do
  liftIO $ putStrLn "Awaking br2"
  n <- get
  if n < 5 then
    continue (static end)
  else do
    liftIO $ putStrLn "Suspending br2"
    suspend
end :: StateT Int IO (SM Int IO)
end = do
  n <- get
  liftIO $ putStrLn . show $ n
  stop

stateMachine :: StateT Int IO (SM Int IO)
stateMachine = continue (static first)

runMachine :: Typeable a
           => [SM a IO] -> IO ()
runMachine [] = return ()
runMachine (x:xs) = do
  sm' <- stepSM x
  runMachine (xs ++ sm')

main :: IO ()
main = runMachine [start sm1 0, start sm1 11]
  where
    sm1 = static stateMachine
