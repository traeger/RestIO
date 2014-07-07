-----------------------------------------------------------------------------
-- |
-- Module      :  RestIO.HistoryBuffer
-- Copyright   :  (c) 2014 Marco Traeger (marco.traeger@googlemail.com)
-- License     :  MIT License (see http://opensource.org/licenses/MIT)
--
-- Maintainer  :  marco.traeger@googlemail.com 
-- Stability   :  experimental
--
-- A STM-based thread-save queue-like buffer with history access for readers.
--
-----------------------------------------------------------------------------

module RestIO.HistoryBuffer where

import Control.Monad
import Data.Array.Base
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TArray
import GHC.Conc
import Debug.Trace
import Data.List (intercalate)

import Prelude hiding (read)

-- | Imperative driven / STM-based implementation 
-- of the queue-like buffer with history access for readers
-- The implemention is not
data HistoryBuffer e = HistoryBuffer 
  { getRing :: (TArray Int e)
  , getSize :: Int
  , getFront :: TVar Int
  , getCount :: TVar Int
  , getLastIdx :: TVar Int }

-----------------------------------------------------------------------------
-- Creates an empty buffer of a given size and default value for
-- non-used buffer spaces
empty :: Int -> e -> IO (HistoryBuffer e)
empty size e = atomically $ do
  ring <- newArray (0, size-1) e
  front <- newTVar 0
  count <- newTVar 0
  lastIdx <- newTVar 0
  return $ HistoryBuffer ring size front count lastIdx

-----------------------------------------------------------------------------
-- Operations

-- | Push a new element to the buffer
push :: HistoryBuffer e -> e -> IO ()
push buffer e = atomically $ do
  let cap = capacity buffer
  full <- isFull buffer
  front <- readTVar (getFront buffer)
  count <- readTVar (getCount buffer)
  lastIdx <- readTVar (getLastIdx buffer)

  let end = (front + count) `mod` cap
  writeArray (getRing buffer) end e
  when full $ 
       (writeTVar (getFront buffer) $ (front + 1) `mod` cap)
    >> (writeTVar (getLastIdx buffer) $ (lastIdx + 1))
  when (not full) $ 
       (writeTVar (getCount buffer) $ count + 1)

  return ()

-- | Read all elements in the buffer which are newer then the timeindex @i@
-- A tuple @(lastSeen, es)@ is returned, where @lastSeen@ is the newest 
-- timeindex and @es@ are the newest elements starting at timeindex @i+1@.
-- Elements in @es@ are skipped if they are older then capacity of the history.
read :: HistoryBuffer e -> Int -> IO (Int, [e])
read buffer i = atomically $ do
  let cap = capacity buffer
  front <- readTVar (getFront buffer)
  count <- readTVar (getCount buffer)
  lastIdx <- readTVar (getLastIdx buffer)
  let readFromIdx = max lastIdx (i+1)
  let readFrom = (front + readFromIdx - lastIdx) `mod` cap
  let readCount = count - (readFromIdx - lastIdx)

  es <- sequence [readArray (getRing buffer) j | j <- readIndices readFrom readCount cap]
  return (lastIdx + count - 1, es)

-- Indices to read from the internal ring buffer when beginning at index @start@,
-- @count@ indices need to be read, and the buffer have the given @capacity@
readIndices :: Int -> Int -> Int -> [Int]
readIndices start count capacity = idxR ++ idxL where
  countR = min count (capacity - start)
  countL = max 0 (count - countR)
  idxR = if countR > 0 then [start .. start+countR-1] else []
  idxL = if countL > 0 then [0 .. countL-1] else []

-----------------------------------------------------------------------------
-- Properties

-- | Maxium number of element which are stored in the history
-- before dropped.
capacity :: HistoryBuffer e -> Int
capacity buffer = getSize buffer

-- | Whether the buffer is empty.
isEmpty :: HistoryBuffer e -> STM Bool
isEmpty buffer = do
  let cap = capacity buffer
  count <- readTVar (getCount buffer)
  return $ count == 0 && cap > 0

-- | Whether the buffer is full.
isFull :: HistoryBuffer e -> STM Bool
isFull buffer = do
  let cap = capacity buffer
  count <- readTVar (getCount buffer)
  return $ count == cap

-----------------------------------------------------------------------------
-- Debug

-- | Dump the buffer in a human readable manner.
dump :: Show e => HistoryBuffer e -> IO String
dump buffer = atomically $ do
  let cap = capacity buffer
  full <- isFull buffer
  front <- readTVar (getFront buffer)
  count <- readTVar (getCount buffer)
  lastIdx <- readTVar (getLastIdx buffer)
  elems <- getElems (getRing buffer)

  return $ 
       show elems 
    ++ " cap: " ++ show cap ++ " f:" ++ show front 
    ++ " c:" ++ show count ++ " isfull: " ++ show full 
    ++ " lastidx: " ++ show lastIdx

-- | A simple testrun
test = do 
  buffer <- empty 3 ""
  putStrLn =<< (liftM show $ read buffer (-1))
  putStrLn =<< dump buffer
  push buffer "0"
  putStrLn =<< (liftM show $ read buffer (-1))
  putStrLn =<< dump buffer
  push buffer "1"
  putStrLn =<< (liftM show $ read buffer (-1))
  putStrLn =<< dump buffer
  push buffer "2"
  putStrLn =<< (liftM show $ read buffer (-1))
  putStrLn =<< dump buffer
  push buffer "3"
  putStrLn =<< (liftM show $ read buffer (-1))
  putStrLn =<< dump buffer
  push buffer "4"
  putStrLn =<< (liftM show $ read buffer (-1))
  putStrLn =<< dump buffer

  putStrLn =<< (liftM show $ read buffer 2)
--  elm <- readUnsafe arr 1
--  return elm