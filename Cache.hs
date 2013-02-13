{-# LANGUAGE DoRec, TypeSynonymInstances, FlexibleInstances #-}
module Cache where

import Utils

import Data.Bits
import Data.HashTable (HashTable)
import qualified Data.HashTable as HashTable
import Data.Int
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust, fromJust)
import Data.Unique

import Control.Monad (when)

import System.IO.Unsafe
import System.Mem.StableName


class Hashable a where
    hash :: a -> Int32
    
instance Hashable String where
    hash s = HashTable.hashString s
    
instance Hashable Int where
    hash i = HashTable.hashInt i
    
instance Hashable Unique where
    hash u = hash (hashUnique u)
    
instance Hashable (StableName a) where
    hash n = hash (hashStableName n)
    
instance (Hashable a, Hashable b) => Hashable (a,b) where
    hash (x,y) = hash x `xor` HashTable.hashInt (fromIntegral $ hash y)
    

type LinkedList a = IORef (LinkedNode a)

data LinkedNode a = 
    LinkedNode { lnPrev :: LinkedList a, lnValue :: IORef a, 
                 lnNext :: LinkedList a }
  | End { lnPrev :: LinkedList a, lnNext :: LinkedList a }
  
isEnd (LinkedNode _ _ _) = False
isEnd (End        _   _) = True
    
newList :: IO (LinkedList a)
newList = do
 rec let end = End p n
     p <- newIORef end
     n <- newIORef end
 list <- newIORef end
 return list

newNode :: a -> IO (LinkedNode a)
newNode x = do let err = error "Cache: access to not-yet-linked node"
               p <- newIORef err; val <- newIORef x; n <- newIORef err
               return (LinkedNode p val n)
               
appendNode :: LinkedNode a -> LinkedList a -> IO ()
appendNode node list = do n <- readIORef list; p <- readIORef (lnPrev n)
                          writeIORef (lnNext p) node; writeIORef (lnPrev n) node
                          writeIORef (lnPrev node) p; writeIORef (lnNext node) n
                    
removeFirst :: LinkedList a -> IO a
removeFirst list = do l <- readIORef list; node <- readIORef (lnNext l)
                      removeNode node
                      readIORef (lnValue node)

removeNode :: LinkedNode a -> IO ()
removeNode node = do when (isEnd node) $ error "Cache: remove from empty list"
                     p <- readIORef (lnPrev node); n <- readIORef (lnNext node)
                     let err = error "Cache: access to unlinked node"
                     writeIORef (lnPrev node) err; writeIORef (lnNext node) err
                     writeIORef (lnNext p) n; writeIORef (lnPrev n) p
    
access :: LinkedList a -> LinkedNode a -> IO ()
access list node = do removeNode node; appendNode node list

add :: a -> LinkedList a -> IO (LinkedNode a)
add x list = do node <- newNode x; appendNode node list; return node


byAddress :: a -> StableName a
byAddress = unsafePerformIO . makeStableName


type Cache key value =
    (IORef Int, Int, HashTable key (value, LinkedNode key), LinkedList key)

newCache :: (Eq key, Hashable key) => Int -> Cache key value
newCache maxsize = unsafePerformIO $ do ht <- HashTable.new (==) hash
                                        lru <- newList; size <- newIORef 0
                                        return (size, maxsize, ht, lru)

cached :: (Eq k, Hashable k) => k -> Cache k v -> v -> v
cached key (sizeRef, maxsize, cache, lru) val = unsafePerformIO $ do
    mval' <- HashTable.lookup cache key
    if isJust mval' then do
        let (val', node) = fromJust mval'
        access lru node
        --putStrLn "Cache access"
        return val'
      else do
        size <- readIORef sizeRef
        --putStrLn ("Cache add, former size " ++ show size)
        if size < maxsize then writeIORef sizeRef (size+1)
                          else do dropped <- removeFirst lru
                                  HashTable.delete cache dropped
        node <- add key lru
        HashTable.insert cache key (val, node)
        return val
