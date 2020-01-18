{-# Language Trustworthy #-}
{-|
Module      : Intcode.Machine
Description : Intcode machine representation
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

The module implements the representation of the intcode machine state.

The 'Machine' type stores the initial memory image in an array and
only stores changes to that initial image. This allows for more efficient
comparisons of machine states for equality when there are few changes to
memory.

This implementation of the machine supports negative memory addresses.
These are defined not to be used in the Advent of Code problems.

This implementation stores machine-sized 'Int' values in memory.

-}
module Intcode.Machine
  (
  -- * Machine state
  Machine(..), new,

  -- * Register operations
  jmp, addRelBase,

  -- * Memory operations
  (!), set, memoryList,
  )
 where

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Primitive.PrimArray as P

-- | Machine state.
data Machine = Machine
  { pc         :: !Int          -- ^ program counter
  , relBase    :: !Int          -- ^ relative base pointer
  , memUpdates :: !(IntMap Int) -- ^ memory updates
  , memInitial :: {-# Unpack #-} !(P.PrimArray Int) -- ^ initial memory
  }
  deriving (Eq, Ord, Show)

-- | Value stored in initial memory image at given index.
indexImage ::
  Machine {- ^ machine  -} ->
  Int     {- ^ position -} ->
  Int     {- ^ value    -}
indexImage m i
  | a `seq` True, 0 <= i, i < P.sizeofPrimArray a = P.indexPrimArray a i
  | otherwise                                     = 0
  where
    a = memInitial m
{-# INLINE indexImage #-}

-- | Memory lookup.
(!) ::
  Machine {- ^ machine  -} ->
  Int     {- ^ position -} ->
  Int     {- ^ value    -}
m ! i = IntMap.findWithDefault (indexImage m i) i (memUpdates m)
{-# INLINE (!) #-}

-- | Construct machine from a list of initial values starting
-- at address 0. Program counter and relative base start at 0.
new ::
  [Int] {- ^ initial memory -} ->
  Machine
new initialValues = Machine
  { pc         = 0
  , relBase    = 0
  , memUpdates = IntMap.empty
  , memInitial = P.primArrayFromList initialValues
  }

-- | Store value at given memory position.
set ::
  Int {- ^ position -} ->
  Int {- ^ value    -} ->
  Machine -> Machine
set i v m
  | v == o    = m { memUpdates = IntMap.delete i   (memUpdates m) }
  | otherwise = m { memUpdates = IntMap.insert i v (memUpdates m) }
  where
    o = indexImage m i

-- | Add offset to relative base pointer.
addRelBase ::
  Int {- ^ offset -} ->
  Machine -> Machine
addRelBase i mach = mach { relBase = relBase mach + i }
{-# INLINE addRelBase #-}

-- | Set program counter to a new address.
jmp ::
  Int {- ^ program counter -} ->
  Machine -> Machine
jmp i mach = mach { pc = i }
{-# INLINE jmp #-}

-- | Generate a list representation of memory starting from
-- zero. This can get big for sparsely filled memory using
-- large addresses. Returned values start at position 0.
--
-- >>> memoryList (set 8 10 (new [1,2,3]))
-- [1,2,3,0,0,0,0,0,10]
memoryList :: Machine -> [Int]
memoryList mach
  | IntMap.null (memUpdates mach) = P.primArrayToList (memInitial mach)
  | otherwise                  = [mach ! i | i <- [0 .. top]]
  where
    top = max (P.sizeofPrimArray (memInitial mach) - 1)
              (fst (IntMap.findMax (memUpdates mach)))
