{-# Language DeriveTraversable, Safe #-}
{-|
Module      : Intcode.Opcode
Description : Intcode opcodes
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

This module provides a representation of the intcode machine's opcodes.

Opcodes are parameterized over their parameters. This allows the
implementation to store both parameter modes and resolved parameter
pointers in the same constructors.

-}
module Intcode.Opcode
  (
  -- * Types
  Opcode(..), Mode(..),

  -- * Decoder
  decode,
  ) where

------------------------------------------------------------------------
-- Opcode decoder
------------------------------------------------------------------------

-- | Parameter modes
data Mode
  = Abs -- ^ absolute position
  | Imm -- ^ immediate
  | Rel -- ^ relative position
  deriving (Eq, Ord, Read, Show)

-- | Opcodes parameterized over argument representations.
data Opcode a
  = Add !a !a !a -- ^ addition:        @c = a + b@
  | Mul !a !a !a -- ^ multiplication:  @c = a * b@
  | Inp !a       -- ^ input:           @a = input()@
  | Out !a       -- ^ output:          @output(a)@
  | Jnz !a !a    -- ^ jump-if-true:    @if a then goto b@
  | Jz  !a !a    -- ^ jump-if-false:   @if !a then goto b@
  | Lt  !a !a !a -- ^ less-than:       @c = a < b@
  | Eq  !a !a !a -- ^ equals:          @c = a == b@
  | Arb !a       -- ^ adjust-rel-base: @rel += a@
  | Hlt          -- ^ halt
  deriving (Eq, Ord, Read, Show, Functor, Foldable)

-- | Decode an instruction to determine the opcode and parameter modes.
--
-- >>> decode 1002
-- Just (Mul Abs Imm Abs)
decode :: Int {- ^ opcode -} -> Maybe (Opcode Mode)
decode n =
  case n `rem` 100 of
    1  -> fill (Add 1 2 3)
    2  -> fill (Mul 1 2 3)
    3  -> fill (Inp 1)
    4  -> fill (Out 1)
    5  -> fill (Jnz 1 2)
    6  -> fill (Jz  1 2)
    7  -> fill (Lt  1 2 3)
    8  -> fill (Eq  1 2 3)
    9  -> fill (Arb 1)
    99 -> fill Hlt
    _  -> Nothing
  where
    fill = traverse (parameter n)
{-# INLINABLE decode #-}

-- | Compute the parameter mode for an argument at a given position.
parameter ::
  Int {- ^ opcode   -} ->
  Int {- ^ position -} ->
  Maybe Mode
parameter n i =
  case digit (i+1) n of
    0 -> Just Abs
    1 -> Just Imm
    2 -> Just Rel
    _ -> Nothing

-- | Arguments visited from left to right.
instance Traversable Opcode where
  {-# INLINE traverse #-}
  traverse f o =
    case o of
      Add x y z -> Add <$> f x <*> f y <*> f z
      Mul x y z -> Mul <$> f x <*> f y <*> f z
      Inp x     -> Inp <$> f x
      Out x     -> Out <$> f x
      Jnz x y   -> Jnz <$> f x <*> f y
      Jz  x y   -> Jz  <$> f x <*> f y
      Lt  x y z -> Lt  <$> f x <*> f y <*> f z
      Eq  x y z -> Eq  <$> f x <*> f y <*> f z
      Arb x     -> Arb <$> f x
      Hlt       -> pure Hlt

-- | Extract the ith digit from a number.
--
-- >>> digit 0 2468
-- 8
-- >>> digit 3 2468
-- 2
-- >>> digit 4 2468
-- 0
digit :: Int {- ^ position -} -> Int {- ^ number -} -> Int {- ^ digit -}
digit i x = x `quot` (10^i) `rem` 10
