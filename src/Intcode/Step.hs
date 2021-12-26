{-# Language Safe #-}
{-|
Module      : Intcode.Step
Description : Intcode small-step semantics
Copyright   : (c) Eric Mertens, 2019,2020
License     : ISC
Maintainer  : emertens@gmail.com

This module advances a 'Machine' by interpreting the opcode at the
current program counter.

-}
module Intcode.Step (Step(..), step) where

import Data.Traversable    (mapAccumL)
import Text.Show.Functions ()

import Intcode.Opcode      (Mode(..), Opcode(..), decode)
import Intcode.Machine     (Machine, (!), addRelBase, jmp, pc, relBase, set)

-- | Result of small-step semantics.
data Step
  = Step         !Machine    -- ^ update machine without output
  | StepOut !Int !Machine    -- ^ update machine with output
  | StepIn  (Int -> Machine) -- ^ machine blocked waiting for input
  | StepHalt                 -- ^ halt
  | StepFault                -- ^ bad instruction
  deriving Show

-- | Small-step semantics of virtual machine.
step :: Machine -> Step
step mach =
  case populateParams <$> decode (mach ! pc mach) of
    Nothing            -> StepFault
    Just (pc', opcode) -> opcodeImpl opcode $! jmp pc' mach

  where
    populateParams :: Opcode Mode -> (Int, Opcode Int)
    populateParams = mapWithIndex toPtr (pc mach + 1)

    toPtr :: Int -> Mode -> Int
    toPtr i Imm =        i
    toPtr i Abs = mach ! i
    toPtr i Rel = mach ! i + relBase mach

-- | Apply a decoded opcode to the machine state.
opcodeImpl ::
  Opcode Int {- ^ opcode with pointers    -} ->
  Machine    {- ^ machine with PC updated -} ->
  Step
opcodeImpl o m =
  case o of
    Add a b c -> Step    (set c (at a + at b) m)
    Mul a b c -> Step    (set c (at a * at b) m)
    Inp a     -> StepIn  (\i -> set a i m)
    Out a     -> StepOut (at a) m
    Jnz a b   -> Step    (if at a /= 0 then jmp (at b) m else m)
    Jz  a b   -> Step    (if at a == 0 then jmp (at b) m else m)
    Lt  a b c -> Step    (set c (if at a <  at b then 1 else 0) m)
    Eq  a b c -> Step    (set c (if at a == at b then 1 else 0) m)
    Arb a     -> Step    (addRelBase (at a) m)
    Hlt       -> StepHalt
  where
    at i = m ! i

mapWithIndex :: (Int -> a -> b) -> Int -> Opcode a -> (Int, Opcode b)
mapWithIndex f = mapAccumL (\i a -> (i+1, f i a))
{-# INLINE mapWithIndex #-}
