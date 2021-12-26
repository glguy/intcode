{-# Language Safe #-}
{-|
Module      : Intcode
Description : Intcode interpreter
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

Intcode is a virtual machine environment defined to have some arithmetic,
conditional jumps, and simple input and output facilities.

The instruction set is designed with independently selectable address modes for
each of its input and output parameters. The architecture is designed to be
simple to implement while powerful enough to write interesting programs
efficiently. The addition of a /relative base pointer/ makes it easy to
implement function calls in the language.

This Intcode architecture is defined across multiple
<https://adventofcode.com/2019/about Advent of Code 2019> tasks:
<https://adventofcode.com/2019/day/2 2>,
<https://adventofcode.com/2019/day/5 5>,
<https://adventofcode.com/2019/day/7 7>, and
<https://adventofcode.com/2019/day/9 9>

Common use modes:

* Machine construction: 'new'
* List functions: 'intcodeToList', 'effectList'
* Individual machine step processing: 'Step', 'step'
* Input/output interpretation: 'Effect', 'run'

Submodules:

* "Intcode.Machine" exposes the implementation details of the interpreter state.
* "Intcode.Parse" provides a parser for intcode text files.
* "Intcode.Opcode" provides types and the decoder for opcodes.

-}
module Intcode
  (
  -- * Simple list interface
  intcodeToList,

  -- * Machine state
  Machine, (!), new, set, memoryList,

  -- * Big-step semantics
  Effect(..), run,

  -- * Effect operations
  effectList,

  -- * Small-step semantics
  Step(..), step,

  -- * Exceptions
  IntcodeFault(..),

  -- * ASCII I/O interface
  runIO, hRunIO,

  ) where

import Control.Exception   (Exception(..), throw, throwIO)
import Data.Char           (chr, ord)
import System.IO           (Handle, hGetChar, hPutChar, hPutStrLn, stdin, stdout)

import Intcode.Machine     (Machine(..), (!), memoryList, new, set)
import Intcode.Step        (Step(..), step)

------------------------------------------------------------------------
-- ASCII I/O
------------------------------------------------------------------------

-- | Run intcode program using stdio. Non-ASCII outputs are printed as
-- integers.
--
-- Note that input and output is affected by handle buffering modes.
--
-- >>> runIO (run (new [104,72,104,101,104,108,104,108,104,111,104,33,104,10,99]))
-- Hello!
--
-- >>> runIO (run (new [104,-50,104,1000,99]))
-- <<-50>>
-- <<1000>>
runIO :: Effect -> IO ()
runIO = hRunIO stdin stdout

-- | 'runIO' generalized to an arbitrary input and output handle.
hRunIO ::
  Handle {- ^ input handle  -} ->
  Handle {- ^ output handle -} ->
  Effect {- ^ effect        -} ->
  IO ()
hRunIO inH outH = go
  where
    go (Output o e)
      | 0 <= o, o < 0x80 = hPutChar outH (chr (fromIntegral o)) >> go e
      | otherwise        = hPutStrLn outH ("<<" ++ show o ++ ">>") >> go e
    go (Input f)         = go . f . fromIntegral . ord =<< hGetChar inH
    go Halt              = return ()
    go Fault             = throwIO IntcodeFault

------------------------------------------------------------------------
-- High-level interface
------------------------------------------------------------------------

-- | Run a given memory image as a list transducer.
--
-- Use 'effectList' when you want to provide a specific 'Effect'.
--
-- Throws: 'IntcodeFault' when machine faults or too few inputs are provided.
--
--
-- >>> intcodeToList [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] <$> [[0],[10]]
-- [[0],[1]]
--
-- >>> intcodeToList [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] <$> [[0],[10]]
-- [[0],[1]]
--
-- >>> :{
-- >>> intcodeToList
-- >>>   [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
-- >>>    1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
-- >>>    999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
-- >>> <$> [[7],[8],[9]]
-- >>> :}
-- [[999],[1000],[1001]]
intcodeToList ::
  [Int] {- ^ initial memory -} ->
  [Int] {- ^ inputs         -} ->
  [Int] {- ^ outputs        -}
intcodeToList = effectList . run . new

-- | Evaluate a program's effect as a function from a list of
-- inputs to a list of outputs.
--
-- Throws: 'IntcodeFault' when machine faults or too few inputs are provided.
effectList ::
  Effect {- ^ program effect -} ->
  [Int]  {- ^ inputs         -} ->
  [Int]  {- ^ outputs        -}
effectList effect inputs =
  case effect of
    Fault      -> throw IntcodeFault
    Halt       -> []
    Output o e -> o : effectList e inputs
    Input f    ->
      case inputs of
        x:xs -> effectList (f x) xs
        []   -> throw IntcodeFault

------------------------------------------------------------------------
-- Big-step semantics
------------------------------------------------------------------------

-- | Possible effects from running a machine
data Effect
  = Output !Int Effect    -- ^ Output an integer
  | Input (Int -> Effect) -- ^ Input an integer
  | Halt                  -- ^ Halt execution
  | Fault                 -- ^ Execution failure
  deriving Show

-- | Big-step semantics of virtual machine. The implementation details
-- of 'Machine' are abstracted away and the program behavior can be
-- observed by interpreting the various 'Effect' constructors.
--
-- >>> run (new [1102,34915192,34915192,7,4,7,99,0])
-- Output 1219070632396864 Halt
--
-- >>> run (new [3,1,99])
-- Input <function>
run :: Machine -> Effect
run mach =
  case step mach of
    Step mach'        -> run mach'
    StepOut out mach' -> Output out (run mach')
    StepIn f          -> Input (run . f)
    StepHalt          -> Halt
    StepFault         -> Fault

------------------------------------------------------------------------
-- Exceptions
------------------------------------------------------------------------

-- | Error when a machine fails to decode an instruction.
data IntcodeFault = IntcodeFault
  deriving (Eq, Ord, Show, Read)

instance Exception IntcodeFault where
  displayException _ = "intcode machine fault"
