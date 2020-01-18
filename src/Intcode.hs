{-# Language DeriveTraversable, Safe #-}
{-|
Module      : Intcode
Description : Intcode interpreter
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

This Intcode interpreter is defined across multiple Advent of Code days:

* <https://adventofcode.com/2019/day/2>
* <https://adventofcode.com/2019/day/5>
* <https://adventofcode.com/2019/day/7>
* <https://adventofcode.com/2019/day/9>

This implementation works with the following passes:

  1. Parse input text file into a list of numbers
  2. Execute op codes to single-step input/output effects.
  3. Execute single-stop effects into big-step effects.

Common use modes:

* List functions: 'intcodeToList'
* Individual machine step processing: 'Step', 'new', 'step'
* Input/output interpretation: 'Effect', 'new', 'run'

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
  (>>>), followedBy, feedInput, effectList,

  -- * Small-step semantics
  Step(..), step,

  -- * Exceptions
  IntcodeFault(..),

  -- * ASCII I/O interface
  runIO, hRunIO,

  ) where

import Control.Exception   (Exception(..), throw, throwIO)
import Data.Char           (chr, ord)
import Data.Traversable    (mapAccumL)
import System.IO           (Handle, hGetChar, hPutChar, hPutStrLn, stdin, stdout)
import Text.Show.Functions ()

import Intcode.Machine     (Machine(..), (!), addRelBase, jmp, memoryList, new, set)
import Intcode.Opcode      (Mode(..), Opcode(..), decode)

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
    Input f | x:xs <- inputs -> effectList (f x) xs
            | otherwise      -> throw IntcodeFault
    Output o e               -> o : effectList e inputs
    Halt                     -> []
    Fault                    -> throw IntcodeFault

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

-- | Compose two effects together. Outputs from first argument are
-- used as inputs to the second effect. Composed effect halts when
-- the second machine halts.
--
-- >>> let mult n = Input (\i -> Output (i*n) Halt)
-- >>> let add  n = Input (\i -> Output (i+n) Halt)
-- >>> effectList (mult 3 >>> add 1) [4]
-- [13]
(>>>) :: Effect -> Effect -> Effect
x          >>> Output o y = Output o (x >>> y)
_          >>> Halt       = Halt
_          >>> Fault      = Fault
Output o x >>> Input f    = x >>> f o
Halt       >>> Input _    = Fault
Fault      >>> Input _    = Fault
Input f    >>> y          = Input (\i -> f i >>> y)

infixl 9 >>>

-- | Run first effect until it halts, then run the second effect.
--
-- >>> Output 1 Halt `followedBy` Output 2 Halt
-- Output 1 (Output 2 Halt)
--
-- >>> Output 1 Halt `followedBy` Fault
-- Output 1 Fault
--
-- >>> Fault `followedBy` undefined
-- Fault
followedBy :: Effect -> Effect -> Effect
followedBy Halt         y = y
followedBy Fault        _ = Fault
followedBy (Output o x) y = Output o (followedBy x y)
followedBy (Input  f  ) y = Input (\i -> followedBy (f i) y)

-- | Provide an input to the first occurrence of an input request
-- in a program effect. It is considered a fault if a program
-- terminates before using the input.
--
-- >>> feedInput [5,6] (Input (\x -> Input (\y -> Output (x*y) Halt)))
-- Output 30 Halt
--
-- >>> feedInput [7] Halt
-- Fault
feedInput :: [Int] {- ^ inputs -} -> Effect -> Effect
feedInput []     e            = e
feedInput xs     (Output o e) = Output o (feedInput xs e)
feedInput (x:xs) (Input f)    = feedInput xs (f x)
feedInput _ _                 = Fault

------------------------------------------------------------------------
-- Small-step semantics
------------------------------------------------------------------------

-- | Result of small-step semantics.
data Step
  = Step    !Machine         -- ^ no effect
  | StepOut !Int !Machine    -- ^ output
  | StepIn  (Int -> Machine) -- ^ input
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

------------------------------------------------------------------------
-- Exceptions
------------------------------------------------------------------------

-- | Error when a machine fails to decode an instruction.
data IntcodeFault = IntcodeFault
  deriving (Eq, Ord, Show, Read)

instance Exception IntcodeFault where
  displayException _ = "intcode machine fault"
