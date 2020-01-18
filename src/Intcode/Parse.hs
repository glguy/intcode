{-# Language Safe #-}
{-|
Module      : Intcode.Parse
Description : Intcode source file parser
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

This module implements a parser for the simple comma, separated format
used in the Advent of Code input files.

-}
module Intcode.Parse (parseInts) where

-- | Parse a list of comma separated integers.
--
-- >>> parseInts "1, - 2, 3,-4"
-- Just [1,-2,3,-4]
--
-- >>> parseInts " "
-- Just []
--
-- >>> parseInts "1,2,3,x"
-- Nothing
parseInts ::
  String      {- ^ parser input    -} ->
  Maybe [Int] {- ^ parsed integers -}
parseInts str
  | [(i,str1)] <- reads str = parseInts' [i] str1
  | [("","")]  <- lex str   = Just []
  | otherwise               = Nothing

-- | Helper function for 'parseInts'
parseInts' ::
  [Int]       {- ^ reversed accumulator -} ->
  String      {- ^ parser input         -} ->
  Maybe [Int] {- ^ parsed integers      -}
parseInts' xs str =
  case lex str of
    [(",",str1)] | [(x,str2)] <- reads str1 -> parseInts' (x:xs) str2
    [("","")]                               -> Just (reverse xs)
    _                                       -> Nothing
