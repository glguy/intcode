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

import Text.ParserCombinators.ReadP

-- | Parse a list of comma separated integers.
--
-- >>> parseInts "1,-2,3"
-- Just [1,-2,3]
parseInts :: String -> Maybe [Int]
parseInts str =
  case readP_to_S (parseIntsP <* eof) str of
    [(xs, rest)] | [("","")] <- lex rest -> Just xs
    _                                    -> Nothing

parseIntsP :: ReadP [Int]
parseIntsP = readS_to_P reads `sepBy` char ','
