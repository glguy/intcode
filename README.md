# Intcode Interpreter

[![Hackage](https://img.shields.io/hackage/v/intcode.svg)](https://hackage.haskell.org/package/intcode) [![Build Status](https://secure.travis-ci.org/glguy/intcode.png?branch=master)](http://travis-ci.org/glguy/intcode)

Implementation of the Intcode virtual machine as defined by
[Advent of Code 2019](https://adventofcode.com/2019).

This implementation provides an efficient, pure implementation
of the interpreter and exposes multiple levels of abstraction
to make it easy to use in a variety of situations.

This implementation is derived from my puzzle solutions
[glguy/advent2019](https://github.com/glguy/advent2019).
Example uses include
[Day13.hs](https://github.com/glguy/advent2019/blob/master/execs/Day13.hs)
and
[Day15.hs](https://github.com/glguy/advent2019/blob/master/execs/Day15.hs).
