module Main where

import Prelude

import Data.LengthParameterized.Word
import Data.Typelevel.Num
import Effect (Effect)

cabbageRolls :: Word D12
cabbageRolls =
  let
    cabbage = unsafeToWord "cabbage" :: Word D7

    rolls = unsafeToWord "rolls" :: Word D5
  in
    cabbage `concat` rolls

-- This is where it gets awkward: 
hello :: Word D5
hello = 'h' ** 'e' ** 'l' ** 'l' ** 'o' ** emptyWord

-- doesNotCompile :: Word D2
-- doesNotCompile = 'h' ** 'e' ** 'l' ** 'l' ** 'o' ** emptyWord

main :: Effect Unit
main = pure unit
