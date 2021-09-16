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

main :: Effect Unit
main = pure unit
