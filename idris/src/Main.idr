module Main

import Data.LengthParameterized.Word 

testWord1 : Word 3
testWord1 = drop 3 (take 6 (fromString "cabbagerolls"))

main : IO ()
main = print testWord1
