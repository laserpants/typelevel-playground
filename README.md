# typelevel-playground 

### PureScript

```purescript
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
```

### Haskell (with GHC extensions)

```haskell

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import GHC.TypeLits
import Language.Haskell.TH
import Data.LengthParameterized.Word
import Prelude hiding (Word, concat, head)

-- Create a Word
foo :: Word 3
foo = $(word "foo")

-- We can even omit the type signature
foo2 = $(word "foo")
--
-- ... in ghci:
--
-- > :type foo2 
-- Word 3

-- Does not compile:
--bork :: Word 3
--bork = $(word "bork")

baz :: Word 6
baz = foo `concat` $(word "bar")

bar :: Char
bar = head foo

-- Does not compile:
--moo :: Char
--moo = head $(word "") 
```

### Idris

```idris
module Main

import Data.LengthParameterized.Word 

testWord1 : Word 3
testWord1 = drop 3 (take 6 (fromString "cabbagerolls"))

main : IO ()
main = print testWord1
```
