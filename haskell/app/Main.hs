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

main :: IO ()
main = pure ()
