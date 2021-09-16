module Data.LengthParameterized.Word 
  ( Word
  , (**)
  , concat
  , cons
  , emptyWord
  , fromChar
  , head
  , unsafeToWord
  ) where

import Data.Maybe
import Data.String (singleton)
import Data.String as String
import Data.String.CodePoints (CodePoint, codePointFromChar)
import Data.String.CodeUnits (charAt)
import Data.Typelevel.Num hiding ((==))
import Data.Typelevel.Num.Aliases
import Partial.Unsafe
import Prelude
import Type.Proxy

-- The constructor must be hidden, otherwise one can
-- easily write, e.g., Word "foo" :: Word D1 
newtype Word n
  = Word String

instance showWord :: Show (Word n) where
  show (Word str) = str

-- Zero length Word
emptyWord :: Word D0
emptyWord = Word ""

-- Singleton Word
fromChar :: Char -> Word D1
fromChar ch = fromCodePoint (codePointFromChar ch)
  where
  fromCodePoint cp = Word (singleton cp)

-- Think of type signature as: Word m -> Word n -> Word (m + n)
concat :: forall m n p. Nat m => Nat n => Add m n p => Word m -> Word n -> Word p
concat (Word a) (Word b) = Word (a <> b)

-- Results in a compile-time error if we try to apply to an empty word
head :: forall n. (Pos n) => Word n -> Char
head (Word str) = unsafePartial (fromJust (charAt 0 str))

-- Think of type signature as: Char -> Word m -> Word (m + 1)
cons :: forall m n. Nat m => Nat n => Succ m n => Char -> Word m -> Word n
cons ch (Word str) = Word (singleton (codePointFromChar ch) <> str)

infixr 5 cons as **

-- Crashes at runtime if we try to create a Word of incorrect length
unsafeToWord :: forall n. (Nat n) => String -> Word n
unsafeToWord str =
  if String.length str == toInt' (Proxy :: Proxy n) then
    Word str
  else
    unsafeCrashWith "unsafeToWord"
