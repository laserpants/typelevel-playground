module Data.LengthParameterized.Word

import Data.String

export
data Word : Nat -> Type where
  StrWord : String -> Word n

export
Show (Word n) where
  show (StrWord str) = str

export
emptyWord : Word 0
emptyWord = StrWord ""

export
toString : Word m -> String
toString (StrWord str) = str

export
fromString : (s : String) -> Word (length s)
fromString str = StrWord str 

export
concat : Word m -> Word n -> Word (m + n)
concat s t = StrWord (toString s ++ toString t)

export
cons : Char -> Word n -> Word (n + 1)
cons ch (StrWord str) = StrWord (singleton ch ++ str)

export
head : Word (S n) -> Char
head (StrWord str) = assert_total (strIndex str 0)

export
drop : (n : Nat) -> Word m -> Word (m `minus` n)
drop n (StrWord str) = StrWord (substr n (length str `minus` n) str)

export
take : (n : Nat) -> Word m -> Word n
take n (StrWord str) = StrWord (substr 0 n str)
