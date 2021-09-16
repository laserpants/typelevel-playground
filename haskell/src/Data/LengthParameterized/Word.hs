{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
module Data.LengthParameterized.Word
  ( Word  -- Constructor is hidden
  , concat
  , cons
  , emptyWord
  , fromChar
  , head
  , word
  ) where

import GHC.TypeLits
import Language.Haskell.TH
import Prelude hiding (Word, head, concat)
import qualified Prelude as Prelude

newtype Word (n :: Nat)
  = Word String
  deriving (Show, Eq, Ord)

emptyWord :: Word (0 :: Nat)
emptyWord = Word ""

fromChar :: Char -> Word (1 :: Nat)
fromChar ch = Word [ch]

concat :: Word m -> Word n -> Word (m + n)
concat (Word a) (Word b) = Word (a <> b)

cons :: Char -> Word m -> Word (m + 1)
cons ch (Word str) = Word (ch:str)

head :: (1 <= n) => Word n -> Char
head (Word str) = Prelude.head str

word :: String -> Q Exp
word str = return $ 
    SigE (AppE (ConE 'Word) (LitE (StringL str))) 
         (AppT (ConT ''Word) (LitT (NumTyLit (fromIntegral (length str)))))
