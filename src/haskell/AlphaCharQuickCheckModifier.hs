{-# LANGUAGE TypeApplications #-}

import Test.QuickCheck
import Data.String (IsString (fromString))

-- https://stackoverflow.com/questions/20934506/haskell-quickcheck-how-to-generate-only-printable-strings

newtype AlphaChar = AlphaChar {unAlphaChar :: Char} deriving (Eq, Show)

genAlphaChar :: Gen AlphaChar
genAlphaChar = do
    let lowerCaseChar = AlphaChar <$> elements ['a' .. 'z']
    let upperCaseChar = AlphaChar <$> elements ['A' .. 'Z']
    oneof [lowerCaseChar, upperCaseChar]

instance Arbitrary AlphaChar where
    arbitrary = genAlphaChar

newtype AlphabeticString a = AlphabeticString {unAlphabeticString :: a} deriving (Eq, Show, Ord)

instance (IsString a) => Arbitrary (AlphabeticString a) where
    arbitrary = AlphabeticString . fromString <$> listOf ((.unAlphaChar) <$> genAlphaChar)
