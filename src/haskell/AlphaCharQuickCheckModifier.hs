{-# LANGUAGE TypeApplications #-}

import Test.QuickCheck

newtype AlphaChar = AlphaChar {unAlphaChar :: Char} deriving (Eq, Show)

genAlphaChar :: Gen AlphaChar
genAlphaChar = do
    let lowerCaseChar = AlphaChar <$> arbitrary @Char `suchThat` isLowerCase
    let upperCaseChar = AlphaChar <$> arbitrary @Char `suchThat` isUpperCase
    oneof [lowerCaseChar, upperCaseChar]

instance Arbitrary AlphaChar where
    arbitrary = genAlphaChar
