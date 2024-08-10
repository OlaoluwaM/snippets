-- https://akashagrawal.me/2017/01/19/beginners-guide-to-megaparsec.html
-- https://markkarpov.com/tutorial/megaparsec.html
-- https://akashagrawal.me/2017/01/19/beginners-guide-to-megaparsec.html

module MegaparsecTut where

import Data.Text (Text)

import Control.Monad (guard, void, when)
import Data.Text qualified as T
import Data.Void
import System.Environment
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

singleLetterP :: Parser Text
singleLetterP = string "p"

singleLetterHOrJ :: Parser Char
singleLetterHOrJ = char 'h' <|> char 'j'

letterHA :: Parser Char
letterHA = char 'h' >> char 'a'

bar :: Parser (Int, Int)
bar = do
    lowerBoundStr <- some digitChar
    void $ char '-'
    upperBoundStr <- some digitChar
    pure (read @Int lowerBoundStr, read @Int upperBoundStr)

bar'' :: Parser (Int, Int)
bar'' = do
    lowerBoundStr <- some digitChar
    void $ char '-'
    upperBoundStr <- some digitChar
    let lowerBound = read @Int lowerBoundStr
    let upperBound = read @Int upperBoundStr
    when (upperBound < lowerBound) (fail [fmt|expecting upperBound ({upperBound}) to be greater than lowerBound ({lowerBound})|])
    pure (lowerBound, upperBound)

barAlt :: Parser (Int, Int)
barAlt = do
    lowerBoundStr <- some digitChar
    void $ char '-'
    upperBoundStr <- some digitChar
    let lowerBound = read @Int lowerBoundStr
    let upperBound = read @Int upperBoundStr
    guard (upperBound >= lowerBound)
    pure (lowerBound, upperBound)

bar' :: Parser [Char]
bar' = digitChar `sepBy` char '-'

foo :: Parser [Char]
foo = many digitChar

baz :: Text -> Parser ([Char], [Char])
baz x = let (a, b) = T.partition (== ',') x in (,) <$> (setInput a >> some digitChar) <*> (setInput b >> some digitChar)

main :: IO ()
main = getArgs >>= (parseTest singleLetterP . head) . map T.pack

mainOR :: IO ()
mainOR = getArgs >>= (parseTest singleLetterHOrJ . head) . map T.pack

mainAnd :: IO ()
mainAnd = getArgs >>= (parseTest letterHA . head) . map T.pack
