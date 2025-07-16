module SkipEOl where

-- / Skip end of line and whitespace beyond.
skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

-- / Definition in Attoparsec
skipEOL :: Parser ()
skipEOL = skipMany endOfLine
