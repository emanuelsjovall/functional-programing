-- Emanuel Sjövall & Rafael Holgersson
module Parser(module CoreParser, T, digit, digitVal, chars, letter, err,
              lit, number, iter, accept, require, token,
              spaces, word, (-#), (#-)) where
import Prelude hiding (return, fail)
import Data.Char
import CoreParser
infixl 7 -#, #-

type T a = Parser a

err :: String -> Parser a
err message cs = error (message++" near "++cs++"\n")

iter :: Parser a -> Parser [a]
iter m = m # iter m >-> cons ! return []

cons(a, b) = a:b

(-#) :: Parser a -> Parser b -> Parser b
m -# n = (m # n) >-> snd

(#-) :: Parser a -> Parser b -> Parser a
m #- n = (m # n) >-> fst

untilNewline :: Parser String
untilNewline = iter (char ? (/= '\n')) #- lit '\n'

comment :: Parser String
comment = lit '-' -# lit '-' -# iter (char ? (/= '\n')) #- lit '\n'

spaces :: Parser String
spaces = 
    (space # iter space >-> cons) #- comment ! -- spaces then comment
    (space # iter space >-> cons) !            -- just spaces
    comment !                                  -- just comment
    return []                                  -- nothing

space :: Parser Char
space [] = char []
space s = if isSpace $ head s then char s else fail s

token :: Parser a -> Parser a
token m = m #- spaces

letter :: Parser Char
letter [] = char []
letter s = if isAlpha $ head s then char s else fail s

word :: Parser String
word = token (letter # iter letter >-> cons)

chars :: Int -> Parser String
chars 0 = return []
chars n = char # chars (n-1) >-> cons

accept :: String -> Parser String
accept w = (token (chars (length w))) ? (==w)

require :: String -> Parser String
require w = accept w ! err ("expecting " ++ w)

lit :: Char -> Parser Char
lit c = token char ? (==c)

digit :: Parser Char
digit = char ? isDigit

digitVal :: Parser Integer
digitVal = digit >-> digitToInt >-> fromIntegral

number' :: Integer -> Parser Integer
number' n = digitVal #> (\ d -> number' (10*n+d))
          ! return n
number :: Parser Integer
number = token (digitVal #> number')

