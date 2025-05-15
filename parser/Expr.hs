-- Emanuel Sjövall & Rafael Holgersson
module Expr(Expr, T, parse, fromString, value, toString) where

{-
   An expression of type Expr is a representation of an arithmetic expression 
   with integer constants and variables. A variable is a string of upper- 
   and lower case letters. The following functions are exported
   
   parse :: Parser Expr
   fromString :: String -> Expr
   toString :: Expr -> String
   value :: Expr -> Dictionary.T String Int -> Int
   
   parse is a parser for expressions as defined by the module Parser.
   It is suitable for use in parsers for languages containing expressions
   as a sublanguage.
   
   fromString expects its argument to contain an expression and returns the 
   corresponding Expr. 
  
   toString converts an expression to a string without unneccessary 
   parentheses and such that fromString (toString e) = e.
  
   value e env evaluates e in an environment env that is represented by a
   Dictionary.T Int.  
-}
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import Data.Maybe (fromMaybe)

data Expr = Num Integer | Var String | Add Expr Expr
       | Sub Expr Expr | Mul Expr Expr | Div Expr Expr | Expo Expr Expr
         deriving Show

type T = Expr

var, num, factor, term, expr, expo :: Parser Expr

term', expr', expo' :: Expr -> Parser Expr

var = word >-> Var

num = number >-> Num

expOp = lit '^' >-> (\_ -> Expo)

mulOp = lit '*' >-> (\_ -> Mul) !
        lit '/' >-> (\_ -> Div)

addOp = lit '+' >-> (\_ -> Add) !
        lit '-' >-> (\_ -> Sub)

bldOp e (oper,e') = oper e e'

factor = num !
         var !
         lit '(' -# expr #- lit ')' !
         err "illegal factor"

expo' e = expOp # expo >-> bldOp e #> expo' ! return e
expo = factor #> expo'

term' e = mulOp # expo >-> bldOp e #> term' ! return e
term = expo #> term'

expr' e = addOp # term >-> bldOp e #> expr' ! return e
expr = term #> expr'

parens cond str = if cond then "(" ++ str ++ ")" else str

shw :: Int -> Expr -> String
shw prec (Num n) = show n
shw prec (Var v) = v
shw prec (Add t u) = parens (prec>5) (shw 5 t ++ "+" ++ shw 5 u)
shw prec (Sub t u) = parens (prec>5) (shw 5 t ++ "-" ++ shw 6 u)
shw prec (Mul t u) = parens (prec>6) (shw 6 t ++ "*" ++ shw 6 u)
shw prec (Div t u) = parens (prec>6) (shw 6 t ++ "/" ++ shw 7 u)
shw prec (Expo t u) = parens (prec>7) (shw 7 t ++ "^" ++ shw 8 u)

value :: Expr -> Dictionary.T String Integer -> Integer
value (Num n) _ = n
value (Var v) env = fromMaybe (error ("undefined variable " ++ v)) (Dictionary.lookup v env)
value (Add lh rh) env = value lh env + value rh env
value (Sub lh rh) env = value lh env - value rh env
value (Mul lh rh) env = value lh env * value rh env
value (Div lh rh) env =
        let rh' = value rh env
        in if rh' == 0 then error "division by zero" else value lh env `div` rh'
value (Expo lh rh) env = value lh env ^ value rh env

instance Parse Expr where
    parse = expr
    toString = shw 0
