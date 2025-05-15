-- Emanuel Sjövall & Rafael Holgersson
module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
import Distribution.Simple.Command (reqArg)
import GHC.Profiling (requestHeapCensus)
import Distribution.Compiler (perCompilerFlavorToList)
import GHC.RTS.Flags (DebugFlags(stm))
type T = Statement
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    Skip |
    While Expr.T Statement |
    Read String |
    Write Expr.T |
    Begin [Statement]
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

ifer = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIf
buildIf ((cond, thenStmt), elseStmt) = If cond thenStmt elseStmt

skip = accept "skip" #- require ";" >-> const Skip

while = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (e, stmt) = While e stmt

readI = accept "read" -# word #- require ";" >-> Read

write = accept "write" -# Expr.parse #- require ";" >-> Write

begin = accept "begin" -# iter parse #- require "end" >-> Begin

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input =
    if Expr.value cond dict > 0
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
exec (Assignment v e: stmts) dict input = exec stmts (Dictionary.insert (v, Expr.value e dict) dict) input
exec (Skip: stmts) dict input = exec stmts dict input
exec (While cond stmt : stmts) dict input =
    if Expr.value cond dict > 0
    then exec (stmt:While cond stmt:stmts) dict input
    else exec stmts dict input
exec (Read s: stmts) dict (n:input) = exec stmts (Dictionary.insert (s, n) dict) input
exec (Write e: stmts) dict input = Expr.value e dict : exec stmts dict input
exec (Begin ostmts: stmts) dict input = exec (ostmts ++ stmts) dict input
exec [] _ _ = []

pad :: Int -> String
pad = concat . flip replicate "  "

privToString :: Int -> Statement -> String
privToString n (Assignment v e) = pad n ++ v ++ " := " ++ Expr.toString e ++ ";\n"
privToString n Skip = pad n ++ "skip;\n"
privToString n (If cond thenStnt elseStmt) = pad n ++ "if " ++ Expr.toString cond ++ " then\n" ++ privToString (n+1) thenStnt ++ pad n ++ "else\n" ++ privToString (n+1) elseStmt
privToString n (While cond stmt) = pad n ++ "while " ++ Expr.toString cond ++ " do\n" ++ privToString (n+1) stmt
privToString n (Read s) = pad n ++ "read " ++ s ++ ";\n"
privToString n (Write e) = pad n ++ "write " ++ Expr.toString e ++ ";\n"
privToString n (Begin stmts) = pad n ++ "begin\n" ++ concatMap (privToString (n+1)) stmts ++ pad n ++ "end\n"

instance Parse Statement where
  parse = assignment ! skip ! ifer ! while ! readI ! write ! begin
  toString = privToString 0
