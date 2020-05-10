module QntSyn.Output where

import QntSyn
import Text.Printf

isOperator :: String -> Bool
isOperator "" = False
isOperator x = head x `elem` "!#$%&*+./<=>?@\\^|-~:"

pPrintType :: Type -> String

pPrintType (TApplication (TApplication (TConcrete x) y) z)
  | isOperator x = printf "(%s %s %s)" (pPrintType y) x (pPrintType z)
  | otherwise    = printf "((%s %s) %s)" x (pPrintType y) (pPrintType z)

pPrintType (TApplication x y) = printf "(%s %s)" (pPrintType x) (pPrintType y)

pPrintType (TConcrete x)
  | isOperator x = printf "(%s)" x
  | otherwise = x

pPrintType (TVariable i) = 'a' : show i
