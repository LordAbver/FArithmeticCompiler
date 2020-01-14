module Types
open System

//Supported operators
type Operators =
  | Add      // '+'
  | Sub      // '-'
  | Mul      // '*'
  | Div      // '/'
    
type Lexem =
  | Const of double       // Numeric const, double here, because the result can be not integer
  | OpenPs                // '('
  | ClosePs               // ')'
  | Operator of Operators // Any Supported operator
