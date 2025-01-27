type t =
    | EOF
    | TLet
    | TDelim
    | TAssign
    | TIdent of string
    | TNumber of float
    | TString of string
    | TBool of bool
    | TLParen
    | TRParen
    | TLBrace
    | TRBrace
    | TLSquare
    | TRSquare
    | TColon
    | TComma
    | TPlus
    | TMinus
    | TMultiply
    | TDivide
    | TInput
    | TOutput

let pretty_print = function
    | EOF -> "EOF"
    | TLet -> "TLet"
    | TDelim -> "TDelim"
    | TAssign -> "TAssign"
    | TIdent(ident) -> Printf.sprintf "TIdent(%s)" ident
    | TString(string) -> Printf.sprintf "TString(%s)" string
    | TNumber(number) -> Printf.sprintf "TNumber(%f)" number
    | TBool(bool) -> Printf.sprintf "TBool(%b)" bool
    | TLParen -> "TLParen"
    | TRParen -> "TRParen"
    | TLBrace -> "TLBrace"
    | TRBrace -> "TRBrace"
    | TLSquare -> "TLSquare"
    | TRSquare -> "TRSquare"
    | TColon -> "TColon"
    | TComma -> "TComma"
    | TPlus -> "TPlus"
    | TMinus -> "TMinus"
    | TMultiply -> "TMultiply"
    | TDivide -> "TDivide"
    | TInput -> "TInput"
    | TOutput -> "TOutput"
