type t =
    | EOF
    | TLet
    | TDelim
    | TAssign
    | TIdent of string
    | TNumber of float
    | TString of string
    | TTrue
    | TFalse
    | TLBrace
    | TRBrace
    | TLSquare
    | TRSquare
    | TColon
    | TComma
    | TNewline
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
    | TTrue -> "TTrue"
    | TFalse -> "TFalse"
    | TLBrace -> "TLBrace"
    | TRBrace -> "TRBrace"
    | TLSquare -> "TLSquare"
    | TRSquare -> "TRSquare"
    | TColon -> "TColon"
    | TComma -> "TComma"
    | TNewline -> "\n"
    | TPlus -> "TPlus"
    | TMinus -> "TMinus"
    | TMultiply -> "TMultiply"
    | TDivide -> "TDivide"
    | TInput -> "TInput"
    | TOutput -> "TOutput"
