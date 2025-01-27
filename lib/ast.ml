type binop =
    | Plus
    | Minus
    | Multiply
    | Divide

type expr =
    | Ident of string
    | Number of float
    | String of string
    | Bool of bool
    | Object of (string * expr) list
    | List of expr list
    | BinOp of binop * expr * expr

type block = stmt list
and stmt =
    (* | Compound of block *)
    | Declare of string
    | Assign of string * expr
    | Input of expr
    | Output of expr

