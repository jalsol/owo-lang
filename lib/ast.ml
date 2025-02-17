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
    | FuncCall of string * expr list

type block = stmt list
and stmt =
    (* | Compound of block *)
    | Declare of string
    | Assign of string * expr
    | Input of expr
    | Output of expr
    | FuncDef of string * string list * stmt list
    | Return of expr

