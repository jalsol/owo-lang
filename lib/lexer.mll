{
    open Parser
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let digits = ['0'-'9']
let lowercase_chars = ['a'-'z']
let uppercase_chars = ['A'-'Z']
let chars = lowercase_chars | uppercase_chars

let number = '-'? (digits)+ ('.' (digits)*)?
let ident = (chars | '_')(chars | digits | '_')*

rule tokenize = parse
    | eof { EOF }
    | white { tokenize lexbuf }
    | newline { tokenize lexbuf }
    | "I haz" { TLet }
    | "! owo" { TDelim }
    | "itz" { TAssign }
    | "ril" { TBool(true) }
    | "fek" { TBool(false) }
    | "pluz" { TPlus }
    | "minuz" { TMinus }
    | "muwipwy" { TMultiply }
    | "dividid" { TDivide }
    | "GIMME" { TInput }
    | "AAAAA" { TOutput }
    | "(" { TLParen }
    | ")" { TRParen }
    | "{" { TLBrace }
    | "}" { TRBrace }
    | "[" { TLSquare }
    | "]" { TRSquare }
    | ":" { TColon }
    | "," { TComma }
    | "haii" { TBegin }
    | "plz" { TEnd }
    | "CALL ME" { TFuncDef }
    | "HERE'S UR" { TReturn }
    | "\"" { TString(read_string_literal (Buffer.create 16) lexbuf) }
    | number as lexeme { TNumber(float_of_string lexeme) }
    | ident as lexeme { TIdent(lexeme) }
    | _ as c { failwith (Printf.sprintf "Unexpected character: %c" c) }

and read_string_literal buf = parse
    | '"'        { Buffer.contents buf }
    | '\\' 'n'   { Buffer.add_char buf '\n'; read_string_literal buf lexbuf }
    | '\\' 't'   { Buffer.add_char buf '\t'; read_string_literal buf lexbuf }
    | '\\' '\\'  { Buffer.add_char buf '\\'; read_string_literal buf lexbuf }
    | '\\' '"'   { Buffer.add_char buf '"'; read_string_literal buf lexbuf }
    | _ as c     { Buffer.add_char buf c; read_string_literal buf lexbuf }
    | eof        { failwith "Unterminated string literal" }

