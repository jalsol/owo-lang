open Owo2

let get_token_list lexbuf =
  let rec work acc =
    match Lexer.tokenize lexbuf with
    | Token.EOF -> acc
    | t -> work (t::acc)
  in List.rev (work [])

let _data_types = {|I haz numba! owo
numba itz 42! owo
I haz decimal! owo
decimal itz 3.14! owo
I haz message! owo
message itz "Hello, world!"! owo
I haz person! owo
person itz { name: "Alice", age: 30 }! owo
I haz array_of_numbas! owo
array_of_numbas itz [1, 2, 3]! owo
I haz boolean! owo
boolean itz ril! owo
I haz another_boolean! owo
another_boolean itz fek! owo
|}

let _math_ops = {|I haz numba! owo
numba itz 42! owo
I haz anotha_numba! owo
anotha_numba itz 3! owo
I haz result! owo
result itz numba pluz anotha_numba! owo
I haz anotha_result! owo
anotha_result itz numba minuz anotha_numba! owo
I haz yet_anotha_result! owo
yet_anotha_result itz numba muwipwy anotha_numba! owo
I haz even_anotha_result! owo
even_anotha_result itz numba dividid anotha_numba! owo
|}

let _in_out = {|I haz numba! owo
GIMME numba! owo
AAAAA numba! owo
|}

let () =
    let lexbuf = Lexing.from_string _data_types in
    let token_list = get_token_list lexbuf in
    List.map Token.pretty_print token_list |> List.iter (Printf.printf "%s ")

