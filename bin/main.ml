open Owo2
open Ast

let make_indent indent = String.make indent ' '

let rec pretty_print_expr indent expr =
    let ind = make_indent indent in
    match expr with
    | Ident(name) -> Printf.sprintf "%sIdent: %s" ind name
    | Number(num) -> Printf.sprintf "%sNumber: %f" ind num
    | String(str) -> Printf.sprintf "%sString: \"%s\"" ind str
    | Bool(b) -> Printf.sprintf "%sBool: %b" ind b
    | BinOp(op, left, right) ->
        let op_str = match op with
            | Plus -> "+"
            | Minus -> "-"
            | Multiply -> "*"
            | Divide -> "/"
        in
        Printf.sprintf "%sBinOp (%s)\n%s\n%s" ind op_str
            (pretty_print_expr (indent + 2) left)
            (pretty_print_expr (indent + 2) right)
    | Object(fields) ->
        let fields_str = fields |> List.map (fun (key, value) ->
            Printf.sprintf "%s%s:\n%s" (make_indent (indent + 2)) key (pretty_print_expr (indent + 4) value)
        ) |> String.concat "\n" in
        Printf.sprintf "%sObject {\n%s\n%s}" ind fields_str ind
    | List(elements) ->
        let elements_str = elements |> List.map (fun e -> pretty_print_expr (indent + 2) e) |> String.concat "\n" in
        Printf.sprintf "%sList [\n%s\n%s]" ind elements_str ind
    | FuncCall(ident, args) ->
        Printf.sprintf "%sCall %s\n" ind ident
        ^ begin
            args
            |> List.map (fun arg -> pretty_print_expr (indent + 2) arg)
            |> String.concat "\n"
        end

and pretty_print_stmt indent stmt =
    let open Printf in
    let ind = make_indent indent in
    let pad = pretty_print_expr (indent + 2) in
    match stmt with
    | Declare(ident) -> sprintf "%sDeclare: %s" ind ident
    | Assign(ident, expr) -> sprintf "%sAssign: %s = \n%s" ind ident (pad expr)
    | Output(expr) -> sprintf "%sOutput: \n%s" ind (pad expr)
    | Input(expr) -> sprintf "%sInput: \n%s" ind (pad expr)
    | FuncDef(ident, args, block) ->
        sprintf "%sFunction %s (%s)\n" ind ident (String.concat ", " args)
        ^ sprintf "%s" (pretty_print_block block)
    | Return(expr) -> sprintf "%sReturn: \n%s" ind (pad expr)

and pretty_print_block block =
  String.concat "\n" (List.map (pretty_print_stmt 0) block)

let () =
    Printexc.record_backtrace true;
    if Array.length Sys.argv < 2 then
        print_endline "Usage: program <filename>"
    else
        let filename = Sys.argv.(1) in
        try
            let content = Core.In_channel.read_all filename in
            let lexbuf = Lexing.from_string content in
            let parsed_ast = Parser.program Lexer.tokenize lexbuf in
            print_endline (pretty_print_block parsed_ast)
        with
        | Sys_error err -> Printf.printf "Error: %s\n" err
        | exn ->
            Printf.printf "Unhandled exception: %s\n%s\n"
                (Printexc.to_string exn)
                (Printexc.get_backtrace ())
