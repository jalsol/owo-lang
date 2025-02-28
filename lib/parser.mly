%{
    open Ast
%}

%token <float> TNumber
%token <string> TString
%token <string> TIdent
%token <bool> TBool
%token EOF TLet TDelim TAssign
%token TLParen TRParen TLBrace TRBrace TLSquare TRSquare
%token TColon TComma
%token TPlus TMinus TMultiply TDivide
%token TInput TOutput
%token TBegin TEnd
%token TFuncDef TReturn

%left TPlus TMinus
%left TMultiply TDivide

%start <Ast.block> program
%%

program:
    | stmts EOF { $1 }
    ;

stmts:
    | stmt { [$1] }
    | stmts stmt { $1 @ [$2] }
    ;

stmt:
    | TLet TIdent TDelim { Declare($2) }
    | TIdent TAssign expr TDelim { Assign($1, $3) }
    | TInput expr TDelim { Input($2) }
    | TOutput expr TDelim { Output($2) }
    | TReturn expr TDelim { Return($2) }
    | func_def TDelim { $1 }
    ;

block:
    | TBegin stmts TEnd { $2 }
    | TBegin TEnd { [] }
    ;

func_def:
    | TFuncDef TIdent TLParen func_def_args TRParen block { FuncDef($2, $4, $6) }
    ;

expr:
    | TLParen expr TRParen { $2 }
    | TIdent { Ident($1) }
    | TNumber { Number($1) }
    | TString { String($1) }
    | TBool { Bool($1) }
    | TLBrace object_fields TRBrace { Object($2) }
    | TLSquare list_fields TRSquare { List($2) }
    | expr binop expr { BinOp($2, $1, $3) }
    | TIdent TLParen call_args TRParen { FuncCall($1, $3) }
    ;

%inline binop:
    | TPlus { Plus }
    | TMinus { Minus }
    | TMultiply { Multiply }
    | TDivide { Divide }
    ;

object_fields:
    | separated_list(TComma, object_field) { $1 }
    ;

object_field:
    | TIdent TColon expr { ($1, $3) }
    ;

list_fields:
    | separated_list(TComma, expr) { $1 }
    ;

func_def_args:
    | separated_list(TComma, TIdent) { $1 }
    ;

call_args:
    | separated_list(TComma, expr) { $1 }
    ;
