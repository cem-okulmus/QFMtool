%{ open Sfquantifier
   open Qfm
   open Parser
   open Num
   open Base
%} 
%token <string> IDENT DECIMAL 
%token PLUS MINUS STAR SLASH ZERO NUMERIC CNF INFIX_EQUALITY
%token LPAREN RPAREN NUMERIC  EOL EXCLAMATION LBRACKET
%token UNDERSCORE SINGLE_QUOTE DOUBLE_QUOTE VLINE RBRACKET
%token DOT W COMMA DOLLAR NEGATED INCLUDE
%left PLUS LOWER_ALPHAMINUS         /* lowest precedence */
%left STAR SLASH        		        /* medium precedence */
%start main                         /* one entry point   */
%start query_name                   /* second entry point   */
%type<Num.num> main
%type<string> query_name
%%
main:
    query EOL  { $1 }
;

query_name:
    nquery EOL  { $1 }
; 

name: 
  | IDENT { $1 }
; 

crisp_list: 
 | name { [ $1 ]}
 | name COMMA crisp_list { $1 :: $3 }
;

fuzzy_list: 
 | LPAREN  name COMMA name RPAREN  { [ ($2,num_of_string $4) ]}
 | LPAREN  name COMMA name RPAREN COMMA fuzzy_list {($2,num_of_string $4) :: $7 }
;

fuzzy_declarations:
 | name INFIX_EQUALITY LBRACKET fuzzy_list RBRACKET  			{ add_fuzzy $1 $4}
 |  name INFIX_EQUALITY LBRACKET fuzzy_list RBRACKET COMMA fuzzy_declarations { add_fuzzy $1 $4 }
;

crisp_declarations:
 | name INFIX_EQUALITY LBRACKET crisp_list RBRACKET  			{ add_crisp $1 $4}
 | name INFIX_EQUALITY LBRACKET crisp_list RBRACKET  COMMA crisp_declarations  {add_crisp $1 $4  }
;

farglist: 
 | name 	       { [get_fuzzy $1]       }
 | name COMMA farglist {  get_fuzzy $1 :: $3  }
;

carglist: 
 | name 	       { [get_crisp $1]       }
 | name COMMA carglist {  get_crisp $1 :: $3  }
;

nfarglist: 
 | name          {  ignore $[get_fuzzy $1]; $1  }
 | name COMMA nfarglist {  ignore $ get_fuzzy $1; $1 ++ ", " ++ $3 }
;

ncarglist: 
 | name          {  ignore $ [get_crisp $1]; $1       }
 | name COMMA ncarglist {  ignore $ get_crisp $1; $1 ++ ", " ++ $3   }
;

sf_quantifier: 
    | name              { get_sfq $1 }
    | name name         {  get_sfq_unary $1  (int_of_string $2)}
    | name name name    { get_sfq_giles $1  (int_of_string $2) (int_of_string $3)}
    | W name sf_quantifier { w (int_of_string $2) $3   }
;

nsf_quantifier: 
    | name              { ignore $ get_sfq $1; $1  }
    | name name         { ignore $  get_sfq_unary $1  (int_of_string $2); $1 ++ " " ++$2}
    | name name name    { ignore $ get_sfq_giles $1  (int_of_string $2) (int_of_string $3); 
                          $1 ++ " " ++ $2 ++ " " ++ $3}
    | W name nsf_quantifier {  "W " ++ $2 ++ " " ++ $3   }
;

qfm: 
  | name LBRACKET sf_quantifier RBRACKET  LPAREN RPAREN   	  { (get_qfm $1) $3 [] }
  | name LBRACKET sf_quantifier RBRACKET LPAREN farglist RPAREN   { (get_qfm $1) $3 $6 }	 
;

sfq:
 | sf_quantifier LPAREN  RPAREN { $1.q []  } 
 | sf_quantifier LPAREN carglist RPAREN { $1.q $3  } 
;

nqfm: 
  | name LBRACKET nsf_quantifier RBRACKET  LPAREN RPAREN       { $1 ++ "[ " ++ $3  ++" ] ()"}
  | name LBRACKET nsf_quantifier RBRACKET LPAREN nfarglist RPAREN   { $1++"["++$3++"] ("++$6++")" }

nsfq:
 | nsf_quantifier LPAREN  RPAREN { $1 ++ "()"  } 
 | nsf_quantifier LPAREN ncarglist RPAREN {  $1 ++ "( " ++ $3 ++ " )"  } 
;

query:
  | qfm   { $1 }
  | sfq   { $1  }
  | fuzzy_declarations qfm   { $2  } 
  | crisp_declarations sfq   { $2  }
;

nquery:
  | nqfm   { $1 }
  | nsfq   { $1  }
  | fuzzy_declarations nqfm   { $2  } 
  | crisp_declarations nsfq   { $2  }
;