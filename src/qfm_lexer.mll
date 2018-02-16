{
open Qfm_parser     (* The type token is defined in parser.mli *)
exception Eof
}

let alpha = (['a' - 'z'] | ['A' - 'Z'] | ['0'-'9'] | '_' | '.' | '/')+

rule token = parse
  | [ '#' '%' ] [^'\n']*     { token lexbuf }        
  | [' ' '\t' '\n' '\r' ]    { token lexbuf }     (* skip blanks *)
  | ['\n' ]                  { EOL }
  | "W"                     { W }
  | alpha  as l             { IDENT l}
  | '\''                     { SINGLE_QUOTE }
  | '"'                      { DOUBLE_QUOTE }
  | '$'                      { DOLLAR }
  | '_'                      { UNDERSCORE }
  | '~'                      { NEGATED }
  | "!"                      { EXCLAMATION }  
  | '='                      { INFIX_EQUALITY }
  | '|'                      { VLINE }
  | '.'                      { DOT }
  | '['                      { LBRACKET }
  | ']'                      { RBRACKET }
  | '('                      { LPAREN }
  | ')'                      { RPAREN }
  | ','                      { COMMA }
  | '+'                      { PLUS }
  | '-'                      { MINUS }
  | eof                      { EOL }
