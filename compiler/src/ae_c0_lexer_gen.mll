{
module Token = Ae_c0_token
}

let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let hexnum = '0' ['x' 'X'] ['0'-'9' 'a'-'f' 'A'-'F']+
let decnum = ['0'-'9'] ['0'-'9']*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule lex =
  parse
  | white { lex lexbuf }
  | newline { Lexing.new_line lexbuf; lex lexbuf }
  | "//" { line_comment lexbuf }
  | "/*" { block_comment 1 lexbuf }
  | ';' { Token.Semi }
  | '+' { Token.Plus }
  | '*' { Token.Star }
  | '=' { Token.Eq }
  | '-' { Token.Dash }
  | '/' { Token.Slash }
  | '%' { Token.Percent }
  | "+=" { Token.PlusEq }
  | "*=" { Token.StarEq }
  | "%=" { Token.PercentEq }
  | "/=" { Token.SlashEq }
  | "-=" { Token.DashEq }
  | '(' { Token.LParen }
  | ')' { Token.RParen }
  | '{' { Token.LBrace }
  | '}' { Token.RBrace }
  | "return" { Token.Return }
  | "int" { Token.Int }
  | ident { Token.Ident (Lexing.lexeme lexbuf) }
  | hexnum { Token.Hexnum (Lexing.lexeme lexbuf) }
  | decnum { Token.Decnum (Lexing.lexeme lexbuf) }
  | eof { Token.Eof }
  | _ { Token.Unknown (Lexing.lexeme lexbuf) }

and line_comment =
  parse
  | newline { lex lexbuf }
  | _ { line_comment lexbuf }
  | eof { Token.Eof }

and block_comment nesting =
  parse
  | "*/" { if nesting = 1 then lex lexbuf else block_comment (nesting - 1) lexbuf }
  | "/*" { block_comment (nesting + 1) lexbuf }
  | _ { block_comment nesting lexbuf }
  | eof { Token.Eof }

{

}