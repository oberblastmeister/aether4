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
  | hexnum { Token.Hexnum (Lexing.lexeme lexbuf) }
  | decnum { Token.Decnum (Lexing.lexeme lexbuf) }
  | "//" { line_comment lexbuf }
  | "/*" { block_comment 1 lexbuf }
  | ',' { Token.Comma }
  | ':' { Token.Colon }
  | ';' { Token.Semi }
  | '+' { Token.Plus }
  | '*' { Token.Star }
  | '.' { Token.Dot }
  | '=' { Token.Eq }
  | "==" { Token.EqEq }
  | "!=" { Token.BangEq }
  | '-' { Token.Dash }
  | '/' { Token.Slash }
  | '%' { Token.Percent }
  | "+=" { Token.PlusEq }
  | "*=" { Token.StarEq }
  | "%=" { Token.PercentEq }
  | "/=" { Token.SlashEq }
  | "-=" { Token.DashEq }
  | "&=" { Token.AmpersandEq }
  | "|=" { Token.PipeEq }
  | "^=" { Token.CaretEq }
  | "<<=" { Token.LangleLangleEq }
  | ">>=" { Token.RangleRangleEq }
  | "++" { Token.PlusPlus }
  | "--" { Token.DashDash }
  | '<' { Token.Langle }
  | '>' { Token.Rangle }
  | "<=" { Token.LangleEq }
  | ">=" { Token.RangleEq }
  | "<<" { Token.LangleLangle }
  | ">>" { Token.RangleRangle }
  | "->" { Token.DashLangle }
  | "null" { Token.Null }
  | '~' { Token.Tilde }
  | '^' { Token.Caret }
  | '&' { Token.Ampersand }
  | '?' { Token.Question }
  | '!' { Token.Bang }
  | '|' { Token.Pipe }
  | "||" { Token.PipePipe }
  | "&&" { Token.AmpersandAmpersand }
  | '(' { Token.LParen }
  | ')' { Token.RParen }
  | '{' { Token.LBrace }
  | '}' { Token.RBrace }
  | "[]" { Token.LBrackRBrack }
  | '[' { Token.LBrack }
  | ']' { Token.RBrack }
  | "assert" { Token.Assert }
  | "extern" { Token.Extern }
  | "typedef" { Token.Typedef }
  | "struct" { Token.Struct }
  | "return" { Token.Return }
  | "if" { Token.If }
  | "else" { Token.Else }
  | "while" { Token.While }
  | "for" { Token.For }
  | "int" { Token.Int }
  | "bool" { Token.Bool }
  | "void" { Token.Void }
  | "true" { Token.True }
  | "false" { Token.False }
  | "alloc" { Token.Alloc }
  | "alloc_array" { Token.AllocArray }
  | "break" { Token.Break }
  | "continue" { Token.Continue }
  | ident { Token.Ident (Lexing.lexeme lexbuf) }
  | eof { Token.Eof }
  | _ { Token.Unknown (Lexing.lexeme lexbuf) }

and line_comment =
  parse
  | newline { Lexing.new_line lexbuf; lex lexbuf }
  | _ { line_comment lexbuf }
  | eof { Token.Eof }

and block_comment nesting =
  parse
  | "*/" { if nesting = 1 then lex lexbuf else block_comment (nesting - 1) lexbuf }
  | "/*" { block_comment (nesting + 1) lexbuf }
  | newline { Lexing.new_line lexbuf; block_comment nesting lexbuf }
  | _ { block_comment nesting lexbuf }
  | eof { Token.Eof }

{

}