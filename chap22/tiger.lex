type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end


%%
%%
\n			=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
(" "|"\t")+ => (continue());
":="		=> (Tokens.ASSIGN(yypos,yypos+2));
var  		=> (Tokens.VAR(yypos,yypos+3));
[0-9]+		=> (Tokens.INT(valOf(Int.fromString yytext),yypos,yypos+size yytext));
[a-z][a-zA-Z0-9_]* => (Tokens.ID(yytext,yypos,yypos+size yytext));
.       	=> (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

