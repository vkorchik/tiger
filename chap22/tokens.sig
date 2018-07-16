signature Tiger_TOKENS =
sig
type linenum (* = int *)
type token
val TYPE:  linenum * linenum -> token
val VAR:  linenum * linenum -> token
val ASSIGN:  linenum * linenum -> token
val INT: (int) *  linenum * linenum -> token
val ID: (string) *  linenum * linenum -> token
val EOF:  linenum * linenum -> token
end
