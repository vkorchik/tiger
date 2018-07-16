structure Tokens : Tiger_TOKENS =
struct
  (* A "scaffold" structure for debugging lexers. *)

type linenum = int
type token = string
fun TYPE(i,j) = "TYPE   " ^ Int.toString(i)
fun VAR(i,j) = "VAR   " ^ Int.toString(i)
fun ASSIGN(i,j) = "ASSIGN   " ^ Int.toString(i)
fun INT(c,i,j) = "INT("^Int.toString(c)^")   " ^ Int.toString(i)
fun ID(s,i,j) = "ID("^s^")     " ^ Int.toString(i)
fun EOF(i,j) = "EOF   " ^ Int.toString(i)
end
