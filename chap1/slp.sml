type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
| AssignStm of id * exp
| PrintStm of exp list

and exp = IdExp of id
  | NumExp of int
  | OpExp of exp * binop * exp
  | EseqExp of stm * exp

  val prog = 
  CompoundStm(
    AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
    CompoundStm( 
      AssignStm("b",EseqExp(
        PrintStm[IdExp"a",EseqExp(PrintStm[IdExp"c", IdExp"d"],IdExp"e"),OpExp(IdExp"a", Minus,NumExp 1)],
        OpExp(NumExp 10, Times, IdExp"a")
        )),
      PrintStm[IdExp "b"]
      )
    )

  fun maxargs stm = let

   fun expargs (OpExp(l, _, r)) = expargs(l) + expargs(r)
    | expargs (EseqExp(s, e)) = maxargs(s) + expargs(e)
    | expargs _ = 0

    fun stmargs (CompoundStm(l, r)) = maxargs(l) + maxargs(r)
     | stmargs (AssignStm(_, exp)) = expargs(exp)
     | stmargs (PrintStm(x :: xs)) = expargs(x) + maxargs(PrintStm(xs))
     | stmargs (PrintStm([])) = 1

   in
    stmargs(stm)
  end

  type table = (id * int) list
  
  fun interp (stm) = let

    fun update (t, id, n) = (id, n) :: t

      fun lookup ((l, r) :: xs, id) =
        if l = id then r else lookup(xs, id)

          fun doOp (l, Plus, r) = l + r
            | doOp (l, Minus, r) = l - r
            | doOp (l, Times, r) = l * r

            fun interpStm (CompoundStm(l, r), t) = let
                val t1 = interpStm(l, t)
                val t2 = interpStm(r, t1)
              in
                t2
              end
             | interpStm (AssignStm(id, exp), t) = let
              val (n, t1) = interpExp(exp, t)
            in
              update(t1, id, n)
            end 
            | interpStm (PrintStm(_), t) = t
          and 
            interpExp (IdExp(id), t) = (lookup(t, id), t)
            | interpExp (NumExp(i), t) = (i, t)
            | interpExp (OpExp(l, oper, r), t) = let
              val (ln, t1) = interpExp(l, t)
              val (rn, t2) = interpExp(r, t1)
              val n = doOp(ln, oper, rn)
            in
              (n, t2)
            end
            | interpExp (EseqExp(stm, exp), t) = let
              val t1 = interpStm(stm, t)
              val (i, t2) = interpExp(exp, t1)
            in
              (i, t2)
            end
          in
            interpStm(stm, nil)
          end

