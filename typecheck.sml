structure Typecheck : sig

(* compute the type of the given term, raise Fail if term is ill-typed *)
  val typeof : AST.term -> Type.ty
  val helper : AST.term * TypeEnv.env -> (Type.ty * TypeEnv.env)

(* check record labels and well-typedness *)

  datatype checked
    = Well of Type.ty
    | Ill of string

  val check : AST.term -> checked
				
end = struct

  structure A = AST
  structure T = Type

  fun typeof ty = #1 (helper (ty, TypeEnv.empty))

  and helper ((A.Const (str,ty)), env) = (ty, TypeEnv.extend (env,str,ty))
    | helper ((A.Some ty), env) = (helper (ty, env))
    | helper ((A.None ty), env) = ((T.Option ty), env)
    | helper ((A.Unwrap (A.Some t1,t2)), env) = helper (t1, env)
    | helper ((A.Unwrap (A.None t1,t2)), env) = helper (t2, env)
    | helper ((A.Ref ty), env) = 
      (case helper (ty, env)
        of (ty',env') => ((T.Ref ty'), env'))
    | helper ((A.Read (A.Ref ty)), env) = helper (ty, env)
    | helper ((A.Write ((A.Ref t1), t2)), env) = 
      (case helper (t1, env)
        of (t1',env') =>
          (case helper (t2, env')
            of t1' => helper (t2, env')))
    | helper ((A.Pair (t1,t2)), env) = 
      (case (helper (t1, env), helper (t2, env))
        of ((t1', env'), (t2', env'')) => (T.Product (t1',t2'), env'))
    | helper ((A.First (A.Pair (t1,t2))), env) =
      (case helper ((A.Pair (t1,t2)), env)
        of (T.Product (t1',t2'), env') => (t1', env')
        |  _                           => raise Fail "")
    | helper ((A.Second (A.Pair (t1,t2))), env) =
      (case helper ((A.Pair (t1,t2)), env)
        of (T.Product (t1',t2'), env') => (t2', env')
        |  _                        => raise Fail "")
    | helper ((A.Inl (t,t')), env) = 
      (case (helper (t, env))
        of (t'', env') => ((T.Sum (t'',t')), env'))
    | helper ((A.Inr (t',t)), env) = 
      (case (helper (t, env))
        of (t'', env') => ((T.Sum (t',t'')), env'))
    | helper ((A.Case (A.Inl (tr1,t1),s1,A.Inl (tr2,t2),s2,A.Inr (t3,tr3))), env) = raise Fail "no idea"
    | helper ((A.Var str), env) = (T.Ground str, TypeEnv.extend (env,str, T.Ground str))
    | helper (A.Abs (str, ty, tr), env) = (T.Function (ty, typeof tr), TypeEnv.extend (env, str, (T.Function (ty, typeof tr))))
    | helper ((A.App (tr1, tr2)), env) = 
      (case helper (tr1, env)
        of (T.Function (t1,t2), env') => 
          (case helper (tr2, env)
            of (t1, env'') => (t2, env''))
        | _ => raise Fail "" )
    | helper _  = raise Fail "too tired to finish"


    

  datatype checked
    = Well of Type.ty
    | Ill of string

  fun check term =
    if CheckRecords.dupFree term
    then (Well (typeof term) handle Fail msg => Ill msg)
    else Ill "duplicate label(s) in record type or record term"

end
