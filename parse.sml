structure Parse : sig

  val ty   : Token.token list -> Type.ty
  val term : Token.token list -> AST.term

end = struct

  structure To = Token
  structure Ty = Type

  fun err info = raise Fail ("parse error: " ^ info)

  fun checkLabel label =
    (case explode label
       of c :: _ => if Char.isLower c
                    then label
		    else raise Fail ("label should start with lowercase letter (" ^ label ^ ")")
	| [] => raise Fail "bug in Parse (checkLabel)")

  fun checkGround ground =
    (case explode ground
       of c :: _ => if Char.isUpper c
                    then ground
		    else raise Fail ("ground type should start with uppercase letter (" ^ ground ^ ")")
	| _ => raise Fail "bug in Parse (checkGround)")

  fun nextType tokens =
    let
      fun lp [] = NONE
      	| lp (To.ID g :: toks) = SOME (Ty.Ground (checkGround g), toks)
	| lp (To.LParen :: To.QuestionMark :: toks) = lp1 Ty.Option toks
	| lp (To.LParen :: To.Bang :: toks) = lp1 Ty.Ref toks
	| lp (To.LParen :: To.Arrow :: toks) = lp2 Ty.Function toks
	| lp (To.LParen :: To.Asterisk :: toks) = lp2 Ty.Product toks
	| lp (To.LParen :: To.Plus :: toks) = lp2 Ty.Sum toks
	| lp (To.LParen :: To.LAngle :: toks) = lpRecord (To.LAngle :: toks) []
	| lp (To.LParen :: To.RParen :: toks) = SOME (Ty.Record [], toks) (* empty record allowed, means Unit *)
	| lp (t :: _) = err ("at " ^ To.tos t)      
      and lp1 k toks =
        (case lp toks
	   of SOME (ty, To.RParen :: toks') => SOME (k ty, toks')
	    | SOME _ => err "rparen expected"
	    | NONE => err "type ended unexpectedly")
      and lp2 k toks =
        (case lp toks
	   of SOME (ty1, toks1) =>
	        (case lp toks1
		   of SOME (ty2, To.RParen :: toks2) => SOME (k (ty1, ty2), toks2)
		    | SOME _ => err "rparen expected"
		    | NONE => err "type ended unexpectedly")
            | NONE => err "type ended unexpectedly")
      and lpRecord toks items =
        (case toks
	   of To.LAngle :: To.ID label :: To.Colon :: toks' =>
	      (case lp toks'
		   of SOME (ty, To.RAngle :: toks'') => lpRecord toks'' ((checkLabel label, ty)::items)
		    | SOME _ => err "rangle expected in record type"
		    | NONE => err "record term ended unexpectedly")
	    | To.RParen :: toks' => SOME (Ty.Record (List.rev items), toks')
	    | _ => err "record, malformed label, type pair")
    in
      lp tokens
    end

  fun nextTerm tokens =
    let
	fun lp [] = NONE
	  | lp (To.LParen :: To.Some :: toks) = lp1 AST.Some toks
	  | lp (To.LParen :: To.None :: toks) = lpNone toks
	  | lp (To.LParen :: To.Percent :: toks) = lp2 AST.Unwrap toks
	  | lp (To.LParen :: To.Ref :: toks) = lp1 AST.Ref toks
	  | lp (To.LParen :: To.Read :: toks) = lp1 AST.Read toks
	  | lp (To.LParen :: To.ColonEquals :: toks) = lp2 AST.Write toks
	  | lp (To.LParen :: To.Pair :: toks) = lp2 AST.Pair toks
	  | lp (To.LParen :: To.Hash1 :: toks) = lp1 AST.First toks
	  | lp (To.LParen :: To.Hash2 :: toks) = lp1 AST.Second toks
	  | lp (To.LParen :: To.Inl :: toks) = lpInl toks
	  | lp (To.LParen :: To.Inr :: toks) = lpInr toks
	  | lp (To.LParen :: To.Case :: toks) = lpCase toks
	  | lp (To.LParen :: To.Carat :: toks) = lpAbs toks
	  | lp (To.ID x :: toks) = SOME (AST.Var x, toks)
	  | lp (To.LParen :: To.At :: toks) = lp2 AST.App toks
	  | lp (To.LParen :: To.RParen :: toks) = SOME (AST.Record [], toks)
	  | lp (To.LParen :: To.LAngle :: toks) = lpRecord (To.LAngle :: toks) []
	  | lp (To.LParen :: To.Hash :: toks) = lpSelect toks
	  | lp (To.Dollar :: toks) = const toks
	  | lp (t :: _) = raise Fail ("parse error at " ^ To.tos t)
	and lp1 k toks =
	    (case lp toks
	       of SOME (t1, To.RParen :: toks') => SOME (k t1, toks')
		| SOME (_, t::_)=> raise Fail ("parse error: expected rparen at " ^ To.tos t)
		| _ => raise Fail "parse error: unary expression ended unexpectedly")
	and lp2 k toks =
            (case lp toks
	       of SOME (t1, toks1) =>
	           (case lp toks1
		      of SOME (t2, To.RParen :: toks2) => SOME (k (t1, t2), toks2)
		       | SOME (_,t::_) => raise Fail ("parse error: expected rparen after second expression at " ^ To.tos t)
		       | _ => raise Fail "parse error: unary expression ended unexpectedly (2)")
		| NONE => raise Fail "parse error: unary expression ended unexpectedly (1)")
	and lpNone toks =
	    (case nextType toks
	       of SOME (tau1, To.RParen :: toks') => SOME (AST.None tau1, toks')
		| SOME _ => raise Fail ("parse error: expected rparen after none's type")
		| _ => raise Fail "parse error: none expression ended unexpectedly")
	and lpInl toks =
	    (case lp toks
	       of SOME (t1, toks1) => (case nextType toks1
				         of SOME (tau2, To.RParen :: toks2) => SOME (AST.Inl (t1, tau2), toks2)
					  | SOME (_, t::_) => raise Fail ("parse error: expected rparen after inl at " ^ To.tos t)
					  | _ => raise Fail "parse error: inl expression ended unexpectedly")
		| _ => raise Fail "parse error: inl")
	and lpInr toks =
	    (case nextType toks
	       of SOME (tau1, toks1) => (case lp toks1
				           of SOME (t2, To.RParen :: toks2) => SOME (AST.Inr (tau1, t2), toks2)
					    | SOME (_, t::_) => raise Fail ("parse error: expected rparen after inr at " ^ To.tos t)
					    | _ => raise Fail "parse error: inr expression ended unexpectedly")
		| _ => raise Fail "parse error: inr")
	and lpCase toks =
            (case nextTerm toks
	       of SOME (t0, toks1) => (case lpCase' toks1
				         of (x1, t1, toks2) =>
					    (case lpCase' toks2
					       of (x2, t2, To.RParen :: toks3) => SOME (AST.Case (t0, x1, t1, x2, t2), toks3)
						| _ => raise Fail "parse error: expected rparen after case"))
		| NONE => raise Fail "parse error: case expression ended unexpectedly")
	and lpCase' (To.LBrack :: To.ID x1 :: toks) =
	    (case lp toks
	       of SOME (t1, To.RBrack :: toks1) => (x1, t1, toks1)
		| SOME _ => raise Fail "parse error: rbrack expected in case branch"
		| NONE => raise Fail "parse error: case branch ended unexpectedly")
	  | lpCase' _ = raise Fail "parse error: lbrack expected in case"
	and lpAbs (To.ID x :: To.Colon :: toks) =
	    (case nextType toks
	       of SOME (tau, To.Dot :: toks') => (case lp toks'
	 					    of SOME (t1, To.RParen :: toks'') => SOME (AST.Abs (x, tau, t1), toks'')
						     | SOME (_, t::_) => raise Fail ("parse error: expected rparen after abstraction at " ^ To.tos t)
						     | _ => raise Fail "parse error: abstraction ended unexpectedly (2)")
		| SOME (_, t::_) => raise Fail ("parse error: expected . after type at " ^ To.tos t)
		| _ => raise Fail "parse error: abstraction ended unexpectedly (1)")
	  | lpAbs _ = raise Fail "parse error in Abs"
	and lpRecord toks items =
          (case toks
	     of To.LAngle :: To.ID label :: To.Equals :: toks' =>
	          (case lp toks'
		     of SOME (tm, To.RAngle :: toks'') => lpRecord toks'' ((checkLabel label, tm)::items)
		      | SOME _ => err "rangle expected in record term"
		      | NONE => err "record term ended unexpectedly")
	      | To.RParen :: toks' => SOME (AST.Record (List.rev items), toks')
	      | _ => err "record, malformed label, type pair")
	and lpSelect toks =
	    (case toks
	       of To.ID label :: toks => (case lp toks
					    of SOME (t1, To.RParen :: toks') => SOME (AST.Select (label, t1), toks')
					     | SOME (_, t::_) => raise Fail ("parse error: expected rparen after select at " ^ To.tos t)
					     | _ => raise Fail "parse error: select expression ended unexpectedly")
		| _ => raise Fail "parse error: expected label after hash")
	and const (To.ID a :: To.Slash :: To.ID g :: toks) = SOME (AST.Const (a, Ty.Ground g), toks)
	  | const _ = raise Fail "parse error parsing const"	    
    in
      lp tokens
    end
			
  fun ty tokens =
      (case nextType tokens
	of SOME (ty, []) => ty
         | SOME _ => err "more than one type given"
	 | NONE => err "no tokens given")
	  
  fun term tokens =
      (case nextTerm tokens
	of SOME (t, []) => t
	 | SOME _ => err "more than one term given"
	 | NONE => err "no terms given")
end
