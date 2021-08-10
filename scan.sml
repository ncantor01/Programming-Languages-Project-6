structure Scan : sig

  val scan : string -> Token.token list
	    
end = struct

  structure T = Token

  fun cons1 x (ys,zs) = (x::ys, zs)

  fun takeWhile test items =
    let
      fun lp xs =
        (case xs
	   of [] => ([], [])
	    | h::t => if test h then cons1 h (lp t) else ([], xs))
    in
      lp items
    end

  val letters = takeWhile Char.isAlpha

  val alphanum = takeWhile (fn c => Char.isAlpha c orelse Char.isDigit c)

  fun nextToken chars =
    (case chars 
       of [] => NONE
	| #"(" :: cs => SOME (T.LParen, cs)
	| #")" :: cs => SOME (T.RParen, cs)
	| #"<" :: cs => SOME (T.LAngle, cs)
	| #">" :: cs => SOME (T.RAngle, cs)
	| #"?" :: cs => SOME (T.QuestionMark, cs)
	| #"!" :: cs => SOME (T.Bang, cs)
	| #"-" :: #">" :: cs => SOME (T.Arrow, cs)
	| #"*" :: cs => SOME (T.Asterisk, cs)
	| #"+" :: cs => SOME (T.Plus, cs)
	| #":" :: #"=" :: cs => SOME (T.ColonEquals, cs)
	| #":" :: cs => SOME (T.Colon, cs)
	| #"#" :: #"1" :: cs => SOME (T.Hash1, cs)
	| #"#" :: #"2" :: cs => SOME (T.Hash2, cs)
	| #"#" :: cs => SOME (T.Hash, cs)
	| #"[" :: cs => SOME (T.LBrack, cs)
	| #"]" :: cs => SOME (T.RBrack, cs)
	| #"^" :: cs => SOME (T.Carat, cs)
	| #"." :: cs => SOME (T.Dot, cs)
	| #"@" :: cs => SOME (T.At, cs)
	| #"$" :: cs => SOME (T.Dollar, cs)
	| #"/" :: cs => SOME (T.Slash, cs)
	| #"%" :: cs => SOME (T.Percent, cs)
	| #"=" :: cs => SOME (T.Equals, cs)			
	| c :: cs =>
	    if Char.isAlpha c
	    then case alphanum (c::cs)
		   of (cs1, cs2) =>
		        (case implode cs1
		           of "some" => SOME (T.Some, cs2)
			    | "none" => SOME (T.None, cs2)
			    | "ref"  => SOME (T.Ref, cs2)
			    | "read" => SOME (T.Read, cs2)
			    | "pair" => SOME (T.Pair, cs2)
			    | "inl"  => SOME (T.Inl, cs2)
			    | "inr"  => SOME (T.Inr, cs2)
			    | "case" => SOME (T.Case, cs2)
			    | s      => SOME (T.ID s, cs2))
	    else (if Char.isSpace c 
	          then nextToken cs
	          else raise Fail ("scan error at " ^ implode [c])))

  fun scan sourceCode =
    let
      fun lp chars =
        (case nextToken chars
	   of NONE => []
	    | SOME (tok, cs) => tok :: lp cs)
    in
      lp (explode sourceCode)
    end

end
