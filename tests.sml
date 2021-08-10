structure Tests = struct

  structure A = AST
  structure T = Type

  val itos = Int.toString
  
  fun parseTy () =
    let
      val exp = Check.expect
      fun test i str ty = exp (Parse.ty (Scan.scan str), ty, "parseTy"^itos(i))
      val g = T.Ground
      val _ = test 0 "Nat" (g "Nat")
      val _ = test 1 "A" (g "A")	
      val _ = test 2 "(? A)" (T.Option (g "A"))
      val _ = test 3 "(! A)" (T.Ref (g "A"))
      val _ = test 4 "(-> A B)" (T.Function (g "A", g "B"))
      val _ = test 5 "(* A B)" (T.Product (g "A", g "B"))
      val _ = test 6 "(+ A B)" (T.Sum (g "A", g "B"))
      val _ = test 7 "(<a:A> <b:B> <c:C>)" (T.Record [("a", g "A"),("b", g "B"),("c", g "C")])
      val _ = test 8 "(+ (-> A B) (* C (! D)))" (T.Sum (T.Function (g "A", g "B"), T.Product (g "C", T.Ref (g "D"))))
    in
      "parse type tests done"
    end

  fun parseTerm () =
    let
      val exp = Check.expect
      fun test i str term = exp (Parse.term (Scan.scan str), term, "parseTerm"^itos(i))
      val g = T.Ground
      fun k x t = A.Const (x, g t)
      val v = A.Var
      val _ = test  0 "$a/A" (k "a" "A")
      val _ = test  1 "(some $a/A)" (A.Some (k "a" "A"))
      val _ = test  2 "(none A)" (A.None (g "A"))
      val _ = test  3 "(% (some $a1/A) $a2/A)" (A.Unwrap (A.Some (k "a1" "A"), k "a2" "A"))
      val _ = test  4 "(ref $a/A)" (A.Ref (k "a" "A"))
      val _ = test  5 "(read x)" (A.Read (v "x"))
      val _ = test  6 "(:= a z)" (A.Write (v "a", v "z"))
      val _ = test  7 "(pair a b)" (A.Pair (v "a", v "b"))
      val _ = test  8 "(#1 x)" (A.First (v "x"))
      val _ = test  9 "(#2 y)" (A.Second (v "y"))
      val _ = test 10 "(inl a B)" (A.Inl (v "a", g "B"))
      val _ = test 11 "(inr A b)" (A.Inr (g "A", v "b"))
      val _ = test 12 "(case a [b c] [d e])" (A.Case (v "a", "b", v "c", "d", v "e"))
      val _ = test 13 "(^x:A.y)" (A.Abs ("x", g "A", v "y"))
      val _ = test 14 "abc" (v "abc")
      val _ = test 15 "(@ a b)" (A.App (v "a", v "b"))
      val _ = test 16 "(<a=$a/A> <b=$b/B>)" (A.Record [("a", k "a" "A"), ("b", k "b" "B")])
      val _ = test 17 "(#a (<a=$x/X>))" (A.Select ("a", A.Record [("a", k "x" "X")]))
      val _ = test 18 "(pair (some x) (none Y))" (A.Pair (A.Some (v "x"), A.None (g "Y")))
    in
      "parse term tests done"
    end

end
