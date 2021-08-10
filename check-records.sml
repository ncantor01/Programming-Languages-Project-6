structure CheckRecords : sig

(* check that record terms and record types contain no duplicate labels *)
(* return true if there are no duplicate labels, false if there are *)
  val dupFree : AST.term -> bool
  val test    : string * AST.term -> bool 

end = struct

  structure A = AST
  structure T = Type

  fun dupFree (A.Record ([])) = true
    | dupFree (A.Record ((str, _)::lst)) = (test (str, A.Record lst)) andalso (dupFree (A.Record lst))
    | dupFree _ = raise Fail "unknown case"
  
  and test (str, (A.Record ([]))) = true 
    | test (str, (A.Record ((str',_):: lst))) =
      (case String.compare (str,str')
        of EQUAL => false
        |  _     => test (str,(A.Record (lst))))
    | test _ = raise Fail "you shouldnt be here"
	  	  
end
