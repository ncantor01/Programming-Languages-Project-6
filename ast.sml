structure AST = struct

  datatype term
    = Const of string * Type.ty
    | Some of term
    | None of Type.ty
    | Unwrap of term * term
    | Ref of term
    | Read of term
    | Write of term * term
    | Pair of term * term
    | First of term
    | Second of term
    | Inl of term * Type.ty
    | Inr of Type.ty * term
    | Case of term * string * term * string * term
    | Abs of string * Type.ty * term
    | Var of string
    | App of term * term
    | Record of (string * term) list
    | Select of string * term
			 
end
