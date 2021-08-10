structure Type = struct

  datatype ty
    = Ground of string
    | Option of ty
    | Ref of ty
    | Function of ty * ty
    | Product of ty * ty
    | Sum of ty * ty
    | Record of (string * ty) list

  infix ^^
  fun s1 ^^ s2 = s1 ^ " " ^ s2

  fun par s = "(" ^ s ^ ")"
  val spaces = String.concatWith " "

  fun tos (Ground g) = g
    | tos (Option t1) = par ("?" ^^ tos t1)
    | tos (Ref t1) = par ("!" ^^ tos t1)
    | tos (Function (t1, t2)) = par ("->" ^^ tos t1 ^^ tos t2)
    | tos (Product (t1, t2)) = par ("*" ^^ tos t1 ^^ tos t2)
    | tos (Sum (t1, t2)) = par ("+" ^^ tos t1 ^^ tos t2)
    | tos (Record items) = par (spaces (map item items))
  and item (label, t1) = "<" ^ label ^ ":" ^ tos t1 ^ ">"

end