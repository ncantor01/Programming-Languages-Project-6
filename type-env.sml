structure TypeEnv :> sig

  type env

  val empty  : env
  val lookup : env * string -> (Type.ty option)
  val extend : env * string * Type.ty -> env

end = struct

  type env = (string * Type.ty) list

  val empty = [] (* this must be changed *) 
  fun lookup ([],chk) = NONE
    | lookup ((str,ty) :: lst, chk) = 
      (case String.compare (str,chk)
        of EQUAL => SOME ty
        |  _     => lookup (lst,chk))

  fun extend (env,str,ty) = 
    (case lookup (env, str)
      of NONE => env @ [(str,ty)]
      |  SOME ty' =>
        let
          fun lp ([],str',ty') = []
            | lp (((str'',ty'')::lst),str',ty') = 
              (case String.compare (str'',str')
                of EQUAL => (str',ty') :: lp (lst,str',ty')
                | _      => (str'',ty'') :: lp (lst,str',ty'))
        in
          lp (env,str,ty)
        end)
      

end
