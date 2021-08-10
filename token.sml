structure Token = struct

  datatype token
    = ID of string
    | LParen
    | RParen
    | Some
    | None
    | Percent
    | Ref
    | Read
    | ColonEquals
    | Pair
    | Hash1
    | Hash2
    | Inl
    | Inr
    | Case
    | LBrack
    | RBrack
    | Carat
    | Dot
    | At
    | Equals
    | Hash
    | LAngle
    | RAngle
    | QuestionMark
    | Bang
    | Arrow
    | Asterisk
    | Plus
    | Colon
    | Dollar
    | Slash

  fun tos (ID s) = "ID("^s^")"
    | tos LParen = "LParen"
    | tos RParen = "RParen"
    | tos Some = "Some"
    | tos None = "None"
    | tos Percent = "Percent"
    | tos Ref = "Ref"
    | tos Read = "Read"
    | tos ColonEquals = "ColonEquals"
    | tos Pair = "Pair"
    | tos Hash1 = "Hash1"
    | tos Hash2 = "Hash2"
    | tos Inl = "Inl"
    | tos Inr = "Inr"
    | tos Case = "Case"
    | tos LBrack = "LBrack"
    | tos RBrack = "RBrack"
    | tos Carat = "Carat"
    | tos Dot = "Dot"
    | tos At = "At"
    | tos Equals = "Equals"
    | tos Hash = "Hash"
    | tos LAngle = "LAngle"
    | tos RAngle = "RAngle"
    | tos QuestionMark = "QuestionMark"
    | tos Bang = "Bang"
    | tos Arrow = "Arrow"
    | tos Asterisk = "Asterisk"
    | tos Plus = "Plus"
    | tos Colon = "Colon"
    | tos Dollar = "Dollar"
    | tos Slash = "Slash"
	  
end
