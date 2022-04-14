module FMCt.TypeChecker.NormalForm where


-- | Converts a term into its normal form.
-- Example:
-- > a,l(al(b),a),c ~> a,c,l(a,al(b))
normalForm :: T -> T
normalForm x = undefined
{-
  case x of
    TEmp -> x
    TVar _ -> x
    TCon _ -> x
    TVec [] -> TEmp
    TVec (m : n : p) -> case m of
	TLoc l t -> case n of
	    TLoc k t' ->
		if l < k
		    then TLoc l (normalForm t) <> normalForm (TVec (n : p))
		    else TLoc k (normalForm t') <> normalForm (TVec (m : p))
	    _ -> normalForm n <> normalForm (TVec (m : p))
	_ -> normalForm m <> normalForm (TVec (n : p))
    TVec [x'] -> normalForm x'
    TLoc l t -> TLoc l (normalForm t)
    m :=> n -> normalForm m :=> normalForm n

-}
