{-# OPTIONS -Wall #-}

module Lambda where

-- compiled program

data Term val
	= TermVar Int
	| TermLam (Term val)
	| TermApp (Term val) (Term val)
	| TermLit val
	| TermPrm (val -> val -> val) (Term val) (Term val)

isClosed :: Term val -> Int -> Bool
isClosed (TermVar idx) lev = idx < lev
isClosed (TermLam sub) lev = isClosed sub (lev + 1)
isClosed (TermApp sub1 sub2) lev
	= (isClosed sub1 lev) && (isClosed sub2 lev)
isClosed (TermLit _) _ = True
isClosed (TermPrm _ sub1 sub2) lev
	= (isClosed sub1 lev) && (isClosed sub2 lev)

-- runtime values

data Value val
	= Value val
	| Closure (Term val) [Value val]

execute :: Term val -> [Value val] -> Value val
execute (TermVar idx) env
	= if idx < length env
		then env !! idx
		else error "invalid variable"
execute (TermLam term) env
	= Closure term env
execute (TermApp fun arg) env
	= case execute fun env of
		Closure t e -> execute t ((execute arg env) : e)
		Value _ -> error "not applicable"
execute (TermLit val) _ = Value val
execute (TermPrm fun arg1 arg2) env
	= let
		a1 = execute arg1 env
		a2 = execute arg2 env
	in case (a1, a2) of
		(Value v1, Value v2) -> Value (fun v1 v2)
		_ -> error "invalid value"
