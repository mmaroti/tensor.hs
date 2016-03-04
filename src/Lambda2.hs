{-# OPTIONS -Wall #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module Lambda where

import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable

-- compiled program

data Term node
	= TermVar Int
	| TermLam (Term node)
	| TermApp (Term node) (Term node)
	| TermNode (node (Term node))

deriving instance Show (node (Term node)) => Show (Term node)
deriving instance Eq (node (Term node)) => Eq (Term node)

isClosed :: (Functor node, Foldable node) => Int -> Term node -> Bool
isClosed lev (TermVar idx) = idx < lev
isClosed lev (TermLam sub) = isClosed (lev + 1) sub
isClosed lev (TermApp sub1 sub2)
	= (isClosed lev sub1) && (isClosed lev sub2)
isClosed lev (TermNode sub)
	= Foldable.foldl' (&&) True (fmap (isClosed lev) sub)

-- basic datatype and ops

class (Functor node, Foldable node) => Evaluable node val where
	evaluate :: node val -> val

data Node sub
	= NodeLit Int
	| NodeAdd sub sub
	| NodeMul sub sub
	deriving (Show, Eq, Functor, Foldable)

instance Evaluable Node Int where
	evaluate (NodeLit arg) = arg
	evaluate (NodeAdd arg1 arg2) = arg1 + arg2
	evaluate (NodeMul arg1 arg2) = arg1 * arg2

-- runtime values

data Value node val
	= Value val
	| Closure [Value node val] (Term node)

execute :: (Functor node, Evaluable node val) => [Value node val] -> Term node -> Value node val
execute env (TermVar idx)
	= if idx < length env
		then env !! idx
		else error "invalid variable"
execute env (TermLam term)
	= Closure env term
execute env (TermApp fun arg)
	= case execute env fun of
		Closure e t -> execute ((execute env arg) : e) t
		Value _ -> error "not applicable"
execute env (TermNode node)
	= let
		peel (Value v) = v
		peel (Closure _ _) = error "not primitive"
	in Value (evaluate (fmap (peel . (execute env)) node))
