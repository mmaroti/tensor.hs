{-# OPTIONS -Wall #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}

module Lambda where

import Control.Applicative (Applicative, pure, (<*>), liftA)
import Data.Traversable (Traversable, sequenceA)
import Data.Foldable (Foldable, foldl')

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
	= isClosed lev sub1 && isClosed lev sub2
isClosed lev (TermNode sub)
	= foldl' (&&) True (fmap (isClosed lev) sub)

-- basic datatype and ops

data Node arg
	= NodeLit Int
	| NodeNeg arg
	| NodeAdd arg arg
	| NodeMul arg arg
	deriving (Show, Eq, Functor, Foldable, Traversable)

instance Calculus Node Int where
	calculate (NodeLit arg) = arg
	calculate (NodeNeg arg) = negate arg
	calculate (NodeAdd arg1 arg2) = arg1 + arg2
	calculate (NodeMul arg1 arg2) = arg1 * arg2

--instance Traversable node => Calculus node (Term node) where
--	calculate node = TermNode node

-- runtime data

class Traversable node => Calculus node val where
	calculate :: node val -> val

data Data node val
	= DataVal val
	| DataErr String
	| Closure [Data node val] (Term node)

deriving instance (Show (Term node), Show val) => Show (Data node val)
deriving instance (Eq (Term node), Eq val) => Eq (Data node val)
deriving instance Functor (Data node)

instance Applicative (Data node) where
	pure = DataVal
	(DataVal fun) <*> arg = fmap fun arg
	(DataErr err) <*> _ = DataErr err
	(Closure _ _) <*> _ = DataErr "not primitive"

execute :: Calculus node val => [Data node val] -> Term node -> Data node val
execute env (TermVar idx)
	= if idx < length env
		then env !! idx
		else DataErr "invalid variable"
execute env (TermLam term)
	= Closure env term
execute env (TermApp fun arg)
	= case execute env fun of
		Closure e t -> execute (execute env arg : e) t
		DataVal _ -> DataErr "not applicable"
		e@(DataErr _) -> e
execute env (TermNode node)
	= liftA calculate (sequenceA (fmap (execute env) node))
