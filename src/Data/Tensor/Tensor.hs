{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}

module Data.Tensor.Tensor where

import Data.Proxy (Proxy(..))
import qualified Data.Tensor.Shape as Shape
import qualified Data.Vector.Unboxed as Vector

newtype Tensor s a = Tensor (Vector.Vector a)
	deriving (Eq, Ord, Show)

constant :: forall s a . (Shape.Shape s, Vector.Unbox a) => a -> Tensor s a
constant x =
	let
		n = Shape.size (Proxy :: Proxy s)
		v = Vector.replicate n x
	in Tensor v
