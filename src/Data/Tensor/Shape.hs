{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}

module Data.Tensor.Shape where

import Data.Proxy (Proxy(..))

class Dimension d where
	dimension :: Proxy d -> Int

instance Dimension Bool where
	dimension _ = 2

data Dim0
data Dim1
data Dim2
data Dim3
data Dim4
data Dim5
data Dim6
data Dim7
data Dim8
data Dim9

instance Dimension Dim0 where
	dimension _ = 0

instance Dimension Dim1 where
	dimension _ = 1

instance Dimension Dim2 where
	dimension _ = 2

instance Dimension Dim3 where
	dimension _ = 3

instance Dimension Dim4 where
	dimension _ = 4

instance Dimension Dim5 where
	dimension _ = 5

instance Dimension Dim6 where
	dimension _ = 6

instance Dimension Dim7 where
	dimension _ = 7

instance Dimension Dim8 where
	dimension _ = 8

instance Dimension Dim9 where
	dimension _ = 9

class Shape s where
	shape :: Proxy s -> [Int]
	order :: Proxy s -> Int
	order p = length (shape p)
	size :: Proxy s -> Int
	size p = product (shape p)

instance Shape () where
	shape _ = []

instance forall d s . (Dimension d, Shape s) => Shape (d,s) where
	shape _ =
		let
			x = dimension (Proxy :: Proxy d)
			xs = shape (Proxy :: Proxy s)
		in x : xs

type Shape0 = ()
type Shape1 d0 = (d0, ())
type Shape2 d0 d1 = (d0, (d1, ()))
type Shape3 d0 d1 d2 = (d0, (d1, (d2, ())))
type Shape4 d0 d1 d2 d3 = (d0, (d1, (d2, (d3, ()))))
type Shape5 d0 d1 d2 d3 d4 = (d0, (d1, (d2, (d3, (d4, ())))))
