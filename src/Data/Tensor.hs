{-# OPTIONS -Wall #-}

module Data.Tensor
	( Tensor
	, fromList
	, toList
	, shape
	, order
	, constant
	, iota
	, unary
	, inner
	, outer
	, rotate
	, sEqu
	, sNeq
	, sLeq
	, sLess
	, bNeg
	, bAnd
	, bOr
	, bAdd
	, bEqu
	, bImp
	) where

import qualified Data.List as List
import Data.Word (Word8)
import qualified Data.Bits as Bits
import Data.Vector.Primitive (Prim)
import qualified Data.Vector.Primitive as Vector

data Tensor a = Tensor [Int] (Vector.Vector a)
	deriving (Show, Eq)

fromList :: Prim a => [Int] -> [a] -> Tensor a
fromList shp vec =
	let
		len1 = product shp
		vec2 = Vector.fromListN (len1 + 1) vec
		len2 = Vector.length vec2
	in if len1 == len2
		then Tensor shp vec2
		else error "length mismatch"

toList :: Prim a => Tensor a -> [a]
toList (Tensor _ vec) = Vector.toList vec

shape :: Tensor a -> [Int]
shape (Tensor shp _) = shp

order :: Tensor a -> Int
order = List.length . shape

constant :: Prim a => a -> [Int] -> Tensor a
constant val shp = Tensor shp (Vector.replicate (product shp) val)

iota :: Int -> [Int] -> Tensor Int
iota val shp = Tensor shp (Vector.generate (product shp) (+ val))

unary :: (Prim a, Prim b) => (a -> b) -> Tensor a -> Tensor b
unary fun (Tensor shp vec) = Tensor shp (Vector.map fun vec)

inner :: (Prim a, Prim b, Prim c) => (a -> b -> c) -> Tensor a -> Tensor b -> Tensor c
inner fun (Tensor shp1 vec1) (Tensor shp2 vec2)
	= if shp1 == shp2
		then Tensor shp1 (Vector.zipWith fun vec1 vec2)
		else error "shape mismatch"

outer :: (Prim a, Prim b, Prim c) => (a -> b -> c) -> Tensor a -> Tensor b -> Tensor c
outer fun (Tensor shp1 vec1) (Tensor shp2 vec2) =
	let
		len1 = Vector.length vec1
		len2 = Vector.length vec2
		idx1 = Vector.unsafeIndex vec1
		idx2 = Vector.unsafeIndex vec2
		gen3 i =
			let (i2, i1) = quotRem i len1
			in fun (idx1 i1) (idx2 i2)
		vec3 = Vector.generate (len1 * len2) gen3
	in Tensor (shp1 ++ shp2) vec3

rotate :: Prim a => Int -> Tensor a -> Tensor a
rotate num ten@(Tensor shp vec) =
	let
		pos = mod num (List.length shp)
		(shp1, shp2) = List.splitAt pos shp
		len1 = product shp1
		len2 = product shp2
	in if len1 <= 1 || len2 <= 1
		then ten
		else
			let gen i =
				let (i2, i1) = quotRem i len2
				in Vector.unsafeIndex vec ((i1 * len1) + i2)
			in Tensor (shp2 ++ shp1) (Vector.generate (len1 * len2) gen)

sEqu :: Eq a => a -> a -> Word8
sEqu x y = if x == y then 1 else 0

sNeq :: Eq a => a -> a -> Word8
sNeq x y = if x == y then 0 else 1

sLeq :: Ord a => a -> a -> Word8
sLeq x y = if x <= y then 1 else 0

sLess :: Ord a => a -> a -> Word8
sLess x y = if x < y then 1 else 0

bNeg :: Word8 -> Word8
bNeg = Bits.complement

bAnd :: Word8 -> Word8 -> Word8
bAnd = (Bits..&.)

bOr :: Word8 -> Word8 -> Word8
bOr = (Bits..|.)

bAdd :: Word8 -> Word8 -> Word8
bAdd = Bits.xor

bEqu :: Word8 -> Word8 -> Word8
bEqu x y = Bits.complement (Bits.xor x y)

bImp :: Word8 -> Word8 -> Word8
bImp x y = (Bits..|.) (Bits.complement x) y
