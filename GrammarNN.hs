{-# LANGUAGE TypeFamilies, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, PolyKinds, ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GrammarNN where

import Data.Constraint
import List
import Generics.SOP
import qualified Generics.SOP as SOP
import Tensor
import Nats
import TypeLevel
import Zippable
import Generics.SOP.NP
import Generics.SOP.NS
import Data.Vinyl

import Prelude hiding (zipWith)

type family Codes xs where
  Codes '[] = '[]
  Codes (x ': xs) = Code x ': Codes xs

type family All3 (c :: k -> Constraint) (ksss :: [[[k]]]) :: Constraint where
  All3 c '[] = ()
  All3 c (kss ': ksss) = (All2 c kss, All3 c ksss)

--newtype FloatTensor n = FloatTensor { getFloatTensor :: Tensor n Float }

class Neural a where
  type Size a :: Nat

-- need to postpone applying the dimension
newtype Repr a dim = Repr { runRepr :: Tensor '[dim] a }
  deriving (Show)

newtype Linear a outDim inDim = Linear (Tensor '[outDim, inDim] a)
  deriving (Show)

linear :: Num a => Linear a outDim inDim -> Repr a inDim -> Tensor '[outDim] a
linear (Linear m) (Repr v) = mv m v

data Affine a outDim inDims = Affine (Tensor '[outDim] a) (NP (Linear a outDim) inDims)
  deriving (Show)

affine :: Num a => Affine a outDim inDims -> NP (Repr a) inDims -> Tensor '[outDim] a
affine (Affine bias weights) inputs =
  foldr (zipWith (+)) bias (map runRepr . collapse_NP $ liftA2_NP' linear' weights inputs)
  where linear' m v = K $ Repr $ linear m v

-- like liftA2_NP but without the SingI xs constraint
liftA2_NP' :: (forall a. f a -> g a -> h a) -> NP f xs -> NP g xs -> NP h xs
liftA2_NP' _ Nil Nil = Nil
liftA2_NP' f (x :* xs) (y :* ys) = f x y :* liftA2_NP' f xs ys

-- like liftA2_NS but without the SingI xs constraint
liftA2_NS' :: forall f g h xs. (forall a. f a -> g a -> h a) -> NP f xs -> NS g xs -> NS h xs
liftA2_NS' f (fx :* _) (SOP.Z gx) = SOP.Z (f fx gx)
liftA2_NS' f (_ :* fxs) (SOP.S gxs) = SOP.S (liftA2_NS' f fxs gxs)

type family MapSize ts where
  MapSize '[] = '[]
  MapSize (t ': ts) = Size t ': MapSize ts

type family MapSize2 (tss :: [[*]]) :: [[Nat]] where
  MapSize2 '[] = '[]
  MapSize2 (ts ': tss) = MapSize ts ': MapSize2 tss

type family MapSum dimss where
  MapSum '[] = '[]
  MapSum (dims ': dimss) = Sum dims ': MapSum dimss

encodeParent :: (Floating a) =>
  NP (Affine a outDim) inDims -> SOP (Repr a) inDims -> Repr a outDim

encodeParent params (SOP sop) = Repr $ tanh <$> (collapse_NS $ liftA2_NS' affine' params sop)
  where affine' p i = K $ affine p i

data Encoding a t where
  Primitive :: Neural t => Repr a (Size t) -> Encoding a t
  Generic :: (Neural t, Generic t) => Repr a (Size t) -> SOP (Encoding a) (Code t) -> Encoding a t

getRepr :: Encoding a t -> Repr a (Size t)
getRepr (Primitive repr) = repr
getRepr (Generic repr _) = repr

-- no way to write these with combinators?
getReprNP :: NP (Encoding a) ts -> NP (Repr a) (MapSize ts)
getReprNP Nil = Nil
getReprNP (e :* es) = getRepr e :* getReprNP es

getReprSOP' :: NS (NP (Encoding a)) ts -> NS (NP (Repr a)) (MapSize2 ts)
getReprSOP' (SOP.Z es) = SOP.Z $ getReprNP es
getReprSOP' (SOP.S es) = SOP.S $ getReprSOP' es

getReprSOP :: SOP (Encoding a) ts -> SOP (Repr a) (MapSize2 ts)
getReprSOP (SOP sop) = SOP $ getReprSOP' sop

{-
oneHot' :: Num a => NS f l -> Vec (Len l) 
oneHot' 
-}

newtype EncodeParams a t = EncodeParams (NP (Affine a (Size t)) (MapSize2 (Code t)))

class Encode ts t where
  encode :: Floating a => NP (EncodeParams a) ts -> t -> Encoding a t

--instance Encode Int where

-- utilites for conversion between Vinyl and SOP
rec2NP :: Rec f xs -> NP f xs
rec2NP RNil = Nil
rec2NP (fx :& fxs) = fx :* rec2NP fxs

np2Rec :: NP f xs -> Rec f xs
np2Rec Nil = RNil
np2Rec (fx :* fxs) = fx :& np2Rec fxs

instance {-# OVERLAPPABLE #-}
  (SingI (Code t), All2 (Encode ts) (Code t), Generic t, Neural t, Find ts t)
  => Encode ts t where
  encode :: forall a. Floating a => NP (EncodeParams a) ts -> t -> Encoding a t
  encode params t = Generic repr $ children
    where
      {-
      cliftA_SOP' :: forall f g. (forall t'. Encode t' => f t' -> g t') ->
        SOP f (Code t) -> SOP g (Code t)
      -}
      
      encode' :: (Encode ts t') => I t' -> Encoding a t'
      encode' (I t') = encode params t'

      cliftA_SOP' = cliftA_SOP (Proxy :: Proxy (Encode ts))
      children = cliftA_SOP' encode' (from t)
      
      EncodeParams param = index (find :: Index ts t) (np2Rec params)
      repr = encodeParent param (getReprSOP children)
      --childReprs = liftA_SOP getRepr 

--data DecodeParams a t = DecodeParams (Affine a (Len 


