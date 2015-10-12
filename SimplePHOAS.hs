{-# LANGUAGE RankNTypes #-}

module SimplePHOAS where

data Exp a = Var a | App (Exp a) (Exp a) | Lam (a -> Exp a)

showExp' vs (Var v) = v
showExp' vs (App x y) = "(" ++ (showExp' vs x) ++ " " ++ (showExp' vs y) ++ ")"
showExp' (v:vs) (Lam f) = "(\\" ++ v ++ " -> " ++ showExp' vs (f v) ++ ")"

vars = [[i] | i <- ['a'..'z']] ++ [i:show j | j <- [1..], i <- ['a'..'z']]

showExp :: Exp String -> String
showExp = showExp' vars 

joinExp :: Exp (Exp a) -> Exp a
joinExp (Var v) = v
joinExp (App x y) = App (joinExp x) (joinExp y)
joinExp (Lam f) = Lam $ \a -> joinExp $ f (Var a)

type Exp' = forall a. Exp a

getApp1 :: Exp' -> Exp'
getApp1 (App f _) = f

getApp2 :: Exp' -> Exp'
getApp2 (App _ a) = a

getLam :: Exp' -> (forall a. a -> Exp a)
getLam (Lam f) = f

{- it appears that parametricity implies totality,
   rendering normalization impossible?
-}

whnf :: Exp' -> Exp'

whnf exp@(App _ _) = helper $ whnf (getApp1 exp)
  where
    helper :: Exp' -> Exp'
    helper e@(Lam _) = whnf (joinExp $ (getLam e) (getApp2 exp))
    helper f''       = App f'' (getApp2 exp)

whnf exp = exp


