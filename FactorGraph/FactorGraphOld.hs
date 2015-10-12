{-# LANGUAGE RecordWildCards #-}

module FactorGraph where

data Factor a = Factor {
  variables :: [Variable a], 
  f :: [a] -> a -> Double
}

data Variable a = Variable {
  values :: [a],
  factors :: [Factor a]
}

marginal :: Variable a -> [Double]
marginal Variable {..} = map (\x -> product $ [f x | f <- fs]) values
  where fs = map factor factors

factor :: Factor a -> a -> Double
factor Factor {..} = \i -> sum [(f' i) * m | (f', m) <- domain]
  where prod = sequence [zip (values var) (marginal var) | var <- variables]
        -- this variable needs a better name than "domain"
        domain = map
          (\vms ->
            let (vs, ms) = unzip vms in
            (f vs, product ms))
          prod



