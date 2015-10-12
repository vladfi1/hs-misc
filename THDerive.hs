{-# LANGUAGE TemplateHaskell #-}

module THDerive where

import Language.Haskell.TH

derive :: Name -> Name -> Dec
derive class_ type_ =
  StandaloneDerivD [] (AppT (ConT class_) (ConT type_))

