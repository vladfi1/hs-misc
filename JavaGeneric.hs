{-# LANGUAGE DataKinds, PolyKinds, ConstraintKinds #-}
{-# LANGUAGE TypeFamilies, TypeOperators #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module JavaGeneric where

import Language.Java.Syntax
import Language.Java.Parser

import qualified GHC.Generics as GHC
import Generics.SOP
import Generics.SOP.TH

import Data.Constraint
--import JavaTH
import TypeLevel
import List
--  import Language.Haskell.TH

type NoConstraint = (() :: Constraint)

type family All' (c :: a -> Constraint) (x :: k) :: Constraint where
  All' c x = c x
  All' c '[] = ()
  All' c (x ': xs) = (All' c x, All' c xs)

-- use a type family to break the cyclic class
-- also to do case analysis on the type
type family AllGeneric' x where
  AllGeneric' Char = NoConstraint
  AllGeneric' Int = NoConstraint
  AllGeneric' Double = NoConstraint
  AllGeneric' Integer = NoConstraint
  AllGeneric' x = (Generic x, All2 AllGeneric (Code x))

class AllGeneric' x => AllGeneric x
instance AllGeneric' x => AllGeneric x

--deriving instance GHC.Generic CompilationUnit
--instance Generic CompilationUnit

--concat <$> (mapM deriveGeneric =<< javaSyntax)

concat <$> traverse deriveGeneric
  [''CompilationUnit
  ,''TypeDecl
  ,''PackageDecl
  ,''Name
  ,''InterfaceDecl
  ,''TypeParam
  ,''RefType
  ,''Type
  ,''PrimType
  ,''Modifier
  ,''InterfaceBody
  ,''MemberDecl
  ,''VarDecl
  ,''VarInit
  ,''VarDeclId
  ,''MethodBody
  ,''ImportDecl
  ,''Ident
  ,''FormalParam
  ,''Exp
  ,''TypeArgument
  ,''WildcardBound
  ,''Op
  ,''MethodInvocation
  ,''Literal
  ,''Lhs
  ,''FieldAccess
  ,''ConstructorBody
  ,''ExplConstrInv
  ,''ClassType
  ,''ClassDecl
  ,''ClassBody
  ,''Annotation
  ,''ArrayInit
  ,''ArrayIndex
  ,''AssignOp
  ,''Block
  ,''BlockStmt
  ,''Stmt
  ,''SwitchBlock
  ,''SwitchLabel
  ,''Decl
  ,''ElementValue
  ,''EnumBody
  ,''EnumConstant
  ,''ForInit
  ,''Catch
  ]

allGeneric :: Dict (AllGeneric CompilationUnit)
allGeneric = Dict

