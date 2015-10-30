{-# LANGUAGE DataKinds, PolyKinds, ConstraintKinds #-}
{-# LANGUAGE TypeFamilies, TypeOperators #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module JavaGeneric where

import Language.Java.Syntax

import Generics.SOP
import Generics.SOP.TH

import Data.Constraint
import Constraints
import List
import TypeLevel

-- use a type family to break the cyclic class
-- also to do case analysis on the type
type family AllGeneric' x :: Constraint where
  AllGeneric' Char = ()
  AllGeneric' Int = ()
  AllGeneric' Double = ()
  AllGeneric' Integer = ()
  AllGeneric' x = (Generic x, All2 AllGeneric (Code x))

class AllGeneric' x => AllGeneric x
instance AllGeneric' x => AllGeneric x

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

type PrimTypes = [Char, Int, Integer, Double]

type GenericTypes =
  [ CompilationUnit
  , TypeDecl
  , PackageDecl
  , Name
  , InterfaceDecl
  , TypeParam
  , RefType
  , Type
  , PrimType
  , Modifier
  , InterfaceBody
  , MemberDecl
  , VarDecl
  , VarInit
  , VarDeclId
  , MethodBody
  , ImportDecl
  , Ident
  , FormalParam
  , Exp
  , TypeArgument
  , WildcardBound
  , Op
  , MethodInvocation
  , Literal
  , Lhs
  , FieldAccess
  , ConstructorBody
  , ExplConstrInv
  , ClassType
  , ClassDecl
  , ClassBody
  , Annotation
  , ArrayInit
  , ArrayIndex
  , AssignOp
  , Block
  , BlockStmt
  , Stmt
  , SwitchBlock
  , SwitchLabel
  , Decl
  , ElementValue
  , EnumBody
  , EnumConstant
  , ForInit
  , Catch
  , Bool
  , [Char]
  , [(Ident, [TypeArgument])]
  , (Ident, [TypeArgument])
  , [TypeArgument]
  , [(Ident, ElementValue)]
  , (Ident, ElementValue)
  , [VarInit]
  , [VarDecl]
  , [TypeParam]
  , [TypeDecl]
  , [SwitchBlock]
  , [RefType]
  , [Modifier]
  , [MemberDecl]
  , [ImportDecl]
  , [Ident]
  , [FormalParam]
  , [Argument]
  , [EnumConstant]
  , [Decl]
  , [Catch]
  , [BlockStmt]
  , [Exp]
  , Maybe [Exp]
  , Maybe Exp
  , Maybe WildcardBound
  , Maybe VarInit
  , Maybe Type
  , Maybe RefType
  , Maybe PackageDecl
  , Maybe Ident
  , Maybe ForInit
  , Maybe ExplConstrInv
  , Maybe ClassBody
  , Maybe Block
  ]


type AllTypes = PrimTypes :++: GenericTypes

type family Codes xs where
  Codes '[] = '[]
  Codes (x ': xs) = Code x ': Codes xs

type family All3 (c :: k -> Constraint) (ksss :: [[[k]]]) :: Constraint where
  All3 c '[] = ()
  All3 c (kss ': ksss) = (All2 c kss, All3 c ksss)

type AllCodes = (TypeLevel.Concat (TypeLevel.Concat (Codes GenericTypes)))

complete :: Dict (All (Find AllTypes) AllCodes)
--complete :: Dict (All3 (Find AllTypes) (Codes GenericTypes))
complete = Dict

