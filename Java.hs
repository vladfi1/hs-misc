{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies, TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds, PolyKinds, ConstraintKinds #-}

module Java where

import Language.Java.Syntax
import Language.Java.Parser

import qualified GHC.Generics as GHC
import Generics.SOP
import Generics.SOP.TH

import Data.Constraint
import JavaGeneric
import TypeLevel
import List

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
  , Maybe [Exp]
  , [Exp]
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

complete :: Dict (All' (Find AllTypes) (Codes GenericTypes))
complete = Dict

