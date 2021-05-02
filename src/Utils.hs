{-# LANGUAGE FlexibleInstances #-}
module Utils where

import Control.Applicative
import qualified Data.Map as M

data DataType
  = TChar
  | TShort          -- int or short int
  | TLong
  | TLongLong
  | TUChar
  | TUShort         -- unsigned int or unsigned short
  | TULong
  | TULongLong
  | TFloat
  | TDouble
  | TLongDouble
  | TPointer CType
  | TArray CType (Maybe Int)
  | TUnion (Maybe String) (M.Map String CType)
  | TStruct (Maybe String) [(CType, Maybe String, Maybe Int)]
  | TFunction String CType [CType] Bool
  | TEnum (Maybe String) (M.Map String Int)
  | TVoid
  deriving (Show, Eq)

data StorageClass
  = SCAuto
  | SCRegister
  | SCStatic
  | SCExtern
  | SCTypedef
  deriving (Show, Eq)

data TypeQualifier
  = TQConst
  | TQVolatile
  deriving (Show, Eq)

data CType =
  CType
    { storageClass  :: [StorageClass]
    , typeQualifier :: [TypeQualifier]
    , dataType      :: DataType
    }
  deriving (Show, Eq)

data SymbolTable =
  SymbolTable
    { typedef :: M.Map String CType
    , labels  :: M.Map String Coordinates
    , symbols :: M.Map String CType
    , structured :: M.Map String CType
    , parent  :: Maybe SymbolTable
    }
  deriving (Show, Eq)

data Error
  = ScanError Coordinates String
  | ParseError Coordinates String
  | SyntaxError Coordinates String
  | TypeError Coordinates String
  | InternalError Coordinates String
  | PreProcessError Coordinates String
  deriving (Show, Eq)

errorLoc :: Error -> Coordinates
errorLoc (ScanError c _) = c
errorLoc (ParseError c _) = c
errorLoc (SyntaxError c _) = c
errorLoc (TypeError c _) = c
errorLoc (InternalError c _) = c
errorLoc (PreProcessError c _) = c

errorMsg :: Error -> String
errorMsg (ScanError _ s) = s
errorMsg (ParseError _ s) = s
errorMsg (SyntaxError _ s) = s
errorMsg (TypeError _ s) = s
errorMsg (InternalError _ s) = s
errorMsg (PreProcessError _ s) = s

instance Alternative (Either Error) where
  empty = Left $ InternalError (1, 1) "empty"
  Left _ <|> e2 = e2
  e1 <|> _ = e1

type Row = Int
type Col = Int
type Coordinates = (Row, Col)

