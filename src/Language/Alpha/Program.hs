module Language.Alpha.Program
  ( Type(..)
  , Program(..)
  , HeapValue(..)
  , Term(..)
  , Decl(..)
  , Value(..)
  , RightHand(..)
  ) where

import Data.List
import qualified Data.Map.Lazy as Map
import Data.Monoid
import Data.Word

data Program = Program
  { heap :: Map.Map String HeapValue
  , main :: Term
  }
  deriving (Eq, Show)

data HeapValue = Block
  { params :: [Param] -- Keys of 'Param's must be distinct.
  , body :: Term
  }
  deriving (Eq, Show)

type Param = (String, Type)

data Type
  = Int
  | Function [Type] -- CPS.
  deriving (Eq, Show)

data Term
  = Halt Value
  | App Value [Value]
  | Let Decl Term
  deriving (Eq, Show)

data Value
  = Var String
  | IntLit Word32
  deriving (Eq, Show)

data Decl = Assign String RightHand
  deriving (Eq, Show)

data RightHand
  = Mov Value
  | Bin Value BinOp Value
  deriving (Eq, Show)

data BinOp
  = Plus
  | Minus
  deriving (Eq, Show)

class Display a where
  display :: a -> String
  displays :: a -> ShowS

  display x = displays x ""

interleave :: ShowS -> [ShowS] -> ShowS
interleave x = appEndo . foldMap Endo . intersperse x

spaces :: [ShowS] -> ShowS
spaces = interleave $ showChar ' '

list :: [ShowS] -> ShowS
list = showParen True . interleave (showString ", ")

instance Display Program where
  displays p = spaces [showString "letrec", f $ heap p, showString "in", displays $ main p]
    where
      f :: Map.Map String HeapValue -> ShowS
      f = interleave (showChar '\n') . map g . Map.assocs

      g :: (String, HeapValue) -> ShowS
      g (s, h) = spaces [showString s, showString "=>", displays h]

instance Display HeapValue where
  displays b = showString "code" . list (map f $ params b) . showChar '.' . displays (body b)
    where
      f :: Param -> ShowS
      f (s, t) = showString s . showChar ':' . displays t

instance Display Type where
  displays Int           = showString "Int"
  displays (Function ts) = spaces [list $ map displays ts, showString "->", showString "void"]

instance Display Term where
  displays (Halt v)   = displays v
  displays (App v vs) = displays v . list (map displays vs)
  displays (Let d t)  = spaces [showString "let", displays d, showString "in", displays t]

instance Display Value where
  displays (Var s) = showString s
  displays (IntLit w) = shows w

instance Display Decl where
  displays (Assign s r) = spaces [showString s, showChar '=', displays r]

instance Display RightHand where
  displays (Mov v) = displays v
  displays (Bin v1 o v2) = spaces [displays v1, displays o, displays v2]

instance Display BinOp where
  displays Plus = showChar '+'
  displays Minus = showChar '-'
