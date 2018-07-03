module Language.Alpha.CodeGen
  (
  ) where

import Control.Monad.Trans.State.Lazy
import qualified Data.Map.Lazy as Map

import Language.Alpha.Program
import qualified Language.ELT0.Program as E

-- | The number of registers.
nreg :: Num a => a
nreg = 256

translateType :: Type -> E.Type
translateType Int = E.Int
translateType (Function ts) =
  let (xs, ys) = splitAt nreg $ map translateType ts in
    E.Code E.Env { E.binding = ["@t"], E.file = f xs, E.stack = g ys }
  where
    f = Map.fromList . zip (map E.Reg [0..])
    g = foldl (flip (:)) [E.StackVar "@t" 0] . map E.Slot

type Alloc = Map.Map String E.Reg

translateValue :: Value -> State Alloc E.Operand
translateValue (IntLit w) = return $ E.wordO $ w
translateValue (Var s)    = E.Register <$> gets (Map.findWithDefault (error "unexpected error: translateValue: missing register") s)
