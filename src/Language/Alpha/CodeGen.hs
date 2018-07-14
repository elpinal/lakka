module Language.Alpha.CodeGen
  ( graph
  ) where

import Control.Monad.Trans.State.Lazy
import Data.Bifunctor
import Data.Foldable
import Data.Functor
import qualified Data.Map.Lazy as Map
import Data.Maybe
import qualified Data.Set as Set

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
translateValue (IntLit w) = return $ E.wordO w
translateValue (Var s)    = E.Register <$> gets (Map.findWithDefault (error "unexpected error: translateValue: missing register") s)

-- Represents a pseudo register. The first component is its function name.
-- The second component is its original variable name.
data Pseudo = Pseudo (Maybe String) String
  deriving (Eq, Ord, Show)

-- Represents an interference graph.
type Graph = Map.Map Pseudo (Set.Set Pseudo)

type GraphW = Map.Map Pseudo (Weight, Set.Set Pseudo)

type Weight = Integer

type Lives = Set.Set String

regalloc :: Program -> Coloring
regalloc = coloring . mcs . init0 . graph

graph :: Program -> Graph
graph p = foldr (mergeGraph . eval . uncurry buildGraph) (eval $ termGraph Nothing $ main p) $ Map.toList $ heap p
  where
    eval = flip evalState mempty

mergeGraph :: Graph -> Graph -> Graph
mergeGraph = Map.unionWith Set.union

buildGraph :: String -> HeapValue -> State Lives Graph
buildGraph name hv = do
  g1 <- parameters $ Set.map (Pseudo $ Just name) $ Set.fromList $ map fst $ params hv
  g2 <- termGraph (Just name) $ body hv
  return $ mergeGraph g1 g2

parameters :: Set.Set Pseudo -> State s Graph
parameters u = return $ Map.fromSet (\k -> Set.delete k u) u

termGraph :: Maybe String -> Term -> State Lives Graph
termGraph _ (Halt v) = use v $> mempty
termGraph ms (Let (Assign s rh) t) = mergeGraph <$> termGraph ms t <*> letGraph ms s rh
termGraph ms (App v vs) = appGraph ms (fromMaybe (error "termGraph: internal error: application of a non-variable") $ variable v) vs

variable :: Value -> Maybe String
variable (Var s) = return s
variable (IntLit _) = Nothing

use :: Value -> State Lives ()
use = maybe (return ()) (modify . Set.insert) . variable

def :: String -> State Lives ()
def = modify . Set.delete

appGraph :: Maybe String -> String -> [Value] -> State Lives Graph
appGraph ms s = flip foldrM mempty $ \v g -> do
  u <- gets $ maybe id Set.delete $ variable v
  use v
  return $ mergeGraph g $ interfere ms u $ Pseudo (Just s) s

letGraph :: Maybe String -> String -> RightHand -> State Lives Graph
letGraph ms s (Mov v) = do
  def s
  u <- gets $ maybe id Set.delete $ variable v
  use v
  return $ interfere ms u $ Pseudo ms s
letGraph ms s (Bin v1 _ v2) = do
  def s
  u <- get
  use v1
  use v2
  return $ interfere ms u $ Pseudo ms s

interfere :: Maybe String -> Set.Set String -> Pseudo -> Graph
interfere ms u p = Map.insert p (globalize ms u) $ Map.fromSet (const $ Set.singleton p) $ globalize ms u

globalize :: Maybe String -> Set.Set String -> Set.Set Pseudo
globalize = Set.map . Pseudo

init0 :: Graph -> GraphW
init0 = Map.map $ \u -> (0, u)

-- Maximum cardinality search
mcs :: GraphW -> [(Pseudo, Set.Set Pseudo)]
mcs g = maybe [] (\(p, ns) -> (p, ns) : mcs (inc ns $ Map.delete p g)) $ heaviest g

heaviest :: GraphW -> Maybe (Pseudo, Set.Set Pseudo)
heaviest g
  | Map.null g = Nothing
  | otherwise  = Just $ second snd $ maximumBy (\x y -> fst x `compare` fst y) $ Map.toList g

inc :: Set.Set Pseudo -> GraphW -> GraphW
inc u g = Map.map (first (+ 1)) (Map.restrictKeys g u) `Map.union` Map.withoutKeys g u

type Color = Integer

type Coloring = Map.Map Pseudo Color

coloring :: [(Pseudo, Set.Set Pseudo)] -> Coloring
coloring = foldl (\m (p, u) -> Map.insert p (lowest $ Map.restrictKeys m u) m) mempty

lowest :: Coloring -> Color
lowest m = loop 0
  where
    u = Set.fromList $ Map.elems m
    loop n = if n `Set.member` u then loop (n + 1) else n
