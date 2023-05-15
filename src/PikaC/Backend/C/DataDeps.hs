-- | Data dependencies

module PikaC.Backend.C.DataDeps
  where

import PikaC.Syntax.Heaplet
import qualified PikaC.Backend.C.Syntax as C
import PikaC.Backend.C.Syntax (Command (..), CFunction (..))
import PikaC.Syntax.PikaCore.Expr (PointsToExpr)
import PikaC.Preorder.Preorder
import Data.Function

data DataDeps a =
  DataDeps
    { dataDepsReads :: [Loc a]
    , dataDepsWrites :: [Loc a]
    }

instance Semigroup (DataDeps a) where
  x <> y =
    DataDeps
      { dataDepsReads = dataDepsReads x <> dataDepsReads y
      , dataDepsWrites = dataDepsWrites x <> dataDepsWrites y
      }

instance Monoid (DataDeps a) where
  mempty = DataDeps [] []

instance LayoutRename DataDeps where
  renameLayoutArg old new d =
    DataDeps
      { dataDepsReads = map (renameLayoutArg old new) $ dataDepsReads d
      , dataDepsWrites = map (renameLayoutArg old new) $ dataDepsWrites d
      }

dataDepsLe :: Eq a => DataDeps a -> DataDeps a -> Bool
dataDepsLe x y =
  all (`notElem` dataDepsWrites y) (dataDepsReads x)

topologicalSortByDataDeps :: (Ord a, Eq b) => (a -> DataDeps b) -> [a] -> [a]
topologicalSortByDataDeps f =
  topologicalSortBy (dataDepsLe `on` f)

type FnDataDeps a = [(String, [a] -> DataDeps a)]

class HasDataDeps f where
  findDataDeps :: FnDataDeps a -> f a -> DataDeps a

instance HasDataDeps Command where
  findDataDeps fns cmd =
    DataDeps
      { dataDepsReads = dataDependencies cmd
      , dataDepsWrites = dataModified cmd
      }

restrictTo :: Eq a => [a] -> DataDeps a -> DataDeps a
restrictTo params d =
  DataDeps
    { dataDepsReads = filter ((`elem` params) . locBase) (dataDepsReads d)
    , dataDepsWrites = filter ((`elem` params) . locBase) (dataDepsWrites d)
    }

getFnDataDeps :: Ord a => FnDataDeps a -> CFunction a -> [a] -> DataDeps a
getFnDataDeps globals fn args =
  renameLayoutArg (LayoutArg (cfunctionParams fn)) (LayoutArg args)
    $ restrictTo (cfunctionParams fn)
    $ mconcat
    $ map (findDataDeps globals)
    $ cfunctionBody fn

topologicalSortPointsTo :: (Show a, Ord a) => [PointsToExpr a] -> [PointsToExpr a]
topologicalSortPointsTo =
    map to . topologicalSortBy isLe . map from
  where
    to (x, y) = x :-> y
    from (x :-> y) = (x, y)

    isLe (lhs1, rhs1) (lhs2, rhs2) =
      all (`notElem` getLocs lhs2) (getLocs rhs1)

topologicalSortCommands :: (Show a, Ord a) => [Command a] -> [Command a]
topologicalSortCommands =
    topologicalSortBy isLe
  where
    isLe cmd1 cmd2 =
      all (`notElem` dataModified cmd2) (dataDependencies cmd1)

dataDependencies :: Command a -> [Loc a]
dataDependencies (C.Assign _ rhs) = getLocs rhs
dataDependencies (C.IfThenElse c t f) =
  getLocs c
  ++ concatMap dataDependencies t
  ++ concatMap dataDependencies f
dataDependencies (C.Call _ inArgs _) = concatMap getLocs inArgs
dataDependencies (C.IntoMalloc {}) = []
dataDependencies (C.Free x) = [x :+ 0]

dataModified :: Command a -> [Loc a]
dataModified (C.Assign lhs _) = getLocs lhs
dataModified (C.IfThenElse c t f) =
  concatMap dataModified t ++ concatMap dataModified f
dataModified (C.Call _ _ outArgs) = concatMap getLocs outArgs
dataModified (C.IntoMalloc x _) = [x :+ 0]
dataModified (C.Free {}) = []

